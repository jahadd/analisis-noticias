# Funciones auxiliares para scraping de prensa (noticias)
# Basado en bastianolea/prensa_chile

library(glue)
library(lubridate)
library(stringr)

# Validar elementos vacíos
validar_elementos <- function(input, colapsar = FALSE) {
  if (colapsar == TRUE) {
    input2 <- paste(input, collapse = "\n")
  } else {
    input2 <- input
  }
  output <- ifelse(length(input) == 0, NA_character_, input2)
  return(output)
}

# Comprobar que la URL responda (status 200)
revisar_url <- function(url) {
  estado <- try(httr::GET(url) |> httr::status_code(), silent = TRUE)
  if (!inherits(estado, "integer")) return(NULL)
  if (estado != 200) {
    message(glue("error http en {url} (status {estado})"))
    return(NULL)
  }
  return(estado)
}

rng <- function() {
  sample(1111:9999, 1)
}

# Ruta de salida: scraping/datos/{fuente}/{fuente}_cron_{rng}_{fecha}.rds
ruta_resultado <- function(fuente, hist = "", formato = "rds") {
  if (inherits(hist, "function")) hist <- ""
  dir_out <- file.path("scraping", "datos", fuente)
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  glue::glue("{dir_out}/{fuente}_cron_{rng()}_{lubridate::today()}{hist}.{formato}")
}

# Parsea la columna fecha (texto) a Date; devuelve NA si no se puede interpretar.
# Acepta "YYYY M D", "YYYY-MM-DD", "D month YYYY", etc.
parse_fecha_noticia <- function(fecha_txt) {
  if (is.null(fecha_txt) || length(fecha_txt) == 0) return(as.Date(NA))
  x <- as.character(fecha_txt)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(as.Date(NA))
  x <- trimws(x[1])
  out <- suppressWarnings(
    lubridate::parse_date_time(x, orders = c("ymd", "dmy", "dby", "Ymd", "Y m d"), quiet = TRUE)
  )
  if (length(out) == 0 || is.na(out)) return(as.Date(NA))
  as.Date(out)
}

# Filtra resultados para conservar solo noticias con fecha >= SCRAPING_FECHA_DESDE (ej. 2016-01-01).
filtrar_desde_fecha <- function(resultados, fecha_desde_env = "SCRAPING_FECHA_DESDE") {
  fecha_desde <- Sys.getenv(fecha_desde_env, "")
  if (!nzchar(fecha_desde)) return(resultados)
  lim <- tryCatch(as.Date(fecha_desde), error = function(e) as.Date(NA))
  if (is.na(lim)) return(resultados)
  if (!inherits(resultados, "data.frame") || !"fecha" %in% names(resultados)) return(resultados)
  fechas_parsed <- vapply(resultados$fecha, function(f) {
    d <- parse_fecha_noticia(f)
    is.na(d) || d >= lim
  }, FUN.VALUE = logical(1))
  resultados[fechas_parsed, , drop = FALSE]
}

# Guardar noticias: si GUARDAR_NOTICIAS_EN=postgres → insertar en PostgreSQL noticias_chile;
# si no → escribir RDS en scraping/datos/{fuente}/ (comportamiento anterior).
# Requiere RPostgres y variables PGHOST, PGPORT, PGUSER, PGPASSWORD (y opcional PGDATABASE_NOTICIAS o PGDATABASE).
guardar_noticias <- function(resultados, fuente, hist = "") {
  if (inherits(hist, "function")) hist <- ""
  if (is.null(resultados) || (inherits(resultados, "data.frame") && nrow(resultados) == 0)) {
    message(glue("  {fuente}: 0 noticias, nada que guardar."))
    return(invisible(0L))
  }
  # Filtrar por SCRAPING_FECHA_DESDE (ej. solo desde 1 ene 2016)
  resultados <- filtrar_desde_fecha(resultados)
  if (nrow(resultados) == 0) {
    message(glue("  {fuente}: 0 noticias después de filtrar desde fecha, nada que guardar."))
    return(invisible(0L))
  }
  if (Sys.getenv("GUARDAR_NOTICIAS_EN") != "postgres") {
    dir_out <- file.path("scraping", "datos", fuente)
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
    ruta <- ruta_resultado(fuente, hist)
    readr::write_rds(resultados, ruta)
    message(glue("Listo {fuente}: {nrow(resultados)} noticias → {ruta} ({lubridate::now()})"))
    return(invisible(nrow(resultados)))
  }
  # Guardar en PostgreSQL noticias_chile (tabla noticias_crudo)
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Para guardar en Postgres instale: install.packages('RPostgres')")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) stop("Instale: install.packages('DBI')")
  dbname <- Sys.getenv("PGDATABASE_NOTICIAS", Sys.getenv("PGDATABASE", "noticias_chile"))
  tabla  <- "noticias_crudo"
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = dbname,
    host     = Sys.getenv("PGHOST", "localhost"),
    port     = as.integer(Sys.getenv("PGPORT", "5432")),
    user     = Sys.getenv("PGUSER", ""),
    password = Sys.getenv("PGPASSWORD", "")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql_create <- glue::glue("
    CREATE TABLE IF NOT EXISTS {tabla} (
      id               SERIAL PRIMARY KEY,
      titulo           TEXT,
      bajada           TEXT,
      fecha            TEXT,
      cuerpo           TEXT,
      fuente           VARCHAR(100) NOT NULL,
      url              TEXT NOT NULL,
      fecha_scraping   TIMESTAMPTZ,
      created_at       TIMESTAMPTZ DEFAULT now(),
      UNIQUE(url, fuente)
    );
  ")
  DBI::dbExecute(con, sql_create)
  cols <- c("titulo", "bajada", "fecha", "cuerpo", "fuente", "url", "fecha_scraping")
  datos <- as.data.frame(resultados)
  for (c in cols) if (!c %in% names(datos)) datos[[c]] <- NA
  datos <- datos[, cols]
  datos$fuente <- fuente
  if (!inherits(datos$fecha_scraping, "POSIXct")) {
    datos$fecha_scraping <- as.POSIXct(datos$fecha_scraping, origin = "1970-01-01", tz = "UTC")
    datos$fecha_scraping[is.na(datos$fecha_scraping)] <- as.POSIXct(Sys.time(), tz = "UTC")
  }
  tmp <- "noticias_crudo_tmp"
  DBI::dbWriteTable(con, tmp, datos, temporary = TRUE, overwrite = TRUE)
  sql_insert <- paste0("
    INSERT INTO ", tabla, " (titulo, bajada, fecha, cuerpo, fuente, url, fecha_scraping)
    SELECT titulo, bajada, fecha, cuerpo, fuente, url, fecha_scraping FROM ", tmp, "
    ON CONFLICT (url, fuente) DO UPDATE SET
      titulo = EXCLUDED.titulo,
      bajada = EXCLUDED.bajada,
      fecha = EXCLUDED.fecha,
      cuerpo = EXCLUDED.cuerpo,
      fecha_scraping = EXCLUDED.fecha_scraping
  ")
  DBI::dbExecute(con, sql_insert)
  n <- nrow(datos)
  message(glue("Listo {fuente}: {n} noticias → Postgres {dbname}.{tabla} ({lubridate::now()})"))
  invisible(n)
}

# Lanzar script de scraping (asinc = RStudio Background Job, secuencial = source en la misma sesión)
# ejecucion "secuencial" para medios que usan Chromote/Chrome headless (comparten un mismo navegador)
scraping_prensa <- function(script,
                            ruta = "scraping/fuentes/",
                            ejecucion = "asinc") {
  message(glue("iniciando {script} - {format(now(), '%d/%m/%y %H:%M:%S')}"))
  path_script <- paste0(ruta, script)
  if (ejecucion == "asinc") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::jobRunScript(path_script, workingDir = getwd())
    } else {
      source(path_script, local = new.env())
    }
  } else if (ejecucion == "secuencial") {
    source(path_script, local = new.env())
  }
}

# Notificación de escritorio (macOS: osascript; en otros SO solo message)
notificacion <- function(titulo = "Título", texto = "texto") {
  message(titulo, ": ", texto)
  try(system(paste0("osascript -e 'display notification \"", texto, "\" with title \"", titulo, "\"'")), silent = TRUE)
}

# Revisar que cada carpeta de fuente tenga archivos recientes (> 10kb, modificados hoy)
revisar_resultados <- function(ruta = "scraping/datos") {
  if (!dir.exists(ruta)) {
    message("No existe la carpeta ", ruta)
    return(invisible(NULL))
  }
  dirs <- list.dirs(ruta, full.names = TRUE, recursive = FALSE)
  for (x_carpeta in dirs) {
    Sys.sleep(0.05)
    nombre_fuente <- str_extract(x_carpeta, "[^/\\\\]+$")
    fls <- list.files(x_carpeta, full.names = TRUE)
    if (length(fls) == 0) {
      message("ERROR ", nombre_fuente, " (carpeta vacía)")
      next
    }
    revision <- tryCatch(subset(file.info(fls), size > 10000), error = function(e) data.frame(ctime = numeric(0)))
    if (nrow(revision) == 0) {
      message("ERROR ", nombre_fuente, " (sin archivos > 10kb)")
    } else {
      ultimo <- max(as.Date(revision$ctime))
      if (!is.na(ultimo) && ultimo == lubridate::today()) {
        message(nombre_fuente, " OK")
      } else {
        message("ERROR ", nombre_fuente, " (sin archivos de hoy)")
      }
    }
  }
}
