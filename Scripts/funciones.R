
# palabras ----
stopwords <- tryCatch(readr::read_lines("datos/stopwords_es.txt"), error = function(e) character(0L)) #tidytext::get_stopwords("es") |> pull(word)

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", "comunidad", "personas",
                          "región",
                          "año", "años", "añosa", "añosen",
                          "país", "persona", "comunicación", "señor",
                          "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre",
                          "youtube", "aaron",
                          "leer", "artículo", "completo", "articular", "completar", # cooperatva ("leer articulo completo")
                          "publicación", # elmostrador
                          "mercer", #cnnchile y otros
                          "detallar", # meganoticias
                          "cabe", "pese", "abc", "abcdin"
)

palabras_basura = c("aaa", "aba", "aaisa", "aap", "aas",
                    "RelacionadasDetalle", "sd", "hd",
                    "TradingView", "Widget BEGIN", "Widget END",
                    "jpg", "like", "new", "child", "https", "length", "https", "http", "domcontentloaded", "flexdirection", "firstdiv", "pointer", "addeventlistener", "queryselector", "marginbottom", "containers", "lastdiv", "foreach", "innerwidth",
                    "right", "left", "top", "align", "gnews", "px", "twitter", "com", "pic", "font", "height", "width",
                    "pred", "fs", "us", "april", "flickr", "datawrapper", "data", "fried", "ftx", "medium", "exante", "server", "family", "loc", "lon", "mag", "prof", "lat", "gpt", "banner", "donación",
                    "style", "relacionadasdetalle", "null",
                    "containerstyleflexdirection", "firstdivstylemarginbottom", "first", "div", "formstyleflexdirection", "formulario", "style", "flex", "last", "cursor", 
                    "document", "deventlistenerdomloedevent", "query", "selector", "all", "responsive", "firstchild",
                    "tablaennotici", "notatablaemol", "public",
                    "demr", "detalleicon", "important", "notatablaemol", "tablaennotici", "tdnthchildn", "arial",
                    "updatelayout", "function", "formulario", "transmision", "containerstyleflexdirection", "firstdivstylemarginbottom", 
                    "fontsize", "backgroundcolor", "display", "padding", "width", "lineheight", "ifslength", "kok", "queryselectorall", "queryselectorinputtyp",
                    "elements", "submit", "www", "ficon",
                    "alignitems", "margin", "left", "sans", "serif", "height", "margin", "top",
                    "aton", "emolmlt", "font", "border", "margin", #emol
                    "rectangle", "container", "img", "display", "sans", "end", "weight", "content", "rem", "flex", "border", "bottom", "margin", "padding", "center", 
                    "radius", "text", "síguenos", "solid", "items", "dadada", "droidsans", "justify", "serif", "push", "function", "cmd", "div", "googletag", "ad",
                    "protected", "email",
                    "aach", "aafp", "aafdf") |> unique()

palabras_eliminar = c(palabras_irrelevantes,
                      palabras_basura)



# —----

ejecutar <- function(script = "modulos/cron_elsiglo.r", 
                     esperar = TRUE) {
  # browser()
  ruta_log = script |> 
    stringr::str_replace("\\.r$", ".log") |> 
    stringr::str_replace("modulos/", "logs/")
  
  invisible(suppressWarnings(file.remove(ruta_log)))
  
  comando <- paste0("/usr/local/bin/Rscript ", script, " >> ", ruta_log, " 2>&1")
  # comando <- paste0("nohup Rscript ", script, " >> ", ruta_log, " 2>&1")
  
  Sys.sleep(0.1)
  system(comando, wait = esperar)
}


scraping_prensa <- function(script = "cron_radiopaulina.r",
                            ruta = "scraping/fuentes/", 
                            ejecucion = "asinc") {
  message(glue::glue("iniciando {script} - {format(now(), '%d/%m/%y %H:%M:%S')}"))
  
  if (ejecucion == "asinc") {
  ### en subproceso (sesión) interactivo (RStudio Background Job, compatible con cualquier sistema operativo)
  rstudioapi::jobRunScript(paste0(ruta, script), workingDir = getwd())
    
  } else if (ejecucion == "secuencial") {
    source(paste0(ruta, script))  
  }
  
  ###
  # # en subproceso (callr) no interactivo
  # proc <- callr::r_bg(\(script) source(script), 
  #                     supervise = TRUE,
  #                     stderr = stringr::str_replace(script, "\\.r$", "\\.log"),
  #                     stdout = stringr::str_replace(script, "\\.r$", "\\.log")
  #                     )
  
  ### 
  # ejecutar script en el fondo no interactivo (sólo en macOS, probablemente en Linux)
  # ejecutar(paste0("/Users/baolea/R/prensa/", script))
}



beep_n <- function(x = 3) {
  walk(1:3, ~{beep(1); Sys.sleep(0.15)}) 
}

# 
# iniciar_selenium <- function(espera = 10, puerto = 4445L) {
#   message("iniciando contenedor en puerto ", puerto)
#   ##detener contenedores
#   #system("docker stop $(docker ps -a -q)", wait = T, timeout = 10, ignore.stdout = T)
#   #Sys.sleep(espera)
#   #iniciar nuevo contenedor
#   system(glue::glue('docker run -d -p {puerto}:4444 selenium/standalone-firefox'), 
#          wait = T, timeout = 10)#, ignore.stdout = T)
#   Sys.sleep(espera)
# }
# 
# 
# reiniciar_selenium <- function(espera = 5) {
#   message("reiniciando contenedores")
#   #detener contenedores
#   system("docker stop $(docker ps -a -q)", wait = T, timeout = 10, ignore.stdout = T)
#   Sys.sleep(espera)
#   #iniciar nuevo contenedor
#   system('docker run -d -p 4445:4444 selenium/standalone-firefox', wait = T, timeout = 10, ignore.stdout = T)
#   Sys.sleep(espera)
# }
# 
# 
# ver_contenedores <- function() {
#   system("docker ps")
# }
# 
# cerrar_contenedores <- function() {
#   message("cerrando todos los contenedores")
#   system("docker stop $(docker ps -a -q)", ignore.stderr = T)
#   Sys.sleep(5)
#   system("docker rm $(docker ps -a -q)", ignore.stderr = T)
# }
# #cerrar_contenedores()


#cambiar elementos sin contenido por missing
validar_elementos <- function(input, colapsar = FALSE) {
  
  if (colapsar == TRUE) {
    input2 <- paste(input, collapse = "\n")
  } else {
    input2 <- input
  }
  
  output <- ifelse(length(input) == 0, NA_character_, input2)
  
  return(output)
}



intentar <- function(x, nombre = "prueba") {
  tryCatch(x, 
           error = function(e) message("Error en ", nombre, ": ", e), 
           finally = message("OK ", nombre))
}

# scraping_prensa <- function(f = "cron_radiopaulina.r") {
#   message("iniciando ", f, " - ", lubridate::now())
#   #here(glue("scraping/{f}")) |> source() |> intentar(f) 
#   glue("scraping/{f}") |> source() |> intentar(f) 
#   #intentar(source(here("scraping/cron_tarapacaonline.r")), "tarapacaonline")
# }




revisar_resultados <- function(ruta) {
  walk(list.dirs(ruta, full.names = T, recursive = F), ~{
    Sys.sleep(0.05)
    #x_carpeta <- carpetas[1]
    x_carpeta <- .x
    
    revision <- x_carpeta |> 
      list.files(full.names = T) |> 
      file.info() |> 
      tibble::tibble() |> 
      filter(size > 10000)
    
    #mensaje
    if (max(revision$ctime) |> as.Date() == lubridate::today()) {
      message(x_carpeta |> stringr::str_extract("\\w+$"), " OK")
    } else {
      message("ERROR ", x_carpeta |> stringr::str_extract("\\w+$"))  
    }
  })
}
# revisar_resultados("resultados")


continuar_si_hay_enlaces <- function(enla, num = 3) {
  #continuar sólo si hay enlaces
  if (length(enla) <= num) {
    message("enlaces insuficientes, terminando")
    return(FALSE)
    #q()
  } else {
    message(glue("{length(enla)} enlaces obtenidos"))
    return(TRUE)
  } 
}

revisar_url <- function(url) {
  # url <- "https://www.eldinamo.cl/pais/2023/03/14/empresarios-agroindustriales-forman-consejo-empresarial-sectorial-para-contribuir-a-la-formacion-tp-y-la-empleabilidad-del-rubro/"
  estado <- url |> 
    httr::GET() |> 
    httr::status_code() |> 
    try()
  
  if (class(estado) != "integer") return(NULL)
  
  message(url, " (estado: ", estado, ")") |> try()
  
  if (estado != 200) {
    message(glue("error http en {url}"))
    return(NULL)
  } else {
    return(estado)
  }
}

# 
# #ejecutar procesos en el fondo
# source_bg <- function(file) {
#   log <- stringr::str_remove(file, "\\w+$") |> paste0("log")
#   system(glue::glue("Rscript '{file}' >> '{log}' 2>&1"), wait = F)
# }
# 
# 
# 
# #definir si es local o nacional
# 
# definir_escala <- function(x) {
#   prensa_nacional = c("adnradio", "agricultura", "cnnchile",
#                       "cooperativa_pais", "elciudadano", "elmostrador",
#                       "latercera_pais", "t13", "biobio_pais",
#                       "lahora",
#                       "emol_pais", "diariofinanciero_pais")
#   
#   if (x %in% prensa_nacional) {
#     y = "nacional" 
#   } else {
#     y = "local"
#   }
#   return(y)
# }
# 
# selenium_crear_driver <- function(puerto) {
#   remoteDriver(remoteServerAddr = "localhost", port = puerto, browserName = "firefox")
# }

# 




limpiar_texto_poquito <- function(x) {
  x |> 
    str_replace_all("dfp:|\\n|\\r", " ") |> 
    str_trim() |> 
    str_squish()
}


limpiar_texto <- function(x) {
  
  palabras_basura <- paste("\\b", palabras_basura, "\\b", sep = "") |> 
    paste(collapse = "|")
  
  x |> 
    # eliminar código
    str_replace_all("\\{.*\\}", " ") |>
    # eliminar hashtags
    str_replace_all("\\#.*\\b", " ") |>
    # eliminar código ciper
    str_replace_all("var divElement.*\\);", " ") |> 
    str_remove_all("\\{\\{.*\\}\\}") |> 
    # es mejor convertir a espacios que eliminar, porque así se separan de la anterior/siguiente palabra
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("[[:digit:]]", " ") |>
    str_replace_all("\\||\\<|\\>|@|-|—|\\{|\\}|\\[|\\]|\\=|“", " ") |>
    textclean::strip() |> 
    # tolower() |> 
    str_replace_all(palabras_basura, " ") |> 
    str_squish() |> 
    str_trim()
}



revisar_scraping <- function(data) {
  try({
    message(paste("listo", deparse(substitute(data)), "-", lubridate::now()))
    if ("tbl" %in% class(data)) message(paste(nrow(data), "noticias obtenidas"))
  })
}

# ==============================================================================
# Funciones de base de datos (PostgreSQL)
# ==============================================================================

conectar_db <- function() {
  for (env_file in c(".env", "../.env", "../../.env")) {
    if (file.exists(env_file)) {
      for (line in readLines(env_file, warn = FALSE)) {
        line <- sub("^[[:space:]]*#.*$", "", line)
        if (!nzchar(trimws(line))) next
        if (grepl("^PGHOST=", line))              Sys.setenv(PGHOST              = sub("^PGHOST=([^[:space:]]+).*", "\\1", line))
        if (grepl("^PGPORT=", line))              Sys.setenv(PGPORT              = sub("^PGPORT=([^[:space:]]+).*", "\\1", line))
        if (grepl("^PGUSER_NOTICIAS=", line))     Sys.setenv(PGUSER_NOTICIAS     = sub("^PGUSER_NOTICIAS=[\"']?([^\"']*)[\"']?$",     "\\1", line))
        if (grepl("^PGPASSWORD_NOTICIAS=", line)) Sys.setenv(PGPASSWORD_NOTICIAS = sub("^PGPASSWORD_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
        if (grepl("^PGDATABASE_NOTICIAS=", line)) Sys.setenv(PGDATABASE_NOTICIAS = sub("^PGDATABASE_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
        if (grepl("^PGUSER=", line))              Sys.setenv(PGUSER              = sub("^PGUSER=[\"']?([^\"']*)[\"']?$",              "\\1", line))
        if (grepl("^PGPASSWORD=", line))          Sys.setenv(PGPASSWORD          = sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$",          "\\1", line))
        if (grepl("^PGDATABASE=", line))          Sys.setenv(PGDATABASE          = sub("^PGDATABASE=[\"']?([^\"']*)[\"']?$",          "\\1", line))
      }
      break
    }
  }
  pg_user     <- Sys.getenv("PGUSER_NOTICIAS")
  if (!nzchar(pg_user))     pg_user     <- Sys.getenv("PGUSER",     "noticias")
  pg_password <- Sys.getenv("PGPASSWORD_NOTICIAS")
  if (!nzchar(pg_password)) pg_password <- Sys.getenv("PGPASSWORD")
  pg_db       <- Sys.getenv("PGDATABASE_NOTICIAS")
  if (!nzchar(pg_db))       pg_db       <- Sys.getenv("PGDATABASE", "noticias_chile")
  if (!nzchar(pg_password)) stop("Definir PGPASSWORD_NOTICIAS o PGPASSWORD en el entorno o en .env")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("PGHOST", "localhost"),
    port     = as.integer(Sys.getenv("PGPORT", "5432")),
    user     = pg_user,
    password = pg_password,
    dbname   = pg_db
  )
}

ya_scrapeado_en_db <- function(url, con) {
  tryCatch(
    nrow(DBI::dbGetQuery(con, "SELECT 1 FROM noticias WHERE url = $1 LIMIT 1", params = list(url))) > 0L,
    error = function(e) FALSE
  )
}

guardar_noticias_en_postgres <- function(df, con) {
  if (is.null(df) || nrow(df) == 0L) {
    message("guardar_noticias_en_postgres: sin datos para guardar.")
    return(invisible(0L))
  }
  cols_requeridas <- c("titulo", "fecha", "url", "fuente")
  faltantes <- setdiff(cols_requeridas, names(df))
  if (length(faltantes) > 0L) stop("Faltan columnas: ", paste(faltantes, collapse = ", "))

  if (!"bajada"         %in% names(df)) df$bajada         <- NA_character_
  if (!"cuerpo"         %in% names(df)) df$cuerpo         <- NA_character_
  if (!"fecha_scraping" %in% names(df)) df$fecha_scraping <- Sys.Date()

  df$titulo        <- as.character(df$titulo)
  df$url           <- as.character(df$url)
  df$fuente        <- as.character(df$fuente)
  df$bajada        <- as.character(df$bajada)
  df$cuerpo        <- as.character(df$cuerpo)
  df$fecha         <- as.character(as.Date(df$fecha))
  df$fecha_scraping <- as.character(as.Date(df$fecha_scraping))

  df <- df[!is.na(df$url) & nzchar(df$url) & !is.na(df$fuente) & !is.na(df$fecha), ]
  if (nrow(df) == 0L) {
    message("guardar_noticias_en_postgres: todas las filas tienen url/fuente/fecha NA.")
    return(invisible(0L))
  }

  # Deduplicar por (url, fuente) para evitar ON CONFLICT DO UPDATE sobre filas duplicadas
  df <- df[!duplicated(paste(df$url, df$fuente, sep = "|||")), ]

  df$id <- vapply(seq_len(nrow(df)), function(i) {
    digest::digest(paste0(df$url[i], "_", df$fuente[i]), algo = "md5", serialize = FALSE)
  }, character(1L))

  df$cuerpo_limpio <- vapply(df$cuerpo, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
    tryCatch(limpiar_texto(x), error = function(e) NA_character_)
  }, character(1L))

  df$año <- as.integer(format(as.Date(df$fecha), "%Y"))

  staging <- df[, c("id", "titulo", "bajada", "cuerpo", "cuerpo_limpio",
                    "fecha", "fecha_scraping", "url", "fuente", "año")]

  DBI::dbExecute(con, "DROP TABLE IF EXISTS _stg_noticias_scraping")
  DBI::dbWriteTable(con, "_stg_noticias_scraping", staging, temporary = FALSE, overwrite = TRUE)
  DBI::dbExecute(con, "ALTER TABLE _stg_noticias_scraping ALTER COLUMN fecha          TYPE DATE USING fecha::date")
  DBI::dbExecute(con, "ALTER TABLE _stg_noticias_scraping ALTER COLUMN fecha_scraping TYPE DATE USING fecha_scraping::date")

  n <- DBI::dbExecute(con, "
    INSERT INTO noticias (id, titulo, bajada, cuerpo, cuerpo_limpio, fecha, fecha_scraping, url, fuente, año)
    SELECT id, titulo, bajada, cuerpo, cuerpo_limpio, fecha, fecha_scraping, url, fuente, año
    FROM _stg_noticias_scraping
    WHERE id IS NOT NULL AND titulo IS NOT NULL AND url IS NOT NULL AND fuente IS NOT NULL AND fecha IS NOT NULL
    ON CONFLICT (url, fuente) DO UPDATE SET
      titulo        = EXCLUDED.titulo,
      bajada        = EXCLUDED.bajada,
      cuerpo        = EXCLUDED.cuerpo,
      cuerpo_limpio = EXCLUDED.cuerpo_limpio,
      fecha_scraping = EXCLUDED.fecha_scraping,
      año           = EXCLUDED.año
  ")

  DBI::dbExecute(con, "DROP TABLE IF EXISTS _stg_noticias_scraping")
  message("guardar_noticias_en_postgres: ", n, " filas insertadas/actualizadas.")
  invisible(n)
}

# ==============================================================================
# Helpers de pipeline
# ==============================================================================

# Retorna cuántos días debe cubrir el scraper actual.
# Si el pipeline exportó DIAS_SCRAPING, usa ese valor;
# si no, usa el default (3 días) para que el scraper funcione en modo independiente.
dias_a_scrapear <- function(default = 3L) {
  val <- suppressWarnings(as.integer(Sys.getenv("DIAS_SCRAPING", "")))
  if (!is.na(val) && val >= 1L) val else as.integer(default)
}

# Fecha mínima a cubrir (día de la última noticia en BD), o NULL si no está disponible.
fecha_desde_scraping <- function() {
  val <- Sys.getenv("FECHA_DESDE_SCRAPING", "")
  if (nzchar(val)) tryCatch(as.Date(val), error = function(e) NULL) else NULL
}

# ==============================================================================

recodificar_fuentes <- function(data) {
  data |> 
    mutate(fuente = case_match(fuente,
                               "24horas" ~ "24 Horas",
                               "adnradio" ~ "ADN Radio",
                               "agricultura" ~ "Agricultura",
                               "biobio" ~ "Radio BíoBío",
                               "chvnoticias" ~ "CHV Noticias",
                               "ciper" ~ "Ciper",
                               "cnnchile" ~ "CNN Chile",
                               "cooperativa" ~ "Cooperativa",
                               "diariofinanciero" ~ "D. Financiero",
                               "elciudadano" ~ "El Ciudadano",
                               "eldinamo" ~ "El Dínamo",
                               "elmostrador" ~ "El Mostrador",
                               "elsiglo" ~ "El Siglo",
                               "emol" ~ "Emol",
                               "exante" ~ "Ex-Ante",
                               "lacuarta" ~ "La Cuarta",
                               "lahora" ~ "La Hora",
                               "lanacion" ~ "La Nación",
                               "latercera" ~ "La Tercera",
                               "meganoticias" ~ "Meganoticias",
                               "publimetro" ~ "Publimetro",
                               "radiouchile" ~ "Radio U. de Ch.",
                               "t13" ~ "T13",
                               "theclinic" ~ "The Clinic", 
                               "redgol" ~ "RedGol",
                               "lasegunda" ~ "La Segunda",
                               "eldesconcierto" ~ "El Desconcierto",
                               "quintopoder" ~ "El Quinto Poder",
                               "izquierdadiario" ~ "La Izquierda Diario",
                               .default = fuente))
}

redactar_fecha <- function(x) { 
  mes = lubridate::month(x)
  mes_t = recode(mes, 
                 "1" = "enero",
                 "2" = "febrero",
                 "3" = "marzo",
                 "4" = "abril",
                 "5" = "mayo",
                 "6" = "junio",
                 "7" = "julio",
                 "8" = "agosto",
                 "9" = "septiembre",
                 "10" = "octubre",
                 "11" = "noviembre",
                 "12" = "diciembre")
  
  fecha_etiqueta = paste(lubridate::day(x), "de", mes_t)
  return(fecha_etiqueta)
}

mes_a_numero <- function(x) {
  recode(x, 
         "enero" = "1",
         "febrero" = "2",
         "marzo" = "3",
         "abril" = "4",
         "mayo" = "5",
         "junio" = "6",
         "julio" = "7",
         "agosto" = "8",
         "septiembre" = "9",
         "octubre" = "10",
         "noviembre" = "11",
         "diciembre" = "12")
}

# sólo funciona en macOS
notificacion <- function(titulo = "Título", texto = "texto") {
  # system("osascript -e 'display notification \"Datos de noticias descargados\" with title \"Scraping de prensa\"'")
  message(titulo, ": ", texto)
  
  system(
    paste0("osascript -e 'display notification \"", texto, "\" with title \"", titulo, "\"'")
  ) |> try()
}

# notificacion("Scraping de prensa", "Datos de noticias descargados")


cargar_modelo_udpipe <- function(
    ruta = file.path("datos", "modelos", "spanish-ancora-ud-2.5-191206.udpipe")
) {
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    message("udpipe no disponible — lematización desactivada")
    return(NULL)
  }
  if (!file.exists(ruta)) {
    message("Modelo udpipe no encontrado en: ", ruta, " — lematización desactivada")
    return(NULL)
  }
  udpipe::udpipe_load_model(ruta)
}

lematizar_titulos <- function(titulos, ids, modelo_udpipe,
                               upos_permitidos = c("NOUN", "PROPN", "ADJ", "VERB"),
                               min_len = 3L) {
  if (is.null(modelo_udpipe)) return(NULL)
  anotado <- tryCatch(
    as.data.frame(
      udpipe::udpipe_annotate(modelo_udpipe, x = titulos, doc_id = as.character(ids)),
      stringsAsFactors = FALSE
    ),
    error = function(e) { message("Error udpipe: ", e$message); NULL }
  )
  if (is.null(anotado) || nrow(anotado) == 0L) return(NULL)
  anotado <- anotado[!is.na(anotado$lemma) & !is.na(anotado$upos), ]
  anotado <- anotado[anotado$upos %in% upos_permitidos, ]
  anotado <- anotado[nchar(anotado$lemma) >= min_len, ]
  anotado$lemma <- tolower(trimws(anotado$lemma))
  anotado[, c("doc_id", "lemma")]
}

rng <- function() {
  sample(1111:9999, 1)
}


ruta_resultado <- function(fuente = "latercera", hist = "", formato = "rds") {
  if (class(hist)[1] == "function") hist <- ""
  glue::glue("scraping/datos/{fuente}/{fuente}_cron_{rng()}_{lubridate::today()}{hist}.{formato}")
}


modulos_n <- function() {
  fs::dir_ls("scraping/datos") |> length()
}

sin_cambios_hoy <- function() {
  directorios <- fs::dir_info("scraping/datos") |> 
    arrange(desc(modification_time))
  
  # directorios sin cambios hoy
  sin_cambios <- directorios |> 
    filter(modification_time < lubridate::today()) |> 
    mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
    select(fuente, size, modification_time)
  
  return(sin_cambios)
}


estimar_tiempo <- function(muestra, estimacion = 4.9) {
  message(paste("tiempo aproximado de procesamiento:", round((muestra * estimacion)/60/60, 1), "horas")) 
}

detencion_manual <- function() {
  read.delim("otros/stop.txt", header = FALSE)[[1]] == "stop"
}
# if detencion_manual() return(NULL)

# mensaje_segundos <- function(...) {
#   message("(", seconds(round(..., 1)) |> as.numeric(), " segundos)")
# }

mensaje_segundos <- function(palabras, tiempo) {
  segundos = seconds(round(tiempo, 1)) |> as.numeric()
  palabras_segundos = round(palabras/segundos, 0)
  
  message("    ",  segundos, " segundos, ",
          palabras_segundos, " palabras/seg.")
}


fecha_limite <- function() {
  floor_date(today(), unit = "week", week_start = 7) # domingo que termina la semana, para prensa semanal
}



# para no scrapear si ya se obtuvo
revisar_scrapeado <- function(enlace) {
  
  # datos generados en p1
  scrapeados <- readRDS("otros/urls.rds")
  
  # comparar
  veredicto <- enlace %in% scrapeados
  
  if (veredicto) {
    message("url repetida")
  }
  
  return(veredicto)
}
