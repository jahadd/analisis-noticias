#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_scraping_datamedios.R
# Scrapeo de noticias chilenas con el paquete datamedios: todos los meses,
# desde 2015 hasta 2026, todas las fuentes ("todas").
# Inserta en PostgreSQL (tabla noticias). No sube a la BD interna del paquete.
#
# Requisitos: install.packages(c("datamedios", "DBI", "RPostgres", "lubridate"))
# Variables de entorno: PGUSER, PGPASSWORD, PGHOST, PGPORT, PGDATABASE
#
# Orden recomendado:
#   1. Vaciar BD: psql -U noticias -d noticias_chile -f vaciar_db.sql
#   2. Ejecutar: Rscript run_scraping_datamedios.R
#   3. Análisis: Rscript run_analisis_titulos.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(datamedios)
  library(DBI)
  library(RPostgres)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Configuración
# ------------------------------------------------------------------------------
PGHOST     <- Sys.getenv("PGHOST",     "localhost")
PGPORT     <- as.integer(Sys.getenv("PGPORT", "5432"))
PGUSER     <- Sys.getenv("PGUSER",     "noticias")
PGPASSWORD <- Sys.getenv("PGPASSWORD")
PGDATABASE <- Sys.getenv("PGDATABASE", "noticias_chile")

if (!nzchar(PGPASSWORD)) stop("Definir PGPASSWORD (ej. export PGPASSWORD='...')")

# Rango: todos los meses desde 2015-01 hasta 2026-12
FECHA_INICIO <- as.Date("2015-01-01")
FECHA_FIN    <- as.Date("2026-12-31")

# Frases de búsqueda: cada una hace una llamada por mes. Menos queries = proceso más rápido.
# Las que suelen devolver 0 en muchos meses (economia, sociedad, tecnologia, pais, region,
# mundo, salud, etc.) disparan el warning "replacement has 1 row, data has 0" del paquete y
# consumen tiempo sin aportar filas. Se deja un núcleo que suele dar resultados.
# Puedes añadir más (ej. "economia", "tecnologia") si prefieres más cobertura a cambio de tiempo.
SEARCH_QUERIES <- c(
  "chile",
  "actualidad",
  "politica",
  "deportes",
  "farandula",
  "cultura",
  "noticias",
  "internacional"
)

# Fuentes: "todas" según documentación (bbcl + fuentes Emol)
FUENTES <- "todas"

# Pausa entre cada query (segundos) para no saturar los sitios
PAUSA_ENTRE_QUERIES <- 2

# ------------------------------------------------------------------------------
# Conexión PostgreSQL
# ------------------------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  host = PGHOST,
  port = PGPORT,
  user = PGUSER,
  password = PGPASSWORD,
  dbname = PGDATABASE
)
on.exit(dbDisconnect(con), add = TRUE)
message("Conectado a ", PGDATABASE)

# ------------------------------------------------------------------------------
# Mapear dataframe de datamedios a columnas de la tabla noticias
# El paquete devuelve columnas como titulo, contenido, url, fecha, medio (ver doc)
# ------------------------------------------------------------------------------
mapear_a_noticias <- function(df, search_query) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  nms <- tolower(names(df))
  # Resolver nombres posibles
  titulo     <- if ("titulo" %in% nms) df[["titulo"]] else if ("title" %in% nms) df[["title"]] else rep(NA_character_, nrow(df))
  contenido  <- if ("contenido" %in% nms) df[["contenido"]] else if ("content" %in% nms) df[["content"]] else rep(NA_character_, nrow(df))
  url        <- if ("url" %in% nms) df[["url"]] else if ("link" %in% nms) df[["link"]] else rep(NA_character_, nrow(df))
  fecha      <- if ("fecha" %in% nms) df[["fecha"]] else rep(NA_character_, nrow(df))
  medio      <- if ("medio" %in% nms) df[["medio"]] else if ("fuente" %in% nms) df[["fuente"]] else rep("desconocido", nrow(df))
  resumen    <- if ("resumen" %in% nms) df[["resumen"]] else rep(NA_character_, nrow(df))
  autor      <- if ("autor" %in% nms) df[["autor"]] else rep(NA_character_, nrow(df))
  url_imagen <- if ("url_imagen" %in% nms) df[["url_imagen"]] else if ("imagen" %in% nms) df[["imagen"]] else rep(NA_character_, nrow(df))
  contenido_limpio <- if ("contenido_limpio" %in% nms) df[["contenido_limpio"]] else rep(NA_character_, nrow(df))

  # Fecha a Date
  if (is.character(fecha)) fecha <- as.Date(fecha, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y-%m-%dT%H:%M:%S"))
  if (inherits(fecha, "POSIXt")) fecha <- as.Date(fecha)

  data.frame(
    titulo            = as.character(titulo),
    contenido         = as.character(contenido),
    contenido_limpio  = as.character(contenido_limpio),
    url               = as.character(url),
    url_imagen        = as.character(url_imagen),
    autor             = as.character(autor),
    fecha             = fecha,
    resumen           = as.character(resumen),
    search_query      = search_query,
    medio             = as.character(medio),
    stringsAsFactors  = FALSE
  )
}

# ------------------------------------------------------------------------------
# Insertar en PostgreSQL (ON CONFLICT url DO UPDATE para no duplicar)
# ------------------------------------------------------------------------------
insertar_noticias <- function(con, df) {
  if (is.null(df) || nrow(df) == 0) return(0L)
  df <- df[!is.na(df$url) & nzchar(trimws(df$url)), ]
  if (nrow(df) == 0) return(0L)
  # Evitar URLs duplicadas dentro del lote
  df <- df[!duplicated(df$url), ]
  df$titulo <- trimws(df$titulo)
  df <- df[nzchar(df$titulo), ]
  if (nrow(df) == 0) return(0L)
  # Fecha obligatoria
  df <- df[!is.na(df$fecha), ]
  if (nrow(df) == 0) return(0L)

  insertados <- 0L
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    q <- "
      INSERT INTO noticias (titulo, contenido, contenido_limpio, url, url_imagen, autor, fecha, resumen, search_query, medio)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
      ON CONFLICT (url) DO UPDATE SET
        titulo = EXCLUDED.titulo,
        contenido = EXCLUDED.contenido,
        contenido_limpio = EXCLUDED.contenido_limpio,
        url_imagen = EXCLUDED.url_imagen,
        autor = EXCLUDED.autor,
        fecha = EXCLUDED.fecha,
        resumen = EXCLUDED.resumen,
        search_query = EXCLUDED.search_query,
        medio = EXCLUDED.medio,
        updated_at = CURRENT_TIMESTAMP
    "
    tryCatch({
      dbExecute(con, q, params = list(
        r$titulo, r$contenido, r$contenido_limpio, r$url, r$url_imagen,
        r$autor, r$fecha, r$resumen, r$search_query, r$medio
      ))
      insertados <- insertados + 1L
    }, error = function(e) {
      message("  Error insertando URL: ", substr(r$url, 1, 60), "... ", conditionMessage(e))
    })
  }
  insertados
}

# ------------------------------------------------------------------------------
# Generar primeros y últimos días de cada mes en el rango
# ------------------------------------------------------------------------------
meses <- seq(from = floor_date(FECHA_INICIO, "month"),
             to   = floor_date(FECHA_FIN, "month"),
             by   = "month")
total_meses <- length(meses)
message("Rango: ", FECHA_INICIO, " a ", FECHA_FIN, " (", total_meses, " meses)")
message("Fuentes: ", FUENTES)
message("Queries (", length(SEARCH_QUERIES), "): ", paste(SEARCH_QUERIES, collapse = ", "))
message("")

total_insercciones <- 0L
errores_meses <- character(0)

for (j in seq_along(meses)) {
  mes_start <- meses[j]
  mes_end   <- ceiling_date(mes_start, "month") - 1L
  f_ini     <- format(mes_start, "%Y-%m-%d")
  f_fin     <- format(mes_end, "%Y-%m-%d")

  message("[", j, "/", total_meses, "] ", f_ini, " a ", f_fin)

  listas_mes <- list()
  for (sq in SEARCH_QUERIES) {
    out_df <- tryCatch({
      suppressWarnings({
        df <- extraer_noticias_fecha(
          search_query  = sq,
          fecha_inicio  = f_ini,
          fecha_fin     = f_fin,
          subir_a_bd    = FALSE,
          fuentes       = FUENTES
        )
      })
      if (!is.data.frame(df) || nrow(df) == 0) {
        NULL
      } else {
        if ("contenido" %in% tolower(names(df))) {
          df <- tryCatch(limpieza_notas(df, sinonimos = c()), error = function(e) df)
        }
        mapear_a_noticias(df, search_query = sq)
      }
    }, error = function(e) {
      message("  Query \"", sq, "\": ", conditionMessage(e))
      NULL
    })
    if (!is.null(out_df) && nrow(out_df) > 0) {
      listas_mes[[length(listas_mes) + 1L]] <- out_df
    }
    Sys.sleep(PAUSA_ENTRE_QUERIES)
  }

  if (length(listas_mes) == 0) {
    message("  0 noticias en este mes.")
    next
  }

  # Combinar todos los resultados del mes y deduplicar por URL (quedarse con la primera aparición)
  tbl_mes <- do.call(rbind, listas_mes)
  tbl_mes <- tbl_mes[!duplicated(tbl_mes$url), ]

  n <- insertar_noticias(con, tbl_mes)
  total_insercciones <- total_insercciones + n
  message("  ", n, " noticias insertadas/actualizadas (", nrow(tbl_mes), " únicas por URL)")
  Sys.sleep(1)
}

message("")
message("Total noticias insertadas/actualizadas: ", total_insercciones)
if (length(errores_meses) > 0) {
  message("Meses con error (", length(errores_meses), "):")
  for (e in errores_meses) message("  ", e)
}
