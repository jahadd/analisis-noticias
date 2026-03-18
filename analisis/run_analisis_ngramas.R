#!/usr/bin/env Rscript
# run_analisis_ngramas.R  [pipeline step NLP-2]
# Calcula bigramas (tipo=2) y trigramas (tipo=3) de títulos de noticias.
# Escribe titulos_ngramas_diarios y titulos_ngramas_por_medio.
# Incremental: procesa desde MAX(fecha) en titulos_ngramas_diarios.
# Ejecutar: Rscript run_analisis_ngramas.R

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(tidytext)
  library(dplyr)
  if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
})

script_dir <- if (length(args <- commandArgs()) > 0L) {
  idx <- grep("^--file=", args)
  if (length(idx)) dirname(sub("^--file=", "", args[idx[1L]])) else getwd()
} else getwd()

env_candidates <- c(
  file.path(script_dir, ".env"),
  file.path(script_dir, "..", ".env")
)
for (env_file in env_candidates) {
  if (file.exists(env_file)) {
    for (line in readLines(env_file, warn = FALSE)) {
      line <- sub("^[[:space:]]*#.*$", "", line)
      if (!nzchar(trimws(line))) next
      if (grepl("^PGHOST=",              line)) Sys.setenv(PGHOST              = sub("^PGHOST=([^[:space:]]+).*",                        "\\1", line))
      if (grepl("^PGPORT=",              line)) Sys.setenv(PGPORT              = sub("^PGPORT=([^[:space:]]+).*",                        "\\1", line))
      if (grepl("^PGUSER_NOTICIAS=",     line)) Sys.setenv(PGUSER_NOTICIAS     = sub("^PGUSER_NOTICIAS=[\"']?([^\"']*)[\"']?$",          "\\1", line))
      if (grepl("^PGPASSWORD_NOTICIAS=", line)) Sys.setenv(PGPASSWORD_NOTICIAS = sub("^PGPASSWORD_NOTICIAS=[\"']?([^\"']*)[\"']?$",      "\\1", line))
      if (grepl("^PGDATABASE_NOTICIAS=", line)) Sys.setenv(PGDATABASE_NOTICIAS = sub("^PGDATABASE_NOTICIAS=[\"']?([^\"']*)[\"']?$",      "\\1", line))
      if (grepl("^PGUSER=",              line)) Sys.setenv(PGUSER              = sub("^PGUSER=[\"']?([^\"']*)[\"']?$",                   "\\1", line))
      if (grepl("^PGPASSWORD=",          line)) Sys.setenv(PGPASSWORD          = sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$",               "\\1", line))
      if (grepl("^PGDATABASE=",          line)) Sys.setenv(PGDATABASE          = sub("^PGDATABASE=[\"']?([^\"']*)[\"']?$",               "\\1", line))
    }
    break
  }
}

`%||%` <- function(x, y) if (nzchar(x)) x else y
PGHOST     <- Sys.getenv("PGHOST", "localhost")
PGPORT     <- as.integer(Sys.getenv("PGPORT", "5432"))
PGUSER     <- Sys.getenv("PGUSER_NOTICIAS") %||% Sys.getenv("PGUSER", "noticias")
PGPASSWORD <- Sys.getenv("PGPASSWORD_NOTICIAS") %||% Sys.getenv("PGPASSWORD")
PGDATABASE <- Sys.getenv("PGDATABASE_NOTICIAS") %||% Sys.getenv("PGDATABASE", "noticias_chile")

if (!nzchar(PGPASSWORD)) stop("Definir PGPASSWORD_NOTICIAS o PGPASSWORD en .env")

source(file.path(script_dir, "..", "stopwords.R"))
source(file.path(script_dir, "..", "funciones.R"))

BATCH_DIAS     <- 30L
MIN_TOKEN_LEN  <- 3L
FECHA_DESDE    <- as.Date("2018-01-01")

message("N-gramas sobre tokens normalizados (sin lematización udpipe) para preservar concordancia de género")

con <- dbConnect(
  RPostgres::Postgres(),
  host = PGHOST, port = PGPORT, user = PGUSER, password = PGPASSWORD, dbname = PGDATABASE
)
on.exit(dbDisconnect(con), add = TRUE)
message("Conectado a ", PGDATABASE, " en ", PGHOST)

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS titulos_ngramas_diarios (
    fecha  DATE         NOT NULL,
    ngrama VARCHAR(300) NOT NULL,
    n      INTEGER      NOT NULL,
    tipo   SMALLINT     NOT NULL,
    CONSTRAINT pk_titulos_ngramas_diarios PRIMARY KEY (fecha, ngrama, tipo)
  )
")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_diarios_fecha  ON titulos_ngramas_diarios(fecha DESC, tipo)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_diarios_ngrama ON titulos_ngramas_diarios(ngrama, tipo)")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS titulos_ngramas_por_medio (
    fecha  DATE         NOT NULL,
    fuente VARCHAR(100) NOT NULL,
    ngrama VARCHAR(300) NOT NULL,
    n      INTEGER      NOT NULL,
    tipo   SMALLINT     NOT NULL,
    CONSTRAINT pk_titulos_ngramas_por_medio PRIMARY KEY (fecha, fuente, ngrama, tipo)
  )
")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_por_medio_fecha  ON titulos_ngramas_por_medio(fecha DESC, tipo)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_por_medio_fuente ON titulos_ngramas_por_medio(fuente, tipo)")

max_fecha_ngramas <- tryCatch(
  as.Date(dbGetQuery(con, "SELECT MAX(fecha)::text AS f FROM titulos_ngramas_diarios")$f),
  error = function(e) NA
)
if (!is.na(max_fecha_ngramas)) {
  fecha_desde_inc <- max_fecha_ngramas - 1L
  dbExecute(con, "DELETE FROM titulos_ngramas_diarios  WHERE fecha >= $1", params = list(fecha_desde_inc))
  dbExecute(con, "DELETE FROM titulos_ngramas_por_medio WHERE fecha >= $1", params = list(fecha_desde_inc))
  message("Incremental: reprocesando desde ", fecha_desde_inc, " (último registrado: ", max_fecha_ngramas, ")")
} else {
  fecha_desde_inc <- FECHA_DESDE
  message("Tablas de n-gramas vacías — procesando desde: ", fecha_desde_inc)
}

max_fecha <- tryCatch(
  as.Date(dbGetQuery(con, "SELECT MAX(fecha)::text AS f FROM noticias")$f),
  error = function(e) Sys.Date()
)

DASHES_UNICODE <- paste0("[",
  intToUtf8(0x2010), intToUtf8(0x2011), intToUtf8(0x2012),
  intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0x2015), "]")

normalizar_titulo <- function(x) {
  x <- tolower(trimws(x))
  if (requireNamespace("stringi", quietly = TRUE)) x <- stringi::stri_trans_nfc(x)
  x <- gsub(DASHES_UNICODE, " ", x)
  x <- gsub("&[a-z0-9]+;", " ", x)
  x <- gsub("&#[0-9]+;", " ", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:digit:]]", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

filtrar_ngrama <- function(ng, sw, min_len) {
  partes <- strsplit(ng, " ", fixed = TRUE)[[1]]
  if (length(partes) < 2L) return(FALSE)
  if (any(partes %in% sw)) return(FALSE)
  if (any(nchar(partes) < min_len)) return(FALSE)
  TRUE
}

procesar_batch_ngramas <- function(chunk_df, sw, min_len) {
  if (nrow(chunk_df) == 0L) return(NULL)

  chunk_df$titulo_norm <- normalizar_titulo(chunk_df$titulo)

  resultados_tipos <- lapply(c(2L, 3L), function(ng_n) {
    ng_df <- chunk_df |>
      unnest_ngrams(output = ngrama, input = titulo_norm, n = ng_n) |>
      filter(vapply(ngrama, filtrar_ngrama, logical(1), sw = sw, min_len = min_len)) |>
      filter(nchar(ngrama) <= 300L)

    if (nrow(ng_df) == 0L) return(NULL)

    diarios <- ng_df |>
      count(fecha, ngrama, name = "n") |>
      mutate(tipo = ng_n, n = as.integer(n), fecha = as.Date(fecha))

    por_medio <- ng_df |>
      count(fecha, fuente, ngrama, name = "n") |>
      mutate(tipo = ng_n, n = as.integer(n), fecha = as.Date(fecha))

    list(diarios = diarios, por_medio = por_medio)
  })

  diarios_all  <- do.call(rbind, Filter(Negate(is.null), lapply(resultados_tipos, `[[`, "diarios")))
  por_medio_all <- do.call(rbind, Filter(Negate(is.null), lapply(resultados_tipos, `[[`, "por_medio")))
  list(diarios = diarios_all, por_medio = por_medio_all)
}

upsert_staging <- function(con, df, tabla, pk_cols) {
  if (is.null(df) || nrow(df) == 0L) return(0L)
  stg <- paste0("_stg_", tabla)
  dbExecute(con, paste0("DROP TABLE IF EXISTS ", stg))
  df$fecha <- as.character(as.Date(df$fecha))
  dbWriteTable(con, stg, df, temporary = FALSE, overwrite = TRUE)
  dbExecute(con, paste0("ALTER TABLE ", stg, " ALTER COLUMN fecha TYPE DATE USING fecha::date"))
  cols <- names(df)
  col_list <- paste(cols, collapse = ", ")
  update_set <- paste(
    setdiff(cols, pk_cols),
    collapse = ", "
  )
  update_set <- paste(
    paste0(setdiff(cols, pk_cols), " = EXCLUDED.", setdiff(cols, pk_cols)),
    collapse = ", "
  )
  n <- dbExecute(con, paste0(
    "INSERT INTO ", tabla, " (", col_list, ") SELECT ", col_list, " FROM ", stg,
    " ON CONFLICT (", paste(pk_cols, collapse = ", "), ") DO UPDATE SET ", update_set
  ))
  dbExecute(con, paste0("DROP TABLE IF EXISTS ", stg))
  n
}

fechas_batch <- seq(fecha_desde_inc, max_fecha, by = paste(BATCH_DIAS, "days"))
total_diarios  <- 0L
total_por_medio <- 0L

for (fecha_ini in as.list(fechas_batch)) {
  fecha_ini <- as.Date(fecha_ini)
  fecha_fin <- min(fecha_ini + BATCH_DIAS - 1L, max_fecha)
  message("N-gramas: ", fecha_ini, " — ", fecha_fin)

  chunk <- tryCatch(
    dbGetQuery(con, "
      SELECT titulo, fecha::text AS fecha, fuente
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
        AND titulo IS NOT NULL AND titulo <> ''
    ", params = list(fecha_ini, fecha_fin)),
    error = function(e) { message("Error leyendo chunk: ", e$message); data.frame() }
  )
  if (nrow(chunk) == 0L) next
  chunk$fecha <- as.Date(chunk$fecha)

  res <- procesar_batch_ngramas(chunk, STOPWORDS, MIN_TOKEN_LEN)
  if (is.null(res)) next

  if (!is.null(res$diarios) && nrow(res$diarios) > 0L) {
    n <- upsert_staging(con, res$diarios, "titulos_ngramas_diarios", c("fecha", "ngrama", "tipo"))
    total_diarios <- total_diarios + n
  }
  if (!is.null(res$por_medio) && nrow(res$por_medio) > 0L) {
    n <- upsert_staging(con, res$por_medio, "titulos_ngramas_por_medio", c("fecha", "fuente", "ngrama", "tipo"))
    total_por_medio <- total_por_medio + n
  }
}

message("run_analisis_ngramas.R completado.")
message("  titulos_ngramas_diarios:  ", total_diarios, " filas upsert")
message("  titulos_ngramas_por_medio: ", total_por_medio, " filas upsert")

# ------------------------------------------------------------------------------
# Vistas unificadas: unigramas + n-gramas en una sola tabla consultable
# ------------------------------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE VIEW terminos_unificados_diarios AS
    SELECT termino, fecha, frecuencia
    FROM titulos_terminos_diarios
    UNION ALL
    SELECT ngrama AS termino, fecha, n AS frecuencia
    FROM titulos_ngramas_diarios
")
dbExecute(con, "
  CREATE OR REPLACE VIEW terminos_unificados_por_medio AS
    SELECT termino, fecha, fuente, frecuencia
    FROM titulos_terminos_por_medio
    UNION ALL
    SELECT ngrama AS termino, fecha, fuente, n AS frecuencia
    FROM titulos_ngramas_por_medio
")
message("Vistas terminos_unificados_diarios y terminos_unificados_por_medio creadas/actualizadas.")
