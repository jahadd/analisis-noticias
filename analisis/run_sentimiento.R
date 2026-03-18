#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_sentimiento.R  [pipeline step sentimiento]
# Clasifica sentimiento (positivo/neutral/negativo) de todos los artículos en
# noticias usando qwen2.5:3b vía Ollama API + batch prompting (30 títulos/llamada)
# + 2 workers paralelos (furrr).
#
# Uso:
#   Rscript run_sentimiento.R              # procesar todos los pendientes
#   Rscript run_sentimiento.R --solo-fallidos  # reprocesar IDs en sentimiento_errores
#
# Variables de entorno: mismas que run_analisis_titulos.R (.env en Paginaweb/)
# Paquetes requeridos: DBI, RPostgres, httr2, furrr, jsonlite
# Requiere: Ollama corriendo con qwen2.5:3b
# Incremental: solo procesa IDs sin entrada en noticias_sentimiento.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(httr2)
  library(furrr)
  library(jsonlite)
})

# ------------------------------------------------------------------------------
# Constantes
# ------------------------------------------------------------------------------
MODELO_OLLAMA   <- "qwen2.5:3b"
OLLAMA_URL      <- "http://localhost:11434/api/chat"
BATCH_SIZE      <- 30L      # títulos por llamada LLM
N_WORKERS       <- 2L       # workers paralelos
MAX_REINTENTOS  <- 3L       # reintentos por lote antes de marcar como error

# ------------------------------------------------------------------------------
# .env
# ------------------------------------------------------------------------------
script_dir <- local({
  args <- commandArgs()
  idx  <- grep("^--file=", args)
  if (length(idx)) dirname(sub("^--file=", "", args[idx[1L]])) else getwd()
})

for (env_file in c(file.path(script_dir, ".env"), file.path(script_dir, "..", ".env"))) {
  if (file.exists(env_file)) {
    for (line in readLines(env_file, warn = FALSE)) {
      line <- sub("^[[:space:]]*#.*$", "", line)
      if (!nzchar(trimws(line))) next
      if (grepl("^PGHOST=",              line)) Sys.setenv(PGHOST              = sub("^PGHOST=([^[:space:]]+).*",                   "\\1", line))
      if (grepl("^PGPORT=",              line)) Sys.setenv(PGPORT              = sub("^PGPORT=([^[:space:]]+).*",                   "\\1", line))
      if (grepl("^PGUSER_NOTICIAS=",     line)) Sys.setenv(PGUSER_NOTICIAS     = sub("^PGUSER_NOTICIAS=[\"']?([^\"']*)[\"']?$",     "\\1", line))
      if (grepl("^PGPASSWORD_NOTICIAS=", line)) Sys.setenv(PGPASSWORD_NOTICIAS = sub("^PGPASSWORD_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGDATABASE_NOTICIAS=", line)) Sys.setenv(PGDATABASE_NOTICIAS = sub("^PGDATABASE_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGUSER=",              line)) Sys.setenv(PGUSER              = sub("^PGUSER=[\"']?([^\"']*)[\"']?$",              "\\1", line))
      if (grepl("^PGPASSWORD=",          line)) Sys.setenv(PGPASSWORD          = sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$",          "\\1", line))
      if (grepl("^PGDATABASE=",          line)) Sys.setenv(PGDATABASE          = sub("^PGDATABASE=[\"']?([^\"']*)[\"']?$",          "\\1", line))
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

# Flag --solo-fallidos
SOLO_FALLIDOS <- "--solo-fallidos" %in% commandArgs(trailingOnly = TRUE)

# ------------------------------------------------------------------------------
# Helpers de conexión (para workers)
# ------------------------------------------------------------------------------
hacer_conexion <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = PGHOST, port = PGPORT, user = PGUSER,
    password = PGPASSWORD, dbname = PGDATABASE
  )
}

# ------------------------------------------------------------------------------
# Crear tablas auxiliares si no existen
# ------------------------------------------------------------------------------
con <- hacer_conexion()
on.exit(dbDisconnect(con), add = TRUE)
message("Conectado a ", PGDATABASE, " en ", PGHOST)

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS sentimiento_errores (
    id          TEXT PRIMARY KEY,
    intentos    INTEGER DEFAULT 1,
    ultima_vez  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
")

# ------------------------------------------------------------------------------
# Contar artículos pendientes (sin cargar todo en memoria)
# ------------------------------------------------------------------------------
if (SOLO_FALLIDOS) {
  message("Modo: --solo-fallidos (reprocesando IDs en sentimiento_errores)")
  n_pendientes <- tryCatch(
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM sentimiento_errores")$n,
    error = function(e) 0L
  )
  SQL_PENDIENTES <- "
    SELECT n.id, n.titulo
    FROM noticias n
    JOIN sentimiento_errores e ON e.id = n.id
    WHERE n.titulo IS NOT NULL AND char_length(n.titulo) > 5
    ORDER BY e.ultima_vez
    LIMIT $1 OFFSET $2
  "
} else {
  message("Modo: normal (artículos sin sentimiento, ordenados por fecha DESC)")
  n_pendientes <- tryCatch(
    dbGetQuery(con, "
      SELECT COUNT(*) AS n FROM noticias n
      WHERE n.titulo IS NOT NULL AND char_length(n.titulo) > 5
        AND NOT EXISTS (SELECT 1 FROM noticias_sentimiento s WHERE s.id = n.id)
    ")$n,
    error = function(e) 0L
  )
  SQL_PENDIENTES <- "
    SELECT n.id, n.titulo
    FROM noticias n
    WHERE n.titulo IS NOT NULL
      AND char_length(n.titulo) > 5
      AND NOT EXISTS (SELECT 1 FROM noticias_sentimiento s WHERE s.id = n.id)
    ORDER BY n.fecha DESC
    LIMIT $1 OFFSET $2
  "
}

message("Artículos pendientes: ", n_pendientes)

if (n_pendientes == 0L) {
  message("No hay artículos pendientes. Saliendo.")
  quit(save = "no", status = 0L)
}

# ------------------------------------------------------------------------------
# Función: llamada a Ollama con batch de títulos
# ------------------------------------------------------------------------------
SYSTEM_PROMPT <- paste0(
  "Eres un clasificador de sentimiento para titulares de prensa chilena. ",
  "Te dare una lista numerada de titulares. Clasifica cada uno como positivo, neutral o negativo. ",
  "IMPORTANTE: Responde SOLO con un objeto JSON, sin texto antes ni despues, en este formato: ",
  "{\"1\":\"positivo\",\"2\":\"neutral\",\"3\":\"negativo\",...} ",
  "Ejemplo de respuesta correcta para 3 titulares: {\"1\":\"negativo\",\"2\":\"neutral\",\"3\":\"positivo\"}"
)

# Parser robusto: maneja JSON, "N":"val" con comillas, y N: val sin comillas
parsear_respuesta_llm <- function(resp_texto, n) {
  etiquetas_validas <- c("positivo", "neutral", "negativo")

  # Parser 1: jsonlite directo
  parsed <- tryCatch(jsonlite::fromJSON(resp_texto), error = function(e) NULL)

  # Parser 2: regex con comillas — "1":"positivo"
  if (is.null(parsed) || length(parsed) == 0L) {
    ms <- regmatches(resp_texto,
      gregexpr('"(\\d+)"\\s*:\\s*"(positivo|neutral|negativo)"', resp_texto, perl = TRUE))[[1]]
    if (length(ms) > 0L) {
      nums <- as.integer(regmatches(ms, regexpr("\\d+", ms)))
      vals <- regmatches(ms, regexpr("positivo|neutral|negativo", ms))
      parsed <- setNames(as.list(vals), as.character(nums))
    }
  }

  # Parser 3: formato simple — 1: positivo  o  1. positivo
  if (is.null(parsed) || length(parsed) == 0L) {
    ms <- regmatches(resp_texto,
      gregexpr("(\\d+)[.:]\\s*(positivo|neutral|negativo)", resp_texto, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(ms) > 0L) {
      nums <- as.integer(regmatches(ms, regexpr("\\d+", ms)))
      vals <- tolower(regmatches(ms, regexpr("positivo|neutral|negativo", ms, ignore.case = TRUE)))
      parsed <- setNames(as.list(vals), as.character(nums))
    }
  }

  if (is.null(parsed) || length(parsed) == 0L) return(NULL)

  vapply(seq_len(n), function(i) {
    v <- parsed[[as.character(i)]]
    if (is.null(v) || !tolower(v) %in% etiquetas_validas) "neutral" else tolower(v)
  }, character(1L))
}

clasificar_lote <- function(titulos_lote, ids_lote, modelo = MODELO_OLLAMA) {
  n <- length(titulos_lote)
  user_msg <- paste(
    sprintf('%d. "%s"', seq_len(n), gsub('"', "'", trimws(titulos_lote))),
    collapse = "\n"
  )

  resp_texto <- tryCatch({
    req <- request(OLLAMA_URL) |>
      req_headers("Content-Type" = "application/json") |>
      req_timeout(120) |>
      req_body_json(list(
        model  = modelo,
        stream = FALSE,
        messages = list(
          list(role = "system", content = SYSTEM_PROMPT),
          list(role = "user",   content = user_msg)
        ),
        options = list(temperature = 0, num_predict = 512L)
      ))
    resp <- req_perform(req)
    content <- resp_body_json(resp)
    content$message$content
  }, error = function(e) {
    message("  Error HTTP Ollama: ", e$message)
    NULL
  })

  if (is.null(resp_texto)) return(NULL)

  sentimientos <- parsear_respuesta_llm(resp_texto, n)
  if (is.null(sentimientos)) return(NULL)

  data.frame(id = ids_lote, sentimiento = sentimientos, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------------
# Función: procesar una partición completa (ejecutada por cada worker)
# Recibe offset/limit para consultar directamente la BD — no serializa dataframes grandes
# ------------------------------------------------------------------------------
procesar_particion <- function(offset_i, limit_i, sql_query, modelo, pg_host, pg_port, pg_user, pg_pw, pg_db, batch_size, max_reintentos) {
  suppressPackageStartupMessages({
    library(DBI); library(RPostgres); library(httr2); library(jsonlite)
  })

  OLLAMA_URL_W  <- "http://localhost:11434/api/chat"
  SYSTEM_PROMPT_W <- paste0(
    "Eres un clasificador de sentimiento para titulares de prensa chilena. ",
    "Te dare una lista numerada de titulares. Clasifica cada uno como positivo, neutral o negativo. ",
    "IMPORTANTE: Responde SOLO con un objeto JSON, sin texto antes ni despues, en este formato: ",
    "{\"1\":\"positivo\",\"2\":\"neutral\",\"3\":\"negativo\",...} ",
    "Ejemplo de respuesta correcta para 3 titulares: {\"1\":\"negativo\",\"2\":\"neutral\",\"3\":\"positivo\"}"
  )

  clasificar_lote_w <- function(titulos_lote, ids_lote) {
    n <- length(titulos_lote)
    user_msg <- paste(
      sprintf('%d. "%s"', seq_len(n), gsub('"', "'", trimws(titulos_lote))),
      collapse = "\n"
    )
    resp_texto <- tryCatch({
      req <- request(OLLAMA_URL_W) |>
        req_headers("Content-Type" = "application/json") |>
        req_timeout(120) |>
        req_body_json(list(
          model  = modelo,
          stream = FALSE,
          messages = list(
            list(role = "system", content = SYSTEM_PROMPT_W),
            list(role = "user",   content = user_msg)
          ),
          options = list(temperature = 0, num_predict = 512L)
        ))
      resp <- req_perform(req)
      content <- resp_body_json(resp)
      content$message$content
    }, error = function(e) NULL)

    if (is.null(resp_texto)) return(NULL)

    # Parser 1: jsonlite
    parsed <- tryCatch(jsonlite::fromJSON(resp_texto), error = function(e) NULL)
    # Parser 2: "N":"val"
    if (is.null(parsed) || length(parsed) == 0L) {
      ms <- regmatches(resp_texto,
        gregexpr('"(\\d+)"\\s*:\\s*"(positivo|neutral|negativo)"', resp_texto, perl = TRUE))[[1]]
      if (length(ms) > 0L)
        parsed <- setNames(as.list(regmatches(ms, regexpr("positivo|neutral|negativo", ms))),
                           as.character(as.integer(regmatches(ms, regexpr("\\d+", ms)))))
    }
    # Parser 3: N: val  o  N. val
    if (is.null(parsed) || length(parsed) == 0L) {
      ms <- regmatches(resp_texto,
        gregexpr("(\\d+)[.:]\\s*(positivo|neutral|negativo)", resp_texto, perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(ms) > 0L)
        parsed <- setNames(as.list(tolower(regmatches(ms, regexpr("positivo|neutral|negativo", ms, ignore.case = TRUE)))),
                           as.character(as.integer(regmatches(ms, regexpr("\\d+", ms)))))
    }
    if (is.null(parsed) || length(parsed) == 0L) return(NULL)

    sentimientos <- vapply(seq_len(n), function(i) {
      v <- parsed[[as.character(i)]]
      if (is.null(v) || !tolower(v) %in% c("positivo", "neutral", "negativo")) "neutral" else tolower(v)
    }, character(1L))
    data.frame(id = ids_lote, sentimiento = sentimientos, stringsAsFactors = FALSE)
  }

  upsert_lote_w <- function(con, df_res) {
    n <- nrow(df_res)
    ph <- paste(sprintf("($%d,$%d,$%d)", 3L * (seq_len(n) - 1L) + 1L,
                                          3L * (seq_len(n) - 1L) + 2L,
                                          3L * (seq_len(n) - 1L) + 3L), collapse = ", ")
    q <- paste0(
      "INSERT INTO noticias_sentimiento (id, sentimiento, modelo) VALUES ", ph,
      " ON CONFLICT (id) DO UPDATE SET sentimiento = EXCLUDED.sentimiento,",
      " modelo = EXCLUDED.modelo, procesado_en = CURRENT_TIMESTAMP"
    )
    params <- as.list(as.vector(t(cbind(df_res$id, df_res$sentimiento, modelo))))
    tryCatch(DBI::dbExecute(con, q, params = params), error = function(e) {
      message("  Error upsert: ", e$message); 0L
    })
  }

  registrar_error_w <- function(con, ids_err) {
    n <- length(ids_err)
    ph <- paste(sprintf("($%d)", seq_len(n)), collapse = ", ")
    q <- paste0(
      "INSERT INTO sentimiento_errores (id, intentos, ultima_vez) VALUES ",
      paste(sprintf("($%d, 1, CURRENT_TIMESTAMP)", seq_len(n)), collapse = ", "),
      " ON CONFLICT (id) DO UPDATE SET intentos = sentimiento_errores.intentos + 1,",
      " ultima_vez = CURRENT_TIMESTAMP"
    )
    tryCatch(DBI::dbExecute(con, q, params = as.list(ids_err)), error = function(e) NULL)
  }

  con_w <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = pg_host, port = pg_port, user = pg_user, password = pg_pw, dbname = pg_db
  )
  on.exit(DBI::dbDisconnect(con_w), add = TRUE)

  # Consultar la partición directamente desde BD
  df_part <- tryCatch(
    DBI::dbGetQuery(con_w, sql_query, params = list(as.integer(limit_i), as.integer(offset_i))),
    error = function(e) { message("  Error al consultar particion offset=", offset_i, ": ", e$message); data.frame() }
  )
  if (nrow(df_part) == 0L) { message("  Particion offset=", offset_i, " vacia, saltando"); return(0L) }
  message("  Particion offset=", offset_i, ": ", nrow(df_part), " articulos")

  n_total   <- nrow(df_part)
  n_lotes   <- ceiling(n_total / batch_size)
  n_escritos <- 0L

  for (lote_i in seq_len(n_lotes)) {
    start <- (lote_i - 1L) * batch_size + 1L
    end   <- min(lote_i * batch_size, n_total)
    lote  <- df_part[start:end, ]

    resultado <- NULL
    for (intento in seq_len(max_reintentos)) {
      resultado <- clasificar_lote_w(lote$titulo, lote$id)
      if (!is.null(resultado)) break
      Sys.sleep(2)
    }

    if (is.null(resultado)) {
      message("  Lote ", lote_i, "/", n_lotes, " — FALLO tras ", max_reintentos, " intentos (", nrow(lote), " IDs marcados en sentimiento_errores)")
      registrar_error_w(con_w, lote$id)
      next
    }

    # Limpiar de sentimiento_errores si estaban ahí
    tryCatch(
      DBI::dbExecute(con_w, "DELETE FROM sentimiento_errores WHERE id = ANY($1)", params = list(resultado$id)),
      error = function(e) NULL
    )

    n_escritos <- n_escritos + upsert_lote_w(con_w, resultado)
    message("  Lote ", lote_i, "/", n_lotes, " — ", nrow(resultado), " clasificados (acum. ", n_escritos, ")")
  }

  n_escritos
}

# ------------------------------------------------------------------------------
# Dividir en particiones (por OFFSET/LIMIT) y lanzar workers
# ------------------------------------------------------------------------------
message("Modelo: ", MODELO_OLLAMA, " | Workers: ", N_WORKERS, " | Lote: ", BATCH_SIZE, " títulos/llamada")

n_workers_real <- min(N_WORKERS, as.integer(n_pendientes))
chunk_size     <- ceiling(n_pendientes / n_workers_real)
offsets        <- seq(0L, by = chunk_size, length.out = n_workers_real)
message("Particiones: ", n_workers_real, " de ~", chunk_size, " artículos cada una")

plan(multisession, workers = n_workers_real)

resultados_workers <- future_map(offsets, function(offset_i) {
  procesar_particion(
    offset_i     = offset_i,
    limit_i      = chunk_size,
    sql_query    = SQL_PENDIENTES,
    modelo       = MODELO_OLLAMA,
    pg_host      = PGHOST,
    pg_port      = PGPORT,
    pg_user      = PGUSER,
    pg_pw        = PGPASSWORD,
    pg_db        = PGDATABASE,
    batch_size   = BATCH_SIZE,
    max_reintentos = MAX_REINTENTOS
  )
}, .options = furrr_options(seed = TRUE))

plan(sequential)

total_escritos <- sum(unlist(resultados_workers), na.rm = TRUE)
message("")
message("======================================================")
message("  run_sentimiento.R completado")
message("  Escritos en noticias_sentimiento: ", total_escritos)

# Distribución final
dist <- tryCatch(
  dbGetQuery(con, "SELECT sentimiento, COUNT(*) AS n FROM noticias_sentimiento GROUP BY sentimiento ORDER BY n DESC"),
  error = function(e) NULL
)
if (!is.null(dist)) {
  message("  Distribución:")
  for (i in seq_len(nrow(dist))) {
    message("    ", dist$sentimiento[i], ": ", dist$n[i])
  }
}

n_errores <- tryCatch(
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM sentimiento_errores")$n,
  error = function(e) NA
)
if (!is.na(n_errores) && n_errores > 0L) {
  message("  IDs en sentimiento_errores (para reintento): ", n_errores)
  message("  Ejecutar con --solo-fallidos para reprocesarlos")
}
message("======================================================")
