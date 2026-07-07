#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_embeddings.R — Indexa artículos nuevos en el vector store RAG (DuckDB)
# Uso: Rscript analisis/run_embeddings.R [--backfill-dias N]
# Depende: ragnar, DBI, RPostgres, dplyr
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ragnar)
})

# ------------------------------------------------------------------------------
# Cargar .env
# ------------------------------------------------------------------------------
script_dir <- if (length(args <- commandArgs()) > 0L) {
  idx <- grep("^--file=", args)
  if (length(idx)) dirname(sub("^--file=", "", args[idx[1L]])) else getwd()
} else getwd()

env_candidates <- c(
  file.path(script_dir, ".env"),
  file.path(script_dir, "..", ".env"),
  file.path(script_dir, "..", "..", ".env")
)
for (env_file in env_candidates) {
  if (file.exists(env_file)) {
    env_lines <- readLines(env_file, warn = FALSE)
    for (line in env_lines) {
      line <- sub("^[[:space:]]*#.*$", "", line)
      if (!nzchar(trimws(line))) next
      if (grepl("^PGHOST=", line))               Sys.setenv(PGHOST               = sub("^PGHOST=([^[:space:]]+).*", "\\1", line))
      if (grepl("^PGPORT=", line))               Sys.setenv(PGPORT               = sub("^PGPORT=([^[:space:]]+).*", "\\1", line))
      if (grepl("^PGUSER_NOTICIAS=", line))      Sys.setenv(PGUSER_NOTICIAS      = sub("^PGUSER_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGPASSWORD_NOTICIAS=", line))  Sys.setenv(PGPASSWORD_NOTICIAS  = sub("^PGPASSWORD_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGDATABASE_NOTICIAS=", line))  Sys.setenv(PGDATABASE_NOTICIAS  = sub("^PGDATABASE_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGUSER=", line))               Sys.setenv(PGUSER               = sub("^PGUSER=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGPASSWORD=", line))           Sys.setenv(PGPASSWORD           = sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGDATABASE=", line))           Sys.setenv(PGDATABASE           = sub("^PGDATABASE=[\"']?([^\"']*)[\"']?$", "\\1", line))
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

if (!nzchar(PGPASSWORD)) {
  stop("Definir PGPASSWORD_NOTICIAS o PGPASSWORD (variable de entorno o .env)")
}

source(file.path(script_dir, "..", "funciones.R"))

# ------------------------------------------------------------------------------
# Argumento CLI: --backfill-dias N (default 30)
# ------------------------------------------------------------------------------
cli_args <- commandArgs(trailingOnly = TRUE)
backfill_dias <- 30L
for (i in seq_along(cli_args)) {
  if (cli_args[i] == "--backfill-dias" && i < length(cli_args)) {
    backfill_dias <- as.integer(cli_args[i + 1L])
  }
}
message("[run_embeddings] backfill-dias: ", backfill_dias)

# ------------------------------------------------------------------------------
# Conexión PostgreSQL
# ------------------------------------------------------------------------------
con <- conectar_db()
on.exit(DBI::dbDisconnect(con), add = TRUE)

# ------------------------------------------------------------------------------
# Abrir/crear store RAG
# ------------------------------------------------------------------------------
STORE_PATH <- file.path(script_dir, "..", "datos", "noticias_rag.duckdb")
message("[run_embeddings] Store: ", STORE_PATH)

store <- if (file.exists(STORE_PATH)) {
  ragnar_store_connect(location = STORE_PATH, read_only = FALSE)
} else {
  ragnar_store_create(
    location = STORE_PATH,
    embed    = embed_ollama(model = "nomic-embed-text"),
    version  = 1
  )
}

# ------------------------------------------------------------------------------
# IDs ya indexados
# ------------------------------------------------------------------------------
ya_indexados <- tryCatch(
  DBI::dbGetQuery(con, "SELECT id FROM noticias_embeddings_meta")$id,
  error = function(e) character(0)
)
message("[run_embeddings] Ya indexados: ", length(ya_indexados))

# ------------------------------------------------------------------------------
# Artículos nuevos en el período de backfill
# ------------------------------------------------------------------------------
# NOT EXISTS (anti-join) en vez de NOT IN: con cientos de miles de ids ya
# indexados, NOT IN degenera en un subplan cuadrático y la consulta se cuelga.
nuevos <- DBI::dbGetQuery(con, paste0("
  SELECT n.id, n.titulo, n.fuente, n.fecha
  FROM noticias n
  WHERE n.fecha >= CURRENT_DATE - INTERVAL '", backfill_dias, " days'
    AND n.titulo IS NOT NULL
    AND NOT EXISTS (
      SELECT 1 FROM noticias_embeddings_meta m WHERE m.id = n.id
    )
  ORDER BY n.fecha DESC
"))
message("[run_embeddings] Artículos nuevos a indexar: ", nrow(nuevos))

if (nrow(nuevos) == 0L) {
  message("[run_embeddings] Nada que indexar. Fin.")
  quit(save = "no", status = 0L)
}

# ------------------------------------------------------------------------------
# Procesar en batches de 200 — inserción por batch (50x más rápido)
# El embedding se llama una vez por batch, no por artículo
# ------------------------------------------------------------------------------
BATCH_SIZE <- 200L
n_insertados <- 0L
lotes <- split(seq_len(nrow(nuevos)), ceiling(seq_len(nrow(nuevos)) / BATCH_SIZE))

for (lote_idx in seq_along(lotes)) {
  idx   <- lotes[[lote_idx]]
  lote  <- nuevos[idx, ]
  t0    <- proc.time()[["elapsed"]]

  # Intentar insertar el lote completo de una vez (una sola llamada a Ollama)
  ok <- tryCatch({
    batch_docs <- data.frame(
      text   = lote$titulo,
      origin = lote$id,
      stringsAsFactors = FALSE
    )
    ragnar_store_insert(store, batch_docs)

    # Actualizar meta table para todos los del lote con un solo INSERT
    ids_quoted <- paste(
      vapply(lote$id, function(x) paste0("(", DBI::dbQuoteLiteral(con, x), ", 'nomic-embed-text', 1)"), character(1L)),
      collapse = ",\n"
    )
    DBI::dbExecute(con, paste0(
      "INSERT INTO noticias_embeddings_meta (id, modelo, store_version) VALUES\n",
      ids_quoted, "\nON CONFLICT (id) DO NOTHING"
    ))
    n_insertados <- n_insertados + nrow(lote)
    TRUE
  }, error = function(e) {
    message("[run_embeddings] Lote ", lote_idx, " falló (", conditionMessage(e), ") — reintentando artículo a artículo")
    FALSE
  })

  # Fallback artículo a artículo si el batch falló
  if (!ok) {
    for (j in seq_len(nrow(lote))) {
      art <- lote[j, ]
      tryCatch({
        ragnar_store_insert(store, data.frame(text = art$titulo, origin = art$id, stringsAsFactors = FALSE))
        DBI::dbExecute(con,
          "INSERT INTO noticias_embeddings_meta (id, modelo, store_version)
           VALUES ($1, 'nomic-embed-text', 1) ON CONFLICT (id) DO NOTHING",
          params = list(art$id)
        )
        n_insertados <- n_insertados + 1L
      }, error = function(e) {
        message("[run_embeddings] ERROR artículo ", art$id, ": ", conditionMessage(e), " — saltando")
      })
    }
  }

  elapsed <- round(proc.time()[["elapsed"]] - t0, 1L)
  pct     <- round(100 * lote_idx / length(lotes))
  message("[run_embeddings] Lote ", lote_idx, "/", length(lotes), " (", nrow(lote), " arts, ",
          elapsed, "s) — ", pct, "% completado — total: ", n_insertados)
}

message("[run_embeddings] Insertados: ", n_insertados)

# ------------------------------------------------------------------------------
# Reconstruir índice VSS si hubo nuevos documentos
# ------------------------------------------------------------------------------
if (n_insertados > 0L) {
  message("[run_embeddings] Construyendo índice VSS…")
  tryCatch(
    ragnar_store_build_index(store),
    error = function(e) message("[run_embeddings] WARN índice: ", conditionMessage(e))
  )
  message("[run_embeddings] Índice listo.")
}

message("[run_embeddings] Completado.")
