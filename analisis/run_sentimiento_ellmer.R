#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_sentimiento_ellmer.R — Clasificación de sentimiento con ellmer + Ollama.
#
# Reescritura de run_sentimiento.R sobre {ellmer} (guía: bastianolea.rbind.io/blog/shinychat):
#   - Structured outputs (type_enum) → salida garantizada por schema, sin parseo
#     regex ni fallback silencioso a "neutral" (la causa del 64% neutral inflado).
#   - parallel_chat_structured() → concurrencia + reintentos nativos (reemplaza
#     la partición manual OFFSET/furrr).
#   - Preflight: aborta si Ollama no responde o el modelo no está (evita marcar
#     todo como error).
#   - Provenance: guarda modelo, confianza y prompt_version → re-runs selectivos.
#   - Idempotente y resumible: procesa en chunks solo IDs sin sentimiento.
#
# Uso:
#   Rscript analisis/run_sentimiento_ellmer.R
# Variables de entorno (opcionales):
#   SENTIMIENTO_MODELO       (def: llama3.1:8b)
#   SENTIMIENTO_MAX_ACTIVE   (def: 4)   concurrencia
#   SENTIMIENTO_CHUNK        (def: 500) checkpoint cada N títulos
#   SENTIMIENTO_LIMIT        (def: 0)   0 = todos; >0 = tope para pruebas
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(ellmer); library(jsonlite)
})
source("funciones.R")  # conectar_db()

MODELO         <- Sys.getenv("SENTIMIENTO_MODELO", "llama3.1:8b")
MAX_ACTIVE     <- as.integer(Sys.getenv("SENTIMIENTO_MAX_ACTIVE", "4"))
CHUNK          <- as.integer(Sys.getenv("SENTIMIENTO_CHUNK", "500"))
LIMIT_TOTAL    <- as.integer(Sys.getenv("SENTIMIENTO_LIMIT", "0"))
DESDE          <- Sys.getenv("SENTIMIENTO_DESDE", "")   # "" = todos; ej "2025-01-01"
DESDE_P        <- if (nzchar(DESDE)) DESDE else NA_character_
PROMPT_VERSION <- "ellmer-v1-2026-06"
OLLAMA_BASE    <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434")

SYSTEM_PROMPT <- paste(
  "Eres un clasificador de sentimiento de titulares de prensa chilena.",
  "Clasifica el TONO de la noticia para el lector: positivo, neutral o negativo.",
  "Reglas: tragedias, muertes, delitos, desastres, crisis, accidentes, escándalos = negativo.",
  "Logros, rescates, premios, mejoras, avances, buenas noticias económicas = positivo.",
  "Datos, anuncios, cifras o procedimientos sin carga emocional = neutral.",
  "Clasifica por el HECHO, no por la redacción."
)

# ------------------------------------------------------------------------------
# Preflight: Ollama vivo + modelo presente
# ------------------------------------------------------------------------------
tags <- tryCatch(
  fromJSON(paste0(OLLAMA_BASE, "/api/tags"))$models$name,
  error = function(e) NULL
)
if (is.null(tags)) stop("Ollama no responde en ", OLLAMA_BASE, " — abortando (no se marcan errores).")
if (!any(startsWith(tags, sub(":.*$", "", MODELO))) && !(MODELO %in% tags)) {
  stop("Modelo '", MODELO, "' no está en Ollama. Disponibles: ", paste(tags, collapse = ", "))
}

con <- conectar_db()
on.exit(dbDisconnect(con), add = TRUE)
message("Conectado | Modelo: ", MODELO, " | max_active: ", MAX_ACTIVE, " | chunk: ", CHUNK)

# ------------------------------------------------------------------------------
# Esquema: columnas de provenance (idempotente)
# ------------------------------------------------------------------------------
invisible(dbExecute(con, "ALTER TABLE noticias_sentimiento ADD COLUMN IF NOT EXISTS confianza INTEGER"))
invisible(dbExecute(con, "ALTER TABLE noticias_sentimiento ADD COLUMN IF NOT EXISTS prompt_version VARCHAR(30)"))

# ------------------------------------------------------------------------------
# Clasificador (ellmer)
# ------------------------------------------------------------------------------
chat <- chat_ollama(model = MODELO, system_prompt = SYSTEM_PROMPT, echo = "none")
tipo <- type_object(
  sentimiento = type_enum(c("positivo", "neutral", "negativo")),
  confianza   = type_number("Qué tan seguro estás de la clasificación, de 0.0 (nada) a 1.0 (totalmente seguro)")
)

upsert_resultados <- function(con, df) {
  if (nrow(df) == 0L) return(0L)
  dbExecute(con, "DROP TABLE IF EXISTS _stg_sentimiento")
  dbWriteTable(con, "_stg_sentimiento", df, temporary = FALSE, overwrite = TRUE)
  n <- dbExecute(con, "
    INSERT INTO noticias_sentimiento (id, sentimiento, modelo, confianza, prompt_version)
    SELECT id, sentimiento, modelo, confianza, prompt_version FROM _stg_sentimiento
    ON CONFLICT (id) DO UPDATE SET
      sentimiento    = EXCLUDED.sentimiento,
      modelo         = EXCLUDED.modelo,
      confianza      = EXCLUDED.confianza,
      prompt_version = EXCLUDED.prompt_version,
      procesado_en   = CURRENT_TIMESTAMP")
  dbExecute(con, "DROP TABLE IF EXISTS _stg_sentimiento")
  n
}

# ------------------------------------------------------------------------------
# Loop por chunks (resumible: selecciona pendientes cada vuelta)
# ------------------------------------------------------------------------------
total_pend <- dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM noticias n
  WHERE n.titulo IS NOT NULL AND char_length(n.titulo) > 5
    AND ($1::date IS NULL OR n.fecha >= $1)
    AND NOT EXISTS (SELECT 1 FROM noticias_sentimiento s WHERE s.id = n.id)",
  params = list(DESDE_P))$n
message("Pendientes", if (nzchar(DESDE)) paste0(" (desde ", DESDE, ")") else "", ": ", total_pend)
if (total_pend == 0L) { message("Nada que procesar."); quit(save = "no", status = 0L) }

procesados <- 0L; escritos <- 0L; fallidos <- 0L; t0 <- Sys.time()
repeat {
  pend <- dbGetQuery(con, "
    SELECT n.id, n.titulo FROM noticias n
    WHERE n.titulo IS NOT NULL AND char_length(n.titulo) > 5
      AND ($2::date IS NULL OR n.fecha >= $2)
      AND NOT EXISTS (SELECT 1 FROM noticias_sentimiento s WHERE s.id = n.id)
    ORDER BY n.fecha DESC NULLS LAST
    LIMIT $1", params = list(CHUNK, DESDE_P))
  if (nrow(pend) == 0L) break

  res <- parallel_chat_structured(
    chat, prompts = as.list(pend$titulo), type = tipo,
    max_active = MAX_ACTIVE, on_error = "return"
  )

  # Solo filas con etiqueta válida (sin fallback silencioso). Las fallidas quedan
  # pendientes y se reintentan en la próxima vuelta / corrida.
  pred <- as.character(res$sentimiento)
  ok <- !is.na(pred) & pred %in% c("positivo", "neutral", "negativo")
  # Normalizar confianza a 0-100 (el modelo a veces responde 0-1 y a veces 0-100)
  conf_raw <- suppressWarnings(as.numeric(res$confianza))
  conf <- ifelse(!is.na(conf_raw) & conf_raw > 1, conf_raw, conf_raw * 100)
  conf <- as.integer(ifelse(!is.na(conf), pmin(pmax(round(conf), 0), 100), NA))

  df <- data.frame(
    id = pend$id[ok], sentimiento = pred[ok],
    modelo = MODELO, confianza = conf[ok], prompt_version = PROMPT_VERSION,
    stringsAsFactors = FALSE
  )
  escritos  <- escritos + upsert_resultados(con, df)
  fallidos  <- fallidos + sum(!ok)
  procesados <- procesados + nrow(pend)

  rate <- procesados / as.numeric(difftime(Sys.time(), t0, units = "mins"))
  message(sprintf("  +%d/%d ok (%d fallidos) | acum: %d escritos | %.0f/min | pend ~%d",
                  as.integer(sum(ok)), nrow(pend), as.integer(sum(!ok)),
                  as.integer(escritos), rate,
                  as.integer(max(0, as.numeric(total_pend) - procesados))))

  if (nrow(pend) < CHUNK) break
  if (LIMIT_TOTAL > 0L && procesados >= LIMIT_TOTAL) { message("LIMIT alcanzado."); break }
}

message("======================================================")
message("Completado | escritos: ", escritos, " | sin etiqueta (reintentar): ", fallidos)
dist <- tryCatch(dbGetQuery(con,
  "SELECT sentimiento, COUNT(*) AS n FROM noticias_sentimiento WHERE prompt_version = $1 GROUP BY 1 ORDER BY 2 DESC",
  params = list(PROMPT_VERSION)), error = function(e) NULL)
if (!is.null(dist)) { message("Distribución (", PROMPT_VERSION, "):"); for (i in seq_len(nrow(dist))) message("  ", dist$sentimiento[i], ": ", dist$n[i]) }
message("======================================================")
