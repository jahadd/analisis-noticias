#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_analisis_titulos.R
# Lee noticias (solo titulo, fecha), tokeniza titulares y escribe en
# titulos_terminos_diarios y metricas_titulos_diarias.
# Ejecutar: Rscript run_analisis_titulos.R
# Variables de entorno: PGUSER, PGPASSWORD, PGHOST, PGPORT, PGDATABASE
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

# ------------------------------------------------------------------------------
# Configuración
# ------------------------------------------------------------------------------
PGHOST   <- Sys.getenv("PGHOST",   "localhost")
PGPORT   <- as.integer(Sys.getenv("PGPORT", "5432"))
PGUSER   <- Sys.getenv("PGUSER",   "noticias")
PGPASSWORD <- Sys.getenv("PGPASSWORD")
PGDATABASE <- Sys.getenv("PGDATABASE", "noticias_chile")

if (!nzchar(PGPASSWORD)) {
  stop("Definir PGPASSWORD (ej. export PGPASSWORD='...')")
}

# Tamaño del lote por fechas: procesar de a N días para no cargar todo en memoria
CHUNK_DAYS <- 90L
# Longitud mínima del término (caracteres)
MIN_TERM_LEN <- 3L

# Stopwords (español) y ruido: artículos, preposiciones, verbos auxiliares, números en texto, restos HTML
STOPWORDS <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas",
  "y", "o", "pero", "que", "en", "a", "de", "del", "al", "a la",
  "por", "para", "con", "sin", "sobre", "entre", "hasta", "desde",
  "su", "sus", "se", "lo", "le", "como", "más", "menos", "muy",
  "este", "esta", "estos", "estas", "ese", "esa", "eso", "aquél", "aquella",
  "qué", "cuál", "cómo", "cuándo", "dónde", "quién", "cuánto",
  "ser", "es", "son", "fue", "fueron", "ha", "han", "hay", "está", "están",
  "también", "solo", "sólo", "después", "antes", "durante", "tras",
  "según", "contra", "mediante", "excepto", "hacia",
  "no", "ni", "nos", "nosotros", "ante", "bajo", "tras",
  "años", "año", "mes", "meses", "día", "días", "hora", "horas",
  "otro", "otra", "otros", "otras", "mismo", "misma", "mismos", "mismas",
  "todo", "toda", "todos", "todas", "algo", "alguno", "alguna", "algunos", "algunas",
  "cada", "cual", "cuales", "cualquier", "cualesquiera",
  "puede", "pueden", "poder", "debe", "deben", "deber",
  "sido", "estado", "será", "serán", "había", "habían", "habrá", "habrán",
  "quot", "amp", "lt", "gt", "nbsp", "mdash", "ndash", "rsquo", "lsquo", "hellip"
)

# ------------------------------------------------------------------------------
# Conexión
# ------------------------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  host     = PGHOST,
  port     = PGPORT,
  user     = PGUSER,
  password = PGPASSWORD,
  dbname   = PGDATABASE
)
on.exit(dbDisconnect(con), add = TRUE)

message("Conectado a ", PGDATABASE, " en ", PGHOST)

# ------------------------------------------------------------------------------
# Rango de fechas a procesar (todo el histórico; para incremental, filtrar)
# ------------------------------------------------------------------------------
rango <- dbGetQuery(con, "
  SELECT COALESCE(MIN(fecha), CURRENT_DATE) AS min_fecha,
         COALESCE(MAX(fecha), CURRENT_DATE) AS max_fecha,
         COUNT(*) AS total
  FROM noticias
")
min_fecha <- as.Date(rango$min_fecha)
max_fecha <- as.Date(rango$max_fecha)
total_noticias_db <- rango$total
message("Rango en BD: ", min_fecha, " a ", max_fecha, " (", total_noticias_db, " noticias)")

# Vaciar tablas de agregados para que esta ejecución regenere todo con los filtros actuales
# (si no, términos ya eliminados del pipeline como "quot" o "no" seguirían en la BD)
dbExecute(con, "TRUNCATE TABLE titulos_terminos_diarios")
dbExecute(con, "TRUNCATE TABLE metricas_titulos_diarias")
message("Tablas de agregados vaciadas; recalculando con filtros actuales.")

# ------------------------------------------------------------------------------
# Tokenización: limpiar HTML/entidades, minúsculas, split, filtrar ruido y números
# ------------------------------------------------------------------------------
tokenizar_titulo <- function(titulo, stopwords, min_len = 3L) {
  if (is.na(titulo) || !nzchar(trimws(titulo))) return(character(0))
  txt <- tolower(trimws(titulo))
  # Quitar entidades HTML (ej. &quot; &amp;) para no dejar "quot", "amp" como términos
  txt <- gsub("&[a-z0-9]+;", " ", txt)
  txt <- gsub("&#[0-9]+;", " ", txt)
  # Quitar comillas y fragmentos que dejan "quot" en el texto crudo
  txt <- gsub("[\"']", " ", txt)
  # Quitar puntuación; mantener letras (ñ, acentos) y números para luego filtrar solo-números
  txt <- gsub("[^a-z0-9ñáéíóúü\\s]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  tokens <- strsplit(txt, " ", fixed = FALSE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  tokens <- tokens[nchar(tokens) >= min_len]
  # Excluir tokens que son solo números (ej. "36", "2024")
  tokens <- tokens[grepl("[a-zñáéíóúü]", tokens, ignore.case = TRUE)]
  tokens <- tokens[!tokens %in% stopwords]
  tokens
}

# ------------------------------------------------------------------------------
# Procesar por chunks de fechas
# ------------------------------------------------------------------------------
fecha_actual <- min_fecha
filas_leidas <- 0L
todas_las_fechas <- character(0)

while (fecha_actual <= max_fecha) {
  fecha_fin <- min(fecha_actual + CHUNK_DAYS - 1L, max_fecha)
  chunk <- dbGetQuery(con, "
    SELECT titulo, fecha
    FROM noticias
    WHERE fecha >= $1 AND fecha <= $2
    ORDER BY fecha
  ", params = list(fecha_actual, fecha_fin))

  if (nrow(chunk) == 0L) {
    fecha_actual <- fecha_fin + 1L
    next
  }

  filas_leidas <- filas_leidas + nrow(chunk)
  message("  Procesando ", nrow(chunk), " filas (", as.character(fecha_actual), " a ", as.character(fecha_fin), ")")

  # Por cada fila: tokenizar y asociar a fecha
  listas_terminos <- lapply(chunk$titulo, tokenizar_titulo, stopwords = STOPWORDS, min_len = MIN_TERM_LEN)
  fechas_chunk <- as.character(chunk$fecha)

  # Construir tabla (fecha, termino) expandida
  fecha_vec <- character(0)
  termino_vec <- character(0)
  for (i in seq_len(nrow(chunk))) {
    terms <- listas_terminos[[i]]
    if (length(terms) > 0L) {
      fecha_vec <- c(fecha_vec, rep(fechas_chunk[i], length(terms)))
      termino_vec <- c(termino_vec, terms)
    }
  }

  if (length(fecha_vec) == 0L) {
    fecha_actual <- fecha_fin + 1L
    next
  }

  # Agregar por (fecha, termino)
  df_agg <- aggregate(
    list(frecuencia = termino_vec),
    by = list(fecha = fecha_vec, termino = termino_vec),
    length
  )
  df_agg$fecha <- as.Date(df_agg$fecha)

  # UPSERT titulos_terminos_diarios (por lotes de 500)
  BATCH <- 500L
  n_agg <- nrow(df_agg)
  for (start in seq(1L, n_agg, by = BATCH)) {
    end <- min(start + BATCH - 1L, n_agg)
    sub <- df_agg[start:end, ]
    n_sub <- nrow(sub)
    # Placeholders: ($1,$2,$3), ($4,$5,$6), ...
    placeholders <- paste(
      sprintf("($%d, $%d, $%d)", 3L * (seq_len(n_sub) - 1L) + 1L, 3L * (seq_len(n_sub) - 1L) + 2L, 3L * (seq_len(n_sub) - 1L) + 3L),
      collapse = ", "
    )
    q <- paste0(
      "INSERT INTO titulos_terminos_diarios (fecha, termino, frecuencia) VALUES ", placeholders,
      " ON CONFLICT (fecha, termino) DO UPDATE SET frecuencia = EXCLUDED.frecuencia"
    )
    params <- as.list(as.vector(t(sub[, c("fecha", "termino", "frecuencia")])))
    dbExecute(con, q, params = params)
  }

  # Métricas diarias para este chunk: por cada fecha, total_noticias y terminos_unicos
  noticias_por_fecha <- aggregate(list(total_noticias = chunk$fecha), by = list(fecha = chunk$fecha), length)
  terminos_por_fecha <- aggregate(list(terminos_unicos = df_agg$termino), by = list(fecha = df_agg$fecha), function(x) length(unique(x)))
  metricas <- merge(noticias_por_fecha, terminos_por_fecha, by = "fecha", all.x = TRUE)
  metricas$terminos_unicos[is.na(metricas$terminos_unicos)] <- 0L
  metricas$fecha <- as.Date(metricas$fecha)

  for (i in seq_len(nrow(metricas))) {
    dbExecute(con, "
      INSERT INTO metricas_titulos_diarias (fecha, total_noticias, terminos_unicos)
      VALUES ($1, $2, $3)
      ON CONFLICT (fecha) DO UPDATE SET
        total_noticias = EXCLUDED.total_noticias,
        terminos_unicos = EXCLUDED.terminos_unicos,
        actualizado_en = CURRENT_TIMESTAMP
    ", params = list(metricas$fecha[i], metricas$total_noticias[i], metricas$terminos_unicos[i]))
  }

  todas_las_fechas <- c(todas_las_fechas, as.character(metricas$fecha))
  fecha_actual <- fecha_fin + 1L
}

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
n_dias <- length(unique(todas_las_fechas))
n_terms <- dbGetQuery(con, "SELECT COUNT(DISTINCT termino) AS n FROM titulos_terminos_diarios")$n
message("Listo. Filas leídas: ", filas_leidas)
message("Días con métricas: ", n_dias)
message("Términos distintos en titulos_terminos_diarios: ", n_terms)
