#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_analisis_titulos.R
# Lee noticias (solo titulo, fecha), tokeniza titulares y escribe en
# titulos_terminos_diarios y metricas_titulos_diarias.
# Ejecutar: Rscript run_analisis_titulos.R
# Variables de entorno: PGUSER, PGPASSWORD, PGHOST, PGPORT, PGDATABASE
# Opcional: install.packages("stringi") para normalizar Unicode (recomendado para bío-bío, colo-colo).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
})

# ------------------------------------------------------------------------------
# Configuración (PGPASSWORD: variable de entorno o archivo .env en este directorio)
# ------------------------------------------------------------------------------
PGHOST   <- Sys.getenv("PGHOST",   "localhost")
PGPORT   <- as.integer(Sys.getenv("PGPORT", "5432"))
PGUSER   <- Sys.getenv("PGUSER",   "noticias")
PGPASSWORD <- Sys.getenv("PGPASSWORD")
PGDATABASE <- Sys.getenv("PGDATABASE", "noticias_chile")

# Si PGPASSWORD no viene del entorno, intentar leer .env (cwd = raíz del proyecto, o carpeta del script)
if (!nzchar(PGPASSWORD)) {
  env_file <- file.path(getwd(), ".env")
  if (!file.exists(env_file) && length(args <- commandArgs()) > 0L) {
    idx <- grep("^--file=", args)
    if (length(idx)) env_file <- file.path(dirname(sub("^--file=", "", args[idx[1L]])), ".env")
  }
  if (file.exists(env_file)) {
    env_lines <- readLines(env_file, warn = FALSE)
    for (line in env_lines) {
      line <- sub("^[[:space:]]*#.*$", "", line)
      if (grepl("^PGPASSWORD=", line)) {
        PGPASSWORD <- sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$", "\\1", line)
        break
      }
    }
  }
}
if (!nzchar(PGPASSWORD)) {
  stop("Definir PGPASSWORD (ej. export PGPASSWORD='...' o en .env en la raíz del proyecto)")
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
  "mil", "nuevo", "nueva", "nuevos", "nuevas", "dos", "tres", "uno",
  "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "ciento", "cientos",
  "primera", "primero", "primer", "segunda", "segundo", "tercera", "tercero", "cuarta", "quinto",
  "otro", "otra", "otros", "otras", "mismo", "misma", "mismos", "mismas",
  "todo", "toda", "todos", "todas", "algo", "alguno", "alguna", "algunos", "algunas",
  "cada", "cual", "cuales", "cualquier", "cualesquiera",
  "puede", "pueden", "poder", "debe", "deben", "deber",
  "sido", "estado", "será", "serán", "había", "habían", "habrá", "habrán",
  "revisa", "cable", "aquí", "tiene", "pide", "así",
  "anuncia", "anuncian", "anuncio", "confirma", "confirman", "confirmó",
  "revela", "revelan", "informa", "informan", "asegura", "aseguran",
  "advierte", "advierten", "destaca", "destacan", "señala", "señalan",
  "indica", "indican", "reporta", "reportan", "denuncia", "denuncian",
  "explica", "explican", "afirma", "afirman", "sostiene", "sostienen",
  "dice", "dicen", "declara", "declaran", "califica", "considera",
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

if (!requireNamespace("stringi", quietly = TRUE))
  message("Recomendado: install.packages(\"stringi\") para que compuestos como bío-bío se detecten bien (normalización Unicode).")

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
# Guiones tipográficos Unicode que se normalizan a guión ASCII (en-dash U+2013, em-dash U+2014, etc.)
# En R el escape \u2013 puede fallar según encoding del archivo; se cubren con intToUtf8 por compatibilidad
DASHES_UNICODE <- paste0(
  "[",
  intToUtf8(0x2010), intToUtf8(0x2011), intToUtf8(0x2012),
  intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0x2015),
  "]"
)

# Compuestos conocidos: cuando aparecen dos palabras consecutivas (con o sin guión en la fuente),
# se emiten como un solo término. Valor = lista de pares c(palabra1, palabra2) en minúsculas.
COMPUESTOS_CONOCIDOS <- list(
  # Deportes / medios Chile
  "Colo Colo" = list(c("colo", "colo")),
  "Bío Bío"   = list(c("bío", "bío"), c("bio", "bio"), c("bio", "bío"), c("bío", "bio")),
  # Países y territorios (bigramas)
  "Estados Unidos"     = list(c("estados", "unidos")),
  "Reino Unido"        = list(c("reino", "unido")),
  "Nueva Zelanda"      = list(c("nueva", "zelanda")),
  "Costa Rica"         = list(c("costa", "rica")),
  "Puerto Rico"        = list(c("puerto", "rico")),
  "San Marino"         = list(c("san", "marino")),
  "Corea del Sur"      = list(c("corea", "sur")),
  "Corea del Norte"    = list(c("corea", "norte")),
  "Emiratos Árabes"    = list(c("emiratos", "árabes"), c("emiratos", "arabes")),
  "Arabia Saudita"     = list(c("arabia", "saudita"), c("arabia", "saudí")),
  "Guinea Ecuatorial"  = list(c("guinea", "ecuatorial")),
  "Sierra Leona"       = list(c("sierra", "leona")),
  "Burkina Faso"       = list(c("burkina", "faso")),
  "Cabo Verde"         = list(c("cabo", "verde")),
  "Timor Oriental"     = list(c("timor", "oriental")),
  "Sri Lanka"          = list(c("sri", "lanka")),
  "Papúa Nueva"        = list(c("papúa", "nueva"), c("papua", "nueva")),
  "Nueva Guinea"       = list(c("nueva", "guinea")),
  "América Latina"     = list(c("américa", "latina"), c("america", "latina")),
  "Medio Oriente"      = list(c("medio", "oriente")),
  "Naciones Unidas"    = list(c("naciones", "unidas")),
  "Ciudad del Vaticano" = list(c("ciudad", "vaticano")),
  "Hong Kong"          = list(c("hong", "kong")),
  # Organizaciones y términos frecuentes en noticias
  "Casa Blanca"        = list(c("casa", "blanca")),
  "Banco Central"      = list(c("banco", "central")),
  "Fuerzas Armadas"    = list(c("fuerzas", "armadas")),
  "Derechos Humanos"   = list(c("derechos", "humanos")),
  "Cruz Roja"          = list(c("cruz", "roja")),
  "Unión Europea"      = list(c("unión", "europea"), c("union", "europea")),
  "Primera Guerra"     = list(c("primera", "guerra")),
  "Segunda Guerra"     = list(c("segunda", "guerra")),
  "Guerra Mundial"     = list(c("guerra", "mundial")),
  "Poder Judicial"     = list(c("poder", "judicial")),
  "Tribunal Constitucional" = list(c("tribunal", "constitucional")),
  "Cámara de Diputados" = list(c("cámara", "diputados"), c("camara", "diputados")),
  "Presidente de la República" = list(c("presidente", "república"), c("presidente", "republica"))
)

# Formas con guión que se normalizan al nombre con espacio (cuando la fuente escribe "estados-unidos" etc.)
GUION_A_NOMBRE <- c(
  "estados-unidos" = "Estados Unidos", "reino-unido" = "Reino Unido", "nueva-zelanda" = "Nueva Zelanda",
  "costa-rica" = "Costa Rica", "puerto-rico" = "Puerto Rico", "corea-del-sur" = "Corea del Sur",
  "corea-del-norte" = "Corea del Norte", "emiratos-árabes" = "Emiratos Árabes", "emiratos-arabes" = "Emiratos Árabes",
  "arabia-saudita" = "Arabia Saudita", "guinea-ecuatorial" = "Guinea Ecuatorial", "sierra-leona" = "Sierra Leona",
  "burkina-faso" = "Burkina Faso", "cabo-verde" = "Cabo Verde", "timor-oriental" = "Timor Oriental",
  "sri-lanka" = "Sri Lanka", "papúa-nueva" = "Papúa Nueva", "papua-nueva" = "Papúa Nueva",
  "nueva-guinea" = "Nueva Guinea", "américa-latina" = "América Latina", "america-latina" = "América Latina",
  "medio-oriente" = "Medio Oriente", "naciones-unidas" = "Naciones Unidas", "hong-kong" = "Hong Kong",
  "casa-blanca" = "Casa Blanca", "banco-central" = "Banco Central", "fuerzas-armadas" = "Fuerzas Armadas",
  "derechos-humanos" = "Derechos Humanos", "cruz-roja" = "Cruz Roja", "unión-europea" = "Unión Europea", "union-europea" = "Unión Europea",
  "san-marino" = "San Marino", "ciudad-vaticano" = "Ciudad del Vaticano",
  "poder-judicial" = "Poder Judicial", "tribunal-constitucional" = "Tribunal Constitucional",
  "colo-colo" = "Colo Colo", "bío-bío" = "Bío Bío", "bio-bio" = "Bío Bío"
)

# Unir bigramas que forman un compuesto conocido (devuelve vector de tokens)
fusionar_compuestos_conocidos <- function(tokens, compuestos) {
  if (length(tokens) < 2L) return(tokens)
  out <- character(0)
  i <- 1L
  while (i <= length(tokens)) {
    fused <- FALSE
    if (i < length(tokens)) {
      par <- c(tokens[i], tokens[i + 1L])
      if (requireNamespace("stringi", quietly = TRUE))
        par <- stringi::stri_trans_nfc(par)
      for (nom in names(compuestos)) {
        pares <- compuestos[[nom]]
        for (p in pares) {
          if (identical(p, par)) {
            out <- c(out, nom)
            i <- i + 2L
            fused <- TRUE
            break
          }
        }
        if (fused) break
      }
    }
    if (!fused) {
      out <- c(out, tokens[i])
      i <- i + 1L
    }
  }
  out
}

tokenizar_titulo <- function(titulo, stopwords, min_len = 3L) {
  if (is.na(titulo) || !nzchar(trimws(titulo))) return(character(0))
  txt <- tolower(trimws(titulo))
  # Normalizar Unicode a NFC para que "bío" (y fusión bío-bío) coincida aunque la BD devuelva NFD (i + acento)
  if (requireNamespace("stringi", quietly = TRUE))
    txt <- stringi::stri_trans_nfc(txt)
  # Normalizar guiones tipográficos a guión ASCII para preservar compuestos escritos con en-dash/em-dash
  txt <- gsub(DASHES_UNICODE, "-", txt)
  # Quitar entidades HTML (ej. &quot; &amp;) para no dejar "quot", "amp" como términos
  txt <- gsub("&[a-z0-9]+;", " ", txt)
  txt <- gsub("&#[0-9]+;", " ", txt)
  # Quitar comillas y fragmentos que dejan "quot" en el texto crudo
  txt <- gsub("[\"']", " ", txt)
  # Quitar puntuación; mantener letras (ñ, acentos), números y guión (para palabras compuestas tipo "colo-colo")
  txt <- gsub("[^a-z0-9ñáéíóúü\\s-]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  tokens <- strsplit(txt, " ", fixed = FALSE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  tokens <- tokens[nchar(tokens) >= min_len]
  # Excluir tokens que son solo números (ej. "36", "2024")
  tokens <- tokens[grepl("[a-zñáéíóúü]", tokens, ignore.case = TRUE)]
  # Fusionar bigramas conocidos (ej. "estados" + "unidos" -> "Estados Unidos") cuando la fuente escribe con espacio
  tokens <- fusionar_compuestos_conocidos(tokens, COMPUESTOS_CONOCIDOS)
  # Unificar forma con guión a la forma con espacio (estados-unidos -> Estados Unidos, etc.)
  for (k in seq_along(GUION_A_NOMBRE)) {
    tokens[tokens == names(GUION_A_NOMBRE)[k]] <- unname(GUION_A_NOMBRE[k])
  }
  # Palabras compuestas con guión: conservar si es un compuesto conocido (Bío Bío, Colo Colo) o si tienen forma palabra-palabra
  compuestos_ok <- vapply(tokens, function(t) {
    if (!grepl("-", t, fixed = TRUE)) return(TRUE)
    if (t %in% names(COMPUESTOS_CONOCIDOS)) return(TRUE)
    partes <- strsplit(t, "-", fixed = TRUE)[[1]]
    if (length(partes) != 2L) return(FALSE)
    nchar(partes[1L]) >= min_len &&
      nchar(partes[2L]) >= min_len &&
      grepl("^[a-zñáéíóúü]+$", partes[1L]) &&
      grepl("^[a-zñáéíóúü]+$", partes[2L])
  }, logical(1))
  tokens <- tokens[compuestos_ok]
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
  # Respetar límite de la tabla (termino VARCHAR(150)); descartar términos más largos para evitar error en INSERT
  max_len_termino <- 150L
  df_agg <- df_agg[nchar(as.character(df_agg$termino)) <= max_len_termino, ]

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
