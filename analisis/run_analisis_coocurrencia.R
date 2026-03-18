#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_analisis_coocurrencia.R  [pipeline step 3 of 5]
# Lee noticias (id, titulo, fecha, fuente), tokeniza titulares y calcula pares de
# términos que co-aparecen en el mismo titular por fuente y fecha.
# Orden de ejecución: 1) cargar_parquet_a_postgres.py  2) run_analisis_titulos.R
#                      3) este script  4) run_analisis_semanal.R  5) run_sentimiento.R
# Ejecutar: Rscript run_analisis_coocurrencia.R
# Variables de entorno: mismas que run_analisis_titulos.R
# Paquetes requeridos: DBI, RPostgres, tidytext, dplyr, purrr, furrr, tidyr
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(tidytext)
  library(dplyr)
  library(purrr)
  library(furrr)
  library(tidyr)
  if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
})

# ------------------------------------------------------------------------------
# Configuración: .env
# ------------------------------------------------------------------------------
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
    env_lines <- readLines(env_file, warn = FALSE)
    for (line in env_lines) {
      line <- sub("^[[:space:]]*#.*$", "", line)
      if (!nzchar(trimws(line))) next
      if (grepl("^PGHOST=", line))   Sys.setenv(PGHOST   = sub("^PGHOST=([^[:space:]]+).*", "\\1", line))
      if (grepl("^PGPORT=", line))   Sys.setenv(PGPORT   = sub("^PGPORT=([^[:space:]]+).*", "\\1", line))
      if (grepl("^PGUSER_NOTICIAS=", line)) Sys.setenv(PGUSER_NOTICIAS = sub("^PGUSER_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGPASSWORD_NOTICIAS=", line)) Sys.setenv(PGPASSWORD_NOTICIAS = sub("^PGPASSWORD_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGDATABASE_NOTICIAS=", line)) Sys.setenv(PGDATABASE_NOTICIAS = sub("^PGDATABASE_NOTICIAS=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGUSER=", line))   Sys.setenv(PGUSER   = sub("^PGUSER=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGPASSWORD=", line)) Sys.setenv(PGPASSWORD = sub("^PGPASSWORD=[\"']?([^\"']*)[\"']?$", "\\1", line))
      if (grepl("^PGDATABASE=", line)) Sys.setenv(PGDATABASE = sub("^PGDATABASE=[\"']?([^\"']*)[\"']?$", "\\1", line))
    }
    break
  }
}

`%||%` <- function(x, y) if (nzchar(x)) x else y

PGHOST    <- Sys.getenv("PGHOST", "localhost")
PGPORT    <- as.integer(Sys.getenv("PGPORT", "5432"))
PGUSER    <- Sys.getenv("PGUSER_NOTICIAS") %||% Sys.getenv("PGUSER", "noticias")
PGPASSWORD <- Sys.getenv("PGPASSWORD_NOTICIAS") %||% Sys.getenv("PGPASSWORD")
PGDATABASE <- Sys.getenv("PGDATABASE_NOTICIAS") %||% Sys.getenv("PGDATABASE", "noticias_chile")

if (!nzchar(PGPASSWORD)) {
  stop("Definir PGPASSWORD_NOTICIAS o PGPASSWORD (variable de entorno o .env en Paginaweb)")
}

source(file.path(script_dir, "..", "stopwords.R"))

CHUNK_DAYS   <- 90L
MIN_TERM_LEN <- 3L
BATCH        <- 5000L
FECHA_DESDE  <- as.Date("2018-01-01")

DASHES_UNICODE <- paste0(
  "[",
  intToUtf8(0x2010), intToUtf8(0x2011), intToUtf8(0x2012),
  intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0x2015),
  "]"
)

COMPUESTOS_CONOCIDOS <- list(
  "Colo Colo" = list(c("colo", "colo")),
  "Bío Bío"   = list(c("bío", "bío"), c("bio", "bio"), c("bio", "bío"), c("bío", "bio")),
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

# ------------------------------------------------------------------------------
# Función: tokenizar chunk y generar pares de co-ocurrencia por artículo
# Devuelve data.frame(fecha, fuente, termino_a, termino_b, n)
# con termino_a < termino_b (orden alfabético)
# ------------------------------------------------------------------------------
coocurrencia_chunk <- function(chunk_df, stopwords, min_len = 3L,
                                compuestos = COMPUESTOS_CONOCIDOS,
                                guion_a_nombre = GUION_A_NOMBRE,
                                dashes_unicode = DASHES_UNICODE) {
  if (nrow(chunk_df) == 0L) return(NULL)

  chunk_prep <- chunk_df |>
    filter(!is.na(titulo), nchar(trimws(titulo)) > 0L) |>
    mutate(
      titulo = tolower(trimws(titulo)),
      titulo = if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc(titulo) else titulo,
      titulo = gsub(dashes_unicode, "-", titulo),
      titulo = gsub("&[a-z0-9]+;", " ", titulo),
      titulo = gsub("&#[0-9]+;", " ", titulo),
      titulo = gsub("[\"']", " ", titulo)
    )

  fusionar_comp <- function(tokens, comp) {
    if (length(tokens) < 2L) return(tokens)
    out <- character(0); i <- 1L
    while (i <= length(tokens)) {
      fused <- FALSE
      if (i < length(tokens)) {
        par <- c(tokens[i], tokens[i + 1L])
        if (requireNamespace("stringi", quietly = TRUE)) par <- stringi::stri_trans_nfc(par)
        for (nom in names(comp)) {
          for (p in comp[[nom]]) {
            if (identical(p, par)) { out <- c(out, nom); i <- i + 2L; fused <- TRUE; break }
          }
          if (fused) break
        }
      }
      if (!fused) { out <- c(out, tokens[i]); i <- i + 1L }
    }
    out
  }

  tokens_por_articulo <- chunk_prep |>
    unnest_tokens(output = termino, input = titulo, token = "words", drop = TRUE) |>
    filter(
      nchar(termino) >= min_len,
      grepl("[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]", termino, ignore.case = TRUE),
      !termino %in% stopwords
    ) |>
    group_by(id, fecha, fuente) |>
    summarise(toks = list(termino), .groups = "drop") |>
    mutate(toks = lapply(toks, function(t) {
      t <- fusionar_comp(t, compuestos)
      for (k in seq_along(guion_a_nombre)) {
        t[t == names(guion_a_nombre)[k]] <- unname(guion_a_nombre[k])
      }
      t <- t[vapply(t, function(x) {
        if (!grepl("-", x, fixed = TRUE)) return(TRUE)
        if (x %in% names(compuestos)) return(TRUE)
        partes <- strsplit(x, "-", fixed = TRUE)[[1]]
        length(partes) == 2L && nchar(partes[1L]) >= min_len && nchar(partes[2L]) >= min_len &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[1L]) &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[2L])
      }, logical(1))]
      sort(unique(t))
    })) |>
    filter(lengths(toks) >= 2L)

  if (nrow(tokens_por_articulo) == 0L) return(NULL)

  pares_list <- lapply(seq_len(nrow(tokens_por_articulo)), function(i) {
    t     <- tokens_por_articulo$toks[[i]]
    fecha <- tokens_por_articulo$fecha[[i]]
    fuente <- tokens_por_articulo$fuente[[i]]
    if (length(t) < 2L) return(NULL)
    idx <- combn(length(t), 2L)
    data.frame(
      fecha    = rep(as.character(fecha), ncol(idx)),
      fuente   = rep(fuente, ncol(idx)),
      termino_a = t[idx[1L, ]],
      termino_b = t[idx[2L, ]],
      stringsAsFactors = FALSE
    )
  })
  pares <- do.call(rbind, Filter(Negate(is.null), pares_list))
  if (is.null(pares) || nrow(pares) == 0L) return(NULL)

  pares$fecha <- as.Date(pares$fecha)

  # Garantizar orden alfabético termino_a < termino_b
  swap <- pares$termino_a > pares$termino_b
  tmp <- pares$termino_a[swap]
  pares$termino_a[swap] <- pares$termino_b[swap]
  pares$termino_b[swap] <- tmp

  # Filtrar auto-pares (no deben ocurrir tras el sort(unique), pero por seguridad)
  pares <- pares[pares$termino_a != pares$termino_b, ]

  # Filtrar longitud máxima VARCHAR(150)
  pares <- pares[nchar(pares$termino_a) <= 150L & nchar(pares$termino_b) <= 150L, ]

  # Agregar por (fecha, fuente, termino_a, termino_b)
  resultado <- pares |>
    count(fecha, fuente, termino_a, termino_b, name = "n")

  resultado
}

# ------------------------------------------------------------------------------
# Función para procesar un chunk en worker paralelo (debe ser self-contained)
# ------------------------------------------------------------------------------
procesar_cooc_chunk <- function(fecha_actual, max_fecha, con_params, stopwords, chunk_days, min_len) {
  suppressPackageStartupMessages({
    library(DBI); library(RPostgres); library(tidytext); library(dplyr); library(tidyr)
    if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
  })

  fecha_fin <- min(fecha_actual + chunk_days - 1L, max_fecha)
  con_local <- dbConnect(
    RPostgres::Postgres(),
    host = con_params$host, port = con_params$port,
    user = con_params$user, password = con_params$password, dbname = con_params$dbname
  )
  on.exit(dbDisconnect(con_local), add = TRUE)

  chunk <- dbGetQuery(con_local, "
    SELECT id, titulo, fecha, fuente
    FROM noticias
    WHERE fecha >= $1 AND fecha <= $2
      AND titulo IS NOT NULL AND titulo <> ''
  ", params = list(fecha_actual, fecha_fin))

  if (nrow(chunk) == 0L) return(NULL)
  chunk$fecha <- as.Date(chunk$fecha)

  dashes_unicode <- paste0("[",
    intToUtf8(8208), intToUtf8(8209), intToUtf8(8210),
    intToUtf8(8211), intToUtf8(8212), intToUtf8(8213), "]")

  COMP_LOC <- list(
    "Colo Colo" = list(c("colo","colo")),
    "Bío Bío"   = list(c("bío","bío"),c("bio","bio"),c("bio","bío"),c("bío","bio")),
    "Estados Unidos" = list(c("estados","unidos")), "Reino Unido" = list(c("reino","unido")),
    "Nueva Zelanda" = list(c("nueva","zelanda")), "Costa Rica" = list(c("costa","rica")),
    "Puerto Rico" = list(c("puerto","rico")), "San Marino" = list(c("san","marino")),
    "Corea del Sur" = list(c("corea","sur")), "Corea del Norte" = list(c("corea","norte")),
    "Emiratos Árabes" = list(c("emiratos","árabes"),c("emiratos","arabes")),
    "Arabia Saudita" = list(c("arabia","saudita"),c("arabia","saudí")),
    "Guinea Ecuatorial" = list(c("guinea","ecuatorial")), "Sierra Leona" = list(c("sierra","leona")),
    "Burkina Faso" = list(c("burkina","faso")), "Cabo Verde" = list(c("cabo","verde")),
    "Timor Oriental" = list(c("timor","oriental")), "Sri Lanka" = list(c("sri","lanka")),
    "Papúa Nueva" = list(c("papúa","nueva"),c("papua","nueva")),
    "Nueva Guinea" = list(c("nueva","guinea")),
    "América Latina" = list(c("américa","latina"),c("america","latina")),
    "Medio Oriente" = list(c("medio","oriente")),
    "Naciones Unidas" = list(c("naciones","unidas")),
    "Ciudad del Vaticano" = list(c("ciudad","vaticano")),
    "Hong Kong" = list(c("hong","kong")), "Casa Blanca" = list(c("casa","blanca")),
    "Banco Central" = list(c("banco","central")), "Fuerzas Armadas" = list(c("fuerzas","armadas")),
    "Derechos Humanos" = list(c("derechos","humanos")), "Cruz Roja" = list(c("cruz","roja")),
    "Unión Europea" = list(c("unión","europea"),c("union","europea")),
    "Primera Guerra" = list(c("primera","guerra")), "Segunda Guerra" = list(c("segunda","guerra")),
    "Guerra Mundial" = list(c("guerra","mundial")),
    "Poder Judicial" = list(c("poder","judicial")),
    "Tribunal Constitucional" = list(c("tribunal","constitucional")),
    "Cámara de Diputados" = list(c("cámara","diputados"),c("camara","diputados")),
    "Presidente de la República" = list(c("presidente","república"),c("presidente","republica")),
    "José Antonio Kast"  = list(c("josé","antonio","kast"), c("jose","antonio","kast")),
    "Gabriel Boric"      = list(c("gabriel","boric")),
    "Sebastián Piñera"   = list(c("sebastián","piñera"), c("sebastian","pinera"), c("sebastian","piñera")),
    "Michelle Bachelet"  = list(c("michelle","bachelet")),
    "Camila Vallejo"     = list(c("camila","vallejo")),
    "Carolina Tohá"      = list(c("carolina","tohá"), c("carolina","toha")),
    "Evelyn Matthei"     = list(c("evelyn","matthei")),
    "Mario Desbordes"    = list(c("mario","desbordes")),
    "Giorgio Jackson"    = list(c("giorgio","jackson")),
    "Rodrigo Valdés"     = list(c("rodrigo","valdés"), c("rodrigo","valdes")),
    "Daniel Jadue"       = list(c("daniel","jadue")),
    "Yasna Provoste"     = list(c("yasna","provoste")),
    "Heraldo Muñoz"      = list(c("heraldo","muñoz"), c("heraldo","munoz")),
    "Felipe Kast"        = list(c("felipe","kast")),
    # Figuras políticas chilenas adicionales
    "Mario Marcel"       = list(c("mario","marcel")),
    "Johannes Kaiser"    = list(c("johannes","kaiser")),
    "Rodolfo Carter"     = list(c("rodolfo","carter")),
    "Jeannette Jara"     = list(c("jeannette","jara")),
    "Gonzalo Winter"     = list(c("gonzalo","winter")),
    "Paulina Vodanovic"  = list(c("paulina","vodanovic")),
    "Tomás Vodanovic"    = list(c("tomás","vodanovic"), c("tomas","vodanovic")),
    "Claudio Orrego"     = list(c("claudio","orrego")),
    "Isabel Allende"     = list(c("isabel","allende")),
    # Líderes internacionales adicionales
    "Lula da Silva"      = list(c("lula","da","silva"), c("lula","silva")),
    "Emmanuel Macron"    = list(c("emmanuel","macron")),
    "Giorgia Meloni"     = list(c("giorgia","meloni")),
    "Benjamin Netanyahu" = list(c("benjamin","netanyahu")),
    "Ursula von der Leyen" = list(c("von","der","leyen")),
    "Nayib Bukele"       = list(c("nayib","bukele")),
    "Nicolás Maduro"     = list(c("nicolás","maduro"), c("nicolas","maduro")),
    "Delcy Rodríguez"    = list(c("delcy","rodríguez"), c("delcy","rodriguez")),
    "Elon Musk"          = list(c("elon","musk")),
    # Partidos políticos chilenos
    "Democracia Cristiana"          = list(c("democracia","cristiana")),
    "Renovación Nacional"           = list(c("renovación","nacional"), c("renovacion","nacional")),
    "Unión Demócrata Independiente" = list(c("unión","demócrata","independiente"), c("union","democrata","independiente")),
    "Partido Comunista"             = list(c("partido","comunista")),
    "Partido Socialista"            = list(c("partido","socialista")),
    "Partido Por la Democracia"     = list(c("partido","democracia")),
    "Partido Republicano"           = list(c("partido","republicano")),
    "Partido Liberal"               = list(c("partido","liberal")),
    "Partido Radical"               = list(c("partido","radical")),
    "Partido de la Gente"           = list(c("partido","gente")),
    "Frente Amplio"                 = list(c("frente","amplio")),
    "Convergencia Social"           = list(c("convergencia","social")),
    "Revolución Democrática"        = list(c("revolución","democrática"), c("revolucion","democratica")),
    "Acción Humanista"              = list(c("acción","humanista"), c("accion","humanista")),
    "Chile Vamos"                   = list(c("chile","vamos")),
    "Apruebo Dignidad"              = list(c("apruebo","dignidad")),
    "Donald Trump"       = list(c("donald","trump")),
    "Joe Biden"          = list(c("joe","biden")),
    "Vladimir Putin"     = list(c("vladimir","putin")),
    "Xi Jinping"         = list(c("xi","jinping")),
    "Corte Suprema"      = list(c("corte","suprema")),
    "Fiscalía Nacional"  = list(c("fiscalía","nacional"), c("fiscalia","nacional")),
    "Contraloría General" = list(c("contraloría","general"), c("contraloria","general")),
    "Banco Estado"       = list(c("banco","estado")),
    "Copa América"       = list(c("copa","américa"), c("copa","america")),
    "Copa Libertadores"  = list(c("copa","libertadores")),
    "Universidad de Chile" = list(c("universidad","chile")),
    "Matteo Romano"      = list(c("matteo","romano")),
    # Ministerios
    "Ministerio del Interior"             = list(c("ministerio","interior")),
    "Ministerio de Defensa"               = list(c("ministerio","defensa")),
    "Ministerio de Hacienda"              = list(c("ministerio","hacienda")),
    "Ministerio de Economía"              = list(c("ministerio","economía"), c("ministerio","economia")),
    "Ministerio de Educación"             = list(c("ministerio","educación"), c("ministerio","educacion")),
    "Ministerio de Justicia"              = list(c("ministerio","justicia")),
    "Ministerio del Trabajo"              = list(c("ministerio","trabajo")),
    "Ministerio de Salud"                 = list(c("ministerio","salud")),
    "Ministerio de Vivienda"              = list(c("ministerio","vivienda")),
    "Ministerio de Agricultura"           = list(c("ministerio","agricultura")),
    "Ministerio de Minería"               = list(c("ministerio","minería"), c("ministerio","mineria")),
    "Ministerio de Transporte"            = list(c("ministerio","transporte")),
    "Ministerio de Energía"               = list(c("ministerio","energía"), c("ministerio","energia")),
    "Ministerio de Deportes"              = list(c("ministerio","deportes")),
    "Ministerio de la Mujer"              = list(c("ministerio","mujer")),
    "Ministerio de Ciencia"               = list(c("ministerio","ciencia")),
    "Ministerio de Obras Públicas"        = list(c("ministerio","obras","públicas"), c("ministerio","obras","publicas")),
    "Ministerio del Medio Ambiente"       = list(c("ministerio","medio","ambiente")),
    "Ministerio de Relaciones Exteriores" = list(c("ministerio","relaciones","exteriores")),
    "Ministerio de Desarrollo Social"     = list(c("ministerio","desarrollo","social")),
    # Comunas y ciudades chilenas con prefijo San/Santa/Santo
    "San Antonio"      = list(c("san","antonio")),
    "San Bernardo"     = list(c("san","bernardo")),
    "San Carlos"       = list(c("san","carlos")),
    "San Clemente"     = list(c("san","clemente")),
    "San Cristóbal"    = list(c("san","cristóbal"), c("san","cristobal")),
    "San Esteban"      = list(c("san","esteban")),
    "San Fabián"       = list(c("san","fabián"), c("san","fabian")),
    "San Felipe"       = list(c("san","felipe")),
    "San Fernando"     = list(c("san","fernando")),
    "San Francisco"    = list(c("san","francisco")),
    "San Ignacio"      = list(c("san","ignacio")),
    "San Javier"       = list(c("san","javier")),
    "San Joaquín"      = list(c("san","joaquín"), c("san","joaquin")),
    "San José"         = list(c("san","josé"), c("san","jose")),
    "San Juan"         = list(c("san","juan")),
    "San Miguel"       = list(c("san","miguel")),
    "San Nicolás"      = list(c("san","nicolás"), c("san","nicolas")),
    "San Pablo"        = list(c("san","pablo")),
    "San Pedro"        = list(c("san","pedro")),
    "San Rafael"       = list(c("san","rafael")),
    "San Ramón"        = list(c("san","ramón"), c("san","ramon")),
    "San Rosendo"      = list(c("san","rosendo")),
    "San Vicente"      = list(c("san","vicente")),
    "Santa Bárbara"    = list(c("santa","bárbara"), c("santa","barbara")),
    "Santa Cruz"       = list(c("santa","cruz")),
    "Santa Juana"      = list(c("santa","juana")),
    "Santa María"      = list(c("santa","maría"), c("santa","maria")),
    "Santa Rosa"       = list(c("santa","rosa")),
    "Santo Domingo"    = list(c("santo","domingo"))
  )
  GUION_LOC <- c(
    "estados-unidos"="Estados Unidos","reino-unido"="Reino Unido","nueva-zelanda"="Nueva Zelanda",
    "costa-rica"="Costa Rica","puerto-rico"="Puerto Rico","corea-del-sur"="Corea del Sur",
    "corea-del-norte"="Corea del Norte","emiratos-árabes"="Emiratos Árabes","emiratos-arabes"="Emiratos Árabes",
    "arabia-saudita"="Arabia Saudita","guinea-ecuatorial"="Guinea Ecuatorial","sierra-leona"="Sierra Leona",
    "burkina-faso"="Burkina Faso","cabo-verde"="Cabo Verde","timor-oriental"="Timor Oriental",
    "sri-lanka"="Sri Lanka","papúa-nueva"="Papúa Nueva","papua-nueva"="Papúa Nueva",
    "nueva-guinea"="Nueva Guinea","américa-latina"="América Latina","america-latina"="América Latina",
    "medio-oriente"="Medio Oriente","naciones-unidas"="Naciones Unidas","hong-kong"="Hong Kong",
    "casa-blanca"="Casa Blanca","banco-central"="Banco Central","fuerzas-armadas"="Fuerzas Armadas",
    "derechos-humanos"="Derechos Humanos","cruz-roja"="Cruz Roja",
    "unión-europea"="Unión Europea","union-europea"="Unión Europea",
    "san-marino"="San Marino","ciudad-vaticano"="Ciudad del Vaticano",
    "poder-judicial"="Poder Judicial","tribunal-constitucional"="Tribunal Constitucional",
    "colo-colo"="Colo Colo","bío-bío"="Bío Bío","bio-bio"="Bío Bío",
    "san-antonio"="San Antonio","san-bernardo"="San Bernardo",
    "san-carlos"="San Carlos","san-clemente"="San Clemente",
    "san-cristóbal"="San Cristóbal","san-cristobal"="San Cristóbal",
    "san-esteban"="San Esteban","san-fabián"="San Fabián","san-fabian"="San Fabián",
    "san-felipe"="San Felipe","san-fernando"="San Fernando",
    "san-francisco"="San Francisco","san-ignacio"="San Ignacio",
    "san-javier"="San Javier","san-joaquín"="San Joaquín","san-joaquin"="San Joaquín",
    "san-josé"="San José","san-jose"="San José",
    "san-juan"="San Juan","san-miguel"="San Miguel",
    "san-nicolás"="San Nicolás","san-nicolas"="San Nicolás",
    "san-pablo"="San Pablo","san-pedro"="San Pedro",
    "san-rafael"="San Rafael","san-ramón"="San Ramón","san-ramon"="San Ramón",
    "san-rosendo"="San Rosendo","san-vicente"="San Vicente",
    "santa-bárbara"="Santa Bárbara","santa-barbara"="Santa Bárbara",
    "santa-cruz"="Santa Cruz","santa-juana"="Santa Juana",
    "santa-maría"="Santa María","santa-maria"="Santa María",
    "santa-rosa"="Santa Rosa","santo-domingo"="Santo Domingo",
    "democracia-cristiana"="Democracia Cristiana",
    "renovación-nacional"="Renovación Nacional","renovacion-nacional"="Renovación Nacional",
    "frente-amplio"="Frente Amplio",
    "partido-comunista"="Partido Comunista",
    "partido-socialista"="Partido Socialista",
    "partido-republicano"="Partido Republicano",
    "chile-vamos"="Chile Vamos",
    "apruebo-dignidad"="Apruebo Dignidad",
    "convergencia-social"="Convergencia Social"
  )

  TAC_LOC <- c(
    "boric"     = "Gabriel Boric", "piñera"  = "Sebastián Piñera", "pinera" = "Sebastián Piñera",
    "bachelet"  = "Michelle Bachelet", "matthei" = "Evelyn Matthei",
    "provoste"  = "Yasna Provoste", "desbordes" = "Mario Desbordes",
    "jadue"     = "Daniel Jadue", "tohá" = "Carolina Tohá", "toha" = "Carolina Tohá",
    "vallejo"   = "Camila Vallejo", "kast" = "José Antonio Kast",
    "trump"     = "Donald Trump", "biden" = "Joe Biden", "putin" = "Vladimir Putin",
    "zelenski"  = "Volodimir Zelenski", "zelensky" = "Volodimir Zelenski",
    "milei"     = "Javier Milei", "bolsonaro" = "Jair Bolsonaro", "lula" = "Lula da Silva",
    "marcel"    = "Mario Marcel", "kaiser" = "Johannes Kaiser", "carter" = "Rodolfo Carter",
    "orrego"    = "Claudio Orrego", "bukele" = "Nayib Bukele", "macron" = "Emmanuel Macron",
    "meloni"    = "Giorgia Meloni", "netanyahu" = "Benjamin Netanyahu", "maduro" = "Nicolás Maduro",
    "musk"      = "Elon Musk",
    "udi"       = "Unión Demócrata Independiente",
    "ppd"       = "Partido Por la Democracia",
    "pdc"       = "Democracia Cristiana",
    "evópoli"   = "Evolución Política", "evopoli" = "Evolución Política"
  )

  fusionar_loc <- function(tokens, comp) {
    if (length(tokens) < 2L) return(tokens)
    out <- character(0); i <- 1L
    while (i <= length(tokens)) {
      fused <- FALSE
      if (i + 1L < length(tokens)) {
        tri <- c(tokens[i], tokens[i + 1L], tokens[i + 2L])
        if (requireNamespace("stringi", quietly = TRUE)) tri <- stringi::stri_trans_nfc(tri)
        for (nom in names(comp)) {
          for (p in comp[[nom]]) {
            if (length(p) == 3L && identical(p, tri)) {
              out <- c(out, nom); i <- i + 3L; fused <- TRUE; break
            }
          }
          if (fused) break
        }
      }
      if (!fused && i < length(tokens)) {
        par <- c(tokens[i], tokens[i + 1L])
        if (requireNamespace("stringi", quietly = TRUE)) par <- stringi::stri_trans_nfc(par)
        for (nom in names(comp)) {
          for (p in comp[[nom]]) {
            if (length(p) == 2L && identical(p, par)) {
              out <- c(out, nom); i <- i + 2L; fused <- TRUE; break
            }
          }
          if (fused) break
        }
      }
      if (!fused) { out <- c(out, tokens[i]); i <- i + 1L }
    }
    out
  }

  chunk_prep <- chunk |>
    filter(!is.na(titulo), nchar(trimws(titulo)) > 0L) |>
    mutate(
      titulo = tolower(trimws(titulo)),
      titulo = if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc(titulo) else titulo,
      titulo = gsub(dashes_unicode, "-", titulo),
      titulo = gsub("&[a-z0-9]+;", " ", titulo),
      titulo = gsub("&#[0-9]+;", " ", titulo),
      titulo = gsub("[\"']", " ", titulo)
    )

  tokens_por_art <- chunk_prep |>
    unnest_tokens(output = termino, input = titulo, token = "words", drop = TRUE) |>
    filter(
      nchar(termino) >= min_len,
      grepl("[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]", termino, ignore.case = TRUE),
      !termino %in% stopwords
    ) |>
    group_by(id, fecha, fuente) |>
    summarise(toks = list(termino), .groups = "drop") |>
    mutate(toks = lapply(toks, function(t) {
      t <- fusionar_loc(t, COMP_LOC)
      for (k in seq_along(GUION_LOC)) { t[t == names(GUION_LOC)[k]] <- unname(GUION_LOC[k]) }
      if (length(TAC_LOC) > 0L) {
        hit <- match(t, names(TAC_LOC))
        t[!is.na(hit)] <- unname(TAC_LOC[hit[!is.na(hit)]])
      }
      t <- t[vapply(t, function(x) {
        if (!grepl("-", x, fixed = TRUE)) return(TRUE)
        if (x %in% names(COMP_LOC)) return(TRUE)
        partes <- strsplit(x, "-", fixed = TRUE)[[1]]
        length(partes) == 2L && nchar(partes[1L]) >= min_len && nchar(partes[2L]) >= min_len &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[1L]) &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[2L])
      }, logical(1))]
      sort(unique(t))
    })) |>
    filter(lengths(toks) >= 2L)

  if (nrow(tokens_por_art) == 0L) return(NULL)

  pares_list <- lapply(seq_len(nrow(tokens_por_art)), function(i) {
    t <- tokens_por_art$toks[[i]]
    if (length(t) < 2L) return(NULL)
    idx <- combn(length(t), 2L)
    data.frame(
      fecha     = rep(as.character(tokens_por_art$fecha[[i]]), ncol(idx)),
      fuente    = rep(tokens_por_art$fuente[[i]], ncol(idx)),
      termino_a = t[idx[1L, ]],
      termino_b = t[idx[2L, ]],
      stringsAsFactors = FALSE
    )
  })
  pares <- do.call(rbind, Filter(Negate(is.null), pares_list))
  if (is.null(pares) || nrow(pares) == 0L) return(NULL)

  pares$fecha <- as.Date(pares$fecha)
  swap <- pares$termino_a > pares$termino_b
  tmp <- pares$termino_a[swap]; pares$termino_a[swap] <- pares$termino_b[swap]; pares$termino_b[swap] <- tmp
  pares <- pares[pares$termino_a != pares$termino_b, ]
  pares <- pares[nchar(pares$termino_a) <= 150L & nchar(pares$termino_b) <= 150L, ]

  pares |> count(fecha, fuente, termino_a, termino_b, name = "n")
}

# ------------------------------------------------------------------------------
# Conexión principal
# ------------------------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  host = PGHOST, port = PGPORT, user = PGUSER, password = PGPASSWORD, dbname = PGDATABASE
)
on.exit(dbDisconnect(con), add = TRUE)
message("Conectado a ", PGDATABASE, " en ", PGHOST)

rango <- dbGetQuery(con, "
  SELECT COALESCE(MIN(fecha), CURRENT_DATE) AS min_fecha,
         COALESCE(MAX(fecha), CURRENT_DATE) AS max_fecha
  FROM noticias WHERE fecha >= $1
", params = list(FECHA_DESDE))
min_fecha <- max(as.Date(rango$min_fecha), FECHA_DESDE)
max_fecha <- as.Date(rango$max_fecha)
message("Rango: ", min_fecha, " a ", max_fecha)

# Incremental: procesar solo fechas nuevas
max_fecha_cooc <- tryCatch(
  as.Date(dbGetQuery(con, "SELECT MAX(fecha)::text AS f FROM titulos_coocurrencia")$f),
  error = function(e) NA
)
if (!is.na(max_fecha_cooc)) {
  min_fecha <- max_fecha_cooc - 1L
  dbExecute(con, "DELETE FROM titulos_coocurrencia WHERE fecha >= $1", params = list(min_fecha))
  message("Incremental: reprocesando desde ", min_fecha, " (último registrado: ", max_fecha_cooc, ")")
} else {
  message("Tabla titulos_coocurrencia vacía — procesando desde: ", min_fecha)
}

plan(multisession, workers = 4L)
fechas_inicio <- seq(min_fecha, max_fecha, by = paste(CHUNK_DAYS, "days"))
con_params <- list(host = PGHOST, port = PGPORT, user = PGUSER, password = PGPASSWORD, dbname = PGDATABASE)

message("Procesando ", length(fechas_inicio), " chunks...")

resultados <- future_map(
  fechas_inicio,
  procesar_cooc_chunk,
  max_fecha   = max_fecha,
  con_params  = con_params,
  stopwords   = STOPWORDS,
  chunk_days  = CHUNK_DAYS,
  min_len     = MIN_TERM_LEN,
  .options = furrr_options(seed = TRUE)
)

plan(sequential)

resultados <- Filter(Negate(is.null), resultados)
if (length(resultados) == 0L) {
  message("Sin datos para procesar.")
  quit(save = "no", status = 0L)
}

todas <- bind_rows(resultados)
message("Pares totales: ", nrow(todas))

# Agregar (en caso de que chunks paralelos hayan generado filas duplicadas por fecha/fuente/par)
final <- todas |>
  group_by(fecha, fuente, termino_a, termino_b) |>
  summarise(n = sum(n), .groups = "drop")

# ------------------------------------------------------------------------------
# Escribir via tabla de staging → INSERT ... SELECT ... ON CONFLICT
# Evita el límite de parámetros en queries parametrizadas grandes.
# ------------------------------------------------------------------------------
final$fecha <- as.character(final$fecha)
final$n     <- as.integer(final$n)

dbExecute(con, "DROP TABLE IF EXISTS _staging_coocurrencia")
dbWriteTable(con, "_staging_coocurrencia", final, temporary = FALSE, overwrite = TRUE)
staging_n <- dbGetQuery(con, "SELECT COUNT(*) FROM _staging_coocurrencia")[[1]]
message("Staging rows written: ", staging_n)
nulls <- dbGetQuery(con, "SELECT COUNT(*) FROM _staging_coocurrencia WHERE termino_a IS NULL OR termino_b IS NULL OR fecha IS NULL OR fuente IS NULL OR n IS NULL")[[1]]
message("Staging NULL rows: ", nulls)
long <- dbGetQuery(con, "SELECT COUNT(*) FROM _staging_coocurrencia WHERE char_length(termino_a) > 150 OR char_length(termino_b) > 150")[[1]]
message("Staging rows with term > 150 chars: ", long)
dbExecute(con, "ALTER TABLE _staging_coocurrencia ALTER COLUMN fecha TYPE DATE USING fecha::date")

# Use LEAST/GREATEST in the INSERT to enforce canonical order regardless of R locale,
# and re-aggregate to merge any (a,b)/(b,a) pairs that R's locale may have split.
n_inserted <- tryCatch(
  dbExecute(con, "
    INSERT INTO titulos_coocurrencia (fecha, fuente, termino_a, termino_b, n)
    SELECT fecha, fuente,
      LEAST(termino_a, termino_b)    AS termino_a,
      GREATEST(termino_a, termino_b) AS termino_b,
      SUM(n) AS n
    FROM _staging_coocurrencia
    GROUP BY fecha, fuente, LEAST(termino_a, termino_b), GREATEST(termino_a, termino_b)
    ON CONFLICT (fecha, fuente, termino_a, termino_b) DO UPDATE SET n = EXCLUDED.n
  "),
  error = function(e) { message("INSERT ERROR: ", e$message); 0L }
)
message("INSERT rows affected: ", n_inserted)
dbExecute(con, "DROP TABLE IF EXISTS _staging_coocurrencia")

n_filas <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM titulos_coocurrencia")$n
message("Listo. Filas escritas en titulos_coocurrencia: ", n_filas)
