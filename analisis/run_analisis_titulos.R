#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# run_analisis_titulos.R  [pipeline step 2 of 5]
# Lee noticias (titulo, fecha, fuente), tokeniza titulares con tidytext y escribe
# titulos_terminos_diarios, titulos_terminos_por_medio y metricas_titulos_diarias.
# Orden de ejecución: 1) cargar_parquet_a_postgres.py  2) este script
#                      3) run_analisis_coocurrencia.R  4) run_analisis_semanal.R
#                      5) run_sentimiento.R (si disponible)
# Ejecutar: Rscript run_analisis_titulos.R
# Variables de entorno: PGHOST, PGPORT, y PGUSER_NOTICIAS/PGPASSWORD_NOTICIAS/PGDATABASE_NOTICIAS
#   (o PGUSER/PGPASSWORD/PGDATABASE). Lee .env en noticias/ o en raíz del proyecto (Paginaweb).
# Paquetes requeridos: DBI, RPostgres, tidytext, dplyr, furrr
# Opcional: stringi para normalización Unicode (recomendado para bío-bío, colo-colo).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(tidytext)
  library(dplyr)
  library(furrr)
  if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
  if (requireNamespace("udpipe",  quietly = TRUE)) library(udpipe)
})

# ------------------------------------------------------------------------------
# Configuración: variables de entorno o .env en noticias/ o en raíz (Paginaweb)
# Usa PGUSER_NOTICIAS, PGPASSWORD_NOTICIAS, PGDATABASE_NOTICIAS si están definidas.
# ------------------------------------------------------------------------------
script_dir <- if (length(args <- commandArgs()) > 0L) {
  idx <- grep("^--file=", args)
  if (length(idx)) dirname(sub("^--file=", "", args[idx[1L]])) else getwd()
} else getwd()
env_candidates <- c(
  file.path(script_dir, ".env"),
  file.path(script_dir, "..", ".env")  # raíz Paginaweb
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

# Stopwords centralizadas
source(file.path(script_dir, "..", "stopwords.R"))
source(file.path(script_dir, "..", "funciones.R"))

USAR_LEMMAS <- FALSE
modelo_udpipe <- NULL
message("udpipe desactivado — usando COMPUESTOS_CONOCIDOS + TOKEN_A_CANONICO para entidades")

# Tamaño del lote por fechas: procesar de a N días para no cargar todo en memoria
CHUNK_DAYS <- 90L
# Longitud mínima del término (caracteres)
MIN_TERM_LEN <- 3L
# Tamaño del lote para UPSERT
BATCH <- 500L
# Stemming opcional: FALSE = desactivado (los términos llegan sin modificar al dashboard)
USAR_STEMMING <- FALSE

# Guiones tipográficos Unicode (para normalizar antes de pasar a unnest_tokens)
DASHES_UNICODE <- paste0(
  "[",
  intToUtf8(0x2010), intToUtf8(0x2011), intToUtf8(0x2012),
  intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0x2015),
  "]"
)

# ------------------------------------------------------------------------------
# Compuestos conocidos y guiones (post-procesamiento de bigramas)
# ------------------------------------------------------------------------------
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
  "Presidente de la República" = list(c("presidente", "república"), c("presidente", "republica")),
  "José Antonio Kast"  = list(c("josé", "antonio", "kast"), c("jose", "antonio", "kast")),
  "Gabriel Boric"      = list(c("gabriel", "boric")),
  "Sebastián Piñera"   = list(c("sebastián", "piñera"), c("sebastian", "pinera"), c("sebastian", "piñera")),
  "Michelle Bachelet"  = list(c("michelle", "bachelet")),
  "Camila Vallejo"     = list(c("camila", "vallejo")),
  "Carolina Tohá"      = list(c("carolina", "tohá"), c("carolina", "toha")),
  "Evelyn Matthei"     = list(c("evelyn", "matthei")),
  "Mario Desbordes"    = list(c("mario", "desbordes")),
  "Giorgio Jackson"    = list(c("giorgio", "jackson")),
  "Rodrigo Valdés"     = list(c("rodrigo", "valdés"), c("rodrigo", "valdes")),
  "Daniel Jadue"       = list(c("daniel", "jadue")),
  "Yasna Provoste"     = list(c("yasna", "provoste")),
  "Heraldo Muñoz"      = list(c("heraldo", "muñoz"), c("heraldo", "munoz")),
  "Felipe Kast"        = list(c("felipe", "kast")),
  "Matteo Romano"      = list(c("matteo", "romano")),
  # Figuras políticas chilenas adicionales
  "Mario Marcel"       = list(c("mario", "marcel")),
  "Johannes Kaiser"    = list(c("johannes", "kaiser")),
  "Rodolfo Carter"     = list(c("rodolfo", "carter")),
  "Jeannette Jara"     = list(c("jeannette", "jara")),
  "Gonzalo Winter"     = list(c("gonzalo", "winter")),
  "Paulina Vodanovic"  = list(c("paulina", "vodanovic")),
  "Tomás Vodanovic"    = list(c("tomás", "vodanovic"), c("tomas", "vodanovic")),
  "Claudio Orrego"     = list(c("claudio", "orrego")),
  "Isabel Allende"     = list(c("isabel", "allende")),
  # Líderes internacionales adicionales
  "Lula da Silva"      = list(c("lula", "da", "silva"), c("lula", "silva")),
  "Emmanuel Macron"    = list(c("emmanuel", "macron")),
  "Giorgia Meloni"     = list(c("giorgia", "meloni")),
  "Benjamin Netanyahu" = list(c("benjamin", "netanyahu")),
  "Ursula von der Leyen" = list(c("von", "der", "leyen")),
  "Nayib Bukele"       = list(c("nayib", "bukele")),
  "Nicolás Maduro"     = list(c("nicolás", "maduro"), c("nicolas", "maduro")),
  "Delcy Rodríguez"    = list(c("delcy", "rodríguez"), c("delcy", "rodriguez")),
  "Elon Musk"          = list(c("elon", "musk")),
  # Partidos políticos chilenos
  "Democracia Cristiana"        = list(c("democracia", "cristiana")),
  "Renovación Nacional"         = list(c("renovación", "nacional"), c("renovacion", "nacional")),
  "Unión Demócrata Independiente" = list(c("unión", "demócrata", "independiente"), c("union", "democrata", "independiente")),
  "Partido Comunista"           = list(c("partido", "comunista")),
  "Partido Socialista"          = list(c("partido", "socialista")),
  "Partido Por la Democracia"   = list(c("partido", "democracia")),
  "Partido Republicano"         = list(c("partido", "republicano")),
  "Partido Liberal"             = list(c("partido", "liberal")),
  "Partido Radical"             = list(c("partido", "radical")),
  "Partido de la Gente"         = list(c("partido", "gente")),
  "Frente Amplio"               = list(c("frente", "amplio")),
  "Convergencia Social"         = list(c("convergencia", "social")),
  "Revolución Democrática"      = list(c("revolución", "democrática"), c("revolucion", "democratica")),
  "Acción Humanista"            = list(c("acción", "humanista"), c("accion", "humanista")),
  "Chile Vamos"                 = list(c("chile", "vamos")),
  "Apruebo Dignidad"            = list(c("apruebo", "dignidad")),
  "Donald Trump"       = list(c("donald", "trump")),
  "Joe Biden"          = list(c("joe", "biden")),
  "Vladimir Putin"     = list(c("vladimir", "putin")),
  "Xi Jinping"         = list(c("xi", "jinping")),
  "Corte Suprema"      = list(c("corte", "suprema")),
  "Ministerio Público" = list(c("ministerio", "público"), c("ministerio", "publico")),
  "Carabineros de Chile" = list(c("carabineros", "chile")),
  "Fiscalía Nacional"  = list(c("fiscalía", "nacional"), c("fiscalia", "nacional")),
  "Contraloría General" = list(c("contraloría", "general"), c("contraloria", "general")),
  "Banco Estado"       = list(c("banco", "estado")),
  "Copa América"       = list(c("copa", "américa"), c("copa", "america")),
  "Copa Libertadores"  = list(c("copa", "libertadores")),
  "Universidad de Chile" = list(c("universidad", "chile")),
  # Ministerios (bigrams: "de"/"del"/"la" son stopwords, quedan adyacentes los términos clave)
  "Ministerio del Interior"             = list(c("ministerio", "interior")),
  "Ministerio de Defensa"               = list(c("ministerio", "defensa")),
  "Ministerio de Hacienda"              = list(c("ministerio", "hacienda")),
  "Ministerio de Economía"              = list(c("ministerio", "economía"), c("ministerio", "economia")),
  "Ministerio de Educación"             = list(c("ministerio", "educación"), c("ministerio", "educacion")),
  "Ministerio de Justicia"              = list(c("ministerio", "justicia")),
  "Ministerio del Trabajo"              = list(c("ministerio", "trabajo")),
  "Ministerio de Salud"                 = list(c("ministerio", "salud")),
  "Ministerio de Vivienda"              = list(c("ministerio", "vivienda")),
  "Ministerio de Agricultura"           = list(c("ministerio", "agricultura")),
  "Ministerio de Minería"               = list(c("ministerio", "minería"), c("ministerio", "mineria")),
  "Ministerio de Transporte"            = list(c("ministerio", "transporte")),
  "Ministerio de Energía"               = list(c("ministerio", "energía"), c("ministerio", "energia")),
  "Ministerio de Deportes"              = list(c("ministerio", "deportes")),
  "Ministerio de la Mujer"              = list(c("ministerio", "mujer")),
  "Ministerio de Ciencia"               = list(c("ministerio", "ciencia")),
  "Ministerio de Obras Públicas"        = list(c("ministerio", "obras", "públicas"), c("ministerio", "obras", "publicas")),
  "Ministerio del Medio Ambiente"       = list(c("ministerio", "medio", "ambiente")),
  "Ministerio de Relaciones Exteriores" = list(c("ministerio", "relaciones", "exteriores")),
  "Ministerio de Desarrollo Social"     = list(c("ministerio", "desarrollo", "social")),
  # Comunas y ciudades chilenas con prefijo San/Santa/Santo
  "San Antonio"      = list(c("san", "antonio")),
  "San Bernardo"     = list(c("san", "bernardo")),
  "San Carlos"       = list(c("san", "carlos")),
  "San Clemente"     = list(c("san", "clemente")),
  "San Cristóbal"    = list(c("san", "cristóbal"), c("san", "cristobal")),
  "San Esteban"      = list(c("san", "esteban")),
  "San Fabián"       = list(c("san", "fabián"), c("san", "fabian")),
  "San Felipe"       = list(c("san", "felipe")),
  "San Fernando"     = list(c("san", "fernando")),
  "San Francisco"    = list(c("san", "francisco")),
  "San Ignacio"      = list(c("san", "ignacio")),
  "San Javier"       = list(c("san", "javier")),
  "San Joaquín"      = list(c("san", "joaquín"), c("san", "joaquin")),
  "San José"         = list(c("san", "josé"), c("san", "jose")),
  "San Juan"         = list(c("san", "juan")),
  "San Miguel"       = list(c("san", "miguel")),
  "San Nicolás"      = list(c("san", "nicolás"), c("san", "nicolas")),
  "San Pablo"        = list(c("san", "pablo")),
  "San Pedro"        = list(c("san", "pedro")),
  "San Rafael"       = list(c("san", "rafael")),
  "San Ramón"        = list(c("san", "ramón"), c("san", "ramon")),
  "San Rosendo"      = list(c("san", "rosendo")),
  "San Vicente"      = list(c("san", "vicente")),
  "Santa Bárbara"    = list(c("santa", "bárbara"), c("santa", "barbara")),
  "Santa Cruz"       = list(c("santa", "cruz")),
  "Santa Juana"      = list(c("santa", "juana")),
  "Santa María"      = list(c("santa", "maría"), c("santa", "maria")),
  "Santa Rosa"       = list(c("santa", "rosa")),
  "Santo Domingo"    = list(c("santo", "domingo"))
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
  "colo-colo" = "Colo Colo", "bío-bío" = "Bío Bío", "bio-bio" = "Bío Bío",
  "san-antonio" = "San Antonio", "san-bernardo" = "San Bernardo",
  "san-carlos" = "San Carlos", "san-clemente" = "San Clemente",
  "san-cristóbal" = "San Cristóbal", "san-cristobal" = "San Cristóbal",
  "san-esteban" = "San Esteban", "san-fabián" = "San Fabián", "san-fabian" = "San Fabián",
  "san-felipe" = "San Felipe", "san-fernando" = "San Fernando",
  "san-francisco" = "San Francisco", "san-ignacio" = "San Ignacio",
  "san-javier" = "San Javier", "san-joaquín" = "San Joaquín", "san-joaquin" = "San Joaquín",
  "san-josé" = "San José", "san-jose" = "San José",
  "san-juan" = "San Juan", "san-miguel" = "San Miguel",
  "san-nicolás" = "San Nicolás", "san-nicolas" = "San Nicolás",
  "san-pablo" = "San Pablo", "san-pedro" = "San Pedro",
  "san-rafael" = "San Rafael", "san-ramón" = "San Ramón", "san-ramon" = "San Ramón",
  "san-rosendo" = "San Rosendo", "san-vicente" = "San Vicente",
  "santa-bárbara" = "Santa Bárbara", "santa-barbara" = "Santa Bárbara",
  "santa-cruz" = "Santa Cruz", "santa-juana" = "Santa Juana",
  "santa-maría" = "Santa María", "santa-maria" = "Santa María",
  "santa-rosa" = "Santa Rosa", "santo-domingo" = "Santo Domingo",
  "democracia-cristiana" = "Democracia Cristiana",
  "renovación-nacional" = "Renovación Nacional", "renovacion-nacional" = "Renovación Nacional",
  "frente-amplio" = "Frente Amplio",
  "partido-comunista" = "Partido Comunista",
  "partido-socialista" = "Partido Socialista",
  "partido-republicano" = "Partido Republicano",
  "chile-vamos" = "Chile Vamos",
  "apruebo-dignidad" = "Apruebo Dignidad",
  "convergencia-social" = "Convergencia Social"
)

# TOKEN_A_CANONICO: mapea apellidos únicos (solitarios, sin bigrama previo) a la forma canónica.
# Se aplica DESPUÉS de la fusión de bigramas/trigramas: solo afecta tokens que no formaron compuesto.
TOKEN_A_CANONICO <- c(
  # Presidentes y expresidentes de Chile
  "boric"       = "Gabriel Boric",
  "piñera"      = "Sebastián Piñera",
  "pinera"      = "Sebastián Piñera",
  "bachelet"    = "Michelle Bachelet",
  # Candidatos y figuras políticas chilenas
  "matthei"     = "Evelyn Matthei",
  "provoste"    = "Yasna Provoste",
  "desbordes"   = "Mario Desbordes",
  "jadue"       = "Daniel Jadue",
  "tohá"        = "Carolina Tohá",
  "toha"        = "Carolina Tohá",
  "vallejo"     = "Camila Vallejo",
  # kast es ambiguo (José Antonio / Felipe), se mapea al más prominente
  "kast"        = "José Antonio Kast",
  # Líderes internacionales de alta frecuencia en prensa chilena
  "trump"       = "Donald Trump",
  "biden"       = "Joe Biden",
  "putin"       = "Vladimir Putin",
  "zelenski"    = "Volodimir Zelenski",
  "zelensky"    = "Volodimir Zelenski",
  "milei"       = "Javier Milei",
  "bolsonaro"   = "Jair Bolsonaro",
  "lula"        = "Lula da Silva",
  "marcel"      = "Mario Marcel",
  "kaiser"      = "Johannes Kaiser",
  "carter"      = "Rodolfo Carter",
  "orrego"      = "Claudio Orrego",
  "bukele"      = "Nayib Bukele",
  "macron"      = "Emmanuel Macron",
  "meloni"      = "Giorgia Meloni",
  "netanyahu"   = "Benjamin Netanyahu",
  "maduro"      = "Nicolás Maduro",
  "musk"        = "Elon Musk",
  # Abreviaciones de partidos chilenos
  "udi"         = "Unión Demócrata Independiente",
  "ppd"         = "Partido Por la Democracia",
  "pdc"         = "Democracia Cristiana",
  "evópoli"     = "Evolución Política",
  "evopoli"     = "Evolución Política"
)

# LEMAS: variantes morfológicas → forma canónica.
# Solo casos donde ILIKE '%canónica%' no encontraría la variante (raíces distintas).
LEMAS <- c(
  # Seguridad / crimen
  "delincuente"      = "delincuencia",
  "delincuentes"     = "delincuencia",
  "criminal"         = "crimen",
  "criminales"       = "crimen",
  "corrupto"         = "corrupción",
  "corruptos"        = "corrupción",
  "corrupta"         = "corrupción",
  "corruptas"        = "corrupción",
  # Migración
  "migrante"         = "migración",
  "migrantes"        = "migración",
  "migratorio"       = "migración",
  "migratoria"       = "migración",
  # Economía
  "económico"        = "economía",
  "económica"        = "economía",
  "económicos"       = "economía",
  "económicas"       = "economía",
  # Protesta / movilización
  "manifestante"     = "manifestación",
  "manifestantes"    = "manifestación",
  "protestas"        = "protesta",
  # Constitución / proceso constituyente
  "constituyente"    = "constitución",
  "constituyentes"   = "constitución",
  "constitucional"   = "constitución",
  # Pensiones / jubilación
  "pensionado"       = "pensión",
  "pensionados"      = "pensión",
  "pensionada"       = "pensión",
  "pensionadas"      = "pensión",
  # Violencia
  "violento"         = "violencia",
  "violenta"         = "violencia",
  "violentos"        = "violencia",
  "violentas"        = "violencia",
  # Elecciones
  "elecciones"       = "elección",
  "electoral"        = "elección",
  "electorales"      = "elección",
  # Salud / sistema sanitario
  "sanitario"        = "salud",
  "sanitaria"        = "salud",
  "sanitarios"       = "salud",
  "sanitarias"       = "salud",
  # Educación
  "educativo"        = "educación",
  "educativa"        = "educación",
  "educativos"       = "educación",
  "educativas"       = "educación",
  # Sismos
  "terremoto"        = "sismo",
  "terremotos"       = "sismo",
  "sismos"           = "sismo"
)

# Lista de exclusión de stemming para nombres propios y palabras del corpus chileno
palabras_excluir_stem <- c(
  "boric", "piñera", "bachelet", "aylwin", "frei", "lagos",
  "matthei", "kast", "tohá", "jara", "winter", "kaiser",
  "orrego", "desbordes", "jarpa",
  "valparaíso", "temuco", "antofagasta", "concepción",
  "caso", "proyecto", "partido", "comunal", "comunidad",
  "salud", "tasa", "constitución"
)

# ------------------------------------------------------------------------------
# Función: fusionar bigramas conocidos en vector de tokens
# ------------------------------------------------------------------------------
fusionar_compuestos_conocidos <- function(tokens, compuestos) {
  if (length(tokens) < 2L) return(tokens)
  out <- character(0)
  i <- 1L
  while (i <= length(tokens)) {
    fused <- FALSE
    if (i + 1L < length(tokens)) {
      tri <- c(tokens[i], tokens[i + 1L], tokens[i + 2L])
      if (requireNamespace("stringi", quietly = TRUE))
        tri <- stringi::stri_trans_nfc(tri)
      for (nom in names(compuestos)) {
        for (p in compuestos[[nom]]) {
          if (length(p) == 3L && identical(p, tri)) {
            out <- c(out, nom)
            i <- i + 3L
            fused <- TRUE
            break
          }
        }
        if (fused) break
      }
    }
    if (!fused && i < length(tokens)) {
      par <- c(tokens[i], tokens[i + 1L])
      if (requireNamespace("stringi", quietly = TRUE))
        par <- stringi::stri_trans_nfc(par)
      for (nom in names(compuestos)) {
        for (p in compuestos[[nom]]) {
          if (length(p) == 2L && identical(p, par)) {
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

# ------------------------------------------------------------------------------
# Función: tokenizar un chunk de noticias con tidytext
# Devuelve data.frame(fecha, fuente, termino) — un row por token por noticia.
# ------------------------------------------------------------------------------
tokenizar_chunk <- function(chunk_df, stopwords, min_len = 3L,
                            compuestos = COMPUESTOS_CONOCIDOS,
                            guion_a_nombre = GUION_A_NOMBRE,
                            dashes_unicode = DASHES_UNICODE,
                            usar_stemming = FALSE,
                            excluir_stem = palabras_excluir_stem) {
  if (nrow(chunk_df) == 0L) return(data.frame(fecha = as.Date(character()), fuente = character(), termino = character()))

  # Pre-normalizar títulos: guiones tipográficos → guión ASCII; entidades HTML
  chunk_df <- chunk_df |>
    filter(!is.na(titulo), nchar(trimws(titulo)) > 0L) |>
    mutate(
      titulo = tolower(trimws(titulo)),
      titulo = if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc(titulo) else titulo,
      titulo = gsub(dashes_unicode, "-", titulo),
      titulo = gsub("&[a-z0-9]+;", " ", titulo),
      titulo = gsub("&#[0-9]+;", " ", titulo),
      titulo = gsub("[\"']", " ", titulo)
    )

  # Tokenizar con tidytext (maneja Unicode correctamente)
  tokens_df <- chunk_df |>
    unnest_tokens(
      output   = termino,
      input    = titulo,
      token    = "words",
      drop     = TRUE
    ) |>
    filter(
      nchar(termino) >= min_len,
      grepl("[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]", termino, ignore.case = TRUE),
      !termino %in% stopwords
    ) |>
    select(fecha, fuente, termino)

  if (nrow(tokens_df) == 0L) return(tokens_df)

  # Post-proceso: fusionar bigramas conocidos por artículo (requiere orden dentro del titular)
  # Se hace agrupando por (fecha, fuente, row original) y procesando el vector de tokens
  # Para preservar el orden usamos un id de fila añadido antes del unnest_tokens
  chunk_df_id <- chunk_df |>
    mutate(.row_id = row_number())

  tokens_df2 <- chunk_df_id |>
    unnest_tokens(
      output   = termino,
      input    = titulo,
      token    = "words",
      drop     = TRUE
    ) |>
    filter(
      nchar(termino) >= min_len,
      grepl("[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]", termino, ignore.case = TRUE),
      !termino %in% stopwords
    ) |>
    group_by(.row_id, fecha, fuente) |>
    summarise(tokens_list = list(termino), .groups = "drop") |>
    mutate(tokens_list = lapply(tokens_list, function(toks) {
      # Fusionar bigramas
      toks <- fusionar_compuestos_conocidos(toks, compuestos)
      # Normalizar formas con guión
      for (k in seq_along(guion_a_nombre)) {
        toks[toks == names(guion_a_nombre)[k]] <- unname(guion_a_nombre[k])
      }
      # Filtrar compuestos con guión que no son conocidos
      toks <- toks[vapply(toks, function(t) {
        if (!grepl("-", t, fixed = TRUE)) return(TRUE)
        if (t %in% names(COMPUESTOS_CONOCIDOS)) return(TRUE)
        partes <- strsplit(t, "-", fixed = TRUE)[[1]]
        if (length(partes) != 2L) return(FALSE)
        nchar(partes[1L]) >= min_len &&
          nchar(partes[2L]) >= min_len &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[1L]) &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[2L])
      }, logical(1))]
      # Normalizar singletons: apellidos → nombre canónico; variantes → lema
      hit <- match(toks, names(TOKEN_A_CANONICO))
      toks[!is.na(hit)] <- unname(TOKEN_A_CANONICO[hit[!is.na(hit)]])
      hit2 <- match(toks, names(LEMAS))
      toks[!is.na(hit2)] <- unname(LEMAS[hit2[!is.na(hit2)]])
      # Stemming opcional
      if (usar_stemming && requireNamespace("corpus", quietly = TRUE)) {
        toks_stemmed <- character(length(toks))
        for (j in seq_along(toks)) {
          if (toks[j] %in% excluir_stem) {
            toks_stemmed[j] <- toks[j]
          } else {
            toks_stemmed[j] <- corpus::stem_snowball(toks[j], algorithm = "es")
          }
        }
        toks <- toks_stemmed
      }
      toks
    }))

  # Expandir de vuelta a filas individuales
  resultado <- tokens_df2 |>
    mutate(termino = lapply(tokens_list, function(x) x)) |>
    select(fecha, fuente, termino) |>
    tidyr::unnest(termino)

  # Filtro de longitud máxima para respetar VARCHAR(150)
  resultado <- resultado |>
    filter(nchar(as.character(termino)) <= 150L)

  resultado
}

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
# Rango de fechas a procesar
# ------------------------------------------------------------------------------
FECHA_DESDE <- as.Date("2018-01-01")

rango <- dbGetQuery(con, "
  SELECT COALESCE(MIN(fecha), CURRENT_DATE) AS min_fecha,
         COALESCE(MAX(fecha), CURRENT_DATE) AS max_fecha,
         COUNT(*) AS total
  FROM noticias
  WHERE fecha >= $1
", params = list(FECHA_DESDE))
min_fecha <- as.Date(rango$min_fecha)
max_fecha <- as.Date(rango$max_fecha)
total_noticias_db <- rango$total
min_fecha <- max(min_fecha, FECHA_DESDE)
message("Rango en BD (desde 2018): ", min_fecha, " a ", max_fecha, " (", total_noticias_db, " noticias)")

# Crear tabla titulos_terminos_por_medio si no existe
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS titulos_terminos_por_medio (
    fecha      DATE NOT NULL,
    fuente     VARCHAR(100) NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER NOT NULL,
    CONSTRAINT pk_titulos_terminos_por_medio PRIMARY KEY (fecha, fuente, termino)
  )
")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha ON titulos_terminos_por_medio(fecha DESC)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fuente ON titulos_terminos_por_medio(fuente)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha_fuente ON titulos_terminos_por_medio(fecha, fuente)")

# Incremental: procesar solo fechas nuevas
max_fecha_terminos <- tryCatch(
  as.Date(dbGetQuery(con, "SELECT MAX(fecha)::text AS f FROM titulos_terminos_diarios")$f),
  error = function(e) NA
)
if (!is.na(max_fecha_terminos)) {
  min_fecha <- max_fecha_terminos - 1L
  dbExecute(con, "DELETE FROM titulos_terminos_diarios  WHERE fecha >= $1", params = list(min_fecha))
  dbExecute(con, "DELETE FROM metricas_titulos_diarias   WHERE fecha >= $1", params = list(min_fecha))
  dbExecute(con, "DELETE FROM titulos_terminos_por_medio WHERE fecha >= $1", params = list(min_fecha))
  message("Incremental: reprocesando desde ", min_fecha, " (último registrado: ", max_fecha_terminos, ")")
} else {
  message("Tablas de agregados vacías — procesando desde: ", min_fecha)
}

# ------------------------------------------------------------------------------
# Paralelismo: 4 workers para procesar chunks de fechas en paralelo
# ------------------------------------------------------------------------------
plan(multisession, workers = 4L)

# Generar lista de chunks de fechas
fechas_inicio <- seq(min_fecha, max_fecha, by = paste(CHUNK_DAYS, "days"))

# Función que procesa un chunk y devuelve listas de df_agg, df_medio, metricas
procesar_chunk_fecha <- function(fecha_actual, max_fecha, con_params, stopwords,
                                  chunk_days, min_term_len, usar_stemming,
                                  token_a_canonico = character(0),
                                  lemas = character(0),
                                  udpipe_model_path = NULL,
                                  compuestos_dinamicos = list()) {
  suppressPackageStartupMessages({
    library(DBI); library(RPostgres); library(tidytext); library(dplyr)
    if (requireNamespace("stringi", quietly = TRUE)) library(stringi)
    if (requireNamespace("tidyr",   quietly = TRUE)) library(tidyr)
    if (!is.null(udpipe_model_path) && requireNamespace("udpipe", quietly = TRUE)) library(udpipe)
  })

  fecha_fin <- min(fecha_actual + chunk_days - 1L, max_fecha)

  con_local <- dbConnect(
    RPostgres::Postgres(),
    host     = con_params$host,
    port     = con_params$port,
    user     = con_params$user,
    password = con_params$password,
    dbname   = con_params$dbname
  )
  on.exit(dbDisconnect(con_local), add = TRUE)

  chunk <- dbGetQuery(con_local, "
    SELECT titulo, fecha, fuente
    FROM noticias
    WHERE fecha >= $1 AND fecha <= $2
    ORDER BY fecha, fuente
  ", params = list(fecha_actual, fecha_fin))

  if (nrow(chunk) == 0L) return(NULL)

  chunk$fecha <- as.Date(chunk$fecha)


  # Reuse the tokenizar_chunk logic inline (workers can't access parent environment)
  # — inlined here to avoid cross-process closures
  dashes_unicode <- paste0("[",
    intToUtf8(0x2010), intToUtf8(0x2011), intToUtf8(0x2012),
    intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0x2015), "]")

  COMPUESTOS_CONOCIDOS_LOCAL <- list(
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
    "Presidente de la República" = list(c("presidente", "república"), c("presidente", "republica")),
    "José Antonio Kast"  = list(c("josé", "antonio", "kast"), c("jose", "antonio", "kast")),
    "Gabriel Boric"      = list(c("gabriel", "boric")),
    "Sebastián Piñera"   = list(c("sebastián", "piñera"), c("sebastian", "pinera"), c("sebastian", "piñera")),
    "Michelle Bachelet"  = list(c("michelle", "bachelet")),
    "Camila Vallejo"     = list(c("camila", "vallejo")),
    "Carolina Tohá"      = list(c("carolina", "tohá"), c("carolina", "toha")),
    "Evelyn Matthei"     = list(c("evelyn", "matthei")),
    "Mario Desbordes"    = list(c("mario", "desbordes")),
    "Giorgio Jackson"    = list(c("giorgio", "jackson")),
    "Rodrigo Valdés"     = list(c("rodrigo", "valdés"), c("rodrigo", "valdes")),
    "Daniel Jadue"       = list(c("daniel", "jadue")),
    "Yasna Provoste"     = list(c("yasna", "provoste")),
    "Heraldo Muñoz"      = list(c("heraldo", "muñoz"), c("heraldo", "munoz")),
    "Felipe Kast"        = list(c("felipe", "kast")),
    # Figuras políticas chilenas adicionales
    "Mario Marcel"       = list(c("mario", "marcel")),
    "Johannes Kaiser"    = list(c("johannes", "kaiser")),
    "Rodolfo Carter"     = list(c("rodolfo", "carter")),
    "Jeannette Jara"     = list(c("jeannette", "jara")),
    "Gonzalo Winter"     = list(c("gonzalo", "winter")),
    "Paulina Vodanovic"  = list(c("paulina", "vodanovic")),
    "Tomás Vodanovic"    = list(c("tomás", "vodanovic"), c("tomas", "vodanovic")),
    "Claudio Orrego"     = list(c("claudio", "orrego")),
    "Isabel Allende"     = list(c("isabel", "allende")),
    # Líderes internacionales adicionales
    "Lula da Silva"      = list(c("lula", "da", "silva"), c("lula", "silva")),
    "Emmanuel Macron"    = list(c("emmanuel", "macron")),
    "Giorgia Meloni"     = list(c("giorgia", "meloni")),
    "Benjamin Netanyahu" = list(c("benjamin", "netanyahu")),
    "Ursula von der Leyen" = list(c("von", "der", "leyen")),
    "Nayib Bukele"       = list(c("nayib", "bukele")),
    "Nicolás Maduro"     = list(c("nicolás", "maduro"), c("nicolas", "maduro")),
    "Delcy Rodríguez"    = list(c("delcy", "rodríguez"), c("delcy", "rodriguez")),
    "Elon Musk"          = list(c("elon", "musk")),
    # Partidos políticos chilenos
    "Democracia Cristiana"        = list(c("democracia", "cristiana")),
    "Renovación Nacional"         = list(c("renovación", "nacional"), c("renovacion", "nacional")),
    "Unión Demócrata Independiente" = list(c("unión", "demócrata", "independiente"), c("union", "democrata", "independiente")),
    "Partido Comunista"           = list(c("partido", "comunista")),
    "Partido Socialista"          = list(c("partido", "socialista")),
    "Partido Por la Democracia"   = list(c("partido", "democracia")),
    "Partido Republicano"         = list(c("partido", "republicano")),
    "Partido Liberal"             = list(c("partido", "liberal")),
    "Partido Radical"             = list(c("partido", "radical")),
    "Partido de la Gente"         = list(c("partido", "gente")),
    "Frente Amplio"               = list(c("frente", "amplio")),
    "Convergencia Social"         = list(c("convergencia", "social")),
    "Revolución Democrática"      = list(c("revolución", "democrática"), c("revolucion", "democratica")),
    "Acción Humanista"            = list(c("acción", "humanista"), c("accion", "humanista")),
    "Chile Vamos"                 = list(c("chile", "vamos")),
    "Apruebo Dignidad"            = list(c("apruebo", "dignidad")),
    "Donald Trump"       = list(c("donald", "trump")),
    "Joe Biden"          = list(c("joe", "biden")),
    "Vladimir Putin"     = list(c("vladimir", "putin")),
    "Xi Jinping"         = list(c("xi", "jinping")),
    "Corte Suprema"      = list(c("corte", "suprema")),
    "Ministerio Público" = list(c("ministerio", "público"), c("ministerio", "publico")),
    "Carabineros de Chile" = list(c("carabineros", "chile")),
    "Fiscalía Nacional"  = list(c("fiscalía", "nacional"), c("fiscalia", "nacional")),
    "Contraloría General" = list(c("contraloría", "general"), c("contraloria", "general")),
    "Banco Estado"       = list(c("banco", "estado")),
    "Copa América"       = list(c("copa", "américa"), c("copa", "america")),
    "Copa Libertadores"  = list(c("copa", "libertadores")),
    "Universidad de Chile" = list(c("universidad", "chile")),
    # Ministerios (bigrams: "de"/"del"/"la" son stopwords, quedan adyacentes los términos clave)
    "Ministerio del Interior"             = list(c("ministerio", "interior")),
    "Ministerio de Defensa"               = list(c("ministerio", "defensa")),
    "Ministerio de Hacienda"              = list(c("ministerio", "hacienda")),
    "Ministerio de Economía"              = list(c("ministerio", "economía"), c("ministerio", "economia")),
    "Ministerio de Educación"             = list(c("ministerio", "educación"), c("ministerio", "educacion")),
    "Ministerio de Justicia"              = list(c("ministerio", "justicia")),
    "Ministerio del Trabajo"              = list(c("ministerio", "trabajo")),
    "Ministerio de Salud"                 = list(c("ministerio", "salud")),
    "Ministerio de Vivienda"              = list(c("ministerio", "vivienda")),
    "Ministerio de Agricultura"           = list(c("ministerio", "agricultura")),
    "Ministerio de Minería"               = list(c("ministerio", "minería"), c("ministerio", "mineria")),
    "Ministerio de Transporte"            = list(c("ministerio", "transporte")),
    "Ministerio de Energía"               = list(c("ministerio", "energía"), c("ministerio", "energia")),
    "Ministerio de Deportes"              = list(c("ministerio", "deportes")),
    "Ministerio de la Mujer"              = list(c("ministerio", "mujer")),
    "Ministerio de Ciencia"               = list(c("ministerio", "ciencia")),
    "Ministerio de Obras Públicas"        = list(c("ministerio", "obras", "públicas"), c("ministerio", "obras", "publicas")),
    "Ministerio del Medio Ambiente"       = list(c("ministerio", "medio", "ambiente")),
    "Ministerio de Relaciones Exteriores" = list(c("ministerio", "relaciones", "exteriores")),
    "Ministerio de Desarrollo Social"     = list(c("ministerio", "desarrollo", "social")),
    # Comunas y ciudades chilenas con prefijo San/Santa/Santo
    "San Antonio"      = list(c("san", "antonio")),
    "San Bernardo"     = list(c("san", "bernardo")),
    "San Carlos"       = list(c("san", "carlos")),
    "San Clemente"     = list(c("san", "clemente")),
    "San Cristóbal"    = list(c("san", "cristóbal"), c("san", "cristobal")),
    "San Esteban"      = list(c("san", "esteban")),
    "San Fabián"       = list(c("san", "fabián"), c("san", "fabian")),
    "San Felipe"       = list(c("san", "felipe")),
    "San Fernando"     = list(c("san", "fernando")),
    "San Francisco"    = list(c("san", "francisco")),
    "San Ignacio"      = list(c("san", "ignacio")),
    "San Javier"       = list(c("san", "javier")),
    "San Joaquín"      = list(c("san", "joaquín"), c("san", "joaquin")),
    "San José"         = list(c("san", "josé"), c("san", "jose")),
    "San Juan"         = list(c("san", "juan")),
    "San Miguel"       = list(c("san", "miguel")),
    "San Nicolás"      = list(c("san", "nicolás"), c("san", "nicolas")),
    "San Pablo"        = list(c("san", "pablo")),
    "San Pedro"        = list(c("san", "pedro")),
    "San Rafael"       = list(c("san", "rafael")),
    "San Ramón"        = list(c("san", "ramón"), c("san", "ramon")),
    "San Rosendo"      = list(c("san", "rosendo")),
    "San Vicente"      = list(c("san", "vicente")),
    "Santa Bárbara"    = list(c("santa", "bárbara"), c("santa", "barbara")),
    "Santa Cruz"       = list(c("santa", "cruz")),
    "Santa Juana"      = list(c("santa", "juana")),
    "Santa María"      = list(c("santa", "maría"), c("santa", "maria")),
    "Santa Rosa"       = list(c("santa", "rosa")),
    "Santo Domingo"    = list(c("santo", "domingo"))
  )

  GUION_A_NOMBRE_LOCAL <- c(
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
    "colo-colo" = "Colo Colo", "bío-bío" = "Bío Bío", "bio-bio" = "Bío Bío",
    "san-antonio" = "San Antonio", "san-bernardo" = "San Bernardo",
    "san-carlos" = "San Carlos", "san-clemente" = "San Clemente",
    "san-cristóbal" = "San Cristóbal", "san-cristobal" = "San Cristóbal",
    "san-esteban" = "San Esteban", "san-fabián" = "San Fabián", "san-fabian" = "San Fabián",
    "san-felipe" = "San Felipe", "san-fernando" = "San Fernando",
    "san-francisco" = "San Francisco", "san-ignacio" = "San Ignacio",
    "san-javier" = "San Javier", "san-joaquín" = "San Joaquín", "san-joaquin" = "San Joaquín",
    "san-josé" = "San José", "san-jose" = "San José",
    "san-juan" = "San Juan", "san-miguel" = "San Miguel",
    "san-nicolás" = "San Nicolás", "san-nicolas" = "San Nicolás",
    "san-pablo" = "San Pablo", "san-pedro" = "San Pedro",
    "san-rafael" = "San Rafael", "san-ramón" = "San Ramón", "san-ramon" = "San Ramón",
    "san-rosendo" = "San Rosendo", "san-vicente" = "San Vicente",
    "santa-bárbara" = "Santa Bárbara", "santa-barbara" = "Santa Bárbara",
    "santa-cruz" = "Santa Cruz", "santa-juana" = "Santa Juana",
    "santa-maría" = "Santa María", "santa-maria" = "Santa María",
    "santa-rosa" = "Santa Rosa", "santo-domingo" = "Santo Domingo",
    "democracia-cristiana" = "Democracia Cristiana",
    "renovación-nacional" = "Renovación Nacional", "renovacion-nacional" = "Renovación Nacional",
    "frente-amplio" = "Frente Amplio",
    "partido-comunista" = "Partido Comunista",
    "partido-socialista" = "Partido Socialista",
    "partido-republicano" = "Partido Republicano",
    "chile-vamos" = "Chile Vamos",
    "apruebo-dignidad" = "Apruebo Dignidad",
    "convergencia-social" = "Convergencia Social"
  )

  # Agregar compuestos detectados automáticamente (no sobreescribir los curados)
  for (.nm in names(compuestos_dinamicos)) {
    if (!.nm %in% names(COMPUESTOS_CONOCIDOS_LOCAL))
      COMPUESTOS_CONOCIDOS_LOCAL[[.nm]] <- compuestos_dinamicos[[.nm]]
  }

  # Índice por primera palabra: evita iterar todos los compuestos en cada posición.
  # Estructura: list[primera_palabra] -> list of list(canon, patron)
  .idx_compuestos <- list()
  for (.nm in names(COMPUESTOS_CONOCIDOS_LOCAL)) {
    for (.p in COMPUESTOS_CONOCIDOS_LOCAL[[.nm]]) {
      .w1 <- .p[1L]
      .idx_compuestos[[.w1]] <- c(.idx_compuestos[[.w1]], list(list(canon = .nm, pat = .p)))
    }
  }

  fusionar_compuestos_local <- function(tokens, idx) {
    if (length(tokens) < 2L) return(tokens)
    out <- character(0)
    i <- 1L
    n <- length(tokens)
    while (i <= n) {
      w1 <- tokens[i]
      candidatos <- idx[[w1]]
      fused <- FALSE
      if (!is.null(candidatos) && i < n) {
        # Intentar trigramas primero (mayor precedencia)
        if (i + 1L < n) {
          tri <- tokens[i:(i + 2L)]
          for (.c in candidatos) {
            if (length(.c$pat) == 3L && identical(.c$pat, tri)) {
              out <- c(out, .c$canon); i <- i + 3L; fused <- TRUE; break
            }
          }
        }
        # Si no hubo trigrama, intentar bigrama
        if (!fused) {
          par <- tokens[i:(i + 1L)]
          for (.c in candidatos) {
            if (length(.c$pat) == 2L && identical(.c$pat, par)) {
              out <- c(out, .c$canon); i <- i + 2L; fused <- TRUE; break
            }
          }
        }
      }
      if (!fused) { out <- c(out, tokens[i]); i <- i + 1L }
    }
    out
  }

  chunk_prep <- chunk |>
    filter(!is.na(titulo), nchar(trimws(titulo)) > 0L) |>
    mutate(
      .row_id = row_number(),
      titulo  = tolower(trimws(titulo)),
      titulo  = if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc(titulo) else titulo,
      titulo  = gsub(dashes_unicode, "-", titulo),
      titulo  = gsub("&[a-z0-9]+;", " ", titulo),
      titulo  = gsub("&#[0-9]+;", " ", titulo),
      titulo  = gsub("[\"']", " ", titulo)
    )

  tokens_raw <- chunk_prep |>
    unnest_tokens(output = termino, input = titulo, token = "words", drop = TRUE) |>
    filter(
      nchar(termino) >= min_term_len,
      grepl("[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]", termino, ignore.case = TRUE),
      !termino %in% stopwords
    ) |>
    group_by(.row_id, fecha, fuente) |>
    summarise(toks = list(termino), .groups = "drop")

  # Precomputar estructuras de lookup una sola vez fuera del lapply (evita recalcular por fila)
  .guion_names <- names(GUION_A_NOMBRE_LOCAL)
  .guion_vals  <- unname(GUION_A_NOMBRE_LOCAL)
  .canon_set   <- names(COMPUESTOS_CONOCIDOS_LOCAL)

  tokens_raw <- tokens_raw |>
    mutate(toks = lapply(toks, function(t) {
      t <- fusionar_compuestos_local(t, .idx_compuestos)
      # GUION_A_NOMBRE: lookup vectorizado en lugar de for-loop
      hit_g <- match(t, .guion_names)
      t[!is.na(hit_g)] <- .guion_vals[hit_g[!is.na(hit_g)]]
      # Filtrar guiones desconocidos (precomputar set de canonicos fuera del lapply)
      t <- t[vapply(t, function(x) {
        if (!grepl("-", x, fixed = TRUE)) return(TRUE)
        if (x %in% .canon_set) return(TRUE)
        partes <- strsplit(x, "-", fixed = TRUE)[[1]]
        if (length(partes) != 2L) return(FALSE)
        nchar(partes[1L]) >= min_term_len && nchar(partes[2L]) >= min_term_len &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[1L]) &&
          grepl("^[a-z\u00f1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc]+$", partes[2L])
      }, logical(1))]
      # Normalizar singletons: apellidos → nombre canónico; variantes → lema
      if (length(token_a_canonico) > 0L) {
        hit <- match(t, names(token_a_canonico))
        t[!is.na(hit)] <- unname(token_a_canonico[hit[!is.na(hit)]])
      }
      if (length(lemas) > 0L) {
        hit2 <- match(t, names(lemas))
        t[!is.na(hit2)] <- unname(lemas[hit2[!is.na(hit2)]])
      }
      t
    }))

  if (nrow(tokens_raw) == 0L) return(NULL)

  # Expandir a (fecha, fuente, termino)
  expandido <- tokens_raw |>
    select(fecha, fuente, termino = toks) |>
    tidyr::unnest(termino) |>
    filter(nchar(termino) <= 150L)

  if (nrow(expandido) == 0L) return(NULL)

  # Agregar por (fecha, termino)
  df_agg <- expandido |>
    count(fecha, termino, name = "frecuencia") |>
    mutate(fecha = as.Date(fecha), frecuencia = as.integer(frecuencia))

  # Agregar por (fecha, fuente, termino)
  df_medio <- expandido |>
    count(fecha, fuente, termino, name = "frecuencia") |>
    mutate(fecha = as.Date(fecha), frecuencia = as.integer(frecuencia))

  # Métricas diarias
  noticias_por_fecha <- chunk |>
    count(fecha, name = "total_noticias") |>
    mutate(fecha = as.Date(fecha))

  terminos_por_fecha <- df_agg |>
    group_by(fecha) |>
    summarise(terminos_unicos = n_distinct(termino), .groups = "drop")

  metricas <- noticias_por_fecha |>
    left_join(terminos_por_fecha, by = "fecha") |>
    mutate(terminos_unicos = coalesce(terminos_unicos, 0L))

  list(
    df_agg   = df_agg,
    df_medio = df_medio,
    metricas = metricas,
    n_leidas = nrow(chunk)
  )
}

con_params <- list(
  host     = PGHOST,
  port     = PGPORT,
  user     = PGUSER,
  password = PGPASSWORD,
  dbname   = PGDATABASE
)

message("Procesando ", length(fechas_inicio), " chunks con paralelismo (workers = 4)...")

# ------------------------------------------------------------------------------
# Detectar compuestos frecuentes desde titulos_ngramas_diarios (si existe)
# Se usan como COMPUESTOS dinámicos: cuando dos palabras aparecen juntas en un
# titular con freq >= MIN_FREQ_COMPUESTO, se fusionan en un solo token.
# ------------------------------------------------------------------------------
MIN_FREQ_COMPUESTO <- 50L  # umbral sobre el TOTAL histórico (suma de todos los días)
COMPUESTOS_FRECUENTES <- list()
tryCatch({
  if (dbExistsTable(con, "titulos_ngramas_diarios")) {
    top_ng <- dbGetQuery(con, "
      SELECT ngrama, SUM(n) AS total
      FROM titulos_ngramas_diarios
      WHERE tipo IN (2, 3)
      GROUP BY ngrama
      HAVING SUM(n) >= $1
      ORDER BY total DESC
    ", params = list(MIN_FREQ_COMPUESTO))

    # Índice de todos los patrones de COMPUESTOS_CONOCIDOS (para detectar sufijos)
    todos_patrones_conocidos <- unlist(COMPUESTOS_CONOCIDOS, recursive = FALSE)

    # ¿Es pts un sufijo de algún patrón conocido más largo?
    es_sufijo_conocido <- function(pts) {
      n_pts <- length(pts)
      for (pat in todos_patrones_conocidos) {
        if (length(pat) > n_pts && identical(tail(pat, n_pts), pts)) return(TRUE)
      }
      FALSE
    }

    # Despluralizador simple para español
    desplural <- function(w) {
      if (endsWith(w, "ces") && nchar(w) > 4) return(paste0(substr(w, 1L, nchar(w) - 3L), "z"))
      if (endsWith(w, "es")  && nchar(w) > 4) return(substr(w, 1L, nchar(w) - 2L))
      if (endsWith(w, "s")   && nchar(w) > 3) return(substr(w, 1L, nchar(w) - 1L))
      w
    }

    for (i in seq_len(nrow(top_ng))) {
      ng  <- top_ng$ngrama[i]
      pts <- strsplit(ng, " ", fixed = TRUE)[[1]]
      if (length(pts) < 2L || length(pts) > 3L) next
      if (ng %in% names(COMPUESTOS_CONOCIDOS)) next     # ya cubierto por lista curada
      if (es_sufijo_conocido(pts)) next                 # sufijo de compuesto mayor → "antonio kast" < "jose antonio kast"

      # Forma canónica: singular de cada palabra
      pts_sg  <- sapply(pts, desplural, USE.NAMES = FALSE)
      canon   <- paste(pts_sg, collapse = " ")

      if (canon %in% names(COMPUESTOS_FRECUENTES)) {
        # Variante plural del mismo compuesto → añadir como patrón extra
        COMPUESTOS_FRECUENTES[[canon]] <- c(COMPUESTOS_FRECUENTES[[canon]], list(pts))
      } else {
        # Entrada nueva: incluir patrón original y singular si difieren
        patrones <- list(pts)
        if (!identical(pts_sg, pts)) patrones <- c(patrones, list(pts_sg))
        COMPUESTOS_FRECUENTES[[canon]] <- patrones
      }
    }
    message("Compuestos frecuentes detectados automáticamente: ", length(COMPUESTOS_FRECUENTES))
  } else {
    message("titulos_ngramas_diarios no existe aún — sin compuestos dinámicos")
  }
}, error = function(e) message("Aviso compuestos dinámicos: ", e$message))

resultados <- future_map(
  fechas_inicio,
  procesar_chunk_fecha,
  max_fecha             = max_fecha,
  con_params            = con_params,
  stopwords             = STOPWORDS,
  chunk_days            = CHUNK_DAYS,
  min_term_len          = MIN_TERM_LEN,
  usar_stemming         = USAR_STEMMING,
  token_a_canonico      = TOKEN_A_CANONICO,
  lemas                 = LEMAS,
  udpipe_model_path     = NULL,
  compuestos_dinamicos  = COMPUESTOS_FRECUENTES,
  .options = furrr_options(seed = TRUE)
)

plan(sequential)

# Filtrar resultados nulos
resultados <- Filter(Negate(is.null), resultados)

filas_leidas <- sum(vapply(resultados, function(x) x$n_leidas, integer(1)))
todas_las_fechas <- character(0)

# ------------------------------------------------------------------------------
# Escribir resultados a la BD (secuencial para evitar conflictos de escritura)
# ------------------------------------------------------------------------------
for (res in resultados) {
  df_agg   <- res$df_agg
  df_medio <- res$df_medio
  metricas <- res$metricas

  # UPSERT titulos_terminos_diarios
  n_agg <- nrow(df_agg)
  for (start in seq(1L, n_agg, by = BATCH)) {
    end <- min(start + BATCH - 1L, n_agg)
    sub <- df_agg[start:end, ]
    n_sub <- nrow(sub)
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

  # UPSERT titulos_terminos_por_medio
  if (nrow(df_medio) > 0L) {
    n_medio <- nrow(df_medio)
    for (start in seq(1L, n_medio, by = BATCH)) {
      end <- min(start + BATCH - 1L, n_medio)
      sub <- df_medio[start:end, ]
      n_sub <- nrow(sub)
      placeholders_medio <- paste(
        sprintf("($%d, $%d, $%d, $%d)", 4L * (seq_len(n_sub) - 1L) + 1L, 4L * (seq_len(n_sub) - 1L) + 2L, 4L * (seq_len(n_sub) - 1L) + 3L, 4L * (seq_len(n_sub) - 1L) + 4L),
        collapse = ", "
      )
      q_medio <- paste0(
        "INSERT INTO titulos_terminos_por_medio (fecha, fuente, termino, frecuencia) VALUES ", placeholders_medio,
        " ON CONFLICT (fecha, fuente, termino) DO UPDATE SET frecuencia = EXCLUDED.frecuencia"
      )
      params_medio <- as.list(as.vector(t(sub[, c("fecha", "fuente", "termino", "frecuencia")])))
      dbExecute(con, q_medio, params = params_medio)
    }
  }

  # UPSERT metricas_titulos_diarias
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
}

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
n_dias <- length(unique(todas_las_fechas))
n_terms <- dbGetQuery(con, "SELECT COUNT(DISTINCT termino) AS n FROM titulos_terminos_diarios")$n
n_por_medio <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM titulos_terminos_por_medio")$n
message("Listo. Filas leídas: ", filas_leidas)
message("Días con métricas: ", n_dias)
message("Términos distintos en titulos_terminos_diarios: ", n_terms)
message("Filas en titulos_terminos_por_medio (para pestaña Medios): ", n_por_medio)
