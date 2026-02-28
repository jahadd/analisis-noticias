# Orquestador: ejecuta scripts de web scraping en paralelo (RStudio Background Jobs)
# para obtener datos de todas las fuentes (1 ene 2016 hasta hoy). Con GUARDAR_NOTICIAS_EN=postgres
# los resultados se guardan directamente en PostgreSQL (noticias_chile.noticias_crudo).
# Sin esa variable se guardan en scraping/datos/{fuente}/*.rds.
# Los medios que usan Chromote se ejecutan secuencialmente.
#
# Cómo ejecutar como RStudio Background Job:
#   En RStudio: File > Run Document, o abrir este archivo y pulsar "Source" (Ctrl/Cmd+Shift+S).
#   Si está activo "Ejecutar en Background Job" (engranaje del Source), el script entero
#   se lanzará como job y la consola quedará libre.
#   Desde la consola (elige según tu directorio actual):
#     • Si estás en la raíz del repo (Paginaweb): rstudioapi::jobRunScript("noticias/scraping/prensa_scraping.R", workingDir = "noticias")
#     • Si estás en noticias/:                   rstudioapi::jobRunScript("scraping/prensa_scraping.R", workingDir = ".")
#
# Ejecutar desde la raíz del repo (Paginaweb) o desde noticias/.

library(dplyr) |> suppressPackageStartupMessages()
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate) |> suppressPackageStartupMessages()

# Trabajar siempre desde noticias/ (directorio donde está funciones.R y scraping/)
if (file.exists("noticias/funciones.R")) setwd("noticias")
if (!file.exists("funciones.R")) stop("Ejecutar desde la raíz del repo (Paginaweb) o desde noticias/")

# Rango de fechas para scraping histórico: 1 enero 2016 hasta hoy (los scripts pueden leerlo con Sys.getenv)
Sys.setenv(
  SCRAPING_FECHA_DESDE = "2016-01-01",
  SCRAPING_FECHA_HASTA = format(Sys.Date(), "%Y-%m-%d")
)

# Si se ejecuta en RStudio de forma interactiva, lanzar este script como Background Job y salir
# (así la consola queda libre y el scraping corre en la pestaña "Jobs")
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::jobRunScript(
    path     = "scraping/prensa_scraping.R",
    workingDir = getwd(),
    name     = "Scraping prensa (2016-actualidad)"
  )
  message("Scraping lanzado como Background Job. Revisa la pestaña 'Jobs' en RStudio.")
  return(invisible())
}

source("funciones.R")
Sys.setenv(GUARDAR_NOTICIAS_EN = "postgres")

# --- Inicio ---
notificacion("Scraping de prensa", "Iniciando scripts…")

# --- Scraping asinc (paralelo con RStudio Background Jobs) ---
scraping_prensa("scraping_meganoticias.r")       # hist
scraping_prensa("scraping_eldinamo.r")           # hist
scraping_prensa("scraping_lanacion.r")           # hist
scraping_prensa("scraping_publimetro.r")         # hist
scraping_prensa("scraping_elciudadano.r")        # hist
scraping_prensa("scraping_radiouchile.r")        # hist
scraping_prensa("scraping_24horas.r")            # hist
scraping_prensa("scraping_cnnchile.r")           # hist
scraping_prensa("scraping_elsiglo.r")            # hist
scraping_prensa("scraping_ciper.r")              # hist
scraping_prensa("scraping_agricultura.r")        # hist
scraping_prensa("scraping_emol.r")               # hist en otro script
scraping_prensa("scraping_diariofinanciero.r")   # histórico solo hasta página 20
scraping_prensa("scraping_lacuarta.r")           # histórico por búsquedas
scraping_prensa("scraping_cooperativa.r")        # hist en otro script
scraping_prensa("scraping_elmostrador.r")        # requiere selenium para hist
scraping_prensa("scraping_chvnoticias.r")
scraping_prensa("scraping_t13.r")                # hist por búsquedas
scraping_prensa("scraping_biobio.r")
scraping_prensa("scraping_lahora.r")
scraping_prensa("scraping_adnradio.r")           # no está obteniendo nada
scraping_prensa("scraping_lasegunda.r")          # sólo titulares, fecha y palabras clave
# scraping_prensa("scraping_cooperativa_h.r")    # este es sólo histórico

# --- Chromote: secuencial (mismo proceso de Chrome headless) ---
scraping_prensa("scraping_latercera.r", ejecucion = "secuencial")   # hist
scraping_prensa("scraping_exante.r", ejecucion = "secuencial")     # hist
scraping_prensa("scraping_theclinic.r", ejecucion = "secuencial")  # hist
scraping_prensa("scraping_eldesconcierto.r", ejecucion = "secuencial")  # hist
scraping_prensa("scraping_quintopoder.r", ejecucion = "secuencial")     # hist
scraping_prensa("scraping_izquierdadiario.R", ejecucion = "secuencial") # hist

# --- Revisión y carga a DB ---
# Si usas jobs en paralelo: descomenta Sys.sleep(60) para esperar antes de revisar/cargar,
# o ejecuta después a mano: source("scraping/cargar_noticias_postgres.R")
# Sys.sleep(60)
source("scraping/otros/revisar/revisar_scraping.R")

# Los datos ya se cargaron a Postgres desde cada script (guardar_noticias).
# Para cargar además RDS existentes en scraping/datos/ descomenta la línea siguiente:
# source("scraping/cargar_noticias_postgres.R")

notificacion("Scraping de prensa", "Datos de noticias descargados y cargados a DB")
