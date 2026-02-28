# Plantilla scraping por medio (basada en prensa_chile/scraping_ejemplo.R)
# Copiar a scraping_FUENTE.r y rellenar FUENTE, enlaces y selectores.

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

# Ejecutar desde carpeta noticias/
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

# --- CONFIGURAR POR MEDIO ---
FUENTE <- "elsiglo"   # slug: meganoticias, eldinamo, lanacion, ...
hist <- ""

# URLs de páginas que listan noticias (ajustar por medio)
enlaces <- paste0("https://elsiglo.cl/category/pais/page/", 1:3)

# Selectores (ajustar según el HTML de cada sitio)
# Listado: dónde están los <a href="..."> a cada noticia
# Artículo: titulo, bajada, fecha, cuerpo

# --- Obtener enlaces de noticias ---
resultados_enlaces <- map(enlaces, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  noticias_sesion <- sesion |>
    html_elements(".entry-title") |>
    html_elements("a") |>
    html_attr("href")
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  return(noticias_sesion)
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    x_titulo   <- noticia |> html_elements("main") |> html_elements(".entry-title") |> html_text2()
    x_bajada   <- noticia |> html_elements("main") |> html_elements("blockquote") |> html_text2()
    x_fecha_t  <- noticia |> html_elements("main") |> html_elements(".entry-meta") |> html_elements(".date") |> html_text2()
    x_cuerpo   <- noticia |> html_elements(".entry-content") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    # Parsear fecha si hace falta (ej. "mes dia, año" -> YYYY-MM-DD)
    x_fecha <- x_fecha_t
    if (length(x_fecha_t) > 0 && nchar(x_fecha_t[1]) > 0) {
      mes <- str_extract(x_fecha_t[1], "\\w+") |> tolower() |>
        recode("enero"="1","febrero"="2","marzo"="3","abril"="4","mayo"="5","junio"="6",
               "julio"="7","agosto"="8","septiembre"="9","octubre"="10","noviembre"="11","diciembre"="12")
      dia_ano <- str_extract(x_fecha_t[1], "\\d+, \\d{4}") |> str_remove(",")
      ano <- str_extract(dia_ano, "\\d{4}$")
      dia <- str_extract(dia_ano, "^\\d+")
      x_fecha <- paste(ano, mes, dia)
    }
    tibble(
      titulo = validar_elementos(x_titulo),
      bajada = validar_elementos(x_bajada),
      fecha = validar_elementos(x_fecha),
      cuerpo = validar_elementos(x_cuerpo),
      fuente = FUENTE,
      url = enlace,
      fecha_scraping = lubridate::now()
    )
  }, error = function(e) {
    message(glue("Error en {FUENTE}: {e}"))
    return(NULL)
  })
}) |> list_rbind()

# --- Guardar ---
dir.create(file.path("scraping", "datos", FUENTE), showWarnings = FALSE, recursive = TRUE)
readr::write_rds(resultados, ruta_resultado(FUENTE, hist))
message(glue("listo {FUENTE} {now()}"))
