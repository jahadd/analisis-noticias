# =============================================================================
# scraping_ciper.r — CIPER Chile (ciperchile.cl)
# =============================================================================
# Secciones: radar, investigación, actualidad, columnas
# Listados: /radar-2/, /investigacion/, /actualidad/, /columnas/ (paginación /p/N)
# Artículos: https://www.ciperchile.cl/YYYY/MM/DD/slug
# =============================================================================
library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "ciper"
hist <- ""
BASE   <- "https://www.ciperchile.cl"

# Bases: radar, investigación, actualidad, columnas
bases_seccion <- c("/radar-2/", "/investigacion/", "/actualidad/", "/columnas/")

obtener_max_pagina_ciper <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?ciperchile\\.cl")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm) & grepl("^/", hrefs_norm)]
    base_sin_barra <- sub("/$", "", base_path)
    patron <- paste0("^", gsub("([.?*+^])", "\\\\\\1", base_sin_barra), "/(?:p|page)/([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/(?:p|page)/([0-9]+).*", "\\1", matches))
    max(1L, max(nums, na.rm = TRUE))
  }, error = function(e) 1L)
}

enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_ciper(base_path)
  message(glue("  {base_path}: {max_pag} página(s)"))
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
  }
  urls
}))

es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  grepl("^https?://www\\.ciperchile\\.cl/[0-9]{4}/[0-9]{2}/[0-9]{2}/[^/]+/?$", url)
}

resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  hrefs <- sesion |> html_elements("a") |> html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  urls <- hrefs[vapply(hrefs, es_url_articulo, FUN.VALUE = logical(1))]
  urls <- unique(urls)
  message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
  return(urls)
})
enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    x_titulo <- noticia |> html_element("h1") |> html_text2()
    # Fecha viene como 25.02.2026
    x_fecha_t <- noticia |> html_elements("time") |> html_text2()
    if (length(x_fecha_t) == 0) {
      x_fecha_t <- noticia |> html_text2() |> str_extract("\\b[0-3]?[0-9]\\.[0-1]?[0-9]\\.[0-9]{4}\\b")
    } else {
      x_fecha_t <- x_fecha_t[1]
    }
    x_fecha <- if (length(x_fecha_t) > 0) x_fecha_t[1] else NA_character_
    if (!is.na(x_fecha) && nchar(trimws(x_fecha)) > 0) {
      dt <- suppressWarnings(lubridate::dmy(gsub("\\.", "/", x_fecha)))
      if (!is.na(dt)) {
        x_fecha <- paste(lubridate::year(dt), lubridate::month(dt), lubridate::day(dt))
      }
    }
    parrafos <- noticia |> html_elements("article p") |> html_text2()
    x_bajada <- if (length(parrafos) > 0) parrafos[1] else NA_character_
    x_cuerpo <- if (length(parrafos) > 0) paste(parrafos, collapse = "\n") else NA_character_
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
    message(glue("Error en {FUENTE}: {e} — {enlace}"))
    return(NULL)
  })
}) |> purrr::compact() |> list_rbind()

guardar_noticias(resultados, FUENTE, hist)
