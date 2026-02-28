# =============================================================================
# scraping_agricultura.r — Ministerio de Agricultura (minagri.gob.cl)
# =============================================================================
# Medio: Noticias Minagri (Ministerio de Agricultura de Chile)
# URL listado: https://minagri.gob.cl/noticias-minagri/
# Artículos: https://minagri.gob.cl/noticia/SLUG/
#
# Nota: El sitio oficial de noticias es minagri.gob.cl (no agricultura.gob.cl).
# Listado sin paginación clásica; "Load More" carga más por JS (scrapeamos lo que
# viene en el HTML inicial).
#
# SELECTORES:
#   Listado:  enlaces <a href="https://minagri.gob.cl/noticia/...">
#   Artículo: h1.elementor-heading-title (título), .post-date (fecha),
#             .elementor-widget-text-editor p (cuerpo); bajada = primer párrafo
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

# --- Configuración Minagri ---
FUENTE <- "agricultura"
hist   <- ""
BASE   <- "https://minagri.gob.cl"

# Base del listado (paginación: se detecta si existe /p/N o /page/N)
bases_seccion <- c("/noticias-minagri/")

obtener_max_pagina_minagri <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?minagri\\.gob\\.cl")
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
  max_pag <- obtener_max_pagina_minagri(base_path)
  message(glue("  {base_path}: {max_pag} página(s)"))
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
  }
  urls
}))

# --- 1) Obtener enlaces de noticias ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  hrefs  <- sesion |> html_elements("a") |> html_attr("href")
  # Solo URLs a noticia individual (excluir noticias-minagri, wp-json, etc.)
  noticias_sesion <- hrefs[
    grepl("^https?://(www\\.)?minagri\\.gob\\.cl/noticia/[^/]+/?$", hrefs, ignore.case = TRUE)
  ]
  noticias_sesion <- unique(noticias_sesion)
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  return(noticias_sesion)
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()

    x_titulo  <- noticia |> html_elements("h1.elementor-heading-title") |> html_text2()
    x_fecha_t <- noticia |> html_elements(".post-date") |> html_text2()
    # Cuerpo: párrafos dentro de los bloques de texto del artículo
    bloques   <- noticia |> html_elements("article.post-wrap .elementor-widget-text-editor")
    x_cuerpo  <- if (length(bloques) > 0) {
      bloques |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    } else {
      noticia |> html_elements("article p") |> html_text2() |> paste(collapse = "\n")
    }
    # Bajada: primer párrafo del cuerpo (o NA)
    parrafos <- noticia |> html_elements("article.post-wrap .elementor-widget-text-editor p") |> html_text2()
    x_bajada <- if (length(parrafos) > 0) parrafos[1] else NA_character_

    # Parsear fecha tipo "25 de febrero de 2026" o "25 febrero 2026" → "YYYY MM DD"
    x_fecha <- x_fecha_t
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      meses <- "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"
      mes_txt <- str_extract(x_fecha_t[1], meses) |> tolower()
      dia     <- str_extract(x_fecha_t[1], "\\d{1,2}(?=\\s+de\\s+(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre))")
      if (is.na(dia)) dia <- str_extract(x_fecha_t[1], "\\d{1,2}(?=\\s+(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre))")
      ano     <- str_extract(x_fecha_t[1], "\\d{4}")
      if (!is.na(mes_txt)) {
        mes <- dplyr::recode(mes_txt,
          "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4",
          "mayo" = "5", "junio" = "6", "julio" = "7", "agosto" = "8",
          "septiembre" = "9", "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
        )
        if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
      }
    }

    tibble(
      titulo         = validar_elementos(x_titulo),
      bajada         = validar_elementos(x_bajada),
      fecha          = validar_elementos(if (length(x_fecha) > 0) x_fecha[1] else NA_character_),
      cuerpo         = validar_elementos(x_cuerpo),
      fuente         = FUENTE,
      url            = enlace,
      fecha_scraping = lubridate::now()
    )
  }, error = function(e) {
    message(glue("Error en {FUENTE}: {e$message} — {enlace}"))
    return(NULL)
  })
})

resultados <- resultados |> purrr::compact() |> list_rbind()

# --- 3) Guardar ---
guardar_noticias(resultados, FUENTE, hist)
