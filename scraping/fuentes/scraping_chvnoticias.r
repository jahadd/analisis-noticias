# =============================================================================
# scraping_chvnoticias.r — CHV Noticias / Chilevisión
# =============================================================================
# Medio: CHV Noticias (Chilevisión)
# Secciones: nacional, internacional, reportajes, cazanoticias, economía, deportes,
#            casos policiales, te ayuda, show, alerta climática
# Listados: /noticias/{seccion}/ con paginación /p/N
# Artículos: https://www.chilevision.cl/noticias/{seccion}/slug
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "chvnoticias"
hist   <- ""
BASE   <- "https://www.chilevision.cl"

# Bases: nacional, internacional, reportajes, cazanoticias, economía, deportes, casos policiales, te ayuda, show, alerta climática
bases_seccion <- c(
  "/noticias/nacional/",
  "/noticias/internacional/",
  "/noticias/reportajes/",
  "/noticias/cazanoticias/",
  "/noticias/economia/",
  "/noticias/deportes/",
  "/noticias/casos-policiales/",
  "/noticias/te-ayuda/",
  "/noticias/show/",
  "/noticias/alerta-climatica/"
)

obtener_max_pagina_chv <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?chilevision\\.cl")
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
  max_pag <- obtener_max_pagina_chv(base_path)
  message(glue("  {base_path}: {max_pag} página(s)"))
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
  }
  urls
}))

normalizar_url <- function(href) {
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  if (stringr::str_starts(href, "http")) return(href)
  if (stringr::str_starts(href, "/")) return(paste0(BASE, href))
  NA_character_
}

es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  ok_seccion <- grepl("^https?://(www\\.)?chilevision\\.cl/noticias/(nacional|internacional|reportajes|cazanoticias|economia|deportes|casos-policiales|te-ayuda|show|alerta-climatica)/[a-z0-9-]+/?$", url)
  no_listado <- !grepl("/p/$|/video/$", url)
  ok_seccion & no_listado
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  hrefs  <- sesion |> html_elements("a") |> html_attr("href")
  hrefs  <- hrefs[!is.na(hrefs)]
  urls   <- vapply(hrefs, normalizar_url, FUN.VALUE = character(1))
  urls   <- urls[!is.na(urls)]
  urls   <- urls[vapply(urls, es_url_articulo, FUN.VALUE = logical(1))]
  urls   <- unique(urls)
  message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
  urls
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()

    x_titulo <- noticia |> html_element("h1") |> html_text2()

    # Párrafos del artículo (contenido principal)
    parrafos <- noticia |> html_elements("article p") |> html_text2()
    if (length(parrafos) == 0) {
      parrafos <- noticia |> html_elements("main p") |> html_text2()
    }
    x_bajada <- if (length(parrafos) > 0) parrafos[1] else NA_character_
    x_cuerpo <- if (length(parrafos) > 0) paste(parrafos, collapse = "\n") else NA_character_

    # Buscar fecha como dd/mm/aaaa en todo el texto de la página
    texto_plano <- noticia |> html_text2()
    fecha_raw   <- stringr::str_extract(texto_plano, "\\\\b[0-3]?[0-9]/[0-1]?[0-9]/[0-9]{4}\\\\b")
    x_fecha <- NA_character_
    if (!is.na(fecha_raw)) {
      dt <- suppressWarnings(lubridate::dmy(fecha_raw))
      if (!is.na(dt)) {
        x_fecha <- paste(lubridate::year(dt), lubridate::month(dt), lubridate::day(dt))
      } else {
        x_fecha <- fecha_raw
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
