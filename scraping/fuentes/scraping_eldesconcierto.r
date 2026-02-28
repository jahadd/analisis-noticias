# =============================================================================
# scraping_eldesconcierto.r — El Desconcierto (eldesconcierto.cl)
# =============================================================================
# Medio: El Desconcierto
# Secciones: noticias (y subsecciones), tendencias (y subsecciones), medio ambiente (y subsecciones),
#            opinion, politica, reportajes, cultura, deportes
# Artículos: https://www.eldesconcierto.cl/YYYY/MM/DD/slug
# Paginación: se detecta el máximo de páginas por sección (/page/N).
#
# SELECTORES:
#   Listado:  .entry-title a o enlaces con /20 en href
#   Artículo: main .entry-title (título), main blockquote (bajada), .entry-meta .date (fecha), .entry-content p (cuerpo)
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "eldesconcierto"
hist   <- ""
BASE   <- "https://www.eldesconcierto.cl"

# Secciones y subsecciones: noticias, tendencias, medio ambiente (con subsecciones), opinion, politica, reportajes, cultura, deportes
bases_seccion <- c(
  "noticias/",
  "noticias/nacional/",
  "noticias/economia/",
  "noticias/regiones/",
  "noticias/mundo/",
  "tendencias/",
  "tendencias/vida/",
  "tendencias/tecnologia/",
  "medio-ambiente/",
  "medio-ambiente/naturaleza/",
  "medio-ambiente/buen-vivir/",
  "medio-ambiente/cambio-climatico/",
  "opinion/",
  "politica/",
  "reportajes/",
  "cultura/",
  "deportes/"
)

# Descubre el número máximo de páginas (enlaces .../page/N)
obtener_max_pagina_ed <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?eldesconcierto\\.cl/")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    base_sin_barra <- sub("/$", "", base_path)
    patron <- paste0("^", gsub("([.?*+^])", "\\\\\\1", base_sin_barra), "/page/([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/page/([0-9]+).*", "\\1", matches))
    max_pag <- max(1L, max(nums, na.rm = TRUE))
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado (página 1 + page/2..max por sección)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_ed(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, "/", base_sin_barra, "/page/", 2:max_pag))
  }
  urls
}))

# Solo URLs de artículo: eldesconcierto.cl/YYYY/MM/DD/slug (excluir /page/)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  grepl("^https?://(www\\.)?eldesconcierto\\.cl/[0-9]{4}/[0-9]{2}/[0-9]{2}/[^/]+/?$", url) &&
    !grepl("/page/", url)
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  hrefs <- sesion |> html_elements(".entry-title a") |> html_attr("href")
  if (length(hrefs) == 0 || all(is.na(hrefs))) {
    hrefs <- sesion |> html_elements("a[href*='/20']") |> html_attr("href")
  }
  hrefs <- hrefs[!is.na(hrefs)]
  hrefs <- str_remove(hrefs, "\\?.*$")
  urls  <- hrefs[vapply(hrefs, es_url_articulo, FUN.VALUE = logical(1))]
  urls  <- unique(urls)
  message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
  return(urls)
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    x_titulo  <- noticia |> html_elements("main") |> html_elements(".entry-title") |> html_text2()
    x_bajada  <- noticia |> html_elements("main") |> html_elements("blockquote") |> html_text2()
    x_fecha_t <- noticia |> html_elements("main") |> html_elements(".entry-meta") |> html_elements(".date") |> html_text2()
    x_cuerpo  <- noticia |> html_elements(".entry-content") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    x_fecha   <- x_fecha_t
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      mes <- str_extract(x_fecha_t[1], "\\w+") |> tolower() |> dplyr::recode(
        "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4", "mayo" = "5",
        "junio" = "6", "julio" = "7", "agosto" = "8", "septiembre" = "9",
        "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
      )
      dia_ano <- str_extract(x_fecha_t[1], "\\d+, \\d{4}") |> str_remove(",")
      if (!is.na(dia_ano)) {
        ano <- str_extract(dia_ano, "\\d{4}$")
        dia <- str_extract(dia_ano, "^\\d+")
        if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
      }
    }
    # Fecha también en URL: /YYYY/MM/DD/slug
    x_fecha_1 <- if (length(x_fecha) > 0) x_fecha[1] else NA_character_
    if (is.na(x_fecha_1) || !nzchar(trimws(x_fecha_1))) {
      url_fecha <- str_extract(enlace, "/[0-9]{4}/[0-9]{2}/[0-9]{2}/")
      if (!is.na(url_fecha)) {
        partes <- str_extract_all(url_fecha, "[0-9]+")[[1]]
        if (length(partes) >= 3) x_fecha <- paste(partes[1], partes[2], partes[3])
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
}) |> purrr::compact() |> list_rbind()

# --- 3) Guardar ---
guardar_noticias(resultados, FUENTE, hist)
