# =============================================================================
# scraping_t13.r — T13 (t13.cl)
# =============================================================================
# Medio: T13
# Secciones: el tiempo, lo ultimo, Chile (nacional), te puede servir, deportes, politica,
#            reportajes t13, mundo, espectaculos, emprendedores, negocios, tendencias
# Listados: https://www.t13.cl/{seccion} (paginación ?page=N)
# Artículos: https://www.t13.cl/noticia/{seccion}/{slug}
#
# SELECTORES:
#   Listado:  a[href*='/noticia/']
#   Artículo: h1, meta og:title/description (título/bajada), .field--name-body p (cuerpo), meta/fecha
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "t13"
hist   <- ""
BASE   <- "https://www.t13.cl"

# Secciones: el tiempo, lo ultimo, Chile (=nacional), te puede servir, deportes, politica, reportajes t13, mundo, espectaculos, emprendedores, negocios, tendencias
bases_seccion <- c(
  "el-tiempo",
  "lo-ultimo",
  "nacional",           # Chile
  "te-puede-servir",
  "deportes13",         # deportes
  "politica",
  "reportajest13",      # reportajes t13
  "mundo",
  "espectaculos",
  "emprendedores",
  "negocios",
  "tendencias"
)

# Normalizar URL
normalizar_url_t13 <- function(href) {
  href <- str_trim(href)
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  href <- str_remove(href, "\\?.*$") |> str_remove("#.*$")
  if (grepl("^/", href) && !grepl("^//", href)) return(paste0(BASE, href))
  if (grepl("^https?://(www\\.)?t13\\.cl/", href)) return(href)
  NA_character_
}

# Artículo: path noticia/{seccion}/{slug} (mínimo 3 segmentos)
es_url_articulo <- function(url) {
  url <- normalizar_url_t13(url)
  if (is.na(url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?t13\\.cl/?") |> str_remove("\\?.*$") |> str_remove("/$")
  if (!grepl("^noticia/", path)) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  length(segmentos) >= 3
}

# Descubre el número máximo de páginas (?page=N)
obtener_max_pagina_t13 <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='page=']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    nums   <- str_extract(hrefs, "page=([0-9]+)") |> str_remove("page=") |> as.integer()
    nums   <- nums[!is.na(nums) & nums >= 1]
    if (length(nums) == 0) return(1L)
    max_pag <- min(max(1L, max(nums, na.rm = TRUE)), 30L)
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado (página 1 + ?page=2..max)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_t13(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_path, "?page=", 2:max_pag))
  }
  urls
}))

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='/noticia/']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    urls   <- vapply(hrefs, normalizar_url_t13, FUN.VALUE = character(1))
    urls   <- urls[!is.na(urls)]
    urls   <- urls[vapply(urls, es_url_articulo, FUN.VALUE = logical(1))]
    urls   <- unique(urls)
    message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
    return(urls)
  }, error = function(e) {
    message(glue("Error listado {enlace}: {e$message}"))
    return(NULL)
  })
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
# T13/Drupal: og:title, og:description, .field--name-body o .node__content
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    x_titulo <- noticia |> html_elements("meta[property='og:title']") |> html_attr("content")
    if (length(x_titulo) == 0 || is.na(x_titulo[1])) {
      x_titulo <- noticia |> html_elements("h1, .page-title, [class*='title']") |> html_text2()
    }
    x_bajada <- noticia |> html_elements("meta[property='og:description'], meta[name='description']") |> html_attr("content")
    if (length(x_bajada) == 0) x_bajada <- noticia |> html_elements("[class*='summary'], [class*='lead'] p") |> html_text2()
    x_fecha_t <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content")
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements("time[datetime]") |> html_attr("datetime")
    }
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements("[class*='date'], time") |> html_text2()
    }
    x_cuerpo <- noticia |> html_elements(".field--name-body p, .node__content p, [class*='body'] p, article p") |> html_text2() |> paste(collapse = "\n")
    if (!nzchar(trimws(x_cuerpo))) x_cuerpo <- noticia |> html_elements("main p, .content p") |> html_text2() |> paste(collapse = "\n")
    x_fecha <- NA_character_
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      date_str <- x_fecha_t[1]
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", date_str)) {
        x_fecha <- str_remove(date_str, "T.*$") |> str_replace_all("-", " ")
      } else {
        mes <- str_extract(date_str, "\\w+") |> tolower() |> dplyr::recode(
          "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4", "mayo" = "5",
          "junio" = "6", "julio" = "7", "agosto" = "8", "septiembre" = "9",
          "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
        )
        dia_ano <- str_extract(date_str, "\\d+, \\d{4}") |> str_remove(",")
        if (!is.na(dia_ano)) {
          ano <- str_extract(dia_ano, "\\d{4}$")
          dia <- str_extract(dia_ano, "^\\d+")
          if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
        }
      }
    }
    if (length(x_fecha) > 1) x_fecha <- x_fecha[1]
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
}) |> purrr::compact() |> purrr::list_rbind()

# --- 3) Guardar ---
guardar_noticias(resultados, FUENTE, hist)
