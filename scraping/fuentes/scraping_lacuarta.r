# =============================================================================
# scraping_lacuarta.r — La Cuarta (lacuarta.com)
# =============================================================================
# Medio: La Cuarta
# Secciones: Chile, espectaculos, deportes, tendencias, servicios, urbana
# Listados: https://www.lacuarta.com/canal/{seccion}/ (paginación /page/N si existe)
# Artículos: https://www.lacuarta.com/{seccion}/noticia/slug
#
# SELECTORES:
#   Listado:  enlaces a lacuarta.com con /noticia/ en el path
#   Artículo: h1 (título), fecha en texto (ej. "27 FEBRERO 2026"), article/main p (cuerpo)
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "lacuarta"
hist   <- ""
BASE   <- "https://www.lacuarta.com"

# Secciones: Chile, espectaculos, deportes, tendencias, servicios, urbana
bases_seccion <- c(
  "canal/chile/",
  "canal/espectaculos/",
  "canal/deportes/",
  "canal/tendencias/",
  "canal/servicios/",
  "canal/urbana/"
)

# Descubre el número máximo de páginas (enlaces canal/SECCION/page/N)
obtener_max_pagina_lc <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?lacuarta\\.com/")
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
  max_pag <- obtener_max_pagina_lc(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, "/", base_sin_barra, "/page/", 2:max_pag))
  }
  urls
}))

# Artículo: lacuarta.com/{seccion}/noticia/slug (excluir /canal/, /autor/, /temas/)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  if (!grepl("^https?://(www\\.)?lacuarta\\.com/", url)) return(FALSE)
  if (grepl("/canal/|/autor/|/temas/", url)) return(FALSE)
  grepl("^https?://(www\\.)?lacuarta\\.com/(chile|espectaculos|deportes|tendencias|servicios|urbana)/noticia/[a-z0-9-]+/?$", url)
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs <- sesion |> html_elements("a[href*='/noticia/']") |> html_attr("href")
    if (length(hrefs) == 0 || all(is.na(hrefs))) {
      hrefs <- sesion |> html_elements("a[href*='lacuarta.com']") |> html_attr("href")
    }
    hrefs <- hrefs[!is.na(hrefs)]
    hrefs <- str_remove(hrefs, "\\?.*$")
    urls  <- hrefs[vapply(hrefs, es_url_articulo, FUN.VALUE = logical(1))]
    urls  <- unique(urls)
    message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
    return(urls)
  }, error = function(e) {
    message(glue("Error listado {enlace}: {e$message}"))
    return(NULL)
  })
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    x_titulo <- noticia |> html_elements("h1") |> html_text2()
    if (length(x_titulo) == 0) {
      x_titulo <- noticia |> html_elements("article h1, .entry-title") |> html_text2()
    }
    x_bajada <- noticia |> html_elements("article blockquote, .summary, .bajada") |> html_text2()
    x_fecha_t <- noticia |> html_elements("time") |> html_attr("datetime")
    if (length(x_fecha_t) == 0) {
      x_fecha_t <- noticia |> html_text2() |> str_extract("\\d{1,2}\\s+(?:ENE|FEB|MAR|ABR|MAY|JUN|JUL|AGO|SEP|OCT|NOV|DIC)[A-Z]*\\s+\\d{4}")
    }
    x_cuerpo  <- noticia |> html_elements("article p, .article-body p, .entry-content p") |> html_text2() |> paste(collapse = "\n")
    x_fecha   <- NA_character_
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x_fecha_t[1])) {
        x_fecha <- str_remove(x_fecha_t[1], "T.*$") |> str_replace_all("-", " ")
      } else {
        date_str <- str_extract(x_fecha_t[1], "\\d{1,2}\\s+\\w+\\s+\\d{4}")
        if (!is.na(date_str)) {
          dia     <- str_extract(date_str, "^\\d{1,2}")
          mes_txt <- str_extract(date_str, "\\s\\w+\\s") |> str_trim() |> tolower()
          ano     <- str_extract(date_str, "\\d{4}$")
          mes <- dplyr::recode(mes_txt,
            "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4", "mayo" = "5",
            "junio" = "6", "julio" = "7", "agosto" = "8", "septiembre" = "9",
            "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
          )
          if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
        }
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
