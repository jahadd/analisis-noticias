# =============================================================================
# scraping_adnradio.r — ADN Radio (adnradio.cl)
# =============================================================================
# Medio: ADN Radio
# Secciones: noticias, deportes, tiempo libre.
# Listados: /noticias/, /deportes/, /tiempo-libre/ (paginación /p/N o /page/N).
#
# Nota: el sitio carga partes por JS; para artículos usamos el JSON-LD
# (<script type="application/ld+json"> con @type = NewsArticle) para obtener:
#   headline, description, datePublished y articleBody.
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "adnradio"
hist   <- ""
BASE   <- "https://www.adnradio.cl"
UA     <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Falta paquete jsonlite. Instalar con: install.packages('jsonlite')")
}

bow_adn <- function(url) polite::bow(url, user_agent = UA)

# Bases de cada sección: noticias, deportes, tiempo libre
bases_seccion <- c("/noticias/", "/deportes/", "/tiempo-libre/")

# Descubre el número máximo de páginas en una sección (enlaces /p/N o /page/N)
obtener_max_pagina_adn <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow_adn(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?adnradio\\.cl")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm) & grepl("^/", hrefs_norm)]
    base_sin_barra <- sub("/$", "", base_path)
    patron <- paste0("^", gsub("([.?*+^])", "\\\\\\1", base_sin_barra), "/(?:p|page)/([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/(?:p|page)/([0-9]+).*", "\\1", matches))
    max(1L, max(nums, na.rm = TRUE))
  }, error = function(e) 1L)
}

# Construir listados: página 1 + p/2..max por sección
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_adn(base_path)
  message(glue("  {base_path}: {max_pag} página(s)"))
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
    # Si el sitio usa /page/N en vez de /p/N, probar ambos (ya tenemos p/; si no hay enlaces p/ no añadimos más)
  }
  urls
}))

normalizar_url <- function(href) {
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  if (str_starts(href, "http")) return(href)
  if (str_starts(href, "/")) return(paste0(BASE, href))
  return(NA_character_)
}

es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  grepl("^https?://(www\\.)?adnradio\\.cl/\\d{4}/\\d{2}/\\d{2}/", url)
}

leer_newsarticle_jsonld <- function(html_doc) {
  scripts <- html_doc |>
    html_elements("script[type='application/ld+json']") |>
    html_text2()
  if (length(scripts) == 0) return(NULL)

  parsed <- purrr::map(scripts, \(txt) {
    tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  }) |> purrr::compact()
  if (length(parsed) == 0) return(NULL)

  # Algunas páginas traen varios JSON-LD; buscamos el NewsArticle
  candidates <- list()
  for (obj in parsed) {
    if (is.list(obj) && !is.null(obj[["@type"]])) {
      candidates[[length(candidates) + 1L]] <- obj
    }
    if (is.list(obj) && !is.null(obj[["@graph"]]) && is.list(obj[["@graph"]])) {
      for (g in obj[["@graph"]]) {
        if (is.list(g) && !is.null(g[["@type"]])) candidates[[length(candidates) + 1L]] <- g
      }
    }
    if (is.list(obj) && is.null(obj[["@type"]]) && is.list(obj) && length(obj) > 1) {
      # lista de objetos
      for (g in obj) {
        if (is.list(g) && !is.null(g[["@type"]])) candidates[[length(candidates) + 1L]] <- g
      }
    }
  }

  if (length(candidates) == 0) return(NULL)
  news <- purrr::keep(candidates, \(x) {
    t <- x[["@type"]]
    (is.character(t) && any(t %in% c("NewsArticle", "Article"))) ||
      (is.list(t) && any(unlist(t) %in% c("NewsArticle", "Article")))
  })
  if (length(news) == 0) return(NULL)
  news[[1]]
}

parsear_fecha <- function(datePublished) {
  if (is.null(datePublished) || length(datePublished) == 0) return(NA_character_)
  dp <- as.character(datePublished[[1]])
  dt <- suppressWarnings(lubridate::ymd_hms(dp, tz = "UTC"))
  if (is.na(dt)) dt <- suppressWarnings(lubridate::ymd(dp, tz = "UTC"))
  if (is.na(dt)) return(dp)
  paste(lubridate::year(dt), lubridate::month(dt), lubridate::day(dt))
}

# --- 1) Obtener enlaces de noticias ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow_adn(enlace) |> scrape()
  # En ADN gran parte de los links vienen embebidos en JSON/script; extraer por regex
  html_txt <- as.character(sesion)
  pat_abs <- "https?://(www\\.)?adnradio\\.cl/[0-9]{4}/[0-9]{2}/[0-9]{2}/[a-z0-9-]+/?"
  abs_urls <- regmatches(html_txt, gregexpr(pat_abs, html_txt, perl = TRUE))[[1]]
  urls <- abs_urls
  urls <- urls[!is.na(urls) & nzchar(urls)]
  # limpiar posibles backslashes de JSON
  urls <- gsub("\\\\+$", "", urls)
  urls <- unique(urls)
  urls <- urls[es_url_articulo(urls)]
  message(glue("Se obtuvieron {length(urls)} artículos en {enlace}"))
  urls
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping por artículo (JSON-LD) ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    html_doc <- enlace |> bow_adn() |> scrape()
    news <- leer_newsarticle_jsonld(html_doc)
    if (is.null(news)) return(NULL)

    x_titulo <- news[["headline"]]
    x_bajada <- news[["description"]]
    x_fecha  <- parsear_fecha(news[["datePublished"]])
    x_cuerpo <- news[["articleBody"]]

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
