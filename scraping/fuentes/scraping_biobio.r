# =============================================================================
# scraping_biobio.r — Radio Bío Bío (biobiochile.cl)
# =============================================================================
# Medio: BioBioChile
# Secciones: nacional, internacional, economia, deportes, tendencias, opinión, bbcl investiga, bbcl contigo
# Listados: /noticias/{seccion}/ con paginación /p/N
# Artículos: .../noticias/{seccion}/... .shtml
#
# Para cada artículo usamos, si está disponible, el JSON-LD (NewsArticle) para
# obtener headline, description, datePublished y articleBody. Si no, caemos a
# selectores HTML simples (h1, párrafos dentro del artículo).
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "biobio"
hist   <- ""
BASE   <- "https://www.biobiochile.cl"

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Falta paquete jsonlite. Instalar con: install.packages('jsonlite')")
}

# Bases: nacional, internacional, economia, deportes, tendencias, opinión, bbcl investiga, bbcl contigo
bases_seccion <- c(
  "/noticias/nacional/",
  "/noticias/internacional/",
  "/noticias/economia/",
  "/noticias/deportes/",
  "/noticias/tendencias/",
  "/noticias/opinion/",
  "/noticias/bbcl-investiga/",
  "/noticias/bbcl-contigo/"
)

obtener_max_pagina_biobio <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?biobiochile\\.cl")
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
  max_pag <- obtener_max_pagina_biobio(base_path)
  message(glue("  {base_path}: {max_pag} página(s)"))
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
  }
  urls
}))

# Aceptar artículos de las secciones configuradas
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  grepl("^https?://(www\\.)?biobiochile\\.cl/noticias/(nacional|internacional|economia|deportes|tendencias|opinion|bbcl-investiga|bbcl-contigo)/.+\\.shtml$", url)
}

leer_newsarticle_jsonld_biobio <- function(html_doc) {
  scripts <- html_doc |>
    html_elements("script[type='application/ld+json']") |>
    html_text2()
  if (length(scripts) == 0) return(NULL)

  parsed <- purrr::map(scripts, \(txt) {
    tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  }) |> purrr::compact()
  if (length(parsed) == 0) return(NULL)

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
    if (is.list(obj) && is.null(obj[["@type"]]) && length(obj) > 1) {
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

parsear_fecha_iso <- function(datePublished) {
  if (is.null(datePublished) || length(datePublished) == 0) return(NA_character_)
  dp <- as.character(datePublished[[1]])
  dt <- suppressWarnings(lubridate::ymd_hms(dp, tz = "UTC"))
  if (is.na(dt)) dt <- suppressWarnings(lubridate::ymd(dp, tz = "UTC"))
  if (is.na(dt)) return(dp)
  paste(lubridate::year(dt), lubridate::month(dt), lubridate::day(dt))
}

# --- 1) Obtener enlaces de noticias desde cada página de listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  html_txt <- as.character(sesion)
  pat <- "https?://(www\\.)?biobiochile\\.cl/noticias/(nacional|internacional|economia|deportes|tendencias|opinion|bbcl-investiga|bbcl-contigo)/[^\"]+?\\.shtml"
  urls <- regmatches(html_txt, gregexpr(pat, html_txt, perl = TRUE))[[1]]
  urls <- unique(urls)
  urls <- urls[es_url_articulo(urls)]
  message(glue("Se obtuvieron {length(urls)} noticias en {enlace}"))
  urls
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    html_doc <- enlace |> bow() |> scrape()

    news <- leer_newsarticle_jsonld_biobio(html_doc)

    if (!is.null(news)) {
      x_titulo <- news[["headline"]]
      x_bajada <- news[["description"]]
      x_fecha  <- parsear_fecha_iso(news[["datePublished"]])
      x_cuerpo <- news[["articleBody"]]
    } else {
      x_titulo <- html_doc |> html_elements("article h1") |> html_text2()
      parrafos <- html_doc |> html_elements("article p") |> html_text2()
      x_bajada <- if (length(parrafos) > 0) parrafos[1] else NA_character_
      x_cuerpo <- if (length(parrafos) > 0) paste(parrafos, collapse = "\n") else NA_character_
      x_fecha  <- NA_character_
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
