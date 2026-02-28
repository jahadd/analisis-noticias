# =============================================================================
# scraping_publimetro.r — Publimetro Chile (publimetro.cl)
# =============================================================================
# Medio: Publimetro
# Secciones: noticias, deportes, entretenimiento, estilo de vida, tecnología, tacómetro, publimetro emprende
# Listados: https://www.publimetro.cl/{seccion}/ (enlaces relativos /seccion/YYYY/MM/DD/slug/)
# Artículos: https://www.publimetro.cl/{seccion}/{YYYY}/{MM}/{DD}/{slug}/
#
# SELECTORES:
#   Listado:  a[href^="/"] con path seccion/YYYY/MM/DD/slug (4 segmentos tras sección)
#   Artículo: h1 (título), meta og:description (bajada), meta article:published_time o time (fecha),
#             main p, article p o .contenido (cuerpo)
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "publimetro"
hist   <- ""
BASE   <- "https://www.publimetro.cl"

# Secciones: noticias, deportes, entretenimiento, estilo de vida, tecnología, tacómetro, publimetro emprende
bases_seccion <- c(
  "noticias",
  "deportes",
  "entretenimiento",
  "estilo-vida",       # estilo de vida
  "tecnologia",        # tecnología
  "tacometro",         # tacómetro
  "esfuerzo-pyme"      # publimetro emprende
)

# Normalizar URL: si es relativa (/noticias/...), convertir a absoluta
normalizar_url_pm <- function(href) {
  href <- str_trim(href)
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  href <- str_remove(href, "\\?.*$")
  if (grepl("^/", href) && !grepl("^//", href)) return(paste0(BASE, href))
  if (grepl("^https?://(www\\.)?publimetro\\.cl/", href)) return(href)
  NA_character_
}

# Artículo: path {seccion}/{YYYY}/{MM}/{DD}/{slug}/ con fecha válida
es_url_articulo <- function(url) {
  url <- normalizar_url_pm(url)
  if (is.na(url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?publimetro\\.cl/?") |> str_remove("\\?.*$") |> str_remove("/$")
  if (nchar(path) == 0) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  # seccion + YYYY + MM + DD + slug = 5 segmentos mínimo
  if (length(segmentos) < 5) return(FALSE)
  seccion <- segmentos[1]
  if (!seccion %in% bases_seccion) return(FALSE)
  if (!grepl("^[0-9]{4}$", segmentos[2])) return(FALSE)
  if (!grepl("^[0-9]{2}$", segmentos[3])) return(FALSE)
  if (!grepl("^[0-9]{2}$", segmentos[4])) return(FALSE)
  TRUE
}

# Descubre el número máximo de páginas (enlaces .../page/N o ?page=N)
obtener_max_pagina_pm <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- vapply(hrefs, normalizar_url_pm, FUN.VALUE = character(1))
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    path_only <- str_remove(hrefs_norm, "^https?://(www\\.)?publimetro\\.cl/?") |> str_remove("\\?.*$")
    # Paginación: /seccion/page/N o /seccion?page=N
    base_esc <- gsub("([.?*+^])", "\\\\\\1", base_path)
    patron_page <- paste0("^", base_esc, "/page/([0-9]+)/?$")
    patron_q <- paste0("^", base_esc, "$")
    nums <- integer(0)
    for (p in path_only) {
      if (grepl(patron_page, p)) nums <- c(nums, as.integer(sub(".*/page/([0-9]+).*", "\\1", p)))
    }
    hrefs_q <- hrefs[grepl("page=[0-9]+", hrefs)]
    if (length(hrefs_q) > 0) {
      nums_q <- str_extract(hrefs_q, "page=([0-9]+)") |> str_remove("page=") |> as.integer()
      nums <- c(nums, nums_q)
    }
    nums <- nums[!is.na(nums) & nums >= 1]
    if (length(nums) == 0) return(1L)
    max_pag <- min(max(1L, max(nums, na.rm = TRUE)), 30L)
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado (página 1 + page/2..max o ?page=2..max)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_pm(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_path, "/page/", 2:max_pag))
  }
  urls
}))

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='/20']") |> html_attr("href")
    hrefs  <- c(hrefs, sesion |> html_elements("a[href^='/noticias'], a[href^='/deportes'], a[href^='/entretenimiento'], a[href^='/estilo-vida'], a[href^='/tecnologia'], a[href^='/tacometro'], a[href^='/esfuerzo-pyme']") |> html_attr("href"))
    hrefs  <- hrefs[!is.na(hrefs)]
    hrefs  <- str_remove(hrefs, "\\?.*$")
    urls   <- vapply(hrefs, normalizar_url_pm, FUN.VALUE = character(1))
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
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    # Título: h1 o c-heading
    x_titulo <- noticia |> html_elements("h1") |> html_text2()
    if (length(x_titulo) == 0) x_titulo <- noticia |> html_elements("[class*='heading'][class*='title'], .c-heading") |> html_text2()
    # Bajada: og:description
    x_bajada <- noticia |> html_elements("meta[property='og:description']") |> html_attr("content")
    if (length(x_bajada) == 0) x_bajada <- noticia |> html_elements("main blockquote, article blockquote") |> html_text2()
    # Fecha: article:published_time o time
    x_fecha_t <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content")
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements("time") |> html_attr("datetime")
    }
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements("time, [class*='date']") |> html_text2()
    }
    # Cuerpo: main p, article p
    x_cuerpo <- noticia |> html_elements("main p, article p") |> html_text2() |> paste(collapse = "\n")
    if (!nzchar(trimws(x_cuerpo))) x_cuerpo <- noticia |> html_elements("[class*='content'] p, [class*='body'] p") |> html_text2() |> paste(collapse = "\n")
    # Normalizar fecha ISO a AAAA M D
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
