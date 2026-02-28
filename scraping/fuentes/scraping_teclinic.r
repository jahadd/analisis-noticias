# =============================================================================
# scraping_teclinic.r — The Clinic (theclinic.cl)
# =============================================================================
# Medio: The Clinic (teclinic)
# Secciones: politica, negocios, ciudad, tendencias, tiempo libre, deportes, reportajes
# Listados: https://www.theclinic.cl/noticias/{seccion}/
# Artículos: https://www.theclinic.cl/{YYYY}/{MM}/{DD}/{slug}/
#
# SELECTORES:
#   Listado:  enlaces a theclinic.cl/YYYY/MM/DD/slug
#   Artículo: h1 (título), meta og:description (bajada), .entry-content p o .post-content p (cuerpo), meta/fecha
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "teclinic"
hist   <- ""
BASE   <- "https://www.theclinic.cl"

# Secciones: politica, negocios, ciudad, tendencias, tiempo libre, deportes, reportajes
bases_seccion <- c(
  "noticias/politica/",
  "noticias/negocios/",
  "noticias/ciudad/",
  "noticias/tendencias/",
  "noticias/tiempo-libre/",   # tiempo libre
  "noticias/deportes/",
  "noticias/reportajes/"
)

# Descubre el número máximo de páginas (/page/N/ o ?page=N)
obtener_max_pagina_tc <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='page']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    num_str <- str_extract(hrefs, "/page/[0-9]+|page=[0-9]+")
    nums   <- str_extract(num_str, "[0-9]+") |> as.integer()
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

# Construir URLs de listado (página 1 + page/2..max o ?page=2..max)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_tc(base_path)
  base_sin_barra <- sub("/$", "", base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_sin_barra, "/page/", 2:max_pag, "/"))
  }
  urls
}))

# Artículo: theclinic.cl/YYYY/MM/DD/slug/ (4 segmentos, YYYY/MM/DD numéricos)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  if (!grepl("^https?://(www\\.)?theclinic\\.cl/", url)) return(FALSE)
  if (grepl("/page/[0-9]+", url) || grepl("/noticias/", url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?theclinic\\.cl/?") |> str_remove("\\?.*$") |> str_remove("/$")
  if (nchar(path) == 0) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  if (length(segmentos) < 4) return(FALSE)
  grepl("^[0-9]{4}$", segmentos[1]) && grepl("^[0-9]{2}$", segmentos[2]) && grepl("^[0-9]{2}$", segmentos[3])
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='theclinic.cl']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    hrefs  <- str_remove(hrefs, "\\?.*$") |> str_remove("#.*$")
    urls   <- hrefs[vapply(hrefs, es_url_articulo, FUN.VALUE = logical(1))]
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
    x_titulo <- noticia |> html_elements("meta[property='og:title']") |> html_attr("content")
    if (length(x_titulo) == 0 || is.na(x_titulo[1])) {
      x_titulo <- noticia |> html_elements("h1, .entry-title, .post-title") |> html_text2()
    }
    x_bajada <- noticia |> html_elements("meta[property='og:description'], meta[name='description']") |> html_attr("content")
    if (length(x_bajada) == 0) x_bajada <- noticia |> html_elements(".entry-summary, .excerpt p") |> html_text2()
    x_fecha_t <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content")
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements("time[datetime]") |> html_attr("datetime")
    }
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements(".entry-meta .date, .post-date, time") |> html_text2()
    }
    x_cuerpo <- noticia |> html_elements(".entry-content p, .post-content p, article .content p") |> html_text2() |> paste(collapse = "\n")
    if (!nzchar(trimws(x_cuerpo))) x_cuerpo <- noticia |> html_elements("article p, main p, [class*='body'] p") |> html_text2() |> paste(collapse = "\n")
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
    if (is.na(x_fecha) || !nzchar(trimws(x_fecha))) {
      m <- str_match(enlace, "/([0-9]{4})/([0-9]{2})/([0-9]{2})/")
      if (!is.na(m[1, 1])) x_fecha <- paste(m[1, 2], m[1, 3], m[1, 4])
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
