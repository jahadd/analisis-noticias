# =============================================================================
# scraping_meganoticias.r — Meganoticias (meganoticias.cl)
# =============================================================================
# Medio: Meganoticias
# Secciones: portada, nacional, dato útil, mundo, tendencias, deportes, mega investiga
# Listados: https://www.meganoticias.cl/{seccion}/ (paginación ?page=N)
# Artículos: https://www.meganoticias.cl/{seccion}/{id}-{slug}.html
#
# SELECTORES:
#   Listado:  a[href*='meganoticias.cl'][href*='.html'] (enlaces a noticias)
#   Artículo: #articulo article .container .cuerpo h1 (título), .contenido-nota p (cuerpo),
#             .fechaHora time o meta article:published_time (fecha)
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "meganoticias"
hist   <- ""
BASE   <- "https://www.meganoticias.cl"

# Secciones: portada (/), nacional, dato útil, mundo, tendencias, deportes, mega investiga
bases_seccion <- c(
  "",                 # portada
  "nacional/",
  "dato-util/",       # dato útil
  "mundo/",
  "tendencias/",
  "deportes/",
  "megainvestiga/"    # mega investiga
)

# Descubre el número máximo de páginas (?page=N)
obtener_max_pagina_mega <- function(base_path) {
  url_primera <- if (nzchar(base_path)) paste0(BASE, "/", base_path) else paste0(BASE, "/")
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='page=']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    nums   <- str_extract(hrefs, "page=([0-9]+)") |> str_remove("page=") |> as.integer()
    nums   <- nums[!is.na(nums) & nums >= 1]
    if (length(nums) == 0) return(1L)
    max_pag <- min(max(1L, max(nums, na.rm = TRUE)), 30L)
    path_label <- if (nzchar(base_path)) base_path else "portada"
    message(glue("  {path_label}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado (página 1 sin query, 2..max con ?page=N)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_mega(base_path)
  base_url <- if (nzchar(base_path)) paste0(BASE, "/", base_path) else paste0(BASE, "/")
  urls <- base_url
  if (max_pag >= 2) {
    base_clean <- str_remove(base_url, "/$")
    urls <- c(urls, paste0(base_clean, "/?page=", 2:max_pag))
  }
  urls
}))

# Artículo: meganoticias.cl con path {seccion}/{id}-{slug}.html o {id}-{slug}.html (excluir listados)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  if (!grepl("^https?://(www\\.)?meganoticias\\.cl/", url)) return(FALSE)
  if (!grepl("\\.html", url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?meganoticias\\.cl/?") |> str_remove("\\?.*$") |> str_remove("/$")
  if (!nzchar(path)) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  # Excluir listados, señales en vivo, temas, etc. (solo sección + archivo o solo archivo)
  if (length(segmentos) == 0) return(FALSE)
  ultimo <- segmentos[length(segmentos)]
  # Archivo tipo 515782-slug.html (con o sin sección delante: nacional/515781-....html)
  if (!grepl("^[0-9]+-.+\\.html$", ultimo)) return(FALSE)
  # Excluir paths como senal-en-vivo/..., temas/..., dato-oferta/...
  if (length(segmentos) >= 2) {
    seccion <- segmentos[1]
    if (seccion %in% c("senal-en-vivo", "temas", "lo-mas-visto", "noticieros", "entrevistas", "reportajes", "elecciones-chile", "permiso-de-circulacion", "calidad-de-vida", "dato-oferta")) return(FALSE)
  }
  TRUE
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs  <- sesion |> html_elements("a[href*='meganoticias.cl'][href*='.html']") |> html_attr("href")
    hrefs  <- hrefs[!is.na(hrefs)]
    hrefs  <- str_remove(hrefs, "\\?.*$")
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
# Selectores Meganoticias: #articulo, article .cuerpo h1, .contenido-nota p, .fechaHora time
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    # Título: article .container .cuerpo h1
    x_titulo <- noticia |> html_elements("#articulo article .cuerpo h1, article .container .cuerpo h1") |> html_text2()
    if (length(x_titulo) == 0) x_titulo <- noticia |> html_elements("main .entry-title, h1") |> html_text2()
    # Bajada: og:description o primer p de contenido
    x_bajada <- noticia |> html_elements("meta[property='og:description']") |> html_attr("content")
    if (length(x_bajada) == 0) x_bajada <- noticia |> html_elements("main blockquote, article blockquote") |> html_text2()
    # Fecha: meta article:published_time o .fechaHora time
    x_fecha_t <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content")
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements(".fechaHora time") |> html_attr("datetime")
    }
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t[1])) {
      x_fecha_t <- noticia |> html_elements(".fechaHora") |> html_text2()
    }
    # Cuerpo: .contenido-nota p
    x_cuerpo <- noticia |> html_elements("#articulo .contenido-nota p, article .contenido-nota p") |> html_text2() |> paste(collapse = "\n")
    if (!nzchar(trimws(x_cuerpo))) x_cuerpo <- noticia |> html_elements("main .entry-content p, .entry-content p") |> html_text2() |> paste(collapse = "\n")
    # Normalizar fecha
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
