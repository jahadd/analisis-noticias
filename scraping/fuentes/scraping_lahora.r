# =============================================================================
# scraping_lahora.r — La Hora (lahora.cl)
# =============================================================================
# Medio: La Hora
# Secciones: cronica, deportes, entretención, hora sustentable, estilo de vida, internacional, mujeres
# Listados: https://www.lahora.cl/{seccion}/ (paginación /page/N si existe)
# Artículos: https://www.lahora.cl/SECCION/YYYY/MM/DD/slug
#
# SELECTORES:
#   Listado:  enlaces a lahora.cl con /YYYY/MM/DD/ en el path
#   Artículo: h1 (título), párrafos del cuerpo; fecha desde URL o .date / time
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "lahora"
hist   <- ""
BASE   <- "https://www.lahora.cl"

# Secciones: cronica, deportes, entretención, hora sustentable, estilo de vida, internacional, mujeres
bases_seccion <- c(
  "cronica/",
  "deportes/",
  "entretencion/",
  "hora-sustentable/",
  "estilo-de-vida/",
  "internacional/",
  "mujeres/"
)

# Descubre el número máximo de páginas (enlaces .../page/N)
obtener_max_pagina_lh <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?lahora\\.cl/")
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
  max_pag <- obtener_max_pagina_lh(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, "/", base_sin_barra, "/page/", 2:max_pag))
  }
  urls
}))

# Artículo: lahora.cl/SECCION/YYYY/MM/DD/slug (excluir /page/, /autor/)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  if (!grepl("^https?://(www\\.)?lahora\\.cl/", url)) return(FALSE)
  if (grepl("/page/|/autor/", url)) return(FALSE)
  grepl("^https?://(www\\.)?lahora\\.cl/(cronica|deportes|entretencion|hora-sustentable|estilo-de-vida|internacional|mujeres)/[0-9]{4}/[0-9]{2}/[0-9]{2}/[^/]+/?$", url)
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs <- sesion |> html_elements("a[href*='/20']") |> html_attr("href")
    if (length(hrefs) == 0 || all(is.na(hrefs))) {
      hrefs <- sesion |> html_elements("a[href*='lahora.cl']") |> html_attr("href")
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
      x_fecha_t <- noticia |> html_elements(".date, .fecha, [class*='date']") |> html_text2()
    }
    x_cuerpo  <- noticia |> html_elements("article p, .article-body p, .entry-content p, .post-content p") |> html_text2() |> paste(collapse = "\n")
    x_fecha   <- NA_character_
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x_fecha_t[1])) {
        x_fecha <- str_remove(x_fecha_t[1], "T.*$") |> str_replace_all("-", " ")
      } else {
        mes_txt <- str_extract(x_fecha_t[1], "\\w+") |> tolower()
        mes <- dplyr::recode(mes_txt,
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
    }
    if (is.na(x_fecha) || !nzchar(trimws(x_fecha))) {
      url_fecha <- str_extract(enlace, "/[0-9]{4}/[0-9]{2}/[0-9]{2}/")
      if (!is.na(url_fecha)) {
        partes <- str_extract_all(url_fecha, "[0-9]+")[[1]]
        if (length(partes) >= 3) x_fecha <- paste(partes[1], partes[2], partes[3])
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
}) |> purrr::compact() |> list_rbind()

# --- 3) Guardar ---
guardar_noticias(resultados, FUENTE, hist)
