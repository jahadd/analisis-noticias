# =============================================================================
# scraping_izquierdadiario.r — La Izquierda Diario Chile (laizquierdadiario.cl)
# =============================================================================
# Medio: La Izquierda Diario (edición Chile)
# Secciones: politica, mundo obrero, estudiantil, opinión, internacional, sociedad,
#            cultura, pueblo mapuche, economía, generos y sexualidades, deportes
# Listados: /Politica-Chile, /Mundo-Obrero-Chile, /Seccion-Juventud, etc. (paginación /page/N si existe)
# Artículos: https://www.laizquierdadiario.cl/Slug-del-articulo/ (un segmento en el path)
#
# SELECTORES:
#   Listado:  enlaces a laizquierdadiario.cl (artículos con slug)
#   Artículo: main .entry-title o h1 (título), blockquote (bajada), .entry-meta .date (fecha), .entry-content p (cuerpo)
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "izquierdadiario"
hist   <- ""
BASE   <- "https://www.laizquierdadiario.cl"

# Secciones (paths exactos del menú Chile): politica, mundo obrero, estudiantil, opinión, internacional, sociedad, cultura, pueblo mapuche, economía, generos y sexualidades, deportes
bases_seccion <- c(
  "Politica-Chile",
  "Mundo-Obrero-Chile",
  "Seccion-Juventud",           # estudiantil
  "Seccion-Opinion",
  "Internacional",
  "Seccion-Sociedad",
  "Seccion-Cultura",
  "Mapuches",                   # pueblo mapuche
  "Seccion-Economia",
  "Seccion-Generos-y-Sexualidades",
  "Seccion-Deportes"
)

# Paths de un segmento que no son artículos (secciones e institucionales)
paths_no_articulo <- c(
  "Politica-Chile", "Mundo-Obrero-Chile", "Seccion-Juventud", "Seccion-Opinion",
  "Internacional", "Seccion-Sociedad", "Seccion-Cultura", "Mapuches",
  "Seccion-Economia", "Seccion-Generos-y-Sexualidades", "Seccion-Deportes",
  "Quienes-Somos", "Ideas-Socialistas", "comunidadcl", "Pronostico-de-Santiago-de-Chile"
)

# Descubre el número máximo de páginas (enlaces .../page/N o SECCION/page/N)
obtener_max_pagina_lid <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?laizquierdadiario\\.cl/")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    base_esc <- gsub("([.?*+^])", "\\\\\\1", base_path)
    patron <- paste0("^", base_esc, "/page/([0-9]+)/?$")
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
  max_pag <- obtener_max_pagina_lid(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_path, "/page/", 2:max_pag))
  }
  urls
}))

# Artículo: laizquierdadiario.cl con un solo segmento en el path (slug), excluir secciones e institucionales
es_url_articulo <- function(url) {
  if (!grepl("^https?://(www\\.)?laizquierdadiario\\.cl/", url)) return(FALSE)
  if (grepl("/page/[0-9]+", url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?laizquierdadiario\\.cl/?") |> str_remove("\\?.*$") |> str_remove("/$")
  path <- str_remove(path, "#.*$")
  if (nchar(path) == 0) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  if (length(segmentos) != 1) return(FALSE)
  slug <- segmentos[1]
  if (slug %in% paths_no_articulo) return(FALSE)
  nchar(slug) >= 8
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs <- sesion |> html_elements(".entry-title a") |> html_attr("href")
    if (length(hrefs) == 0 || all(is.na(hrefs))) {
      hrefs <- sesion |> html_elements("a[href*='laizquierdadiario.cl']") |> html_attr("href")
    }
    hrefs <- hrefs[!is.na(hrefs)]
    hrefs <- str_remove(hrefs, "\\?.*$") |> str_remove("#.*$")
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
    x_titulo  <- noticia |> html_elements("main") |> html_elements(".entry-title") |> html_text2()
    if (length(x_titulo) == 0) {
      x_titulo <- noticia |> html_elements("h1") |> html_text2()
    }
    x_bajada  <- noticia |> html_elements("main") |> html_elements("blockquote") |> html_text2()
    x_fecha_t <- noticia |> html_elements("main") |> html_elements(".entry-meta") |> html_elements(".date") |> html_text2()
    if (length(x_fecha_t) == 0) {
      x_fecha_t <- noticia |> html_elements("time") |> html_attr("datetime")
    }
    x_cuerpo  <- noticia |> html_elements(".entry-content") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    x_fecha   <- x_fecha_t
    if (length(x_fecha_t) > 0 && nchar(trimws(x_fecha_t[1])) > 0) {
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
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x_fecha_t[1])) {
        x_fecha <- str_remove(x_fecha_t[1], "T.*$") |> str_replace_all("-", " ")
      }
    }
    if (length(x_fecha) > 1) x_fecha <- x_fecha[1]
    tibble(
      titulo         = validar_elementos(x_titulo),
      bajada         = validar_elementos(x_bajada),
      fecha          = validar_elementos(x_fecha),
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
dir.create(file.path("scraping", "datos", FUENTE), showWarnings = FALSE, recursive = TRUE)
readr::write_rds(resultados, ruta_resultado(FUENTE, hist))
message(glue("Listo {FUENTE}: {nrow(resultados)} noticias → {ruta_resultado(FUENTE, hist)} ({now()})"))
