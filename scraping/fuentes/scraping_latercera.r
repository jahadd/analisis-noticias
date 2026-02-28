# =============================================================================
# scraping_latercera.r — La Tercera (latercera.com)
# =============================================================================
# Medio: La Tercera
# Secciones: politica, nacional, negocios (pulso), tecnología, eldeportivo (el-deportivo)
# Listados: https://www.latercera.com/canal/{seccion}/
# Artículos: https://www.latercera.com/{seccion}/noticia/{slug}/
#
# SELECTORES:
#   Listado:  enlaces a latercera.com con /noticia/ en el path
#   Artículo: h1 (título), bajada, fecha, párrafos del cuerpo
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "latercera"
hist   <- ""
BASE   <- "https://www.latercera.com"

# Secciones: politica, nacional, negocios (= canal pulso), tecnología, eldeportivo (= el-deportivo)
bases_seccion <- c(
  "canal/politica/",
  "canal/nacional/",
  "canal/pulso/",           # negocios
  "canal/tecnologia/",
  "canal/el-deportivo/"     # eldeportivo
)

# Descubre el número máximo de páginas (canal/seccion/N o canal/seccion/page/N)
obtener_max_pagina_lt <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?latercera\\.com/")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    base_sin_barra <- sub("/$", "", base_path)
    base_esc <- gsub("([.?*+^])", "\\\\\\1", base_sin_barra)
    patron <- paste0("^", base_esc, "/(?:page/)?([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/([0-9]+)/?$", "\\1", matches))
    nums <- nums[nums >= 1]
    if (length(nums) == 0) return(1L)
    max_pag <- min(max(1L, max(nums, na.rm = TRUE)), 30L)
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_lt(base_path)
  base_sin_barra <- sub("/$", "", base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_sin_barra, "/", 2:max_pag))
  }
  urls
}))

# Artículo: latercera.com con /noticia/ en el path (excluir canal, autor, compra-suscripcion, etc.)
es_url_articulo <- function(url) {
  if (is.na(url) || !is.character(url) || !nzchar(trimws(url))) return(FALSE)
  if (!grepl("^https?://(www\\.)?latercera\\.com/", url)) return(FALSE)
  if (!grepl("/noticia/", url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?latercera\\.com/?") |> str_remove("\\?.*$")
  if (grepl("^canal/", path)) return(FALSE)
  TRUE
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs <- sesion |> html_elements("a[href*='latercera.com'][href*='/noticia/']") |> html_attr("href")
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
      x_titulo <- noticia |> html_elements(".article-title, article h1, [data-testid='article-title']") |> html_text2()
    }
    x_bajada <- noticia |> html_elements("article blockquote, .article-summary, .bajada, [data-testid='article-summary']") |> html_text2()
    x_fecha_t <- noticia |> html_elements("time") |> html_attr("datetime")
    if (length(x_fecha_t) == 0) {
      x_fecha_t <- noticia |> html_elements(".article-date, .date, [data-testid='article-date']") |> html_text2()
    }
    x_cuerpo  <- noticia |> html_elements("article p, .article-body p, .article-content p, [data-testid='article-body'] p") |> html_text2() |> paste(collapse = "\n")
    x_fecha   <- NA_character_
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x_fecha_t[1])) {
        x_fecha <- str_remove(x_fecha_t[1], "T.*$") |> str_replace_all("-", " ")
      } else {
        date_str <- x_fecha_t[1]
        dia   <- str_extract(date_str, "^\\d{1,2}|\\d{1,2}\\s")
        if (!is.na(dia)) dia <- str_trim(dia)
        mes_txt <- str_extract(date_str, "\\s\\w+\\s") |> str_trim() |> tolower()
        ano   <- str_extract(date_str, "\\d{4}")
        mes <- dplyr::recode(mes_txt,
          "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4", "mayo" = "5",
          "junio" = "6", "julio" = "7", "agosto" = "8", "septiembre" = "9",
          "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
        )
        if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
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
