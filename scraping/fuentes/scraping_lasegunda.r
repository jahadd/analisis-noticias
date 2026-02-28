# =============================================================================
# scraping_lasegunda.r — La Segunda (lasegunda.com)
# =============================================================================
# Medio: La Segunda
# Secciones: politica, top secret, economía, internacional, conversación, opinión,
#            buena prosa (documentos), redes, radar, sondeo relámpago, ocio ilustrado, círculo empresarial
# Listados: https://www.lasegunda.com/{seccion}
# Artículos: URL /Noticias/{Seccion}/{YYYY}/{MM}/{id}/{slug} (enlaces relativos en el HTML)
#
# SELECTORES:
#   Listado:  a[href^="/Noticias/"] (enlaces relativos); normalizar a BASE + href
#   Artículo: article.item_seg h1 (título), #head_h2Epigrafe (bajada), article.item_seg p (cuerpo),
#             rts_date en script (fecha DD-MM-YYYY) o .txt_date
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

FUENTE <- "lasegunda"
hist   <- ""
BASE   <- "https://www.lasegunda.com"

# Secciones: politica, top secret, economía, internacional, conversación, opinión, buena prosa, redes, radar, sondeo relámpago, ocio ilustrado, círculo empresarial
# Buena Prosa en el sitio = /documentos
bases_seccion <- c(
  "politica",
  "topsecret",
  "economia",
  "internacional",
  "conversacion",
  "opinion",
  "documentos",       # buena prosa
  "redes",
  "radar",
  "sondeorelampago",
  "ocioilustrado",
  "circuloempresarial"
)

# Paths que son secciones (listados), no artículos
paths_seccion <- c(
  "politica", "topsecret", "economia", "internacional", "conversacion", "opinion",
  "documentos", "redes", "radar", "sondeorelampago", "ocioilustrado", "circuloempresarial"
)

# Descubre el número máximo de páginas si existe (ej. politica/2, politica/page/2)
obtener_max_pagina_ls <- function(base_path) {
  url_primera <- paste0(BASE, "/", base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?lasegunda\\.com/")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    base_esc <- gsub("([.?*+^])", "\\\\\\1", base_path)
    patron <- paste0("^", base_esc, "/(?:page/)?([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/([0-9]+)/?$", "\\1", matches))
    max_pag <- max(1L, max(nums, na.rm = TRUE))
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir URLs de listado (página 1 + 2..max si existe)
enlaces_listado <- unlist(purrr::map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina_ls(base_path)
  urls <- paste0(BASE, "/", base_path)
  if (max_pag >= 2) {
    urls <- c(urls, paste0(BASE, "/", base_path, "/", 2:max_pag))
  }
  urls
}))

# Normalizar URL: si es relativa (/Noticias/...), convertir a absoluta
normalizar_url_ls <- function(href) {
  href <- str_trim(href)
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  href <- str_remove(href, "\\?.*$")
  if (grepl("^/", href)) return(paste0(BASE, href))
  if (grepl("^https?://(www\\.)?lasegunda\\.com/", href)) return(href)
  NA_character_
}

# Artículo: path /Noticias/{Seccion}/{YYYY}/{MM}/{id}/{slug} (6 segmentos)
es_url_articulo <- function(url) {
  url <- normalizar_url_ls(url)
  if (is.na(url)) return(FALSE)
  path <- str_remove(url, "^https?://(www\\.)?lasegunda\\.com/?") |> str_remove("\\?.*$") |> str_remove("/$")
  if (!grepl("^Noticias/", path)) return(FALSE)
  segmentos <- str_split(path, "/")[[1]]
  segmentos <- segmentos[nzchar(segmentos)]
  length(segmentos) >= 6
}

# --- 1) Obtener enlaces de noticias desde cada listado ---
# La Segunda usa enlaces relativos: /Noticias/Seccion/YYYY/MM/id/slug
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    sesion <- bow(enlace) |> scrape()
    hrefs  <- sesion |> html_elements("a[href^='/Noticias/']") |> html_attr("href")
    hrefs  <- c(hrefs, sesion |> html_elements("a[href*='lasegunda.com/Noticias/']") |> html_attr("href"))
    hrefs  <- hrefs[!is.na(hrefs)]
    urls   <- vapply(hrefs, normalizar_url_ls, FUN.VALUE = character(1))
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
# Selectores La Segunda: article.item_seg (contenedor), h1, #head_h2Epigrafe (bajada), p; rts_date en script
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    # Título: article.item_seg h1 o .cont_laseg_center_l h1
    x_titulo <- noticia |> html_elements("article.item_seg h1, .cont_laseg_center_l h1") |> html_text2()
    if (length(x_titulo) == 0) x_titulo <- noticia |> html_elements("h1") |> html_text2()
    # Bajada/epígrafe: h2#head_h2Epigrafe
    x_bajada <- noticia |> html_elements("#head_h2Epigrafe, h2#head_h2Epigrafe") |> html_text2()
    if (length(x_bajada) == 0) x_bajada <- noticia |> html_elements("article.item_seg h2") |> html_text2()
    # Fecha: rts_date = 'DD-MM-YYYY' en script, o .txt_date
    scripts  <- noticia |> html_elements("script") |> html_text()
    x_fecha_t <- NA_character_
    for (s in scripts) {
      m <- str_extract(s, "rts_date\\s*=\\s*['\"][0-9]{2}-[0-9]{2}-[0-9]{4}['\"]")
      if (!is.na(m)) {
        x_fecha_t <- str_extract(m, "[0-9]{2}-[0-9]{2}-[0-9]{4}")
        break
      }
    }
    if (length(x_fecha_t) == 0 || is.na(x_fecha_t)) {
      x_fecha_t <- noticia |> html_elements(".txt_date, .date_item") |> html_text2()
    }
    x_cuerpo <- noticia |> html_elements("article.item_seg p, .cont_laseg_center_l .item_seg p") |> html_text2() |> paste(collapse = "\n")
    if (!nzchar(trimws(x_cuerpo))) x_cuerpo <- noticia |> html_elements("article p, .b_r_seg p") |> html_text2() |> paste(collapse = "\n")
    # Normalizar fecha: DD-MM-YYYY → AAAA M D
    x_fecha <- NA_character_
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      date_str <- x_fecha_t[1]
      if (grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}$", date_str)) {
        partes <- str_split(date_str, "-")[[1]]
        x_fecha <- paste(partes[3], partes[2], partes[1])
      } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", date_str)) {
        x_fecha <- str_remove(date_str, "T.*$") |> str_replace_all("-", " ")
      } else {
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
