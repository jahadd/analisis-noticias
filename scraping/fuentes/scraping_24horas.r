# =============================================================================
# scraping_24horas.r — 24 Horas (24horas.cl)
# =============================================================================
# Medio: 24 Horas (TVN)
# Secciones: informe especial, actualidad, internacional, deportes, regiones, tendencias, noticiarios.
# Paginación: se detecta el máximo de páginas en cada sección (/p/N) y se scrapean todas.
# Ejecutar desde carpeta noticias/ (o que funciones.R esté en el path).
#
# SELECTORES (por si el sitio cambia):
#   Listado:  enlaces a artículos por sección
#   Artículo: .art-principal h1.tit (título), p.fecha (fecha), .art-content p (cuerpo);
#             bajada: primer p dentro de .art-principal que no sea .fecha
# =============================================================================

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

if (file.exists("funciones.R")) source("funciones.R") else source("../funciones.R")

# --- Configuración 24 Horas ---
FUENTE <- "24horas"
hist   <- ""
BASE   <- "https://24horas.cl"

# Bases de cada sección (path sin paginación): informe especial, actualidad, internacional, deportes, regiones, tendencias, noticiarios
bases_seccion <- c(
  "/informe-especial/",
  "/actualidad/nacional/",
  "/internacional/noticias/",
  "/deportes/",
  "/regiones/",
  "/tendencias/",
  "/noticiarios/"
)

# Descubre el número máximo de páginas en una sección leyendo los enlaces /p/N de la primera página
obtener_max_pagina <- function(base_path) {
  url_primera <- paste0(BASE, base_path)
  if (is.null(revisar_url(url_primera))) return(1L)
  tryCatch({
    sesion <- bow(url_primera) |> scrape()
    hrefs  <- sesion |> html_elements("a") |> html_attr("href")
    hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?24horas\\.cl")
    hrefs_norm <- hrefs_norm[!is.na(hrefs_norm)]
    hrefs_norm <- hrefs_norm[grepl("^/", hrefs_norm)]
    # Paginación: mismo path + /p/N (ej. /actualidad/nacional/p/5)
    base_sin_barra <- sub("/$", "", base_path)
    patron <- paste0("^", gsub("([.?*+^])", "\\\\\\1", base_sin_barra), "/p/([0-9]+)/?$")
    matches <- hrefs_norm[grepl(patron, hrefs_norm)]
    if (length(matches) == 0) return(1L)
    nums <- as.integer(sub(".*/p/([0-9]+).*", "\\1", matches))
    max_pag <- max(1L, max(nums, na.rm = TRUE))
    message(glue("  {base_path}: {max_pag} página(s)"))
    max_pag
  }, error = function(e) {
    message(glue("  Error en {base_path}: {e$message} → 1 página"))
    1L
  })
}

# Construir todas las URLs de listado (página 1 + p/2 hasta p/max por sección)
enlaces_listado <- unlist(map(bases_seccion, \(base_path) {
  max_pag <- obtener_max_pagina(base_path)
  urls <- paste0(BASE, base_path)
  if (max_pag >= 2) {
    base_sin_barra <- sub("/$", "", base_path)
    urls <- c(urls, paste0(BASE, base_sin_barra, "/p/", 2:max_pag))
  }
  urls
}))

# Devuelve TRUE si href es URL de artículo (no listado ni paginación)
es_url_articulo_24h <- function(href) {
  if (is.na(href) || !grepl("^/", href)) return(FALSE)
  if (grepl("/p/[0-9]+", href)) return(FALSE)
  # Actualidad: /actualidad/SUBSEC/SLUG
  if (grepl("^/actualidad/[^/]+/.+", href)) return(TRUE)
  # Internacional: /internacional/noticias/SLUG
  if (grepl("^/internacional/noticias/.+", href)) return(TRUE)
  # Regiones: /regiones/ZONA/REGION/SLUG
  if (grepl("^/regiones/[^/]+/[^/]+/.+", href)) return(TRUE)
  # Informe especial: /informe-especial/SUBSEC/SLUG
  if (grepl("^/informe-especial/[^/]+/.+", href)) return(TRUE)
  # Deportes: /deportes/SLUG o /deportes/.../SLUG (excl. /deportes/ solo y /deportes/p/)
  if (grepl("^/deportes/[^/]+", href) && !grepl("^/deportes/p/", href)) return(TRUE)
  # Tendencias: /tendencias/SLUG o /tendencias/.../SLUG
  if (grepl("^/tendencias/[^/]+", href) && !grepl("^/tendencias/p/", href)) return(TRUE)
  # Noticiarios: /noticiarios/SLUG o /noticiarios/.../SLUG
  if (grepl("^/noticiarios/[^/]+", href) && !grepl("^/noticiarios/p/", href)) return(TRUE)
  FALSE
}

# --- 1) Obtener enlaces de noticias desde cada página listado ---
resultados_enlaces <- map(enlaces_listado, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  sesion <- bow(enlace) |> scrape()
  hrefs  <- sesion |> html_elements("a") |> html_attr("href")
  # Normalizar: quitar dominio si viene absoluto para aplicar es_url_articulo_24h
  hrefs_norm <- str_remove(hrefs, "^https?://(www\\.)?24horas\\.cl")
  hrefs_norm <- ifelse(grepl("^/", hrefs_norm), hrefs_norm, paste0("/", hrefs_norm))
  noticias_sesion <- hrefs[es_url_articulo_24h(hrefs_norm)]
  noticias_sesion <- if (length(noticias_sesion) > 0) {
    urls <- unique(noticias_sesion)
    urls <- ifelse(grepl("^https?://", urls), urls, paste0(BASE, ifelse(grepl("^/", urls), urls, paste0("/", urls))))
    urls
  } else character(0)
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  return(noticias_sesion)
})

enlaces_noticias <- resultados_enlaces |> unlist() |> unique()

# --- 2) Scraping de cada noticia ---
resultados <- map(enlaces_noticias, \(enlace) {
  if (is.null(revisar_url(enlace))) return(NULL)
  tryCatch({
    noticia <- enlace |> bow() |> scrape()

    x_titulo  <- noticia |> html_elements("h1.tit") |> html_text2()
    x_fecha_t <- noticia |> html_elements("p.fecha") |> html_text2()
    # Bajada: primer p que no sea .fecha dentro de .art-principal o .art-content
    parrafos <- noticia |> html_elements(".art-content p") |> html_text2()
    if (length(parrafos) == 0) {
      parrafos <- noticia |> html_elements(".art-principal p") |> html_text2()
      parrafos <- parrafos[!grepl("^(Lunes|Martes|Miércoles|Jueves|Viernes|Sábado|Domingo)\\s+\\d+", parrafos)]
    }
    x_bajada <- if (length(parrafos) > 0) parrafos[1] else NA_character_
    x_cuerpo <- if (length(parrafos) > 1) paste(parrafos, collapse = "\n") else validar_elementos(parrafos)

    # Parsear fecha tipo "Jueves 26 de febrero de 2026" → "YYYY MM DD"
    x_fecha <- x_fecha_t
    if (length(x_fecha_t) > 0 && !is.na(x_fecha_t[1]) && nchar(trimws(x_fecha_t[1])) > 0) {
      meses <- "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"
      mes_txt <- str_extract(x_fecha_t[1], meses) |> tolower()
      dia     <- str_extract(x_fecha_t[1], "\\d{1,2}(?=\\s+de\\s+(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre))")
      ano     <- str_extract(x_fecha_t[1], "\\d{4}$")
      if (!is.na(mes_txt)) {
        mes <- dplyr::recode(mes_txt,
          "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4",
          "mayo" = "5", "junio" = "6", "julio" = "7", "agosto" = "8",
          "septiembre" = "9", "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
        )
        if (!is.na(ano) && !is.na(mes) && !is.na(dia)) x_fecha <- paste(ano, mes, dia)
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
})

resultados <- resultados |> purrr::compact() |> list_rbind()

# --- 3) Guardar ---
guardar_noticias(resultados, FUENTE, hist)
