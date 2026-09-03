library(dplyr)
library(rvest)
library(xml2)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# K20 (auditor-datos, 2026-09-03): ex-ante.cl lleva 39 días consecutivos en 0
# artículos (desde el 2026-07-27). `curl` confirma un bloqueo WAF de Cloudflare
# DURO ("Sorry, you have been blocked", body de la página de error) en TODAS
# las páginas HTML del sitio -- categorías (`/category/nacional/`, etc.),
# `sitemap.xml`, `/wp-json/` (robots.txt sí responde 200). Chromote tampoco lo
# atraviesa: no es un desafío JS, el bloqueo ocurre en el borde (Cloudflare)
# antes de llegar al HTML real -- mismo patrón que llevó a discontinuar
# laizquierdadiario.cl (y el mismo síntoma que se confirmó en elciudadano.com,
# ver 2026-07-20-k6-elciudadano.md).
#
# Vía alternativa confirmada (2026-09-03): el feed RSS general del sitio
# (`/feed/`, con paginación `/feed/?paged=N`) SÍ responde 200 de forma
# consistente y expone título, fecha, categoría y el CUERPO COMPLETO del
# artículo (`content:encoded`) sin visitar ninguna página bloqueada. A
# diferencia de elciudadano.com, ex-ante no tiene ediciones traducidas
# mezcladas en el feed -- no hace falta filtrar por categoría, se toman todos
# los items. En una muestra de 50 items (5 páginas) el rango de fechas cubrió
# ~5 días (29-ago a 03-sep) a un ritmo de ~10/día, en línea con el histórico
# de la fuente (8-14/día).
#
# Nada de esto requiere Chromote: es una petición HTTP simple a un XML.

UA_FEED <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36"

# Trae y parsea una página del feed general de ex-ante (~10 items c/u).
obtener_pagina_feed <- function(pagina, ua = UA_FEED) {
  url_feed <- if (pagina <= 1) {
    "https://www.ex-ante.cl/feed/"
  } else {
    paste0("https://www.ex-ante.cl/feed/?paged=", pagina)
  }
  tryCatch({
    con_url <- url(url_feed, headers = c("User-Agent" = ua))
    on.exit(try(close(con_url), silent = TRUE), add = TRUE)
    txt <- paste(readLines(con_url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    xml2::read_xml(txt)
  }, error = function(e) {
    message(glue("obtener_pagina_feed: error en página {pagina}: {conditionMessage(e)}"))
    NULL
  })
}

# Parsea pubDate RFC-822 ("Thu, 03 Sep 2026 18:43:15 +0000") sin depender
# del locale del sistema (los meses vienen en inglés abreviado).
parsear_pubdate <- function(pub_raw) {
  meses <- c(Jan = "01", Feb = "02", Mar = "03", Apr = "04", May = "05", Jun = "06",
             Jul = "07", Aug = "08", Sep = "09", Oct = "10", Nov = "11", Dec = "12")
  m <- str_match(pub_raw, "(\\d{2}) (\\w{3}) (\\d{4})")
  if (is.na(m[1, 1])) return(NA_character_)
  paste(m[1, 4], meses[[m[1, 3]]], m[1, 2], sep = "-")
}

# Convierte un <item> del feed en una fila con los mismos campos que usa
# guardar_noticias_en_postgres(). La bajada es el <h4> inicial (el lead del
# artículo, mismo rol que ".contenido-noticia h4" en el HTML bloqueado); el
# cuerpo junta párrafos y listas (<p>, <li>) en orden.
item_a_fila <- function(item, ns) {
  titulo      <- xml_text(xml_find_first(item, "title"))
  enlace      <- xml_text(xml_find_first(item, "link"))
  pub_raw     <- xml_text(xml_find_first(item, "pubDate"))
  cuerpo_html <- xml_text(xml_find_first(item, "content:encoded", ns))

  parseado <- tryCatch(read_html(paste0("<div>", cuerpo_html, "</div>")), error = function(e) NULL)
  bajada <- NA_character_
  cuerpo <- NA_character_
  if (!is.null(parseado)) {
    bajada_v <- parseado |> html_elements("h4") |> html_text2()
    if (length(bajada_v) > 0) bajada <- bajada_v[1]
    parrafos <- parseado |> html_elements("p, li") |> html_text2()
    parrafos <- parrafos[nzchar(str_trim(parrafos))]
    if (length(parrafos) > 0) cuerpo <- paste(parrafos, collapse = "\n")
  }

  tibble(
    titulo         = titulo,
    bajada         = bajada,
    cuerpo         = cuerpo,
    fecha          = parsear_pubdate(pub_raw),
    fecha_scraping = as.character(lubridate::today()),
    fuente         = "exante",
    url            = enlace
  )
}

# páginas ----
n_pags <- n_paginas_fuente("exante", con, pags_por_dia = 1.5, min_pags = 3, max_pags = 30)
message(glue("scraping_exante: pidiendo {n_pags} páginas del feed (~10 items c/u)"))

resultados_paginas <- map(seq_len(n_pags), \(pagina) {
  doc <- obtener_pagina_feed(pagina)
  if (is.null(doc)) return(NULL)
  items <- xml_find_all(doc, "//item")
  if (length(items) == 0) {
    message(glue("Se obtuvieron 0 items en la página {pagina} del feed"))
    return(NULL)
  }
  ns <- xml_ns(doc)
  filas <- map_df(items, \(it) tryCatch(
    item_a_fila(it, ns),
    error = function(e) {
      message(glue("scraping_exante: error en item de la página {pagina}: {conditionMessage(e)}"))
      NULL
    }
  ))
  message(glue("Se obtuvieron {nrow(filas)} items en la página {pagina} del feed"))
  filas
})

resultados_exante <- bind_rows(resultados_paginas) |>
  filter(!is.na(url), !is.na(fecha)) |>
  distinct(url, .keep_all = TRUE) |>
  filter(!vapply(url, ya_scrapeado_en_db, logical(1), con = con))

# guardar ----
guardar_noticias_en_postgres(resultados_exante, con)
DBI::dbDisconnect(con)

message(glue("listo cron exante {lubridate::now()}"))
