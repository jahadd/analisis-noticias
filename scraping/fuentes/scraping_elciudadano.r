library(dplyr)
library(rvest)
library(xml2)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# K6 (curador-bd/auditor-datos, 2026-07-20 a 2026-09-03): elciudadano.com
# lleva 48 días consecutivos en 0 artículos. Confirmado con curl (headers de
# navegador, sin y con user-agent) que hay un bloqueo WAF de Cloudflare
# DURO ("Sorry, you have been blocked") en TODAS las páginas HTML del sitio
# -- listado (/chile/page/N), portada, artículo individual, sitemap.xml e
# incluso /wp-json/. No es un desafío JS que Chromote pudiera atravesar: es
# el mismo bloqueo que llevó a discontinuar laizquierdadiario.cl.
#
# Vía alternativa confirmada (2026-09-03): el feed RSS general del sitio
# (`/feed/`, y su paginación `/feed/?paged=N`) SÍ responde 200 de forma
# consistente y expone título, fecha, categorías, extracto y el CUERPO
# COMPLETO del artículo (`content:encoded`) -- sin necesidad de visitar
# ninguna página HTML bloqueada. El feed no tiene una sección "chile"
# propia (`/chile/feed/` es el feed de comentarios de una página estática,
# no un feed de categoría/taxonomía -- confirmado, devuelve 0 items), así
# que se pagina el feed general (mezcla todas las secciones) y se filtra
# por la categoría "Chile" que WordPress sí asigna a los artículos de esa
# sección -- excluyendo las ediciones traducidas del mismo artículo
# (English Edition/Deutsche Ausgabe, que también llevan la etiqueta
# "Chile"). En una muestra de 30 items (3 páginas) 6 eran de sección Chile
# en español (~7/día), en línea con el volumen histórico de la fuente
# (6-10/día).
#
# Nada de esto requiere Chromote: es una petición HTTP simple a un XML.

UA_FEED <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36"

# Trae y parsea una página del feed general (10 items c/u, todas las secciones).
obtener_pagina_feed <- function(pagina, ua = UA_FEED) {
  url_feed <- if (pagina <= 1) {
    "https://www.elciudadano.com/feed/"
  } else {
    paste0("https://www.elciudadano.com/feed/?paged=", pagina)
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

# Quita el boilerplate de WordPress ("El Ciudadano" como primer párrafo
# suelto, "La entrada X se publicó primero en El Ciudadano" al final) y
# devuelve el texto limpio, párrafo por párrafo.
limpiar_parrafos <- function(html_fragmento) {
  if (is.na(html_fragmento) || !nzchar(html_fragmento)) return(character(0))
  parseado <- tryCatch(read_html(paste0("<div>", html_fragmento, "</div>")), error = function(e) NULL)
  if (is.null(parseado)) return(character(0))
  parrafos <- parseado |> html_elements("p") |> html_text2()
  parrafos <- parrafos[!str_detect(parrafos, "^El Ciudadano$")]
  parrafos <- parrafos[!str_detect(parrafos, "^La entrada .* se public")]
  parrafos[nzchar(str_trim(parrafos))]
}

# Parsea pubDate RFC-822 ("Thu, 03 Sep 2026 19:18:53 +0000") sin depender
# del locale del sistema (los meses vienen en inglés abreviado).
parsear_pubdate <- function(pub_raw) {
  meses <- c(Jan = "01", Feb = "02", Mar = "03", Apr = "04", May = "05", Jun = "06",
             Jul = "07", Aug = "08", Sep = "09", Oct = "10", Nov = "11", Dec = "12")
  m <- str_match(pub_raw, "(\\d{2}) (\\w{3}) (\\d{4})")
  if (is.na(m[1, 1])) return(NA_character_)
  paste(m[1, 4], meses[[m[1, 3]]], m[1, 2], sep = "-")
}

# Convierte un <item> del feed en una fila con los mismos campos que usa
# guardar_noticias_en_postgres().
item_a_fila <- function(item, ns) {
  titulo      <- xml_text(xml_find_first(item, "title"))
  enlace      <- xml_text(xml_find_first(item, "link"))
  pub_raw     <- xml_text(xml_find_first(item, "pubDate"))
  categorias  <- str_trim(xml_text(xml_find_all(item, "category")))
  cuerpo_html <- xml_text(xml_find_first(item, "content:encoded", ns))
  desc_html   <- xml_text(xml_find_first(item, "description"))

  parrafos_cuerpo <- limpiar_parrafos(cuerpo_html)
  parrafos_bajada <- limpiar_parrafos(desc_html)

  # "Chile" se usa como categoría también en las ediciones traducidas del
  # mismo artículo (English Edition /en/..., Deutsche Ausgabe /deustche/...)
  # -- se excluyen explícitamente para no triplicar contenido no-español
  # que nunca estuvo en el alcance original de esta fuente (/chile/page/N).
  es_chile <- ("Chile" %in% categorias) &&
    !any(categorias %in% c("English Edition", "Deutsche Ausgabe")) &&
    !str_detect(enlace, "elciudadano\\.com/(en|deustche)/")

  tibble(
    titulo         = titulo,
    bajada         = if (length(parrafos_bajada) > 0) parrafos_bajada[1] else NA_character_,
    cuerpo         = if (length(parrafos_cuerpo) > 0) paste(parrafos_cuerpo, collapse = "\n") else NA_character_,
    fecha          = parsear_pubdate(pub_raw),
    fecha_scraping = as.character(lubridate::today()),
    fuente         = "elciudadano",
    url            = enlace,
    es_chile       = es_chile
  )
}

# páginas ----
n_pags <- n_paginas_fuente("elciudadano", con, pags_por_dia = 3, min_pags = 5, max_pags = 60)
message(glue("scraping_elciudadano: pidiendo {n_pags} páginas del feed (~10 items c/u, todas las secciones)"))

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
      message(glue("scraping_elciudadano: error en item de la página {pagina}: {conditionMessage(e)}"))
      NULL
    }
  ))
  message(glue("Se obtuvieron {nrow(filas)} items en la página {pagina} del feed ({sum(filas$es_chile, na.rm = TRUE)} de sección Chile)"))
  filas
})

resultados_elciudadano <- bind_rows(resultados_paginas) |>
  filter(es_chile, !is.na(url), !is.na(fecha)) |>
  distinct(url, .keep_all = TRUE) |>
  filter(!vapply(url, ya_scrapeado_en_db, logical(1), con = con)) |>
  select(-es_chile)

# guardar ----
guardar_noticias_en_postgres(resultados_elciudadano, con)
DBI::dbDisconnect(con)

message(glue("listo cron elciudadano {lubridate::now()}"))
