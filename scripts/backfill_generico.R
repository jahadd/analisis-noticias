# ------------------------------------------------------------------------------
# backfill_generico.R — Backfill histórico genérico vía Wayback + scrape en vivo.
# Parametrizado por medio (env BF_FUENTE, BF_URLS). Fecha por cascada:
#   URL YYYY/MM/DD  →  URL -DD-M-YYYY (t13)  →  JSON-LD  →  meta  →  <time>.
# Título = h1, cuerpo = <p>, bajada = meta description. Resumible, por lotes.
# ------------------------------------------------------------------------------
suppressMessages({
  library(dplyr); library(rvest); library(httr); library(stringr); library(lubridate)
})
source("funciones.R")
con <- conectar_db()

FUENTE    <- Sys.getenv("BF_FUENTE")
URLS_FILE <- Sys.getenv("BF_URLS")
stopifnot(nzchar(FUENTE), file.exists(URLS_FILE))
UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
PAUSA <- as.numeric(Sys.getenv("BF_PAUSA", "0.25"))
TIMEOUT <- as.numeric(Sys.getenv("BF_TIMEOUT", "10"))   # timeout corto: los errores no arrastran el ritmo
DESDE <- Sys.getenv("BF_DESDE", "")                     # piso de fecha (ej. 2022-01-01); descarta anteriores

urls <- unique(readLines(URLS_FILE, warn = FALSE)); urls <- urls[nzchar(urls)]
message(sprintf("[%s] Total URLs: %d", FUENTE, length(urls)))

extraer_fecha <- function(url, noticia) {
  f <- str_extract(url, "20[0-9]{2}/[0-9]{1,2}/[0-9]{1,2}")          # YYYY/MM/DD en URL
  if (!is.na(f)) return(str_replace_all(f, "/", "-"))
  f <- str_extract(url, "[0-9]{1,2}-[0-9]{1,2}-20[0-9]{2}")          # DD-M-YYYY (t13)
  if (!is.na(f)) return(f)
  ld <- noticia |> html_elements('script[type="application/ld+json"]') |> html_text() |> paste(collapse = " ")
  f <- ld |> str_extract('datePublished"\\s*:\\s*"[0-9]{4}-[0-9]{2}-[0-9]{2}') |> str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
  if (!is.na(f)) return(f)
  f <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content") |> head(1) |> str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
  if (length(f) == 1 && !is.na(f)) return(f)
  f <- noticia |> html_elements("time") |> html_attr("datetime") |> head(1) |> str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
  if (length(f) == 1 && !is.na(f)) return(f)
  NA_character_
}

LOTE <- 150L; buffer <- vector("list", 0)
n_guardadas <- 0L; n_saltadas <- 0L; n_error <- 0L; t0 <- Sys.time()
flush_buffer <- function() {
  if (length(buffer) == 0) return(invisible())
  df <- bind_rows(Filter(Negate(is.null), buffer))
  if (nrow(df) > 0) n_guardadas <<- n_guardadas + as.integer(guardar_noticias_en_postgres(df, con))
  buffer <<- vector("list", 0)
}

for (i in seq_along(urls)) {
  enlace <- urls[i]
  if (i %% 200 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins")); rate <- i / max(el, 0.01)
    message(sprintf("[%s %d/%d] guard=%d salt=%d err=%d | %.0f url/min | ETA %.1fh",
      FUENTE, i, length(urls), n_guardadas, n_saltadas, n_error, rate, (length(urls)-i)/max(rate,0.01)/60))
  }
  if (ya_scrapeado_en_db(enlace, con)) { n_saltadas <- n_saltadas + 1L; next }
  res <- tryCatch({
    Sys.sleep(PAUSA)
    resp <- httr::GET(enlace, httr::user_agent(UA), httr::timeout(TIMEOUT))
    if (httr::status_code(resp) != 200) stop("status ", httr::status_code(resp))
    noticia <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
    fecha  <- extraer_fecha(enlace, noticia)
    # piso de fecha: descartar artículos anteriores a DESDE (fuera de alcance)
    if (nzchar(DESDE)) {
      fp <- suppressWarnings(.parse_fecha(fecha))
      if (!is.na(fp) && fp < as.Date(DESDE)) stop("fuera de rango")
    }
    # techo: una fecha futura es un parseo erróneo (p. ej. año de una proyección
    # en el texto tomado como fecha) y contamina MAX(fecha) en los análisis
    fp <- suppressWarnings(.parse_fecha(fecha))
    if (!is.na(fp) && fp > lubridate::today() + 1L) stop("fecha futura (parseo erróneo)")
    titulo <- noticia |> html_elements("h1") |> html_text2() |> head(1)
    bajada <- noticia |> html_elements("meta[name='description']") |> html_attr("content") |> head(1)
    cuerpo <- noticia |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    tibble(titulo = titulo |> validar_elementos(), bajada = bajada |> validar_elementos(),
           fecha = fecha |> validar_elementos(), cuerpo = cuerpo |> validar_elementos(),
           fuente = FUENTE, url = enlace, fecha_scraping = lubridate::today())
  }, error = function(e) NULL)
  if (is.null(res)) n_error <- n_error + 1L else buffer[[length(buffer) + 1L]] <- res
  if (length(buffer) >= LOTE) flush_buffer()
}
flush_buffer()
DBI::dbDisconnect(con)
message(sprintf("[%s] LISTO: guard=%d salt=%d err=%d", FUENTE, n_guardadas, n_saltadas, n_error))
