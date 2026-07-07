# ------------------------------------------------------------------------------
# backfill_chv_historico.R — Backfill del gap de CHV Noticias (mar–jun 2026).
# CHV no expone paginación histórica, así que las URLs vienen de Wayback CDX
# (datos/backfill_chv/urls.txt) y se scrapean EN VIVO con los selectores nuevos.
# Fecha desde JSON-LD (el <time datetime> del sitio está roto). Resumible.
# ------------------------------------------------------------------------------
suppressMessages({
  library(dplyr); library(rvest); library(httr); library(stringr)
  library(purrr); library(glue); library(lubridate)
})
source("funciones.R")
con <- conectar_db()

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
PAUSA <- 0.4

urls <- unique(readLines("datos/backfill_chv/urls.txt", warn = FALSE))
urls <- urls[nzchar(urls)]
message(glue("Total URLs: {length(urls)}"))

LOTE <- 150L
buffer <- vector("list", 0); n_guardadas <- 0L; n_saltadas <- 0L; n_error <- 0L
t0 <- Sys.time()

flush_buffer <- function() {
  if (length(buffer) == 0) return(invisible())
  df <- bind_rows(Filter(Negate(is.null), buffer))
  if (nrow(df) > 0) n_guardadas <<- n_guardadas + as.integer(guardar_noticias_en_postgres(df, con))
  buffer <<- vector("list", 0)
}

for (i in seq_along(urls)) {
  enlace <- urls[i]
  if (i %% 100 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins")); rate <- i / max(el, 0.01)
    message(glue("[{i}/{length(urls)}] guardadas={n_guardadas} saltadas={n_saltadas} err={n_error} | {round(rate,0)} url/min | ETA {round((length(urls)-i)/max(rate,0.01)/60,1)}h"))
  }
  if (ya_scrapeado_en_db(enlace, con)) { n_saltadas <- n_saltadas + 1L; next }

  res <- tryCatch({
    Sys.sleep(PAUSA)
    resp <- httr::GET(enlace, httr::user_agent(UA), httr::timeout(25))
    if (httr::status_code(resp) != 200) stop("status ", httr::status_code(resp))
    noticia <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

    titulo <- noticia |> html_elements("h1") |> html_text2() |> head(1)

    ld <- noticia |> html_elements('script[type="application/ld+json"]') |> html_text() |> paste(collapse = " ")
    fecha <- ld |> str_extract('datePublished"\\s*:\\s*"[0-9]{4}-[0-9]{2}-[0-9]{2}') |> str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
    if (length(fecha) == 0 || is.na(fecha)) {
      tt <- noticia |> html_elements("time") |> html_text2() |> head(1)
      fecha <- tt |> str_extract("\\d{1,2}/\\s*\\d{1,2}/\\s*\\d{4}") |> str_remove_all("\\s")
    }
    bajada <- noticia |> html_elements("meta[name='description']") |> html_attr("content") |> head(1)
    cuerpo <- noticia |> html_elements("p") |> html_text2() |> paste(collapse = "\n")

    tibble(titulo = titulo |> validar_elementos(), bajada = bajada |> validar_elementos(),
           fecha = fecha |> validar_elementos(), cuerpo = cuerpo |> validar_elementos(),
           fuente = "chvnoticias", url = enlace, fecha_scraping = lubridate::today())
  }, error = function(e) NULL)

  if (is.null(res)) n_error <- n_error + 1L else buffer[[length(buffer) + 1L]] <- res
  if (length(buffer) >= LOTE) flush_buffer()
}
flush_buffer()
DBI::dbDisconnect(con)
message(glue("LISTO backfill CHV: guardadas={n_guardadas} saltadas={n_saltadas} err={n_error}"))
