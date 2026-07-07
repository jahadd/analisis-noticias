# ------------------------------------------------------------------------------
# backfill_lacuarta_historico.R — Backfill profundo de La Cuarta (gap ~1 año).
# El sitio (Arc XP) no expone paginación histórica, así que las URLs se obtienen
# de Wayback CDX (datos/backfill_lacuarta/urls.txt) y se scrapean EN VIVO con los
# selectores nuevos. Secciones: chile, cronica, mundo, tendencias.
#
# Resumible: ya_scrapeado_en_db() se chequea ANTES del fetch, así que reanudar
# salta lo ya guardado. Guarda por lotes (checkpoint).
# ------------------------------------------------------------------------------
suppressMessages({
  library(dplyr); library(rvest); library(httr); library(stringr)
  library(purrr); library(glue); library(lubridate)
})
source("funciones.R")
con <- conectar_db()

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
PAUSA <- 0.5  # cortesía (robots.txt no exige crawl-delay)

urls <- readLines("datos/backfill_lacuarta/urls.txt", warn = FALSE)
urls <- unique(urls[nzchar(urls)])
message(glue("Total URLs a procesar: {length(urls)}"))

LOTE <- 150L
buffer <- vector("list", 0)
n_guardadas <- 0L
n_saltadas  <- 0L
n_error     <- 0L
t0 <- Sys.time()

flush_buffer <- function() {
  if (length(buffer) == 0) return(invisible())
  df <- bind_rows(Filter(Negate(is.null), buffer))
  if (nrow(df) > 0) {
    n <- guardar_noticias_en_postgres(df, con)
    n_guardadas <<- n_guardadas + as.integer(n)
  }
  buffer <<- vector("list", 0)
}

for (i in seq_along(urls)) {
  enlace <- urls[i]

  # progreso cada 100
  if (i %% 100 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    rate <- i / max(elapsed, 0.01)
    eta <- (length(urls) - i) / max(rate, 0.01)
    message(glue("[{i}/{length(urls)}] guardadas={n_guardadas} saltadas={n_saltadas} err={n_error} | {round(rate,0)} url/min | ETA {round(eta/60,1)}h"))
  }

  # saltar si ya está (antes del fetch: ahorra tiempo al reanudar)
  if (ya_scrapeado_en_db(enlace, con)) { n_saltadas <- n_saltadas + 1L; next }

  res <- tryCatch({
    Sys.sleep(PAUSA)
    resp <- httr::GET(enlace, httr::user_agent(UA), httr::timeout(25))
    if (httr::status_code(resp) != 200) stop("status ", httr::status_code(resp))
    noticia <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

    titulo <- noticia |> html_elements("h1") |> html_text2() |> head(1)

    fecha <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content") |> head(1)
    if (length(fecha) == 0 || is.na(fecha) || !nzchar(fecha)) {
      fecha <- noticia |> html_elements("time") |> html_attr("datetime") |> head(1)
    }
    fecha <- str_extract(fecha, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

    bajada <- noticia |> html_elements("meta[name='description']") |> html_attr("content") |> head(1)
    cuerpo <- noticia |> html_elements("p") |> html_text2() |> paste(collapse = "\n")

    tibble(
      titulo = titulo |> validar_elementos(),
      bajada = bajada |> validar_elementos(),
      fecha  = fecha  |> validar_elementos(),
      cuerpo = cuerpo |> validar_elementos(),
      fuente = "lacuarta",
      url = enlace,
      fecha_scraping = lubridate::today()
    )
  }, error = function(e) { NULL })

  if (is.null(res)) { n_error <- n_error + 1L } else { buffer[[length(buffer) + 1L]] <- res }

  if (length(buffer) >= LOTE) flush_buffer()
}

flush_buffer()
DBI::dbDisconnect(con)
message(glue("LISTO backfill lacuarta histórico: guardadas={n_guardadas} saltadas={n_saltadas} err={n_error}"))
