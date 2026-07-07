library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

#enlaces ----

n_pags <- n_paginas_fuente("lacuarta", con)
# Paginación WordPress: página 1 en /cronica/, páginas siguientes en /cronica/page/N/
enlaces <- c("https://www.lacuarta.com/cronica/",
             paste0("https://www.lacuarta.com/cronica/page/", seq(2L, n_pags), "/"))

# histórico
# enlaces <- paste0("https://www.lacuarta.com/cronica/", 1:9); hist = "_h"
# no deja descargar más allá de la 9

# pero se puede hacer por búsquedas
# enlaces <- paste0("https://www.lacuarta.com/busqueda/boric/", 1:50); hist = "_a"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/boric/", 51:100); hist = "_b"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/chile/", 1:50); hist = "_c"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/chile/", 51:100); hist = "_d"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/gobierno/", 1:50); hist = "_e"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/gobierno/", 51:100); hist = "_f"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/delincuencia/", 1:50); hist = "_g"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/delincuencia/", 51:100); hist = "_h"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/diputado/", 1:50); hist = "_i"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/diputado/", 51:100); hist = "_j"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/comuna/", 1:50); hist = "_k"
# enlaces <- paste0("https://www.lacuarta.com/busqueda/comuna/", 51:100); hist = "_l"


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- session(enlace) |> read_html()

    # rediseño 2026: las noticias están en .story-card__headline a (enlaces relativos)
    noticias <- sesion |>
      html_elements(".story-card__headline a") |>
      html_attr("href")

    noticias <- noticias[!is.na(noticias)]
    noticias <- noticias[str_detect(noticias, "/noticia/")]
    noticias <- ifelse(str_starts(noticias, "http"),
                       noticias,
                       paste0("https://www.lacuarta.com", noticias))

    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping lacuarta: ", e)
    return(NULL)}
  )
})


enlaces_noticias <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_noticias <- map_df(enlaces_noticias, \(enlace) {
  # enlace <- enlaces_noticias[3]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    noticia <- bow(enlace) |> scrape()

    #elementos (rediseño 2026)
    titulo <- noticia |> html_elements("h1") |> html_text2() |> head(1)

    bajada <- noticia |> html_elements("meta[name='description']") |> html_attr("content") |> head(1)

    # fecha desde meta ISO (o <time datetime>), formato YYYY-MM-DD
    fecha <- noticia |> html_elements("meta[property='article:published_time']") |> html_attr("content") |> head(1)
    if (length(fecha) == 0 || is.na(fecha) || !nzchar(fecha)) {
      fecha <- noticia |> html_elements("time") |> html_attr("datetime") |> head(1)
    }
    fecha <- str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}")

    cuerpo <- noticia |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "lacuarta",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping lacuarta: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_noticias, con)
DBI::dbDisconnect(con)

message(glue("listo cron lacuarta {lubridate::now()}"))
