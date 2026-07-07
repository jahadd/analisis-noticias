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
# secciones <- "https://www.cnnchile.com/category/pais/"
n_pags <- n_paginas_fuente("cnnchile", con)
secciones <- paste0("https://www.cnnchile.com/category/pais/page/", seq_len(n_pags))

# para descargar noticias anteriores
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 100:1500)
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 5:250); hist = "_h"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 2000:2500); hist = "_h_b"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 2500:3000); hist = "_h_c"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 3001:3500); hist = "_h_d"



#loop enlaces
resultados_links <- map_df(secciones, \(enlace_seccion) {
  # enlace_seccion <- secciones[3]
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace_seccion))) return(NULL)
  
  noticias_seccion <- bow(enlace_seccion) |>
    scrape()
  
  noticias_seccion_links <- noticias_seccion |>
    html_elements(".main-card__title a") |>   # rediseño 2026
    html_attr("href") |>
    unique() |>
    str_subset("/page/", negate = TRUE)
  noticias_seccion_links <- noticias_seccion_links[!is.na(noticias_seccion_links)]
  
  noticias_links <- tibble("enlace" = noticias_seccion_links,
                           "origen" = enlace_seccion)
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})
# 
# # resultados_links
# # 
# # resultados_links |> readr::write_rds("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/cnnchile/links_extendidos.rds")
#resultados_links <-  readr::read_rds("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/cnnchile/links_extendidos.rds")


#loop ----
resultados_cnnchile <- map(resultados_links$enlace, \(enlace) {
  #enlace <- resultados_links$enlace[6]
  # enlace <- .x

  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)

  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)

  tryCatch({
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  # rediseño 2026
  noticia_titulo <- noticia |>
    html_elements("h1") |>
    html_text2() |>
    head(1)

  noticia_fecha <- noticia |>
    html_elements("time") |>
    html_attr("datetime") |>
    head(1) |>
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")

  noticia_bajada <- noticia |>
    html_elements("meta[name='description']") |>
    html_attr("content") |>
    head(1)

  #texto
  noticia_texto <- noticia |>
    html_elements("p") |>
    html_text2() |>
    paste(collapse = "\n")
  
  noticia_url <- enlace
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_texto,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "cnnchile",
                        "url" = noticia_url)
  
  return(noticia_tabla)
  }, error = function(e) {
    message("Error en scraping cnnchile: ", e)
    return(NULL)
  })
})

# guardar ----
guardar_noticias_en_postgres(resultados_cnnchile, con)
DBI::dbDisconnect(con)

message(glue("listo cron cnnchile {lubridate::now()}"))
