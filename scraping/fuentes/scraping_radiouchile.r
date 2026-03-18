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
enlaces <- paste0("https://radio.uchile.cl/noticias/page/", 1:3)

# para descargar noticias anteriores
# enlaces <- paste0("https://radio.uchile.cl/noticias/page/", 6:200); hist = "_h"
# enlaces <- paste0("https://radio.uchile.cl/noticias/page/", 201:500); hist = "_h_b"

# enlaces <- paste0("https://radio.uchile.cl/noticias/page/", 1:50); hist = "_h_a"
# enlaces <- paste0("https://radio.uchile.cl/noticias/page/", 51:100); hist = "_h_b"


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[2]
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- bow(enlace) |> scrape()
  
  noticias_sesion <- sesion |> 
    html_elements(".details") |>
    html_elements(".post-title") |> 
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  
  return(noticias_sesion)
})

enlaces_uchile <- resultados_enlaces |> 
  unlist() |> 
  unique()

# scraping ----
resultados_radiouchile <- map_df(enlaces_uchile, \(enlace) {
  # enlace <- enlaces_uchile[10]
  
  message(glue("scraping {enlace}"))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    
    #elementos
    # x_titulo <- noticia |> html_elements(".article-header") |> html_elements("h1") |> html_text2()
    x_titulo <- noticia |> html_elements(".post-header") |> html_elements("h1") |> html_text2()
    
    # x_bajada <- noticia |> html_elements(".article-header") |> html_elements(".bajada") |> html_text2()
    x_bajada <- noticia |> html_elements(".post-header") |> html_elements(".d-sm-block") |> html_text2()
    
    x_fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    # x_cuerpo <- noticia |> html_elements(".maintext") |> html_elements("p") |> html_text2() |> 
    #   paste(collapse = "\n")
    # 
    x_cuerpo <- noticia |> html_elements(".post-content") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "bajada" = x_bajada |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "radiouchile",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping radiouchile: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_radiouchile, con)
DBI::dbDisconnect(con)

message(glue("listo cron radiouchile {lubridate::now()}"))
