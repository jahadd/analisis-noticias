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
n_pags <- n_paginas_fuente("meganoticias", con)
enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", seq_len(n_pags))

# para descargar noticias anteriores
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 5:200); hist = "_h_a"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 201:400); hist = "_h_b"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 401:800); hist = "_h_c"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 801:1000); hist = "_h_d"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1001:1200); hist = "_h_e"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1201:1400); hist = "_h_f"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1401:1600); hist = "_h_g"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1601:1800); hist = "_h_h"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1801:2000); hist = "_h_i"


resultados_enlaces <- purrr::map_df(enlaces, \(enlace) {
  #enlace <- enlaces[1]
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- bow(enlace) |> scrape()
  # sesion <- session(enlace) |> read_html(); Sys.sleep(.3)
  
  noticias_sesion <- sesion |> 
    html_elements("figcaption") |> 
    html_elements("a") |> 
    html_attr("href")
  
  noticias_link <- noticias_sesion |> 
    tibble() |> 
    filter(nchar(noticias_sesion) > 40) |> 
    distinct()
  
  message(glue("Se obtuvieron {nrow(noticias_link)} noticias en {enlace}"))
  
  return(noticias_link)
})

enlaces_mega <- resultados_enlaces |>
  pull(noticias_sesion) |>
  unique()


#loop ----
resultados_meganoticias <- map_df(enlaces_mega, \(enlace) {
  # enlace <- enlaces_mega[3]
  
  message(glue("scraping {enlace}"))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    # noticia <- enlace |> session() |> read_html(); Sys.sleep(.3)
    
    #elementos
    x_titulo <- noticia |> html_elements("h1") |> html_text()
    
    #fecha desde codigo
    x_fecha <- noticia |> 
      html_elements(".fechaHora") |> 
      html_elements("time") |> 
      html_attr("datetime") |> 
      stringr::str_trim() |> 
      stringr::str_extract("^\\d{4}-\\d+-\\d+") |> 
      as.character()
    
    x_cuerpo <- noticia |> html_elements(".contenido-nota") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "meganoticias",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    Sys.sleep(1)
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping meganoticias: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_meganoticias, con)
DBI::dbDisconnect(con)

message(glue("listo cron meganoticias {lubridate::now()}"))
