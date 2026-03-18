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
enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 1:3)


# para descargar noticias anteriores
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 6:200); hist = "_h"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 201:500); hist = "_h_b"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 500:1000); hist = "_h_a"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 1001:2000); hist = "_h_b"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 2001:2500); hist = "_h_a" 
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 2501:3000); hist = "_h_b"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 3001:3500); hist = "_h_c"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 3501:4000); hist = "_h_d"

# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 240:370); hist = "_h_a"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 371:500); hist = "_h_b"
# enlaces <- paste0("https://www.eldinamo.cl/category/pais/page/", 200:239); hist = "_h_c"

# hasta 3500 es 2018


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      # html_elements(".titulares") |> 
      html_elements(".main-card__title") |>
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping eldinamo: ", e)
    return(NULL)}
  )
})


enlaces_eldinamo <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_eldinamo <- map_df(enlaces_eldinamo, \(enlace) {
  # enlace <- enlaces_eldinamo[3]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> 
      # html_elements(".principal") |> 
      html_elements(".the-article__title") |>
      # html_elements("h1") |> 
      html_text()
    
    bajada <- noticia |> 
      # html_elements(".principal") |> 
      # html_elements(".bajada") |> 
      html_elements(".the-article__excerpt") |>
      html_text()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> 
      # html_elements(".the-content") |> 
      html_elements(".the-article__body") |>
      html_elements("p") |> 
      html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "eldinamo",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping eldinamo: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_eldinamo, con)
DBI::dbDisconnect(con)

message(glue("listo cron eldinamo {lubridate::now()}"))
