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
enlaces <- c(
  paste0("https://www.ciperchile.cl/category/actualidad/page/", 1:2),
  paste0("https://www.ciperchile.cl/category/investigacion/page/", 1:2)
)

# para descargar noticias anteriores
# enlaces <- c(
#   paste0("https://www.ciperchile.cl/category/actualidad/page/", 4:75),
#   paste0("https://www.ciperchile.cl/category/investigacion/page/", 4:67)
# ); hist = "_h"


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[2]
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
  sesion <- bow(enlace) |> scrape()
  
  noticias_sesion <- sesion |> 
    html_elements(".row") |> 
    # html_elements(".article__text") |>
    html_elements("a") |> 
    html_attr("href") |> 
    unique() |> 
    str_subset("ciperchile.cl/\\d+")
  
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  
  return(noticias_sesion)
  },
  error = function(e) {
    message("Error en scraping ciper: ", e)
    return(NULL)}
  )
})

enlaces_ciper <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_ciper <- map_df(enlaces_ciper, \(enlace) {
  # enlace <- enlaces_ciper[3]
  
  message(glue("scraping {enlace}"))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    
    #elementos
    x_titulo <- noticia |> html_elements(".portadilla") |> html_elements("h1") |> html_text2()
    
    x_bajada <- noticia |> html_elements(".portadilla") |> html_elements(".my-3") |> html_text2()
    
    x_fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    # x_cuerpo <- noticia |> html_elements(".texto-nota") |> html_text2() |> 
    #   paste(collapse = "\n")
    
    x_cuerpo <- noticia |> html_elements(".col-lg-9") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "bajada" = x_bajada |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "ciper",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping ciper: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_ciper, con)
DBI::dbDisconnect(con)

message(glue("listo cron ciper {lubridate::now()}"))
