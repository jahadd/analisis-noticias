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
enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 1:3)

# para descargar noticias anteriores
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 4:100); hist = "_h_a"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 101:200); hist = "_h_b"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 201:300); hist = "_h_c"
# hasta 3587

# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      html_elements(".entry-title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping lanacion: ", e)
    return(NULL)}
  )
})


enlaces_lanacion <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_lanacion <- map_df(enlaces_lanacion, \(enlace) {
  # enlace <- enlaces_lanacion[3]
  # enlace <- "https://www.lanacion.cl/ministra-arredondo-debera-enfrentar-demanda-por-violacion-de-derechos-humanos-por-polemica-en-bafona/"
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".tdb-title-text") |> html_text2()
    
    bajada <- noticia |> html_elements(".wpb_wrapper") |> html_elements(".extracto") |> html_text2()
    
    fecha_texto <- noticia |> html_elements(".entry-date") |> html_text2()
    
    
    mes <- fecha_texto[1] |> str_extract("\\w+") |> 
      recode("Enero" = "1", "Febrero" = "2", "Marzo" = "3",
             "Abril" = "4", "Mayo" = "5", "Junio" = "6",
             "Julio" = "7", "Agosto" = "8", "Septiembre" = "9",
             "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")
    
    dia_año <- fecha_texto[1] |> 
      str_extract("\\d+, \\d{4}") |> 
      str_remove(",")
    
    año <- dia_año |> str_extract("\\d{4}$")
    
    dia <- dia_año |> str_extract("^\\d+")
    
    fecha <- paste(año, mes, dia)
    
    cuerpo <- noticia |> html_elements(".tdb-block-inner") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "lanacion",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping lanacion: ", e)
    return(NULL)}
  )
})


# guardar ----
guardar_noticias_en_postgres(resultados_lanacion, con)
DBI::dbDisconnect(con)

message(glue("listo cron lanacion {lubridate::now()}"))
