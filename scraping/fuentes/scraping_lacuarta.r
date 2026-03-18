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

enlaces <- paste0("https://www.lacuarta.com/cronica/", 1:3)

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
    
    noticias <- sesion |> 
      html_elements(".article-title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    noticias <- paste0("https://www.lacuarta.com", noticias)
    
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
    
    #elementos
    titulo <- noticia |> html_elements(".story-title") |> html_elements("h1") |> html_text2()
    
    bajada <- noticia |> html_elements(".story-subtitle") |> html_elements(".h3") |> html_text2()
    
    fecha_texto <- noticia |> html_elements(".story-subtitle") |> html_elements(".date") |> html_text2()
    
    mes <- fecha_texto |> str_extract("(?<=(de ))\\w+") |> 
      recode("enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")
    
    año <- fecha_texto |> str_extract("\\d{4}$")
    
    dia <- fecha_texto |> str_extract("\\d+")
    
    fecha <- paste(año, mes, dia)
    
    cuerpo <- noticia |> html_elements(".body") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    
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
