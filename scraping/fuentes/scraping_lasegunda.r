library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# por ahora solo permite obtener títulos y palabras clave

# obtener enlaces ----
enlaces <- c("https://www.lasegunda.com/politica/",
             "https://www.lasegunda.com",
             "https://www.lasegunda.com/conversacion/",
             "https://www.lasegunda.com/topsecret/",
             "https://www.lasegunda.com/economia/")


resultados_enlaces <- purrr::map(enlaces, \(enlace) {
  inicio <- bow(enlace) |> 
    scrape()
  
  noticias_enlaces <- inicio |> 
    html_elements("h1") |> 
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  return(noticias_enlaces)
})


enlaces_lasegunda <- resultados_enlaces |>
  unlist() |>
  unique()

# scraping ----
resultados_lasegunda <- map_df(enlaces_lasegunda, \(enlace) {
  message("scraping ", enlace)
  
  enlace <- paste0("https://www.lasegunda.com", enlace)
  
  tryCatch({
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    noticia <- bow(enlace) |> 
      scrape()
    
    atributos <- noticia |> 
      html_elements("meta") |> 
      html_attr("name") |> 
      tolower()
    
    data <- noticia |> 
      html_elements("meta") |> 
      html_attr("content")
    
    datos_1 <- tibble(atributos,
                      data) |> 
      filter(atributos %in% c("title", "keywords")) |> 
      tidyr::pivot_wider(names_from = atributos, values_from = data)
    
    # tira basura, como la siguiente:
    basura <- "Internacional  noticias  noticias de chile  noticias chile  diario hoy  diario tarde  economía  política  topsecret  radar  x  gobierno  road show  conversaciones  opinion  columnas  lasegunda\\.com lasegunda Opinión Road Show Política Economía"
    basura_2 <- str_split(basura, "  | ") |> unlist() |> paste(collapse = "|")
    
    fecha = str_extract(enlace, "\\d{4}.\\d{2}.\\d{2}")
    
    datos_2 <- datos_1 |>
      select(titulo = 1, cuerpo = keywords) |>  
      mutate(cuerpo = str_replace_all(cuerpo, "\\,", " "),
             cuerpo = str_remove_all(cuerpo, basura_2) |> str_squish(),
             fecha = fecha,
             url = enlace,
             fecha_scraping = now(),
             fuente = "lasegunda")
    
    # datos_2 |> pull(cuerpo)
    
    return(datos_2)
  },
  error = function(error) {
    warning(glue("error en exante: error en respuesta"))
    return(NULL)
  }
  )
})


# guardar ----
guardar_noticias_en_postgres(resultados_lasegunda, con)
DBI::dbDisconnect(con)

message(glue("listo cron lasegunda {lubridate::now()}"))
