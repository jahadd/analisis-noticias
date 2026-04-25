library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(future)
library(furrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# en este script se obtienen primero los "enlaces años", que es un sitio que contiene todas las noticias de un año, separadas en grupos por mes,
# luego se accede a estos "enlaces meses", que son más de uno por mes, pero no a todos, sino a un subgrupo de ellos,
# y se obtienen las noticias de cada grupo en "enlaces noticias". Luego se realiza el scraping, ya sea normalmente, 
# o en multiples procesos, uno por cada grupo, donde cada grupo va a descargar aprox 110 noticias y las va a guardar individualmente con un id aleatorio

# enlaces años ----
enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2024, "/index.html"); historico = "_h"

# para obtener años anteriores
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2023, "/index.html"); historico = "_h_2023f"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2022, "/index.html"); historico = "_h_2022c"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2021, "/index.html"); historico = "_h_2021c"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2020, "/index.html"); historico = "_h_2020e"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2019, "/index.html"); historico = "_h_2019h"


# enlaces meses ----
enlaces_meses <- map(enlaces_años, \(enlace_año) {
  # enlace_año <- enlaces_años[1]
  
  tryCatch({
    #revisar url válida
    if (is.null(revisar_url(enlace_año))) return(NULL)
    
    noticias_seccion <- bow(enlace_año) |>
      scrape()
    
    noticias_seccion_links <- noticias_seccion |>
      html_elements(".articlesMonth") |>
      html_elements("a") |>
      html_attr("href")
    
    message(glue("Se obtuvieron {length(noticias_seccion_links)} noticias en {enlace_año}"))
    return(noticias_seccion_links)
  },
  error = function(e) {
    message("Error en scraping emol: ", e)
    return(NULL)
  })
}) |> unlist()

message(glue("Se obtuvieron {length(unlist(enlaces_meses))} categorías de meses"))


# limitar (*) ----
# filtrar los grupos que se van a scrapear, cada grupo tiene aprox 100 noticias, entonces filtrar 10 dan 1000 noticias
# el número después del año en los enlaces es el mes
# enlaces_meses_parcial <- enlaces_meses[1:25]
# enlaces_meses_parcial <- enlaces_meses[26:50]
# enlaces_meses_parcial <- enlaces_meses[51:75]
# enlaces_meses_parcial <- enlaces_meses[75:100]
# enlaces_meses_parcial <- enlaces_meses[101:125]
# enlaces_meses_parcial <- enlaces_meses[126:150]
# enlaces_meses_parcial <- enlaces_meses[151:175]
# enlaces_meses_parcial <- enlaces_meses[176:length(enlaces_meses)]

# enlaces_meses_parcial <- enlaces_meses[length(enlaces_meses):(length(enlaces_meses)-1)] #aprox 200 ultimas noticias
enlaces_meses_parcial <- enlaces_meses[(length(enlaces_meses)-2):length(enlaces_meses)] #aprox 200 ultimas noticias

# enlaces noticias ----
enlaces_noticias <- map(enlaces_meses_parcial, \(enlace_mes) {
  # enlace_mes <- enlaces_meses[1]
  tryCatch({
    
    año <- str_extract(enlace_mes, "\\d{4}")
    enlace <- glue("https://www.emol.com/sitemap/noticias/{año}/{enlace_mes}")
    
    #revisar url válida
    if (is.null(revisar_url(enlace))) return(NULL)
    
    noticias_seccion <- bow(enlace) |>
      scrape()
    
    noticias_seccion_links <- noticias_seccion |>
      html_elements("#mainContent") |>
      html_elements("a") |>
      html_attr("href") |> 
      str_subset("\\/noticias") |> 
      str_subset("Deportes|Autos", negate = T)
    
    message(glue("Se obtuvieron {length(noticias_seccion_links)} noticias en {enlace}"))
    return(noticias_seccion_links)
  },
  error = function(e) {
    message("Error en scraping emol: ", e)
    return(NULL)
  })
}) |> 
  unlist()


message("")
message(glue("En total, a partir de {length(enlaces_meses_parcial)} enlaces se obtuvieron {length(unlist(enlaces_noticias))} noticias"))


# scraping multi ----
# names(length(enlaces_noticias))
plan(multisession, workers = 4)

future_walk(enlaces_noticias, \(enlaces) {
  # enlaces <- enlaces_noticias[[1]]
  historico <- historico
  
  resultados_emol <- map(enlaces, \(enlace) {
    message(historico)
    # enlace <- enlaces[1]
    
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    tryCatch({
      noticia <- enlace |> bow() |> scrape()
      
      titulo <- noticia |> html_elements("#cuDetalle_cuTitular_tituloNoticia") |> html_text()
      
      bajada <- noticia |> html_elements("#cuDetalle_cuTitular_bajadaNoticia") |> html_text()
      
      fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
      
      cuerpo <- noticia |> html_elements("#texto_noticia") |> html_text2() |> paste(collapse = "\n") |> 
        str_remove_all("\\\r|\\\n") |> 
        str_remove_all("RelacionadaDetalle\\(\\'\\d+\\'\\)") |> 
        str_remove_all("\\¿Encontraste algún error\\? Avísanos") |> 
        str_trim()
      
      #unir
      noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                             "bajada" = bajada |> validar_elementos(),
                             "fecha" = fecha |> validar_elementos(),
                             "cuerpo" = cuerpo |>  validar_elementos(),
                             "fuente" = "emol",
                             "url" = enlace,
                             "fecha_scraping" = lubridate::now())
      if (nrow(noticia_data) > 0) {
        return(noticia_data)
      } else {
        return(NULL)
      }
    },
    error = function(e) {
      message("Error en scraping emol: ", e)
      return(NULL)}
    )
  })
  
  # guardar
  guardar_noticias_en_postgres(resultados_emol, con)

  message(glue("listo cron emol {lubridate::now()}"))
})


plan(multisession)

DBI::dbDisconnect(con)
