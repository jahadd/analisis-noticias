library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# https://redgol.cl/tema/actualidad?page=2

n_pags <- n_paginas_fuente("redgol", con)
categorias <- c("https://redgol.cl/tema/actualidad-nacional",
                paste0("https://redgol.cl/tema/actualidad-nacional?page=", seq(2L, n_pags + 1L)),
                "https://redgol.cl/tema/politica",
                paste0("https://redgol.cl/tema/politica?page=", seq(2L, n_pags + 1L)),
                "https://redgol.cl/tema/dato-util",
                paste0("https://redgol.cl/tema/dato-util?page=", seq(2L, n_pags + 1L)),
                "https://redgol.cl/tema/economia"
)

# # obtener noticias anteriores
# categorias <- paste0("https://redgol.cl/tema/actualidad-nacional?page=", 2:40); hist = "_a"
# categorias <- paste0("https://redgol.cl/tema/actualidad-nacional?page=", 41:67); hist = "_b"
# categorias <- paste0("https://redgol.cl/tema/politica?page=", 2:40); hist = "_c"
# categorias <- paste0("https://redgol.cl/tema/politica?page=", 41:67); hist = "_d"
# categorias <- paste0("https://redgol.cl/tema/economia?page=", 2:40); hist = "_e"


# obtener enlaces ----
resultados_enlaces <- map(categorias, \(enlace) {
  tryCatch({
    
    if (is.null(revisar_url(enlace))) return(NULL)
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    sitio <- bow(enlace) |> 
      scrape()
    
    enlaces <- sitio |> 
      html_elements(".card-title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    enlaces_2 <- paste0("https://redgol.cl", enlaces)
    
    message(glue("Se obtuvieron {length(enlaces_2)} noticias en {enlace}"))
    
    return(enlaces_2)
  },
  error = function(e) {
    message("error en scraping redgol: ", e)
    return(NULL)
  })
})

enlaces_redgol <- resultados_enlaces |> 
  unlist() |> 
  str_replace_all("https://redgol\\.clhttps://redgol\\.cl/", "https://redgol.cl/") |> 
  unique()


# scraping ----
resultados_redgol <- map(enlaces_redgol, \(enlace) {
  #enlace <- enlaces_redgol[39]
  
  tryCatch({
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)   
    
    #scraping
    noticia <- enlace |> bow() |> scrape()
    
    titulo <- noticia |> 
      html_elements(".article-title") |> 
      html_text()
    
    cuerpo <- noticia |> 
      html_elements(".article-body") |> 
      html_text2() |> 
      paste(collapse = "\n")
    
    fecha <- noticia |> 
      html_elements("time") |> 
      html_attr("datetime") 
    
    # fecha <- noticia |> 
    #   html_elements("time") |> 
    #   html_attr("datetime") |> 
    #   as_date() |> 
    #   min()
    
    noticia_tabla <- tibble("titulo" = titulo[1],
                            "bajada" = "",
                            "cuerpo" = cuerpo[1],
                            "fecha" = fecha[1],
                            "fecha_scraping" = lubridate::today(),
                            "fuente" = "redgol",
                            "url" = enlace)
    return(noticia_tabla)
  },
  error = function(e) {
    message("error en scraping redgol: ", e)
    return(NULL)
  })
}) |> 
  list_rbind()

# guardar ----
guardar_noticias_en_postgres(resultados_redgol, con)
DBI::dbDisconnect(con)

message(glue("listo cron redgol {lubridate::now()}"))
