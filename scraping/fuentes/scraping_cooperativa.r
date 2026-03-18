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

# parece que se puede hacer histórico:
# https://www.cooperativa.cl/noticias/site/cache/search/pags/search172529266250549.html
# https://www.cooperativa.cl/cgi-bin/prontus_search.cgi?search_prontus=noticias&search_idx=noticias&search_tmp=search_cooperativa_2018.html&search_form=no&search_pag=2&search_resxpag=150&search_maxpags=10000&search_orden=cro&search_meta1=&search_meta2=&search_meta3=&search_seccion=&search_tema=&search_subtema=&search_fechaini=&search_fechafin=&search_texto=pais&search_modo=and&search_comodines=no&vista=

# enlace_noticias <- "http://www.cooperativa.cl/noticias/pais"
enlace_noticias <- "https://www.cooperativa.cl"
enlace_opinion <- "https://opinion.cooperativa.cl"



## enlaces noticias ----
# library(httr)
# set_config(config(ssl_verifypeer = 0L))

# if (is.null(revisar_url(enlace_noticias))) return(NULL)

# noticias_html <- bow(enlace_noticias) |>
#   scrape()

# hay que entregarle el certificado manualmente
noticias_html <- RCurl::getURL(enlace_noticias, 
                              cainfo = "scraping/fuentes/cooperativa-cl.pem") |> 
  read_html()

# noticias_html <- session(enlace_noticias) |> read_html()

noticias_sesion <- noticias_html |>
  html_elements(".contenedor-pagina") |>
  html_elements("#modulo-noticia-7") |> 
  html_elements("a") |>
  html_attr("href")

noticias_enlaces <- noticias_sesion |> 
  tibble() |> 
  filter(nchar(noticias_sesion) > 40,
         !str_detect(noticias_sesion, "send\\?text"),
         !str_detect(noticias_sesion, "twitter.com"),
         !str_detect(noticias_sesion, "site\\/tax")) |> 
  distinct() |> 
  pull(noticias_sesion)

message(glue("Se obtuvieron {nrow(noticias_enlaces)} noticias"))


## enlaces opinión ----
# opinion_html <- bow(enlace_opinion) |> 
#   scrape()
opinion_html <- RCurl::getURL(enlace_opinion, 
                              cainfo = "scraping/fuentes/cooperativa-cl.pem") |> 
  read_html()

opinion_sesion <- opinion_html |> 
  html_elements(".contenedor-opinion") |> 
  html_elements("a") |> 
  html_attr("href")

opinion_enlaces <- opinion_sesion |> 
  str_subset("opinion/site|twitter", negate = T) |> 
  str_subset("/opinion/") |> 
  tibble() |> 
  rename(url = 1) |> 
  filter(nchar(url) > 90) |> 
  pull()


#unir enlaces
noticias_cooperativa <- c(paste0("https://www.cooperativa.cl", 
                                 noticias_enlaces), opinion_enlaces) |>
  unique()


#scraping ----
resultados_cooperativa <- map_df(noticias_cooperativa, \(enlace) {
  # enlace <- noticias_cooperativa[1]
  tryCatch({
    
    if (is.null(revisar_url(enlace))) return(NULL)   
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    # #obtener respuesta
    # respuesta <- bow(url = enlace, force = TRUE, verbose = TRUE) |>
    #   tryCatch(error = function(error) {
    #     warning(glue("error en {enlace}: error en respuesta"))
    #     warning(error)
    #     return(NULL)
    #   })
    
    noticia <- RCurl::getURL(enlace, 
                             .mapUnicode = F,
                                  cainfo = "scraping/fuentes/cooperativa-cl.pem") |> 
      read_html()
    
    # noticia <- scrape(respuesta)
    
    if (is.null(noticia)) {
      warning(glue("error en {enlace}: scrape null"))
      return(NULL)
    }
    
    x_titulo <- noticia |> 
      html_elements("#despliegue-titular") |> 
      html_elements(".titular-audio") |> 
      html_text()
    
    if (length(x_titulo) == 0) {
      x_titulo <- noticia |> 
        html_elements("h1") |> 
        html_text()
    }
    
    # x_fecha <- noticia |> 
    #   html_elements("#despliegue-titular") |> 
    #   html_elements(".fecha-publicacion") |> 
    #   html_elements("time") |> 
    #   html_text()
    
    x_fecha <- enlace |> str_extract("\\d{4}-\\d{2}-\\d{2}")
    
    x_bajada <- noticia |> 
      html_elements("#despliegue-titular") |> 
      html_elements(".texto-bajada") |> 
      html_text()
    
    x_texto <- noticia |> 
      html_elements(".texto-bajada") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
    
    if (length(x_texto) == 0) {
      x_texto <- noticia |> 
        html_elements(".columna") |> 
        html_elements("p") |> 
        html_text() |> 
        paste(collapse = "\n")
    }
    
    resultado <- tibble("titulo" = x_titulo[1],
                        "fecha" = x_fecha[1],
                        "fecha_scraping" = lubridate::now(),
                        "bajada" = x_bajada[1],
                        "cuerpo" = x_texto[1],
                        "fuente" = "cooperativa",
                        "url" = enlace)
    
    Sys.sleep(3)
    return(resultado)
    
  }, 
  error = function(error) {
    warning(glue("error en {enlace}: error en respuesta"))
    warning(error)
    return(NULL)
  })
})

#guardar ----
guardar_noticias_en_postgres(resultados_cooperativa, con)
DBI::dbDisconnect(con)

message(glue("listo cron cooperativa {lubridate::now()}"))
