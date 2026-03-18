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
enlaces <- c(paste0("https://www.latercera.com/etiqueta/nacional/page/", 1:5),
             "https://www.latercera.com/etiqueta/politica/",
             "https://www.latercera.com/etiqueta/economia/")

# para descargar noticias anteriores
# enlaces <- paste0("https://www.latercera.com/etiqueta/nacional/page/", 5:40); hist = "_h_a" #ok
# enlaces <- paste0("https://www.latercera.com/etiqueta/politica/page/", 2:40); hist = "_h_b"
# enlaces <- paste0("https://www.latercera.com/categoria/la-tercera-pm/page/", 2:40); hist = "_h_c"


# la tercera nacional solo llega a la página 592, y desde la página 530 aprox (desde febrero de 2022 hacia atrás), cada día empeiza a tener muchas menos noticias
             
library(chromote)
options(chromote.headless = "new")
chrome <- ChromoteSession$new()
chrome$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
# chrome$close()

resultados_enlaces <- purrr::map_df(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  message("scraping ", enlace)
  # if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
  # chromote
  inicio <- Sys.time()
  chrome$Page$navigate(enlace) # navegar a la página
  chrome$Page$loadEventFired() # esperar a que cargue
  body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
  sitio <- read_html(body)
  final <- Sys.time()
  Sys.sleep((final-inicio)*3) # espera
  
  # noticias_sesion <- session(enlace) |>
  noticias_sesion <- sitio |> 
    # read_html() |>
    # html_elements(".hl") |> 
    html_elements(".story-card__headline") |>
    html_elements("a") |> 
    html_attr("href")
  
  noticias_link <- noticias_sesion |> 
    tibble() |> 
    filter(nchar(noticias_sesion) > 40,
           !str_detect(noticias_sesion, "practico/noticia")) |> 
    distinct()
  
  message(glue("Se obtuvieron {nrow(noticias_link)} noticias en {enlace}"))
  
  return(noticias_link)
  
  },
  error = function(e) {
    message("Error en scraping latercera: ", e)
    return(NULL)}
  )
})


enlaces_lt <- resultados_enlaces |>
  pull(noticias_sesion) |>
  unique()

# loop ----
resultados_latercera <- map_df(enlaces_lt, \(enlace) {
  # enlace <- enlaces_lt[13]
  enlace <- ifelse(!str_detect(enlace, "latercera"),
                              glue("https://www.latercera.com{enlace}"),
                              enlace)
  
  message(glue("scraping {enlace}"))
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  #revisar si existe la página
  # if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
    # sitio <- enlace |> bow() |> scrape()
    # chromote
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sitio <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    #elementos
    # x_titulo <- sitio |> html_elements(".defecto") |> html_elements(".hl") |> html_text()
    
    x_titulo <- sitio |> html_elements(".article-head__title") |> html_text()
    
    #intentar si no tiene título
    if (length(x_titulo) == 0) { 
      x_titulo <- sitio |> html_elements(".titulares") |> html_elements(".hl") |> html_text()
      
      #salir si no tiene título
      if (length(x_titulo) == 0) { 
        message("sin titulo, next") 
        return(NULL) 
      }
    }
    
    # x_bajada <- sitio |> html_elements(".defecto") |> html_elements(".excerpt") |> html_text()
    x_bajada <- sitio |> html_elements(".article-head__subtitle") |> html_text()
    
    #intentar si no tiene texto
    if (length(x_bajada) == 0) { 
      x_bajada <- sitio |> html_elements(".excerpt") |> html_text()
    }
    
    #fecha textual
    # x_fecha_0 <- sitio |> html_elements(".d-flex-center") |> html_text() |> unique()
    # x_fecha_0 <- sitio |> html_elements(".article-body__byline__date") |> html_text() |> unique()
    # x_fecha_texto <- x_fecha_0 |> str_remove("^\\w+ \\w+ ") |> str_remove("Tiempo de lectura.*$") |> str_trim()
    
    #fecha desde codigo
    #<time datetime="Sat Jan 06 2018 13:09:38 GMT-0300 (-03)" title="6 ene 2018" class="p-left-10 "><small><b>6 ene 2018</b> 01:09 PM</small></time>
    x_fecha <- sitio |> 
      # html_elements(".d-flex") |> 
      html_elements(".article-body__byline__date") |> 
      # html_elements("time") |> 
      html_attr("datetime") |> 
      # stringr::str_remove("^\\w+") |> 
      # stringr::str_trim() |> 
      # stringr::str_extract("^\\w+ \\w+ \\w+") |>
      stringr::str_extract("^\\w+-\\w+-\\w{2}") |> 
      lubridate::ymd() |>
      as.character()
    
    # x_cuerpo <- sitio |> html_elements(".abody-col") |> html_elements(".paragraph") |> html_text() |> 
      # paste(collapse = "\n")
    x_cuerpo <- sitio |> 
      html_elements(".article-body__paragraph") |> 
      html_text() |>
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "bajada" = x_bajada |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           # "fecha_original" = x_fecha_0 |> validar_elementos(),
                           # "fecha_textual" = x_fecha_texto |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "latercera_pais",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping latercera: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_latercera, con)
DBI::dbDisconnect(con)

message(glue("listo cron latercera {lubridate::now()}"))

invisible(chrome$close())
