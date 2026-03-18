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

enlaces <- c("https://www.theclinic.cl/noticias/politica/",
             paste0("https://www.theclinic.cl/noticias/politica/page/", 2:3),
             "https://www.theclinic.cl/noticias/reportajes/",
             paste0("https://www.theclinic.cl/noticias/reportajes/page/", 2:3),
             "https://www.theclinic.cl/lo-ultimo/",
             paste0("https://www.theclinic.cl/lo-ultimo/page/", 2:3)
)

# enlaces <- c("https://www.theclinic.cl/noticias/politica/")

# para descargar noticias anteriores
# enlaces <- c(paste0("https://www.theclinic.cl/noticias/politica/page/", 4:28)); hist = "_h_a"
# enlaces <- c(paste0("https://www.theclinic.cl/noticias/reportajes/page/", 4:28)); hist = "_h_c"
# enlaces <- c(paste0("https://www.theclinic.cl/lo-ultimo/page/", 1:28)); hist = "_h_b"

# enlaces <- c(paste0("https://www.theclinic.cl/noticias/politica/page/", 29:60)); hist = "_h_d"
# enlaces <- c(paste0("https://www.theclinic.cl/noticias/reportajes/page/", 29:60)); hist = "_h_e"
# enlaces <- c(paste0("https://www.theclinic.cl/lo-ultimo/page/", 29:60)); hist = "_h_f"

# enlaces <- c(paste0("https://www.theclinic.cl/lo-ultimo/page/", 101:150)); hist = "_h_f"
# enlaces <- c(paste0("https://www.theclinic.cl/lo-ultimo/page/", 151:200)); hist = "_h_g"

# abrir sesión Chromote
library(chromote)
options(chromote.headless = "new")
chrome <- ChromoteSession$new()
chrome$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  # enlace <- enlaces[2]
  
  tryCatch({
    #revisar si existe la página
    # if (is.null(revisar_url(enlace))) return(NULL)
    
    # sesion <- bow(enlace) |> scrape()
    # sesion <- session(enlace) |> read_html(); Sys.sleep(.3)
    
    # chromote
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sesion <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    enlaces <- sesion |> 
      html_elements(".titulares") |> 
      html_elements("a") |> 
      html_attr("href")
    
    enlaces <- enlaces[nchar(enlaces) > 50]
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    return(enlaces)
  },
  error = function(e) {
    message("error en scraping theclinic: ", e)
    return(NULL)
  })
})


enlaces_theclinic <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_theclinic <- map_df(enlaces_theclinic, \(enlace) {
  # enlace <- enlaces_theclinic[4]
  # enlace <- "https://www.theclinic.cl/2024/04/03/daniel-jadue-formalizacion-frente-amplio-cde/"
  # enlace <- "https://www.theclinic.cl/2024/03/23/diario-ruta-piloto-motivaciones-desafios-profesiones-mas-estresantes/"
  # enlace <- "https://www.theclinic.cl/2024/03/31/jose-antonio-kast-a-tres-meses-del-plebiscito-lejos-de-la-atencion-mediatica-activo-en-terreno-y-en-busqueda-de-candidatos-municipales/"
  # enlace <- "https://www.theclinic.cl/2024/09/06/democracia-y-dos-caminos-para-recuperar-la-confianza/" 
  # enlace <- "https://www.theclinic.cl/2024/10/10/encuesta-panel-ciudadano-udd-68-no-esta-de-acuerdo-en-prestarle-sus-fondos-de-pension-al-estado/"
  # enlace <- "https://www.theclinic.cl/2024/10/11/el-lugar-de-la-otra-mirarse-para-ser-vista/"
  # enlace <- "https://www.theclinic.cl/2024/10/11/la-ruta-de-la-chorrillana-todas-las-variantes-del-clasico-plato-porteno-que-conquisto-a-todo-chile/"
  #revisar si existe la página
  # if (is.null(revisar_url(enlace))) return(NULL)
  
  message("scraping ", enlace)
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  tryCatch({
    
    # noticia <- bow(enlace) |> scrape()
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    #elementos
    titulo <- noticia |> html_elements(".titulares") |> html_elements("h1") |> html_text2()
    
    if (length(titulo) == 0) {
      titulo <- noticia |> html_elements(".nota-reportajes") |> html_elements("h1") |> html_text2()
    }
    if (length(titulo) == 0) {
      titulo <- noticia |> html_elements(".nota-opinion") |> html_elements("h1") |> html_text2()
    }
    if (length(titulo) == 0) {
      titulo <- noticia |> html_elements("h1") |> html_text2()
    }
    
    bajada <- noticia |> html_elements(".bajada") |> html_text2()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    if (is.na(fecha)) {
      fecha <- noticia |> html_elements(".fecha") |> html_text2()
      
      fechas <- fecha |> str_split(pattern = " ") |> unlist()

      mes <- fechas[3] |> 
        tolower() |> 
        recode("enero" = "1",
               "febrero" = "2",
               "marzo" = "3",
               "abril" = "4",
               "mayo" = "5",
               "junio" = "6",
               "julio" = "7",
               "agosto" = "8",
               "septiembre" = "9",
               "octubre" = "10",
               "noviembre" = "11",
               "diciembre" = "12")
      
      fecha <- paste(fechas[5], mes, fechas[1], sep = "/")
    }
    
    cuerpo <- noticia |> html_elements(".the-content") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha" = fecha |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "theclinic",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping theclinic: ", e)
    return(NULL)}
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_theclinic, con)
DBI::dbDisconnect(con)

message(glue("listo cron theclinic {lubridate::now()}"))

chrome$close()