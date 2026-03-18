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

enlaces <- c("https://www.laizquierdadiario.cl/Politica-Chile",
             "https://www.laizquierdadiario.cl/Seccion-Economia",
             "https://www.laizquierdadiario.cl/Seccion-Sociedad"
             # "https://www.laizquierdadiario.cl/Mundo-Obrero-Chile",
             # "https://www.laizquierdadiario.cl/Seccion-Opinion"
)

# históricos
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24, 24*40, 24), "#pagination_articulos"); hist = "_aa"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*40, 24*80, 24), "#pagination_articulos"); hist = "_ab"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*80, 24*120, 24), "#pagination_articulos"); hist = "_ac"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*120, 24*160, 24), "#pagination_articulos"); hist = "_ad"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*160, 24*200, 24), "#pagination_articulos"); hist = "_ae"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*200, 24*240, 24), "#pagination_articulos"); hist = "_af"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*240, 24*280, 24), "#pagination_articulos"); hist = "_ag"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*280, 24*320, 24), "#pagination_articulos"); hist = "_ah"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*320, 24*360, 24), "#pagination_articulos"); hist = "_ai"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*360, 24*400, 24), "#pagination_articulos"); hist = "_aj"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Politica-Chile?debut_articulos=", seq(24*400, 24*440, 24), "#pagination_articulos"); hist = "_ak"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Seccion-Economia?debut_articulos=", seq(24, 24*20, 24), "#pagination_articulos"); hist = "_ba"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Seccion-Economia?debut_articulos=", seq(24*20, 24*40, 24), "#pagination_articulos"); hist = "_bb"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Seccion-Sociedad?debut_articulos=", seq(24, 24*20, 24), "#pagination_articulos"); hist = "_ca"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Seccion-Sociedad?debut_articulos=", seq(24*20, 24*40, 24), "#pagination_articulos"); hist = "_cb"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Mundo-Obrero-Chile?debut_articulos=", seq(24, 24*20, 24), "#pagination_articulos"); hist = "_d"
# enlaces <- paste0("https://www.laizquierdadiario.cl/Seccion-Opinion?debut_articulos=", seq(24, 24*20, 24), "#pagination_articulos"); hist = "_e"

library(chromote)
options(chromote.headless = "new")
chrome <- ChromoteSession$new()
chrome$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
# chrome$close()


## enlaces noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[3]
  
  # if (is.null(revisar_url(enlace))) return(NULL)
  
  # sesion <- session(enlace) |> read_html()
    
    message("scraping ", enlace)
  
  tryCatch({
    
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sitio <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
  
  noticias_enlaces <- sitio |> 
    html_elements("h3") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    unique()
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  
  return(noticias_enlaces)
  },
  error = function(error) {
    warning(glue("error en izquierdadiario: error en respuesta"))
    return(NULL)
  })
})


enlaces_izquierdadiario <- resultados_enlaces |>
  unlist() |>
  unique()


# scraping ----
resultados_izquierdadiario <- map_df(enlaces_izquierdadiario, \(enlace) {
  # enlace <- enlaces_izquierdadiario[1]
  
  enlace <- paste0("https://www.laizquierdadiario.cl/", enlace)
  
  # if (is.null(revisar_url(enlace))) return(NULL)
  message("scraping ", enlace)
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  #obtener respuesta
  tryCatch({
    
    # noticia <- read_html(enlace)
    # Sys.sleep(1)
    
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    titulo <- noticia |> 
      html_elements("h1") |> 
      html_text2()
    
    bajada <- noticia |> 
      html_elements(".header-cuerpo-articulo") |> 
      html_text2()
    
    fecha_texto <- noticia |> 
      html_elements(".fecha-articulo") |> 
      html_text2()
    
    mes <- fecha_texto |> 
      str_extract("de \\w+|de \\w+ de") |> 
      str_remove_all("de") |> 
      str_squish() |> 
      recode("enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")
    
    dia <-  fecha_texto |> 
      str_extract("\\d+ de") |> 
      str_remove_all(" de")
    
    año <- fecha_texto |> str_extract("\\d{4}")
    
    if (is.na(año)) año <- year(today())
    
    fecha <- paste(año, mes, dia, sep = "/")
    
    texto <- noticia |> 
      html_elements(".articulo") |> 
      html_elements(".col-md-10") |> 
      # html_elements("p") |> 
      html_elements(xpath = '//*[@id="body"]/main/div[1]/article[2]/div[4]/div/p') |> # para no sacar el div que tiene temas
      html_text2() |> 
      paste(collapse = "\n")
    
    resultado <- tibble("titulo" = titulo[1],
                        "fecha" = fecha[1],
                        "fecha_original" = fecha_texto[1],
                        "fecha_scraping" = lubridate::now(),
                        "bajada" = bajada[1],
                        "cuerpo" = texto[1],
                        "fuente" = "izquierdadiario",
                        "url" = enlace)
    
    return(resultado)
    
  },
  error = function(error) {
    warning(glue("error en izquierdadiario: error en respuesta"))
    return(NULL)
  })
})


# guardar ----
guardar_noticias_en_postgres(resultados_izquierdadiario, con)
DBI::dbDisconnect(con)

message(glue("listo cron izquierdadiario {lubridate::now()}"))

invisible(chrome$close())