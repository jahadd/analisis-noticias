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

# # obtener enlaces de página de inicio
# inicio <- bow("https://www.ex-ante.cl") |> 
#   scrape()
# 
# enlaces_inicio <- inicio |> 
#   html_elements(".box-noticia") |> 
#   html_elements("a") |> 
#   html_attr("href")

enlaces <- c("https://www.ex-ante.cl/category/nacional/",
             "https://www.ex-ante.cl/category/economia/",
             "https://www.ex-ante.cl/category/entrevista/",
             "https://www.ex-ante.cl/category/politica/",
             "https://www.ex-ante.cl/category/ciencia-tecnologia/")
             
# 
# # enlaces <- "https://www.ex-ante.cl/category/nacional/2024/03/10/"
# 
# # para obtener fechas anteriores
# fecha_inicio <- today() %m-% days(30); hist = ""
# fecha_fin <- today()
# 
# 
# fechas <- seq.Date(from = fecha_inicio, to = fecha_fin,
#                    by = "days") |>
#   format("%Y/%m/%d")
# 
# # enlaces <- paste0("http://www.ex-ante.cl/category/nacional/", fechas, "/"); hist = "_a"
# # enlaces <- paste0("http://www.ex-ante.cl/category/politica/", fechas, "/"); hist = "_b"
# # enlaces <- paste0("http://www.ex-ante.cl/category/economia/", fechas, "/"); hist = "_c"


chrome <- iniciar_chrome()
# chrome$close()
             
## enlaces noticias ----
resultados_enlaces <- purrr::map(enlaces, \(enlace) {
  # if (is.null(revisar_url(enlace))) return(NULL)  
  # enlace <- enlaces[1]
    message("scraping ", enlace)
  # sesion <- bow(enlace) |> scrape()
    # sesion <- session(enlace) |> read_html()
    
  tryCatch({
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sitio <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
      
  noticias_enlaces <- sitio |> 
    html_elements(".titulo_grande") |> 
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  
  return(noticias_enlaces)
  },
  error = function(error) {
    warning(glue("error en exante: error en respuesta"))
    return(NULL)
  })
})

enlaces_exante <- resultados_enlaces |>
  unlist() |>
  # c(enlaces_inicio) |> 
  unique()


#scraping ----
resultados_exante <- map_df(enlaces_exante, \(enlace) {
  # enlace <- enlaces_exante[2]
  
  # if (is.null(revisar_url(enlace))) return(NULL)
  message("scraping ", enlace)
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  
  #obtener respuesta
  tryCatch({
    
    # noticia <- bow(enlace) |> scrape()
    # noticia <- read_html(enlace)
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    x_titulo <- noticia |> 
      html_elements("h1") |> 
      html_text2()
    
    x_fecha_texto <- noticia |> 
      html_elements(".fecha") |> 
      html_text2()
    
    mes <- x_fecha_texto |> str_extract("\\w+") |> 
      recode("Enero" = "1", "Febrero" = "2", "Marzo" = "3",
             "Abril" = "4", "Mayo" = "5", "Junio" = "6",
             "Julio" = "7", "Agosto" = "8", "Septiembre" = "9",
             "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")
    
    dia_año <-  x_fecha_texto |> 
      str_extract("\\d+, \\d{4}") |> 
      str_remove(",")
    
    año <- dia_año |> str_extract("\\d{4}$")
    
    dia <- dia_año |> str_extract("^\\d+")
    
    x_fecha <- paste(año, mes, dia)
    
    x_bajada <- noticia |> 
      html_elements(".contenido-noticia") |> 
      html_elements("h4") |> 
      html_text2()
    
    x_texto <- noticia |> 
      html_elements(".contenido-noticia") |> 
      html_elements("p") |> 
      html_text2() |> 
      paste(collapse = "\n")
    
    resultado <- tibble("titulo" = x_titulo[1],
                        "fecha" = x_fecha[1],
                        "fecha_original" = x_fecha_texto[1],
                        "fecha_scraping" = lubridate::now(),
                        "bajada" = x_bajada[1],
                        "cuerpo" = x_texto[1],
                        "fuente" = "exante",
                        "url" = enlace)
    
    return(resultado)
    
  },
  error = function(error) {
    warning(glue("error en exante: error en respuesta"))
    return(NULL)
  }
  )
})

#guardar ----
guardar_noticias_en_postgres(resultados_exante, con)
DBI::dbDisconnect(con)

message(glue("listo cron exante {lubridate::now()}"))

invisible(chrome$close())
