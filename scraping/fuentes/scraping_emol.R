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
enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", year(today()), "/index.html"); historico = ""
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2024, "/index.html"); historico = "_h"

# para obtener años anteriores
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2023, "/index.html"); historico = "_h_2023f"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2022, "/index.html"); historico = "_h_2022c"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2021, "/index.html"); historico = "_h_2021c"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2020, "/index.html"); historico = "_h_2020e"
# enlaces_años <- paste0("https://www.emol.com/sitemap/noticias/", 2019, "/index.html"); historico = "_h_2019h"


chrome <- iniciar_chrome()
# 
# # enlaces meses ----
# enlaces_meses <- map(enlaces_años, \(enlace_año) {
#   # enlace_año <- enlaces_años[1]
#   
#   tryCatch({
#     
#     inicio <- Sys.time()
#     chrome$Page$navigate(enlace_año) # navegar a la página
#     # chrome$Page$loadEventFired() # esperar a que cargue
#     body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
#     noticias_seccion <- read_html(body)
#     final <- Sys.time()
#     Sys.sleep((final-inicio)*3) # espera
#     
#     #revisar url válida
#     if (is.null(revisar_url(enlace_año))) return(NULL)
#     
#     # noticias_seccion <- bow(enlace_año) |>
#     #   scrape()
#     
#     noticias_seccion_links <- noticias_seccion |>
#       html_elements(".articlesMonth") |>
#       html_elements("a") |>
#       html_attr("href")
#     
#     message(glue("Se obtuvieron {length(noticias_seccion_links)} noticias en {enlace_año}"))
#     return(noticias_seccion_links)
#   },
#   error = function(e) {
#     message("Error en scraping emol: ", e)
#     return(NULL)
#   })
# }) |> unlist()
# 
# message(glue("Se obtuvieron {length(unlist(enlaces_meses))} categorías de meses"))
# 
# # enlaces_meses_parcial <- enlaces_meses
# enlaces_meses_parcial <- enlaces_meses[length(enlaces_meses):(length(enlaces_meses)-1)] #aprox 200 ultimas noticias
# 
# # enlaces noticias ----
# enlaces_noticias <- map(enlaces_meses_parcial, \(enlace_mes) {
#   # enlace_mes <- enlaces_meses[1]
#   tryCatch({
#     
#     año <- str_extract(enlace_mes, "\\d{4}")
#     enlace <- glue("https://www.emol.com/sitemap/noticias/{año}/{enlace_mes}")
#     
#     #revisar url válida
#     if (is.null(revisar_url(enlace))) return(NULL)
#     
#     noticias_seccion <- session(enlace) |>
#       read_html()
#     
#     noticias_seccion_links <- noticias_seccion |>
#       html_elements("#mainContent") |>
#       html_elements("a") |>
#       html_attr("href") |> 
#       str_subset("\\/noticias") |> 
#       str_subset("Deportes|Autos", negate = T)
#     
#     message(glue("Se obtuvieron {length(noticias_seccion_links)} noticias en {enlace}"))
#     return(noticias_seccion_links)
#   },
#   error = function(e) {
#     message("Error en scraping emol: ", e)
#     return(NULL)
#   })
# }) |> 
#   unlist()
# 
# 
# message("")
# message(glue("En total, a partir de {length(enlaces_meses_parcial)} enlaces se obtuvieron {length(unlist(enlaces_noticias))} noticias"))
# 
# # sólo recientes
# enlaces_noticias_recientes <- tibble(enlace = enlaces_noticias) |> 
#   mutate(fecha = str_extract(enlace, "\\d{4}\\/\\d{2}\\/\\d{2}"),
#          fecha = as_date(fecha)) |> 
#   filter(fecha > today()-days(5)) |> 
#   pull(enlace)
# 
# # enlaces_noticias_recientes <- enlaces_noticias
# 


enlace <- "https://www.emol.com/todas/"

inicio <- Sys.time()
chrome_navegar(chrome, enlace)
body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
noticia <- read_html(body)
final <- Sys.time()
Sys.sleep((final-inicio)*3) # espera

enlaces_0 <- noticia |> 
  html_elements("#ContenedorLinkNoticia") |>
  html_elements(".cont_bus_txt_detall_2") |>
  html_elements("a") |> 
  html_attr("href")


enlace <- "https://www.emol.com/nacional/"

inicio <- Sys.time()
chrome_navegar(chrome, enlace)
body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
noticia <- read_html(body)
final <- Sys.time()
Sys.sleep((final-inicio)*3) # espera

enlaces_a <- noticia |> 
  html_elements(".cont_736_e_2015") |>
  html_elements("h1, h3") |>
  html_elements("a") |> 
  html_attr("href")

enlaces_b <- noticia |> 
  html_elements(".noticias_caja_texto") |>
  html_elements("a") |> 
  html_attr("href")

enlaces_c <- noticia |> 
  html_elements(".caja_contenedor_masvistos_modulo") |>
  html_elements("a") |> 
  html_attr("href")


enlaces_noticias_recientes <- c(enlaces_0,
             enlaces_a,
  enlaces_b,
  enlaces_c) |> 
  unique() |> 
  str_subset("#comentarios", negate = TRUE) |> 
  str_remove("//") |> 
  str_replace("^/noticias/", "www.emol.com/noticias/") |> 
  str_replace("^www", "https://www")

  
# scraping ----
# plan(multisession, workers = 4)


resultados_emol <- map(enlaces_noticias_recientes, \(enlace) {
  # enlace <- sample(enlaces_noticias_recientes, 1)
  # if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
    message(enlace)
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    # noticia <- enlace |> bow() |> scrape()
    # noticia <- session(enlace) |> read_html()
    
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    titulo <- noticia |> html_elements("#cuDetalle_cuTitular_tituloNoticia") |> html_text()
    
    bajada <- noticia |> html_elements("#cuDetalle_cuTitular_bajadaNoticia") |> html_text()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> 
      html_elements("#texto_noticia") |> 
      # html_elements("#cuDetalle_cuTexto_textoNoticia") |>
      # html_elements(":not(script") |>
      # html_elements(xpath = '//*[@id="cuDetalle_cuTexto_textoNoticia"]') |> 
      html_elements(xpath = '//*[@id="cuDetalle_cuTexto_textoNoticia"]//text()') |>  #sólo los hijos de texto
      html_text2() |> 
      paste(collapse = "\n") |> 
      str_remove_all("\\\r") |> 
      # str_remove_all("RelacionadaDetalle\\(\\'\\d+\\'\\)") |> 
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
    if (nrow(noticia_data) == 0) {
      return(NULL)
    }
   
    return(noticia_data) 
  },
  error = function(e) {
    message("Error en scraping emol: ", e)
    return(NULL)}
  )
}) |> 
  list_rbind()

# resultados_emol |> 
#   slice_sample(n = 1) |> 
#   pull(cuerpo)

# guardar ----
guardar_noticias_en_postgres(resultados_emol, con)
DBI::dbDisconnect(con)

message(glue("listo cron emol {lubridate::now()}"))

invisible(chrome$close())
