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
secciones <- paste0("https://www.elciudadano.com/chile/page/", 1:3)

# para descargar hacia atrás
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 4:30); hist = "_h"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 200:1000); hist = "_h"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 1001:1800); hist = "_h_a"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 1801:2500); hist = "_h_b"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 2501:3200); hist = "_h_c"

#loop de enlaces
resultados_links <- map_df(secciones, \(enlace_seccion) {
  # enlace_seccion <- secciones[2]
  
  if (is.null(revisar_url(enlace_seccion))) return(NULL)
  
  noticias_seccion <- bow(enlace_seccion) |>
    scrape()
  
  noticias_seccion_links <- noticias_seccion |>
    # html_elements(".col-md-9") |>
    html_elements(".mb-3") |>
    html_elements("a") |>
    html_attr("href") |>
    unique()
  
  noticias_seccion_links_2 <- noticias_seccion_links[nchar(noticias_seccion_links) > 60]
  
  noticias_links <- tibble("enlace" = noticias_seccion_links_2,
                           "origen" = enlace_seccion)
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})


# resultados_links <- datos_prensa |> 
#   filter(fuente == "elciudadano") |> 
#   filter(str_detect(titulo, "googletag")) |> 
#   rename(enlace = url)

#loop ----
resultados_elciudadano <- map(resultados_links$enlace, \(enlace) {
  # enlace <- resultados_links$enlace[6]
  # enlace <- "https://www.elciudadano.com/actualidad/quienes-son-los-grandes-contaminadores-del-aire-publican-lista-de-87-plantas-industriales-sujetas-al-impuesto-verde/03/28/"
  # enlace <- "https://www.elciudadano.com/chile/flor-sakeo-la-musica-uruguaya-que-coquetea-con-la-poesia/06/07/"
  # enlace <- "https://www.elciudadano.com/actualidad/la-furia-del-libro-invierno-2025-cerro-este-domingo-una-edicion-historica/06/04/"
  # enlace <- "https://www.elciudadano.com/politica/minga-indigena-nacional-llego-a-importantes-acuerdos-con-el-gobierno/05/31/"
  # enlace <- "https://www.elciudadano.com/mundo/trump-y-musk-dos-egos-heridos-que-rompen-su-alianza-por-intereses-propios/06/05/"
  # enlace <- resultados_links$enlace[34]
  
  tryCatch({
    if (str_detect(enlace, "spotify")) return(NULL)
    
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL) 
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    #scraping
    noticia <- enlace |> bow() |> scrape()
    
    noticia_titulo_a <- noticia |>
      #html_elements(".order-md-2") |>
      html_elements(".my-3") |>
      html_text2() |> 
      pluck(1)
    
    noticia_titulo_b <- noticia |>
      #html_elements(".order-md-2") |>
      html_elements(".mb-4") |>
      html_elements("h1") |>
      html_text2() |> 
      pluck(1)
    
    # si dice googletag, NA
    noticia_titulo_a <- ifelse(str_detect(noticia_titulo_a, "googletag"), NA, noticia_titulo_a)
    
    # si es nulo, NA
    noticia_titulo_a <- ifelse(is.null(noticia_titulo_a), NA, noticia_titulo_a)
    
    noticia_titulo <- ifelse(is.na(noticia_titulo_a),
                             noticia_titulo_b,
                             noticia_titulo_a)
    
    noticia_fecha <- noticia |>
      # html_elements(".time-now-") |>
      # html_attr("data-date") |> 
      html_elements("time") |>
      html_attr("datetime") |> 
      str_extract("\\d{4}-\\d{2}-\\d{2}")
    
    noticia_bajada <- noticia |>
      # html_elements(".order-md-2") |>
      # html_elements(".the-excerpt-") |>
      html_elements(".article-title-excerpt") |> 
      html_text2()
    
    #texto
    noticia_texto <- noticia |>
      # html_elements(".pt-3-") |>
      html_elements(".the-content") |> 
      html_elements("p") |>
      html_text2() |>
      paste(collapse = "\n")
    
    # noticia_tabla <- list("titulo" = noticia_titulo[1],
    #                         "bajada" = noticia_bajada[1],
    #                         "cuerpo" = noticia_texto[1],
    #                         "fecha" = noticia_fecha[1],
    #                         "fecha_scraping" = lubridate::today(),
    #                         "fuente" = "elciudadano",
    #                         "url" = enlace)
    noticia_tabla <- tibble("titulo" = noticia_titulo[1],
                            "bajada" = noticia_bajada[1],
                            "cuerpo" = noticia_texto[1],
                            "fecha" = noticia_fecha[1],
                            "fecha_scraping" = lubridate::today(),
                            "fuente" = "elciudadano",
                            "url" = enlace)
    
    return(noticia_tabla)
  }, error = function(e) {
    warning(e)
    return(NULL)
  })
}) |> 
  list_rbind()

# resultados_elciudadano |> list_rbind() |> print(n=Inf)

# resultados_elciudadano |> list_rbind() |> filter(is.na(titulo)) |> 
#   select(url)

# guardar ----
guardar_noticias_en_postgres(resultados_elciudadano, con)
DBI::dbDisconnect(con)

message(glue("listo cron elciudadano {lubridate::now()}"))
