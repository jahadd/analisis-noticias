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

enlaces <- c("https://www.chilevision.cl/noticias",
  "https://www.chilevision.cl/noticias/casos-policiales",
  "https://www.chilevision.cl/noticias/cazanoticias",
  "https://www.chilevision.cl/noticias/economia")

# las páginas llegan hasta la 42
# enlaces <- paste0("https://www.chilevision.cl/noticias/nacional/p/", 2:40); hist = "_a"
# enlaces <- paste0("https://www.chilevision.cl/noticias/nacional/p/", 41:80); hist = "_b"
# enlaces <- paste0("https://www.chilevision.cl/noticias/casos-policiales/p/", 2:40); hist = "_c"
# enlaces <- paste0("https://www.chilevision.cl/noticias/casos-policiales/p/", 41:80); hist = "_d"
# enlaces <- paste0("https://www.chilevision.cl/noticias/reportajes-chv/p/", 2:40); hist = "_e"
# enlaces <- paste0("https://www.chilevision.cl/noticias/cazanoticias/p/", 2:40); hist = "_g"
# enlaces <- paste0("https://www.chilevision.cl/noticias/cazanoticias/p/", 41:80); hist = "_h"
# enlaces <- paste0("https://www.chilevision.cl/noticias/economia/p/", 2:40); hist = "_i"
# enlaces <- paste0("https://www.chilevision.cl/noticias/economia/p/", 41:80); hist = "_j"


## enlaces noticias ----
resultados_enlaces <- purrr::map(enlaces, \(enlace) {
  # enlace <- enlaces[2]
  
  if (is.null(revisar_url(enlace))) return(NULL)
  
  tryCatch({
  
  message(paste("scraping", enlace))
  
    sesion <- read_html(enlace)

    all_hrefs <- sesion |>
      html_elements("a") |>
      html_attr("href")

    # Filtrar URLs de artículos de chilevision (absolutas o relativas con /noticias/)
    noticias_enlaces <- all_hrefs[
      grepl("chilevision\\.cl/noticias/.+/.+", all_hrefs) |
      grepl("^/noticias/.+/.+", all_hrefs)
    ]
    noticias_enlaces_2 <- ifelse(
      grepl("^http", noticias_enlaces),
      noticias_enlaces,
      paste0("https://www.chilevision.cl", noticias_enlaces)
    ) |> unique()
    
    message(glue("Se obtuvieron {length(noticias_enlaces_2)} noticias en {enlace}"))
    
    return(noticias_enlaces_2)
    
  }, error = function(e) {
    warning(e)
    return(NULL)
  })
})


enlaces_chvnoticias <- resultados_enlaces |>
  unlist() |>
  unique()


#scraping ----
resultados_chvnoticias <- map_df(enlaces_chvnoticias, \(enlace) {
  # enlace <- enlaces_chvnoticias[4]
  
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  
  #obtener respuesta
  respuesta <- bow(url = enlace, force = TRUE, verbose = TRUE) |>
    tryCatch(error = function(error) {
      warning(glue("error en {enlace}: error en respuesta"))
      warning(error)
      return(NULL)
    })
  
  tryCatch({
    
  noticia <- scrape(respuesta)
  
  if (is.null(noticia)) {
    warning(glue("error en scrape {enlace}: scrape null"))
    return(NULL)
  }
  
  x_titulo <- noticia |> 
    # html_elements(".the-single__title") |> 
    html_elements("h1") |> 
    html_text2()

  # codigo <- enlace |> str_extract("\\d{8}") |> str_split(pattern = "") |> unlist()
  
  # x_fecha <- paste0(c(codigo[1:4], "-", codigo[5:6], "-", codigo[7:8]), collapse = "")
  
  # rediseño 2026: OJO el atributo <time datetime> está fijo/erróneo (siempre 2025-12-08).
  # La fecha real está en el JSON-LD ("datePublished").
  ld <- noticia |>
    html_elements('script[type="application/ld+json"]') |>
    html_text() |> paste(collapse = " ")
  x_fecha <- ld |>
    str_extract('datePublished"\\s*:\\s*"[0-9]{4}-[0-9]{2}-[0-9]{2}') |>
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")

  # Fallback: texto visible del <time> (formato "DD/ MM/ YYYY HH:MM")
  if (length(x_fecha) == 0 || is.na(x_fecha[1])) {
    tt <- noticia |> html_elements("time") |> html_text2() |> head(1)
    x_fecha <- tt |> str_extract("\\d{1,2}/\\s*\\d{1,2}/\\s*\\d{4}") |> str_remove_all("\\s")
  }
  # Fallback: fecha en la URL (formato .../YYYY/MM/DD/...)
  if (length(x_fecha) == 0 || is.na(x_fecha[1])) {
    x_fecha <- str_extract(enlace, "\\d{4}/\\d{2}/\\d{2}")
  }

  # x_bajada <- noticia |>
  #   html_elements(".the-single__excerpt") |> 
  #   html_text2()
  
  # x_texto <- noticia |> 
  #   html_elements(".the-single-section__text") |> 
  #   html_elements("p") |> 
  #   html_text2() |> 
  #   paste(collapse = "\n")
  
  # rediseño 2026: cuerpo en los <p> del artículo
  x_texto <- noticia |>
    html_elements("p") |>
    html_text2() |>
    paste(collapse = "\n")
  
  resultado <- tibble("titulo" = x_titulo[1],
                      "fecha" = x_fecha[1],
                      "fecha_scraping" = lubridate::now(),
                      # "bajada" = x_bajada[1],
                      "cuerpo" = x_texto[1],
                      "fuente" = "chvnoticias",
                      "url" = enlace)
  
  return(resultado)
  }, error = function(e) {
    warning(e)
    return(NULL)
  })
})

#guardar ----
guardar_noticias_en_postgres(resultados_chvnoticias, con)
DBI::dbDisconnect(con)

message(glue("listo cron chvnoticias {now()}"))
