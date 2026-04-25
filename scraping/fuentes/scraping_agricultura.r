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
categorias <- c(#"https://www.radioagricultura.cl/nacional/",
  "https://www.radioagricultura.cl/categoria/nacional/",
  "https://www.radioagricultura.cl/categoria/economia/",
  "https://www.radioagricultura.cl/categoria/politica/",
  "https://www.radioagricultura.cl/categoria/economia/"
                # "https://www.radioagricultura.cl/economia/",
                # "https://www.radioagricultura.cl/se-dijo-en-agricultura/",
                # "https://www.radioagricultura.cl/columnas-de-opinion/"
)

# para obtener noticias anteriores
# fechas <- seq.Date(today()-months(1), today(), by = "days") |> format("%Y/%m/%d"); hist = "_h_a"
# fechas <- seq.Date(today()-months(2), today()-months(1), by = "days") |> format("%Y/%m/%d"); hist = "_h_b"
# fechas <- seq.Date(today()-months(3), today()-months(2), by = "days") |> format("%Y/%m/%d"); hist = "_h_c"
# fechas <- seq.Date(today()-months(4), today()-months(3), by = "days") |> format("%Y/%m/%d"); hist = "_h_d"
# fechas <- seq.Date(today()-months(5), today()-months(4), by = "days") |> format("%Y/%m/%d"); hist = "_h_e"
# fechas <- seq.Date(today()-months(6), today()-months(5), by = "days") |> format("%Y/%m/%d"); hist = "_h_f"

# fechas <- seq.Date(today()-years(1)-months(1), today()-years(1), by = "days") |> format("%Y/%m/%d"); hist = "_h_g"
# fechas <- seq.Date(today()-years(1)-months(2), today()-years(1)-months(1), by = "days") |> format("%Y/%m/%d"); hist = "_h_h"
# fechas <- seq.Date(today()-years(1)-months(3), today()-years(1)-months(2), by = "days") |> format("%Y/%m/%d"); hist = "_h_i"
# fechas <- seq.Date(today()-years(1)-months(4), today()-years(1)-months(3), by = "days") |> format("%Y/%m/%d"); hist = "_h_j"
# fechas <- seq.Date(today()-years(1)-months(5), today()-years(1)-months(4), by = "days") |> format("%Y/%m/%d"); hist = "_h_k"
# fechas <- seq.Date(today()-years(1)-months(6), today()-years(1)-months(5), by = "days") |> format("%Y/%m/%d"); hist = "_h_l"
# fechas <- seq.Date(today()-years(1)-months(7), today()-years(1)-months(6), by = "days") |> format("%Y/%m/%d"); hist = "_h_m"
# fechas <- seq.Date(today()-years(1)-months(8), today()-years(1)-months(7), by = "days") |> format("%Y/%m/%d"); hist = "_h_n"
# fechas <- seq.Date(today()-years(1)-months(9), today()-years(1)-months(8), by = "days") |> format("%Y/%m/%d"); hist = "_h_o"
# fechas <- seq.Date(today()-years(1)-months(10), today()-years(1)-months(9), by = "days") |> format("%Y/%m/%d"); hist = "_h_p"
# fechas <- seq.Date(today()-years(1)-months(11), today()-years(1)-months(10), by = "days") |> format("%Y/%m/%d"); hist = "_h_q"
# fechas <- seq.Date(today()-years(1)-months(12), today()-years(1)-months(11), by = "days") |> format("%Y/%m/%d"); hist = "_h_r"

# fechas <- seq.Date(today()-years(3)-months(1), today()-years(3), by = "days") |> format("%Y/%m/%d"); hist = "_h_cg"
# fechas <- seq.Date(today()-years(3)-months(2), today()-years(3)-months(1), by = "days") |> format("%Y/%m/%d"); hist = "_h_ch"
# fechas <- seq.Date(today()-years(3)-months(3), today()-years(3)-months(2), by = "days") |> format("%Y/%m/%d"); hist = "_h_ci"
# fechas <- seq.Date(today()-years(3)-months(4), today()-years(3)-months(3), by = "days") |> format("%Y/%m/%d"); hist = "_h_cj"
# fechas <- seq.Date(today()-years(3)-months(5), today()-years(3)-months(4), by = "days") |> format("%Y/%m/%d"); hist = "_h_ck"
# fechas <- seq.Date(today()-years(3)-months(6), today()-years(3)-months(5), by = "days") |> format("%Y/%m/%d"); hist = "_h_cl"
# fechas <- seq.Date(today()-years(3)-months(7), today()-years(3)-months(6), by = "days") |> format("%Y/%m/%d"); hist = "_h_cm"
# fechas <- seq.Date(today()-years(3)-months(8), today()-years(3)-months(7), by = "days") |> format("%Y/%m/%d"); hist = "_h_cn"
# fechas <- seq.Date(today()-years(3)-months(9), today()-years(3)-months(8), by = "days") |> format("%Y/%m/%d"); hist = "_h_co"
# fechas <- seq.Date(today()-years(3)-months(10), today()-years(3)-months(9), by = "days") |> format("%Y/%m/%d"); hist = "_h_cp"
# fechas <- seq.Date(today()-years(3)-months(11), today()-years(3)-months(10), by = "days") |> format("%Y/%m/%d"); hist = "_h_cq"
# fechas <- seq.Date(today()-years(3)-months(12), today()-years(3)-months(11), by = "days") |> format("%Y/%m/%d"); hist = "_h_cr"

# 
# categorias <- c(paste0("https://www.radioagricultura.cl/nacional/", fechas),
#                 paste0("https://www.radioagricultura.cl/economia/", fechas),
#                 paste0("https://www.radioagricultura.cl/se-dijo-en-agricultura/", fechas),
#                 paste0("https://www.radioagricultura.cl/columnas-de-opinion/", fechas))

#loop enlaces ----
resultados_enlaces <- map(categorias, \(enlace) {
  tryCatch({
    # enlace <- categorias[1]
    # sesion <- enlace_categoria |> bow() |> scrape()
    if (is.null(revisar_url(enlace))) return(NULL)
    
    sesion <- enlace |> session() |> read_html()
    
    enlaces <- sesion |>
      # html_elements(".entry-title") |> 
      html_elements(".main-article-box-card__title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    return(enlaces)
  },
  error = function(e) {
    message("error en scraping agricultura: ", e)
    return(NULL)
  })
})

enlaces_agricultura <- resultados_enlaces |> 
  unlist() |> 
  unique()


#loop ----
resultados_agricultura <- map(enlaces_agricultura, \(enlace) {
  # enlace <- enlaces_agricultura[8]
  
  tryCatch({
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)   
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    #scraping
    noticia <- enlace |> bow() |> scrape()
    
    noticia_titulo <- noticia |> 
      # html_elements(".entry-header") |> 
      html_elements(".main-article__header") |> 
      # html_elements(".entry-title") |> 
      html_elements(".main-article__title") |> 
      html_text2()
    
    # noticia_bajada <- noticia |>
    #   html_elements(".contenido") |>
    #   html_elements("header") |>
    #   html_elements(".detalle") |>
    #   html_text()
    
    # noticia_fecha <- noticia |> 
    #   # html_elements(".entry-meta") |> 
    #   html_elements(".main-author-card__caption") |> 
    #   html_elements("time") |> 
    #   # html_elements(".published") |> 
    #   html_attr("datetime") |> 
    #   str_extract("\\d+\\-\\d+\\-\\d+")
    
    noticia_fecha <- enlace |> str_extract("\\d{8}") |> as_date() |> as.character()
    
    noticia_cuerpo <- noticia |> 
      # html_elements(".entry-content") |> 
      html_elements(".main-article__text") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
    
    noticia_tabla <- tibble("titulo" = noticia_titulo[1],
                            "bajada" = "",
                            "cuerpo" = noticia_cuerpo[1],
                            "fecha" = noticia_fecha[1],
                            "fecha_scraping" = lubridate::today(),
                            "fuente" = "agricultura",
                            "url" = enlace)
    
    return(noticia_tabla)
  },
  error = function(e) {
    message("error en scraping agricultura: ", e)
    return(NULL)
  })
}) |> 
  list_rbind()

# guardar ----
guardar_noticias_en_postgres(resultados_agricultura, con)
DBI::dbDisconnect(con)

message(glue("listo cron agricultura {lubridate::now()}"))
