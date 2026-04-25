library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()


# enlaces ----
categorias <- c("https://www.t13.cl/lo-ultimo",
                "https://www.t13.cl/nacional",
                "https://www.t13.cl/politica",
                "https://www.t13.cl/negocios")

# histórico por búsquedas
# categorias <- paste0("https://www.t13.cl/search?q=chile&offset=", seq(21, 91, 10), "&limit=10"); hist = "_a"
# categorias <- paste0("https://www.t13.cl/search?q=gobierno&offset=", seq(21, 91, 10), "&limit=10"); hist = "_b"
# categorias <- paste0("https://www.t13.cl/search?q=comuna&offset=", seq(21, 91, 10), "&limit=10"); hist = "_c"
# categorias <- paste0("https://www.t13.cl/search?q=carabinero&offset=", seq(21, 91, 10), "&limit=10"); hist = "_d"
# categorias <- paste0("https://www.t13.cl/search?q=diputado&offset=", seq(21, 91, 10), "&limit=10"); hist = "_e"
# categorias <- paste0("https://www.t13.cl/search?q=boric&offset=", seq(21, 91, 10), "&limit=10"); hist = "_f"
# categorias <- paste0("https://www.t13.cl/search?q=alcalde&offset=", seq(21, 91, 10), "&limit=10"); hist = "_g"
# categorias <- paste0("https://www.t13.cl/search?q=caso&offset=", seq(21, 91, 10), "&limit=10"); hist = "_h"
# categorias <- paste0("https://www.t13.cl/search?q=abril&offset=", seq(1, 41, 10), "&limit=10"); hist = "_aa"
# categorias <- paste0("https://www.t13.cl/search?q=mayo&offset=", seq(1, 41, 10), "&limit=10"); hist = "_ab"
# categorias <- paste0("https://www.t13.cl/search?q=junio&offset=", seq(1, 41, 10), "&limit=10"); hist = "_ac"
# categorias <- paste0("https://www.t13.cl/search?q=julio&offset=", seq(1, 41, 10), "&limit=10"); hist = "_ad"
# categorias <- paste0("https://www.t13.cl/search?q=abril&offset=", seq(51, 91, 10), "&limit=10"); hist = "_aa"
# categorias <- paste0("https://www.t13.cl/search?q=mayo&offset=", seq(51, 91, 10), "&limit=10"); hist = "_ab"
# categorias <- paste0("https://www.t13.cl/search?q=junio&offset=", seq(51, 91, 10), "&limit=10"); hist = "_ac"
# categorias <- paste0("https://www.t13.cl/search?q=julio&offset=", seq(51, 91, 10), "&limit=10"); hist = "_ad"





#loop enlaces ----
resultados_links <- map_df(categorias, \(enlace_categoria) {
  # enlace_categoria <- categorias[2]
  
  enlace_categoria_1 <- enlace_categoria |> bow() |> 
    scrape()
  
  enlaces <- enlace_categoria_1 |>
    # html_elements("#last-news-container") |> 
    html_elements(".card") |>
    # html_elements(".item-article") |> 
    html_attr("href")
  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria) |> 
    filter(!stringr::str_detect(enlace, "/videos/"))
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})

enlaces_t13 <- resultados_links |> 
  pull(enlace) |> 
  unique()

#loop ----
resultados_t13 <- map(enlaces_t13, \(enlace) {
# enlace <- enlaces_t13[10]
  
  enlace <- glue("https://www.t13.cl{enlace}")
  
  message(paste("scraping", enlace))
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  #revisar si existe la página
  # if (is.null(revisar_url(enlace))) return(NULL)   
  
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  noticia_titulo <- noticia |> 
    # html_elements(".article-component__header-title") |> 
    html_elements(".articulo-detalle-content") |>
    html_elements("h1") |>
    html_text() |> 
    paste(collapse = "")
  
  noticia_bajada <- noticia |> 
    # html_elements(".article-component__lead") |> 
    html_elements(".bajada") |> 
    html_text() |> 
    paste(collapse = "")
  
  # noticia_fecha <- noticia |> 
  #   html_elements(".article-component__info") |> 
  #   html_elements("time") |> 
  #   html_attr("datetime")
  noticia_fecha <- noticia |> 
    html_elements(".meta") |> 
    html_elements("time") |> 
    html_attr("datetime") |> 
    str_extract("\\d+-\\d+-\\d{2}")
  
  noticia_cuerpo <- noticia |> 
    # html_elements(".article-component__body") |> 
    html_elements(".cuerpo-content") |>
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  noticia_url <- enlace
  
  noticia_tabla <- tibble("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_cuerpo,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "t13",
                        "url" = noticia_url)
  
  return(noticia_tabla)
}) |> 
  list_rbind()

# resultados_t13 <- map(resultados_t13, as_tibble) |> 
#   list_rbind() |> 
#   try()

# guardar ----
guardar_noticias_en_postgres(resultados_t13, con)
DBI::dbDisconnect(con)

message(glue("listo cron t13 {lubridate::now()}"))
