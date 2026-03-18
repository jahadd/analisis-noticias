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
paginas_principal <- 1:4

enlaces <- paste0("https://www.df.cl/ultimasnoticias/p/", paginas_principal) #al parecer hasta p 20
# aprox 10 por página


# enlaces principales ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[2]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticias <- bow(enlace) |> scrape()
    
    enlaces <- noticias |> 
      # html_elements("#wrap-noticias") |> 
      html_elements(".tax-list") |> 
      html_elements(".card__content") |> 
      html_elements("a") |> 
      html_attr("href") |> 
      str_subset("dfsud", negate = T) |> 
      str_subset("ultimasnoticias/p", negate = T) |> 
      str_subset("taxport", negate = T) |> 
      str_trim() |> unique()
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    
    return(enlaces)
  },
  error = function(e) {
    message("Error en scraping: ", e)
    return(NULL)
  })
})

enlaces_diariofinanciero <- resultados_enlaces |> unlist()


# enlaces categorías ----
paginas_categorias <- 1:2

# entrar a varias categorías e intentar obtener más noticias
categoria_mineria = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_1_2__", paginas_categorias, ".html")
categoria_medioambiente = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_1_10__", paginas_categorias, ".html")
categoria_politica = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_2_16__", paginas_categorias, ".html")
categoria_energia = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_1_3__", paginas_categorias, ".html")
categoria_empresas = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_1___", paginas_categorias, ".html")
categoria_mercados = paste0("https://www.df.cl/noticias/site/tax/port/all/taxport_3___", paginas_categorias, ".html")

categorias <- c(categoria_mineria,
                categoria_medioambiente,
                categoria_politica,
                categoria_energia,
                categoria_empresas,
                categoria_mercados)

resultados_categorias <- map(categorias, \(enlace) {
  # enlace <- categorias[3]
  
  tryCatch({
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)
    
    noticias <- enlace |> bow() |> scrape()
    
    enlaces <- noticias |>
      # html_elements("#wrap-noticias") |>
      html_elements(".tax-list") |> 
      html_elements(".card__content") |> 
      html_elements("a") |>
      html_attr("href") |>
      str_subset("taxport", negate = T) |> 
      str_subset("total-brands", negate = T) |> 
      str_subset("dfsud", negate = T) |> 
      unique() |> 
      str_trim()
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    return(enlaces)
  },
  error = function(e) {
    message("Error en scraping: ", e)
    return(NULL)
  })
})

enlaces_categorias <- resultados_categorias |> unlist()


#unir enlaces ----
enlaces_diariofinanciero_2 <- c(enlaces_diariofinanciero, enlaces_categorias) |> unique()


# scraping ----
library(furrr)
plan(multisession, workers = 4)

resultados_diariofinanciero <- future_map(enlaces_diariofinanciero_2, \(enlace) {
  # enlace <- enlaces_diariofinanciero_2[10]
  # enlace <-  enlaces[6]
  
  tryCatch({
    enlace <- if_else(str_detect(enlace, "^http"), 
                      enlace,
                      paste0("https://www.df.cl", enlace))
    
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)   
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    # enlace <- "https://www.df.cl/acceso-a-internet-clave-para-el-desarrollo-economico-social-y-educacional"
    # enlace <- "https://www.df.cl/simone-biles-regresa-a-los-juegos-olimpicos-en-paris"
    noticia <- enlace |> bow() |> scrape()
    
    titulo <- noticia |> 
      html_elements("#titulo_articulo") |> 
      html_text2()
    
    if (length(titulo) == 0) {
      titulo <- noticia |> 
        html_elements(".enc-main__title") |> 
        html_text2()
    }
    
    if (is.na(titulo)) return(NULL)
    
    bajada <- noticia |> 
      html_elements(".bajada") |> 
      html_text2()
    
    if (length(bajada) == 0) {
      bajada <- noticia |> 
        html_elements(".enc-main__description") |> 
        html_text2()
    }
    
    cuerpo <- noticia |> 
      html_elements(".CUERPO") |> 
      html_elements("p") |> 
      html_text2() |> 
      paste(collapse = "\n")
    
    fecha <- noticia |> 
      html_elements("script") |> 
      html_attrs_dfr() |> #obtiene todas las propiedades
      tibble() |>
      filter(type == "application/ld+json") |>
      pull(`.text`) |>
      # str_extract('"datePublished\":\"\\d{4}-\\d{2}-\\d{2}') |> 
      str_extract('datePublished.*\\d{4}-\\d{2}-\\d{2}') |> 
      str_extract('\\d{4}-\\d{2}-\\d{2}') |> 
      as.character()
    
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fecha" = fecha |> validar_elementos(),
                           "fecha_scraping" = lubridate::today(),
                           "fuente" = "diariofinanciero",
                           "url" = enlace)
    
    return(noticia_data)
  },
  error = function(e) {
    message("Error en ", enlace, ": ", e)
    return(NULL)
  })
}) |> 
  list_rbind()

# guardar ----
guardar_noticias_en_postgres(resultados_diariofinanciero, con)
DBI::dbDisconnect(con)

message(glue("listo cron diariofinanciero {lubridate::now()}"))
