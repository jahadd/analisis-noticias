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
categorias <- c("https://www.adnradio.cl/noticias/",
                "https://www.adnradio.cl/noticias/nacional/",
                "https://www.adnradio.cl/noticias/politica/",
                "https://www.adnradio.cl/noticias/economia/")


chrome <- iniciar_chrome()
# chrome$close()

#loop enlaces ----
resultados_links <- map_df(categorias, \(enlace_categoria) {
  # enlace_categoria <- categorias[2]
  
  tryCatch({
    # if (is.null(revisar_url(enlace_categoria))) return(NULL) 
    # 
    # enlace_categoria_1 <- enlace_categoria |> bow() |> 
    #   scrape()
    
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace_categoria)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sitio <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    # K20 (auditor-datos, 2026-09-03): adnradio.cl rediseñó las páginas de
    # listado -- los títulos de artículo ahora están envueltos así:
    # <article><header><a href="..."><h2>título</h2></a></header></article>
    # (el <a> envuelve al <h2>, no al revés como con el <h3> viejo). Se
    # busca el <a> que contiene un <h2> vía xpath (confirmado con HTML
    # crudo, sin necesidad de Chromote: 22/22 enlaces del listado son
    # artículos, cero falsos positivos).
    enlaces <- sitio |>
      html_elements(xpath = "//article//a[h2]") |>
      html_attr("href") |>
      unique()
    
    noticias_links <- tibble("enlace" = paste0("https://www.adnradio.cl", enlaces),
                             "origen" = enlace_categoria) |> 
      filter(!stringr::str_detect(enlace, "facebook\\.com"),
             !stringr::str_detect(enlace, "series-y-peliculas"),
             !stringr::str_detect(enlace, "wa\\.me"),
             !stringr::str_detect(enlace, "twitter\\.com")) |> 
      filter(nchar(enlace) > 30)
    
    
    message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
    return(noticias_links)
  },
  error = function(e) { message("error: ", e); return(NULL) }
  )
})


#loop ----
resultados_adnradio <- map(unique(resultados_links$enlace), \(enlace) {
  # enlace <- resultados_links$enlace[4]
  
  tryCatch({
    
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL)   
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    noticia <- tryCatch(
      bow(enlace) |> scrape(),
      error = function(e) { message("error en adnradio: ", e); NULL }
    )
    if (is.null(noticia)) return(NULL)

    # K20 (2026-09-03): el header del artículo cambió de clase/estructura
    # ("header.art-header", con el título en <h1> y la bajada en un <p>
    # suelto, ya no en <h2>). Antes se buscaba en CUALQUIER <header> del
    # documento -- funcionaba para el título porque el header del sitio no
    # tiene <h1>, pero para la bajada capturaba el <p> equivocado del menú
    # ("Selecciona tu emisora"), ya que ese header sí tiene <p>. Se acota al
    # header del artículo con la clase específica.
    noticia_titulo <- noticia |>
      html_elements("header.art-header") |>
      html_elements("h1") |>
      html_text() |>
      purrr::pluck(1)

    noticia_bajada <- noticia |>
      html_elements("header.art-header") |>
      html_elements("p") |>
      html_text() |>
      purrr::pluck(1)
    
    # noticia_fecha <- noticia |> 
    #   html_elements(".cnt-byline") |> 
    #   html_elements(".a_ti") |> 
    #   html_attr("datetime") |> 
    #   unique() |> 
    #   purrr::pluck(1)
    
    noticia_fecha <- enlace |> 
      str_extract("\\d{4}/\\d{2}/\\d{2}")
    
    noticia_cuerpo <- noticia |> 
      html_elements(".cnt-txt") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
    
    noticia_tabla <- list("titulo" = noticia_titulo,
                          "bajada" = noticia_bajada,
                          "cuerpo" = noticia_cuerpo,
                          "fecha" = noticia_fecha,
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "adnradio",
                          "url" = enlace)
    
    return(noticia_tabla)
  },
  error = function(e) { message("error: ", e); return(NULL) }
  )
})

# guardar ----
guardar_noticias_en_postgres(resultados_adnradio, con)
DBI::dbDisconnect(con)

message(glue("listo cron adnradio {lubridate::now()}"))

invisible(chrome$close())