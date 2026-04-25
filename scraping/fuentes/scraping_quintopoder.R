library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

# https://quintopoder.cl/tema/actualidad?page=2

paginas = 1
# paginas = 3:10; hist = "_a"
# paginas = 11:40; hist = "_b"
# paginas = 41:100; hist = "_c"
# paginas = 101:150; hist = "_d"
# paginas = 151:200; hist = "_e"
# paginas = 201:250; hist = "_f"
# paginas = 251:300; hist = "_g"

# categorias <- paste0("https://www.elquintopoder.cl/columnas/nuevas-columnas/pagina/", paginas)

categorias <- c("https://www.elquintopoder.cl/temas/politica/",
                "https://www.elquintopoder.cl/temas/politica/pagina/2/",
                "https://www.elquintopoder.cl/temas/politica/pagina/3/",
                "https://www.elquintopoder.cl/temas/sociedad/",
                "https://www.elquintopoder.cl/temas/sociedad/pagina/2/",
                "https://www.elquintopoder.cl/temas/sociedad/pagina/3/",
                "https://www.elquintopoder.cl/temas/ciudadania/",
                "https://www.elquintopoder.cl/temas/ciudadania/pagina/2/",
                "https://www.elquintopoder.cl/temas/ciudadania/pagina/3/"
                )

# paginas = 1:20; hist = "_h"
# paginas = 21:40; hist = "_i"
# paginas = 41:60; hist = "_j"
# paginas = 61:100; hist = "_k"
# paginas = 1:100; hist = "_l"
# paginas = 1:100; hist = "_m"
# 
# categorias <- c(paste0("https://www.elquintopoder.cl/temas/economia/pagina/", paginas))
# categorias <- c(paste0("https://www.elquintopoder.cl/temas/justicia/pagina/", paginas))
# categorias <- c(paste0("https://www.elquintopoder.cl/temas/politica/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/salud/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/ciudad/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/sociedad/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/educacion/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/genero/pagina/", paginas),
#                 paste0("https://www.elquintopoder.cl/temas/ciudadania/pagina/", paginas)
# )

chrome <- iniciar_chrome()
# chrome$close()


# obtener enlaces ----
resultados_enlaces <- map(categorias, \(enlace) {
    # enlace <- categorias[1]
    # if (is.null(revisar_url(enlace))) return(NULL)
    
    # sitio <- bow(enlace) |> scrape()
    
    message("scraping ", enlace)
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  tryCatch({
    
    # chromote
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    sitio <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    enlaces <- sitio |> 
      # html_elements("h3") |> 
      html_elements(".titulo-not") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    
    return(enlaces)
  },
  error = function(e) {
    message("error en scraping quintopoder: ", e)
    return(NULL)
  })
})

enlaces_quintopoder <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_quintopoder <- map(enlaces_quintopoder, \(enlace) {
  # enlace <- enlaces_quintopoder[30]
  
  tryCatch({
    #revisar si existe la página
    # if (is.null(revisar_url(enlace))) return(NULL)   
    
    # scraping
    message("scraping ", enlace)
    # noticia <- enlace |> bow() |> scrape()
    
    # chromote
    inicio <- Sys.time()
    chrome_navegar(chrome, enlace)
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    titulo <- noticia |> 
      # html_elements(".single-entry-title") |> 
      html_elements(".titulo") |> 
      html_text()
    
    cuerpo <- noticia |> 
      # html_elements(".single-entry-content") |> 
      html_elements(".texto-noticia") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
    
    fecha <- noticia |> 
      # html_elements(".fecha-single") |> 
      html_elements(".meta-not") |> 
      html_elements("time") |> 
      html_attr("datetime") 
    
    mes <- fecha |> str_extract("\\w{4,}")
    dia <- fecha |> str_extract("^\\d+")
    año <- fecha |> str_extract("\\d{4}")
    mes_2 <- mes_a_numero(mes)
    fecha_2 <- paste(año, mes_2, dia, sep = "-")
    
    noticia_tabla <- tibble("titulo" = titulo[1],
                            "bajada" = "",
                            "cuerpo" = cuerpo[1],
                            "fecha" = fecha_2,
                            "fecha_scraping" = lubridate::today(),
                            "fuente" = "quintopoder",
                            "url" = enlace)
    return(noticia_tabla)
  },
  error = function(e) {
    message("error en scraping quintopoder: ", e)
    return(NULL)
  })
}) |> 
  list_rbind()


# guardar ----
guardar_noticias_en_postgres(resultados_quintopoder, con)
DBI::dbDisconnect(con)

message(glue("listo cron quintopoder {lubridate::now()}"))

invisible(chrome$close())
