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

paginas <- 1:2
# paginas <- 701:800; hist = "_s"
# paginas <- 801:900; hist = "_t"
# paginas <- 901:1000; hist = "_u"
# paginas <- 1001:1100; hist = "_v"
# paginas <- 1101:1200; hist = "_w"
# paginas <- 1201:1300; hist = "_x"
# paginas <- 1301:1400; hist = "_y"
# paginas <- 1401:1500; hist = "_z"
# paginas <- 1501:1600; hist = "_aa"
# paginas <- 1601:1700; hist = "_ab"
# paginas <- 1701:1800; hist = "_ac"
# paginas <- 1801:1900; hist = "_ad"
# paginas <- 1901:2000; hist = "_af"
# paginas <- 2001:2200; hist = "_a"
# paginas <- 2201:2400; hist = "_b"
# paginas <- 2401:2600; hist = "_c"
# paginas <- 2601:2800; hist = "_d"
# paginas <- 2801:2999; hist = "_f"
# paginas <- 3001:3000; hist = "_e" #2018


enlaces <- c(paste0("https://eldesconcierto.cl/noticias/nacional?page=", paginas),
            paste0("https://eldesconcierto.cl/noticias/economia?page=", paginas),
            paste0("https://eldesconcierto.cl/noticias/regiones?page=", paginas),
            paste0("https://eldesconcierto.cl/politica?page=", paginas),
            paste0("https://eldesconcierto.cl/reportajes?page=", paginas),
            paste0("https://eldesconcierto.cl/tendencias/negocios?page=", paginas)
)

# library(httr)
# uastring <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36"

library(chromote)
options(chromote.headless = "new")
chrome <- ChromoteSession$new()
chrome$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
# chrome$close()

## enlaces noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # if (is.null(revisar_url(enlace))) return(NULL)  
  # enlace <- enlaces[1]
  # enlace <- "https://eldesconcierto.cl/"
  # # https://eldesconcierto.cl/noticias/nacional?page=1
  # enlace <- "http://eldesconcierto.cl/search/hoy"
  # sesion <- session(enlace, user_agent(uastring)) |> read_html()
  
  message("scraping ", enlace)
  
  tryCatch({
  inicio <- Sys.time()
  chrome$Page$navigate(enlace) # navegar a la página
  chrome$Page$loadEventFired() # esperar a que cargue
  body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
  sitio <- read_html(body)
  final <- Sys.time()
  Sys.sleep((final-inicio)*3) # espera
  
  noticias_enlaces_a <- sitio |> 
    html_elements(".p-4") |> 
    html_elements("a") |> 
    html_attr("href")
  
  noticias_enlaces_b <- sitio |> 
    html_elements(".px-4") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset(pattern = "\\/\\d{4}")
  
  noticias_enlaces <- c(noticias_enlaces_a,
                        noticias_enlaces_b) |> 
    unique()
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  
  return(noticias_enlaces)
  
  },
  error = function(e) {
    message("Error en scraping latercera: ", e)
    return(NULL)}
  )
})


enlaces_eldesconcierto <- resultados_enlaces |>
  unlist() |>
  unique()

message(length(enlaces_eldesconcierto), " noticias obtenidas en total")


# scraping ----
resultados_eldesconcierto <- map_df(enlaces_eldesconcierto, \(enlace) {
  # enlace <- enlaces_eldesconcierto[20]
  enlace <- paste0("https://www.eldesconcierto.cl", enlace)
  
  message("scraping ", enlace)
  
  #obtener respuesta
  tryCatch({
    # if (is.null(revisar_url(enlace))) return(NULL)
    # noticia <- bow(enlace) |> scrape()
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    inicio <- Sys.time()
    chrome$Page$navigate(enlace) # navegar a la página
    chrome$Page$loadEventFired() # esperar a que cargue
    body <- chrome$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value # obtener datos
    noticia <- read_html(body)
    final <- Sys.time()
    Sys.sleep((final-inicio)*3) # espera
    
    x_titulo <- noticia |> 
      # html_elements("h3") |> 
      html_elements("h1") |>
      html_text2() |> 
      pluck(1)
    
    fecha_a <- noticia |> 
      html_elements("meta") |> 
      html_attr("name")
    
    fecha_b <- noticia |> 
      html_elements("meta") |> 
      html_attr("content")
    
    x_fecha <- tibble(name = fecha_a,
           content = fecha_b) |> 
      filter(name == "date") |> 
      pull(content) |> 
      stringr::str_extract("\\d+-\\d+-\\d{2}")
    
    x_bajada <- noticia |> 
      html_elements(".text-gray-700") |> 
      html_text2()
    
    x_texto <- noticia |> 
      html_elements(".single-post__content") |> 
      html_elements("p") |> 
      html_text2() |> 
      paste(collapse = "\n")
    
    resultado <- tibble("titulo" = x_titulo[1],
                        "fecha" = x_fecha[1],
                        "fecha_original" = x_fecha[1],
                        "fecha_scraping" = lubridate::now(),
                        "bajada" = x_bajada[1],
                        "cuerpo" = x_texto[1],
                        "fuente" = "eldesconcierto",
                        "url" = enlace)
    
    return(resultado)
    
  },
  error = function(error) {
    warning(glue("error en eldesconcierto: error en respuesta"))
    return(NULL)
  }
  )
})


# guardar ----
guardar_noticias_en_postgres(resultados_eldesconcierto, con)
DBI::dbDisconnect(con)

message(glue("listo cron eldesconcierto {lubridate::now()}"))

invisible(chrome$close())
