library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")
con <- conectar_db()

terminos <- c("pais", "chile", "gobierno", 
              "diputado", "senado",
              "delincuencia", "seguridad", 
              "santiago", "region", "comuna",
              "economia", "salud", "hoy", "mañana")

paginas <- c(1:5); hist = "_h"
# paginas <- c(6:10); hist = "_h_b"
# paginas <- c(11:15); hist = "_h_c"
# paginas <- c(16:20); hist = "_h_d"
# paginas <- c(21:25); hist = "_h_e"
# paginas <- c(26:30); hist = "_h_f"
# paginas <- c(35:40); hist = "_h_g"
# paginas <- c(41:45); hist = "_h_h"
# paginas <- c(46:50); hist = "_h_i"
# paginas <- c(51:55); hist = "_h_j"
# paginas <- c(56:60); hist = "_h_k"
# paginas <- c(61:65); hist = "_h_l"
# paginas <- c(66:70); hist = "_h_m"

# enlace_busqueda <- paste0("https://www.cooperativa.cl/cgi-bin/prontus_search.cgi?search_prontus=noticias&search_idx=noticias&search_tmp=search_cooperativa_2018.html&search_form=no&search_pag=",
#                           paginas, 
#                           "&search_resxpag=30&search_maxpags=10000&search_orden=cro&search_meta1=&search_meta2=&search_meta3=&search_seccion=&search_tema=&search_subtema=&search_fechaini=&search_fechafin=&search_texto=", 
#                           terminos,
#                           "&search_modo=and&search_comodines=no&vista=")

# terminos <- c("x", "y", "z")

# glue("A{paginas}B{terminos}C") |> 
#   tibble() |> 
#   tidyr::unite(url, everything(), sep = "")

combinatoria <- expand.grid("https://www.cooperativa.cl/cgi-bin/prontus_search.cgi?search_prontus=noticias&search_idx=noticias&search_tmp=search_cooperativa_2018.html&search_form=no&search_pag=",
                            paginas, 
                            "&search_resxpag=30&search_maxpags=10000&search_orden=cro&search_meta1=&search_meta2=&search_meta3=&search_seccion=&search_tema=&search_subtema=&search_fechaini=&search_fechafin=&search_texto=", 
                            terminos, 
                            "&search_modo=and&search_comodines=no&vista="
)

enlace_busqueda <- combinatoria |> 
  tibble() |>
  tidyr::unite(url, everything(), sep = "") |> 
  pull(url)

# enlace_busqueda

# obtener enlaces desde búsquedas ----
resultados_busquedas <- map(enlace_busqueda, \(enlace) {
  # enlace <- enlace_busqueda[10]
  message("scraping", str_extract(enlace, "search_texto=\\w+"))
  
  # busqueda_html <- bow(enlace) |> 
  #   scrape()
  busqueda_html <- RCurl::getURL(enlace, 
                cainfo = "scraping/fuentes/cooperativa-cl.pem") |> 
    read_html()
  
  enlaces <- busqueda_html |> 
    html_elements(".modulo-busquedas-movil") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset("noticias") |> 
    tryCatch(error = function(error) {
      warning(glue("error en {enlace}: error en respuesta"))
      warning(error)
      return(NULL)
    })
  
  message(length(enlaces), " noticias encontradas") 
  Sys.sleep(1)
  return(enlaces)
})


enlaces_noticias <- resultados_busquedas |> 
  unlist() |> 
  str_subset("cgi-bin", negate = TRUE) |> 
  str_subset("deportes", negate = TRUE) |> 
  unique()

message("en total, se obtuvieron ", length(enlaces_noticias), " enlaces")


# scraping ----
resultados_cooperativa <- map_df(enlaces_noticias, \(enlace) {
  # enlace <- enlaces_noticias[28]
  
  enlace <- paste0("https://www.cooperativa.cl", enlace)
  
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  # desistir si ya se scrapeó
  if (ya_scrapeado_en_db(enlace, con)) return(NULL)
  
  
  tryCatch({
  noticia <- enlace |> 
    # bow() |> 
    # scrape() |>
    session() |> 
    read_html()
  
  if (is.null(noticia)) {
    warning(glue("error en {enlace}: scrape null"))
    return(NULL)
  }
  
  x_titulo <- noticia |> 
    html_elements("#despliegue-titular") |> 
    html_elements(".titular-audio") |> 
    html_text()
  
  # los opcionales son para noticias de opinión, que tienen una disposición distinta
  if (length(x_titulo) == 0) {
    x_titulo <- noticia |> 
      html_elements("h1") |> 
      html_text()
  }
  
  x_fecha <- enlace |> str_extract("\\d{4}-\\d{2}-\\d{2}")
  
  x_bajada <- noticia |> 
    html_elements("#despliegue-titular") |> 
    html_elements(".texto-bajada") |> 
    html_text()
  
  x_texto <- noticia |> 
    html_elements(".texto-bajada") |> 
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  if (length(x_texto) == 0) {
    x_texto <- noticia |> 
      html_elements(".columna") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
  }
  
  resultado <- tibble("titulo" = x_titulo[1],
                      "fecha" = x_fecha[1],
                      "fecha_scraping" = lubridate::now(),
                      "bajada" = x_bajada[1],
                      "cuerpo" = x_texto[1],
                      "fuente" = "cooperativa",
                      "url" = enlace)
  
  Sys.sleep(0.8)
  
  
  
  },
  error = function(error) {
    warning(glue("error en {enlace}: error en respuesta"))
    warning(error)
    return(NULL)
  })
  
  return(resultado)
}); beepr::beep()

#guardar ----
guardar_noticias_en_postgres(resultados_cooperativa, con)
DBI::dbDisconnect(con)

message(glue("listo cron cooperativa {lubridate::now()}"))
