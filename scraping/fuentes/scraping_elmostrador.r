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
secciones <- c(
  "https://www.elmostrador.cl/noticias/pais/",
  "https://www.elmostrador.cl/destacado/",
  "https://www.elmostrador.cl/mercados/"
); veces = 1:3; hist = ""

# la única forma de que entregue noticias para las páginas siguientes es si se siguen los enlaces
# si se indica un "hist", se hará el loop hist; si no se indica (""), se hace el loop normal

# obtener enlaces ----

# hist
# secciones <- "https://www.elmostrador.cl/destacado/"; veces <- 4:50; hist = "_a1"
# secciones <- "https://www.elmostrador.cl/destacado/"; veces <- 51:100; hist = "_a2"
# secciones <- "https://www.elmostrador.cl/destacado/"; veces <- 101:150; hist = "_a3"
# secciones <- "https://www.elmostrador.cl/mercados/"; veces <- 1:50; hist = "_b1"
# secciones <- "https://www.elmostrador.cl/mercados/"; veces <- 51:100; hist = "_b2"
# secciones <- "https://www.elmostrador.cl/mercados/"; veces <- 101:150; hist = "_b3"
# secciones <- "https://www.elmostrador.cl/noticias/pais/"; veces <- 1:50; hist = "_c1"
# secciones <- "https://www.elmostrador.cl/noticias/pais/"; veces <- 51:100; hist = "_c2"
# secciones <- "https://www.elmostrador.cl/noticias/pais/"; veces <- 101:150; hist = "_c3"


# hist ----
if (hist != "") {
  message("scraping histórico")
  
  resultados_enlaces <- map(secciones, \(enlace_seccion) {
    # enlace_seccion <- secciones[3]
    
    tryCatch({
      #revisar url válida
      if (is.null(revisar_url(enlace_seccion))) return(NULL)
      
      sesion <- session(enlace_seccion)
      
      # botón de abajo para entrar a la vista de lista de noticias
      sesion_x <- sesion |> 
        session_follow_link(css = "#mas-noticias > footer > a")
      
      # por cada una de las secciones, avanzar x páginas
      resultados <- map(veces, ~{
        
        # saltar
        sesion_x <- sesion_x |>
          session_jump_to(glue("https://www.elmostrador.cl/categoria/mercados/page/{.x}/"))
        
        seccion <- sesion_x |> read_html()
        
        enlaces_a <- seccion |>
          html_elements("h4") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_b <- seccion |>
          html_elements("h1") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_c <- seccion |>
          html_elements("h2") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_d <- seccion |>
          html_elements("h3") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_e <- seccion |>
          html_elements(".d-main-card") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_f <- seccion |>
          html_elements(".d-tag-card__title") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_obtenidos <- c(enlaces_a,
                               enlaces_b,
                               enlaces_c,
                               enlaces_d,
                               enlaces_e) |> 
          unique()
        
        message(glue("Se obtuvieron {length(enlaces_obtenidos)} noticias en {enlace_seccion} {.x}"))
        
        return(enlaces_obtenidos)
      })
      
      resultados_seccion <- resultados |> unlist() |> unique()
      
    },
    error = function(e) {
      message("Error en scraping: ", e)
      return(NULL)
    })
    
    return(resultados_seccion)
  })
}




# normal ----
if (hist == "") {
  message("scraping normal")
  
  resultados_enlaces <- map(secciones, \(enlace_seccion) {
    # enlace_seccion <- secciones[3]
    
    tryCatch({
      #revisar url válida
      if (is.null(revisar_url(enlace_seccion))) return(NULL)
      
      # # desistir si ya se scrapeó
      # if (revisar_scrapeado(enlace)) return(NULL)
      
      
      sesion <- session(enlace_seccion)
      
      # botón de abajo para entrar a la vista de lista de noticias
      sesion_x <- sesion |> 
        session_follow_link(css = "#mas-noticias > footer > a")
      
      # por cada una de las secciones, avanzad x páginas
      resultados <- map(veces, ~{
        
        seccion <- sesion_x |> read_html()
        
        enlaces_a <- seccion |>
          html_elements("h4") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_b <- seccion |>
          html_elements("h1") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_c <- seccion |>
          html_elements("h2") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_d <- seccion |>
          html_elements("h3") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_e <- seccion |>
          html_elements(".d-main-card") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_f <- seccion |>
          html_elements(".d-tag-card__title") |>
          html_elements("a") |>
          html_attr("href")
        
        enlaces_obtenidos <- c(enlaces_a,
                               enlaces_b,
                               enlaces_c,
                               enlaces_d,
                               enlaces_e) |> 
          unique()
        
        message(glue("Se obtuvieron {length(enlaces_obtenidos)} noticias en {enlace_seccion} {.x}"))
        
        
        # siguiente página para el siguiente paso
        if (.x < 3) {
          sesion_x <<- sesion_x |> 
            session_follow_link(xpath = glue('//*[@id="claves"]/div/div[12]/a[{.x}]'))
          
        } else {
          sesion_x <<- sesion_x |> 
            session_follow_link(xpath = glue('//*[@id="claves"]/div/div[12]/a[3]'))
        }
        
        # sesion_x <- sesion_x |> 
        #   session_jump_to("https://www.elmostrador.cl/categoria/mercados/page/60/")
        
        return(enlaces_obtenidos)
      })
      
      resultados_seccion <- resultados |> unlist() |> unique()
      
    },
    error = function(e) {
      message("Error en scraping: ", e)
      return(NULL)
    })
    
    return(resultados_seccion)
  })
}


enlaces_elmostrador <- resultados_enlaces |> unlist() |> unique()

# enlaces_elmostrador <- datos_prensa |>
#   filter(fuente == "elmostrador") |>
#   filter(str_detect(cuerpo, "first-child|container.style.")) |>
#   pull(url)

# scraping ----
resultados_elmostrador <- map(enlaces_elmostrador, \(enlace) {
  #enlace <- enlaces_elmostrador[5]
  # enlace <- "https://www.elmostrador.cl/noticias/opinion/columnas/2025/06/20/por-que-la-plena-integracion-a-los-brics-constituye-un-camino-fundamental-para-el-futuro-de-chile/"
  # enlace <- "https://www.elmostrador.cl/cultura/2025/06/18/met-opera-de-nueva-york-realizara-su-concurso-de-jovenes-talentos-por-primera-vez-en-sudamerica/"
  
  tryCatch({
    #revisar si existe la página
    if (is.null(revisar_url(enlace))) return(NULL) 
    
    # desistir si ya se scrapeó
    if (ya_scrapeado_en_db(enlace, con)) return(NULL)
    
    
    #scraping
    noticia <- enlace |> bow() |> scrape()
    
    # #en opinion y mercado
    # noticia_titulo_a <- noticia |>
    #   html_elements(".avatar-y-titulo") |>
    #   html_text2()
    # 
    # #en pais o destacado
    # noticia_titulo_b <- noticia |>
    #   html_elements(".titulo-single") |>
    #   html_text2()
    
    noticia_titulo <- noticia |>
      html_elements(".d-the-single__title") |> 
      html_text2()
    
    #   noticia_titulo <- ifelse(length(noticia_titulo_a) != 0,
    #                            noticia_titulo_a,
    #                            noticia_titulo_b)
    
    # #fecha
    # noticia_fecha_a <- noticia |>
    #   html_elements(".autor-y-fecha") |>
    #   html_text()
    
    noticia_fecha <- noticia |>
      html_elements(".d-the-single__date") |> 
      html_attr("datetime")
    
    # #bajada (puede no tener)
    # noticia_bajada <- noticia |>
    #   html_elements(".bloque-principal") |>
    #   html_elements("figcaption") |>
    #   html_text()
    
    noticia_bajada <- noticia |>
      html_elements(".d-the-single__excerpt") |> 
      html_text2()
    
    # #texto
    # noticia_texto <- noticia |>
    #   html_elements(".bloque-principal") |>
    #   html_elements("#noticia") |>
    #   html_text() |> 
    #   paste(collapse = "\n")
    
    noticia_texto <- noticia |>
      html_elements(".d-the-single__wrapper") |> 
      html_elements("p") |>
      html_text2() |> 
      str_subset("Inscríbete en nuestro Newsletter", negate = TRUE) |> 
      paste(collapse = "\n")
    
    noticia_tabla <- tibble("titulo" = noticia_titulo |> validar_elementos(),
                            "bajada" = noticia_bajada |> validar_elementos(),
                            "cuerpo" = noticia_texto |> validar_elementos(),
                            "fecha" = noticia_fecha |> validar_elementos(),
                            "fecha_scraping" = lubridate::today(),
                            "fuente" = "elmostrador",
                            "url" = enlace)
    
    return(noticia_tabla)
  },
  error = function(e) {
    message("Error en scraping: ", e)
    return(NULL)
  })
}) |> 
  list_rbind()

# guardar ----
guardar_noticias_en_postgres(resultados_elmostrador, con)
DBI::dbDisconnect(con)

message(glue("listo cron elmostrador {lubridate::now()}"))
