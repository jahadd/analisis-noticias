# Dashboard de noticias — etiquetas en titulares
# Requiere: shiny, pool, DBI, RPostgres, ggplot2, dplyr
# Variables de entorno: PGUSER, PGPASSWORD, PGHOST, PGPORT, PGDATABASE

library(shiny)
library(pool)
library(DBI)
library(RPostgres)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Configuración y pool
# ------------------------------------------------------------------------------
pg_host     <- Sys.getenv("PGHOST",   "localhost")
pg_port     <- as.integer(Sys.getenv("PGPORT", "5432"))
pg_user     <- Sys.getenv("PGUSER",   "noticias")
pg_password <- Sys.getenv("PGPASSWORD", "")
pg_db       <- Sys.getenv("PGDATABASE", "noticias_chile")

pool_global <- NULL

get_pool <- function() {
  if (is.null(pool_global)) {
    if (!nzchar(pg_password)) stop("Definir PGPASSWORD para el dashboard.")
    pool_global <<- dbPool(
      RPostgres::Postgres(),
      host = pg_host,
      port = pg_port,
      user = pg_user,
      password = pg_password,
      dbname = pg_db
    )
  }
  pool_global
}

onStop(function() {
  if (!is.null(pool_global)) poolClose(pool_global)
})

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Titulares: etiquetas y tendencias", windowTitle = "Dashboard Noticias"),
  tags$head(tags$style(
    ".small-metric { font-size: 0.9em; color: #6c757d; margin-top: 8px; }
     .card-box { padding: 12px; border-radius: 6px; background: #f8f9fa; border: 1px solid #dee2e6; }
     .card-box .valor { font-size: 1.8em; font-weight: bold; color: #0d6efd; }"
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateRangeInput(
        "fechas",
        "Rango de fechas",
        start = Sys.Date() - 90,
        end = Sys.Date(),
        max = Sys.Date(),
        language = "es",
        separator = " a "
      ),
      br(),
      actionButton("preset_7",  "Últimos 7 días", class = "btn-sm"),
      actionButton("preset_30", "Último mes", class = "btn-sm"),
      actionButton("preset_90", "Último trimestre", class = "btn-sm"),
      hr(),
      uiOutput("selector_terminos_evol")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Conceptos",
          fluidRow(
            column(6, uiOutput("card_terminos_distintos")),
            column(6, uiOutput("card_termino_top"))
          ),
          tags$p(class = "small-metric", uiOutput("texto_volumen")),
          hr(),
          h4("Evolución de términos en el tiempo"),
          plotOutput("grafico_evolucion", height = "320px"),
          hr(),
          h4("Top 15 términos del período"),
          plotOutput("grafico_top_terminos", height = "340px"),
          hr(),
          h4("Últimas noticias (5 por página)"),
          uiOutput("paginacion_noticias"),
          div(style = "overflow-x: auto; margin-top: 8px;", tableOutput("tabla_noticias")),
          uiOutput("controles_pagina")
        ),
        tabPanel(
          "Volumen de noticias",
          h4("Noticias por día"),
          plotOutput("grafico_volumen_tiempo", height = "320px"),
          hr(),
          h4("Distribución por medio"),
          plotOutput("grafico_por_medio", height = "380px")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  pool <- get_pool()

  # Presets de fechas
  observeEvent(input$preset_7, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 6, end = Sys.Date())
  })
  observeEvent(input$preset_30, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 29, end = Sys.Date())
  })
  observeEvent(input$preset_90, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 89, end = Sys.Date())
  })

  fechas <- reactive({
    req(input$fechas)
    start <- as.Date(input$fechas[1])
    end   <- as.Date(input$fechas[2])
    anos  <- as.numeric(difftime(end, start, units = "days")) / 365.25
    list(start = start, end = end, anos = anos, por_ano = anos > 2)
  })

  # Términos distintos en el período
  n_terminos <- reactive({
    f <- fechas()
    q <- "SELECT COUNT(DISTINCT termino) AS n FROM titulos_terminos_diarios WHERE fecha >= $1 AND fecha <= $2"
    out <- dbGetQuery(pool, q, params = list(f$start, f$end))
    out$n
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Término más frecuente (nombre + frecuencia)
  termino_top <- reactive({
    f <- fechas()
    q <- "
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 1
    "
    dbGetQuery(pool, q, params = list(f$start, f$end))
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Top 15 términos (para barras y para selector de evolución)
  top_terminos_df <- reactive({
    f <- fechas()
    q <- "
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 15
    "
    dbGetQuery(pool, q, params = list(f$start, f$end))
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Datos de evolución para términos seleccionados
  datos_evolucion <- reactive({
    f <- fechas()
    terms <- input$terminos_evolucion
    if (is.null(terms) || length(terms) == 0L)
      return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    ph <- paste(sprintf("$%d", seq(3L, length.out = length(terms))), collapse = ", ")
    q <- paste0("
      SELECT fecha, termino, frecuencia
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2 AND termino IN (", ph, ")
      ORDER BY fecha
    ")
    params <- c(list(f$start, f$end), as.list(terms))
    dbGetQuery(pool, q, params = params)
  }) %>% bindCache(
    fechas()$start,
    fechas()$end,
    paste(sort(if (is.null(input$terminos_evolucion)) character(0) else input$terminos_evolucion), collapse = "|")
  )

  # Volumen (total noticias, promedio)
  volumen <- reactive({
    f <- fechas()
    q <- "
      SELECT COALESCE(SUM(total_noticias), 0) AS total,
             COALESCE(COUNT(*), 0) AS dias
      FROM metricas_titulos_diarias
      WHERE fecha >= $1 AND fecha <= $2
    "
    r <- dbGetQuery(pool, q, params = list(f$start, f$end))
    list(total = r$total, dias = r$dias, promedio = if (r$dias > 0) round(r$total / r$dias, 1) else 0)
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Total de noticias en el rango (para paginación)
  total_noticias_rango <- reactive({
    f <- fechas()
    q <- "SELECT COUNT(*) AS n FROM noticias WHERE fecha >= $1 AND fecha <= $2"
    dbGetQuery(pool, q, params = list(f$start, f$end))$n
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Página actual para últimas noticias (5 por página); se resetea al cambiar fechas
  page_noticias <- reactiveVal(1L)
  observeEvent(fechas(), { page_noticias(1L) })

  # Tabla últimas noticias (5 por página)
  tabla_noticias <- reactive({
    f <- fechas()
    pg <- max(1L, as.integer(page_noticias()))
    q <- "
      SELECT titulo, fecha, medio, url
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
      ORDER BY fecha DESC
      LIMIT 5 OFFSET $3
    "
    dbGetQuery(pool, q, params = list(f$start, f$end, (pg - 1L) * 5L))
  }) %>% bindCache(fechas()$start, fechas()$end, page_noticias())

  # Serie temporal: noticias por día (para pestaña Volumen)
  volumen_por_dia <- reactive({
    f <- fechas()
    q <- "
      SELECT fecha, total_noticias
      FROM metricas_titulos_diarias
      WHERE fecha >= $1 AND fecha <= $2
      ORDER BY fecha
    "
    dbGetQuery(pool, q, params = list(f$start, f$end))
  }) %>% bindCache(fechas()$start, fechas()$end)

  # Distribución por medio (para pestaña Volumen)
  noticias_por_medio <- reactive({
    f <- fechas()
    q <- "
      SELECT medio, COUNT(*) AS total
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY medio
      ORDER BY total DESC
    "
    dbGetQuery(pool, q, params = list(f$start, f$end))
  }) %>% bindCache(fechas()$start, fechas()$end)

  # ---- Outputs ----
  output$card_terminos_distintos <- renderUI({
    n <- n_terminos()
    if (is.na(n)) n <- 0
    div(class = "card-box",
        h5("Términos distintos en el período"),
        div(class = "valor", format(n, big.mark = "."))
    )
  })

  output$card_termino_top <- renderUI({
    tt <- termino_top()
    if (nrow(tt) == 0) {
      val <- "—"
      sub <- "Sin datos"
    } else {
      val <- tt$termino
      sub <- paste(format(tt$total, big.mark = "."), "apariciones")
    }
    div(class = "card-box",
        h5("Término más frecuente"),
        div(class = "valor", val),
        p(style = "margin-bottom:0; font-size:0.9em;", sub)
    )
  })

  output$texto_volumen <- renderUI({
    v <- volumen()
    tags$span(
      "Volumen: ",
      tags$strong(format(v$total, big.mark = ".")), " noticias en el período · ",
      "promedio ", tags$strong(v$promedio), " por día"
    )
  })

  output$selector_terminos_evol <- renderUI({
    top <- top_terminos_df()
    if (nrow(top) == 0) return(NULL)
    choices <- setNames(top$termino, paste0(top$termino, " (", format(top$total, big.mark = "."), ")"))
    selectizeInput(
      "terminos_evolucion",
      "Términos para el gráfico de evolución (máx. 4)",
      choices = choices,
      selected = head(top$termino, 4),
      multiple = TRUE,
      options = list(maxItems = 4)
    )
  })

  output$grafico_evolucion <- renderPlot({
    d <- datos_evolucion()
    if (nrow(d) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Selecciona al menos un término en el panel izquierdo.", cex = 1.2)
      return(invisible(NULL))
    }
    d <- mutate(d, fecha = as.Date(fecha))
    por_ano <- fechas()$por_ano
    if (por_ano) {
      d <- d %>% mutate(ano = as.integer(format(fecha, "%Y"))) %>%
        group_by(ano, termino) %>% summarise(frecuencia = sum(frecuencia), .groups = "drop")
      ggplot(d, aes(x = ano, y = frecuencia, colour = termino)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = min(d$ano):max(d$ano)) +
        labs(x = "Año", y = "Frecuencia", colour = "Término") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(d, aes(x = fecha, y = frecuencia, colour = termino)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(x = "Fecha", y = "Frecuencia", colour = "Término") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }, res = 96, width = 700, height = 320)

  output$grafico_top_terminos <- renderPlot({
    top <- top_terminos_df()
    if (nrow(top) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "No hay datos para el rango elegido.", cex = 1.2)
      return(invisible(NULL))
    }
    top <- mutate(top, termino = reorder(termino, total))
    ggplot(top, aes(x = total, y = termino)) +
      geom_col(fill = "#0d6efd") +
      labs(x = "Frecuencia total", y = NULL) +
      theme_minimal(base_size = 12)
  }, res = 96, width = 700, height = 340)

  output$paginacion_noticias <- renderUI({
    total <- total_noticias_rango()
    if (total == 0) return(tags$p("No hay noticias en el rango elegido."))
    pg <- page_noticias()
    desde <- (pg - 1L) * 5L + 1L
    hasta <- min(pg * 5L, total)
    tags$p(class = "small-metric", paste0("Mostrando ", desde, "-", hasta, " de ", format(total, big.mark = ".")))
  })

  output$controles_pagina <- renderUI({
    total <- total_noticias_rango()
    if (total == 0) return(NULL)
    npag <- max(1L, ceiling(total / 5L))
    pg <- page_noticias()
    tagList(
      tags$div(style = "margin-top: 12px;"),
      actionButton("pag_prev", "← Anterior", class = "btn-sm", disabled = pg <= 1L),
      tags$span(style = "margin: 0 10px;", paste("Página", pg, "de", npag)),
      actionButton("pag_next", "Siguiente →", class = "btn-sm", disabled = pg >= npag)
    )
  })

  observeEvent(input$pag_prev, {
    if (page_noticias() > 1L) page_noticias(page_noticias() - 1L)
  })
  observeEvent(input$pag_next, {
    total <- total_noticias_rango()
    npag <- max(1L, ceiling(total / 5L))
    if (page_noticias() < npag) page_noticias(page_noticias() + 1L)
  })

  output$tabla_noticias <- renderTable({
    t <- tabla_noticias()
    if (nrow(t) == 0) return(data.frame(Mensaje = "No hay noticias en esta página."))
    t$url <- paste0('<a href="', t$url, '" target="_blank" rel="noopener">Enlace</a>')
    t$titulo <- substr(t$titulo, 1, 70)
    t$fecha <- as.character(t$fecha)
    t
  }, striped = TRUE, hover = TRUE, sanitize.text.function = function(x) x)

  # Gráfico: volumen de noticias en el tiempo (pestaña Volumen)
  output$grafico_volumen_tiempo <- renderPlot({
    d <- volumen_por_dia()
    if (nrow(d) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "No hay datos para el rango elegido.", cex = 1.2)
      return(invisible(NULL))
    }
    d <- mutate(d, fecha = as.Date(fecha))
    por_ano <- fechas()$por_ano
    if (por_ano) {
      d <- d %>% mutate(ano = as.integer(format(fecha, "%Y"))) %>%
        group_by(ano) %>% summarise(total_noticias = sum(total_noticias), .groups = "drop")
      ggplot(d, aes(x = ano, y = total_noticias)) +
        geom_col(fill = "#0d6efd", width = 0.6) +
        scale_x_continuous(breaks = min(d$ano):max(d$ano)) +
        labs(x = "Año", y = "Noticias") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(d, aes(x = fecha, y = total_noticias)) +
        geom_line(colour = "#0d6efd", linewidth = 1) +
        geom_area(fill = "#0d6efd", alpha = 0.2) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(x = "Fecha", y = "Noticias") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }, res = 96, width = 700, height = 320)

  # Gráfico: distribución por medio (pestaña Volumen)
  output$grafico_por_medio <- renderPlot({
    d <- noticias_por_medio()
    if (nrow(d) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "No hay datos para el rango elegido.", cex = 1.2)
      return(invisible(NULL))
    }
    d <- mutate(d, medio = reorder(medio, total))
    ggplot(d, aes(x = total, y = medio, fill = total)) +
      geom_col() +
      scale_fill_gradient(low = "#6c9bd1", high = "#0d6efd") +
      labs(x = "Número de noticias", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  }, res = 96, width = 700, height = 380)
}

# ------------------------------------------------------------------------------
# Lanzar app (desde R o RStudio, o: Rscript -e "shiny::runApp('dashboard', port=3838)")
# Desde la carpeta noticias: runApp("dashboard", port = 3838)
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
