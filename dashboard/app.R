# Dashboard de noticias — etiquetas en titulares
# Requiere: shiny, plotly, pool, DBI, RPostgres, ggplot2, dplyr
# Variables de entorno: PGUSER, PGPASSWORD, PGHOST, PGPORT, PGDATABASE

library(shiny)
library(plotly)
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
  tags$head(
    tags$title("Dashboard Noticias — Titulares y tendencias (2015-2026)"),
    tags$style(HTML("
      .dashboard-title {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-size: 2.6rem;
        font-weight: 700;
        color: #1a1a2e;
        letter-spacing: -0.03em;
        line-height: 1.2;
        margin: 0 0 0.4rem 0;
        padding-bottom: 0.6rem;
        border-bottom: 3px solid #0d6efd;
      }
      .dashboard-subtitle {
        font-size: 0.95rem;
        color: #6c757d;
        margin: 0 0 1.25rem 0;
      }
      .small-metric { font-size: 0.9em; color: #6c757d; margin-top: 8px; }
      .card-box { padding: 12px; border-radius: 6px; background: #f8f9fa; border: 1px solid #dee2e6; }
      .card-box .valor { font-size: 1.8em; font-weight: bold; color: #0d6efd; }
      .sidebar-seccion { margin-bottom: 1.25rem; }
      .sidebar-seccion .control-label { font-weight: 600; margin-bottom: 6px; display: block; color: #1a1a2e; }
      .sidebar-terminos { margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #dee2e6; }
      .preset-buttons { display: flex; flex-direction: column; gap: 8px; margin: 8px 0; }
      .preset-buttons .btn { margin: 0; }
      .term-chips-box { padding: 8px 0; }
      .term-chips-box .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
      .term-chips-box .checkbox { margin: 0; }
      .term-chips-box .checkbox-inline { margin: 0; padding: 5px 12px; border-radius: 6px; border: 1px solid #dee2e6; background: #f8f9fa; cursor: pointer; }
      .term-chips-box .checkbox-inline:hover { background: #e9ecef; }
      .term-chips-box label.checkbox-inline:has(input:checked) { background: #0d6efd; border-color: #0d6efd; color: #fff; }
      .term-chips-box label { cursor: pointer; }
      .term-chips-box input[type='checkbox'] { display: none; }
      .busqueda-termino { margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #e9ecef; }
      .busqueda-termino .control-label { font-weight: 600; margin-bottom: 6px; display: block; color: #1a1a2e; }
      .busqueda-termino .form-group { margin-bottom: 0.5rem; }
      .busqueda-termino .form-control {
        width: 100%; max-width: 100%;
        padding: 8px 12px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        font-size: inherit;
      }
      .busqueda-termino .form-control:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.2);
        outline: none;
      }
      .busqueda-termino .form-control::placeholder { color: #adb5bd; }
      .frecuencia-termino-valor {
        margin-top: 8px; padding: 10px 12px;
        border-radius: 8px; background: #e7f1ff; border: 1px solid #b6d4fe;
        font-size: inherit; color: #0d6efd;
      }
      .frecuencia-termino-valor .numero { font-size: 1.15em; font-weight: 700; }
      .frecuencia-termino-msg { font-size: inherit; color: #6c757d; margin: 8px 0 0 0; }
      .frecuencia-termino-lista { margin-top: 8px; max-height: 280px; overflow-y: auto; }
      .frecuencia-termino-item {
        padding: 6px 10px; margin-bottom: 2px;
        border-radius: 6px; background: #e7f1ff; border: 1px solid #b6d4fe;
        font-size: inherit; color: #0d6efd; display: flex; justify-content: space-between; align-items: baseline; gap: 8px;
      }
      .frecuencia-termino-nombre { font-weight: 500; word-break: break-word; }
      .frecuencia-termino-num { font-weight: 700; white-space: nowrap; }
      .busqueda-noticias { margin-bottom: 0.75rem; }
      .busqueda-noticias .form-group { margin-bottom: 0; }
      .busqueda-noticias .form-control {
        max-width: 320px;
        padding: 6px 12px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        font-size: 0.9rem;
      }
      .busqueda-noticias .form-control:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.2);
      }
      .seccion-ultimas-noticias h4 { margin-bottom: 0.5rem; }
    "))
  ),
  div(style = "padding: 1rem 1rem 0;"),
  h1(class = "dashboard-title", "Titulares: etiquetas y tendencias"),
  p(class = "dashboard-subtitle", "Análisis de términos en titulares y volumen de noticias"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-seccion",
        tags$label(class = "control-label", "Rango de fechas"),
        dateRangeInput(
          "fechas",
          label = NULL,
          start = Sys.Date() - 90,
          end = Sys.Date(),
          max = Sys.Date(),
          language = "es",
          separator = " a "
        ),
        div(class = "preset-buttons",
          actionButton("preset_7",  "Últimos 7 días",  class = "btn-sm"),
          actionButton("preset_30", "Último mes",     class = "btn-sm"),
          actionButton("preset_90", "Último trimestre", class = "btn-sm")
        )
      ),
      conditionalPanel(
        condition = "input.tabs === 'Conceptos'",
        div(class = "sidebar-seccion sidebar-terminos",
          uiOutput("selector_terminos_evol"),
          div(class = "busqueda-termino",
            tags$label(class = "control-label", "Buscar frecuencia de un término"),
            tags$div(
            class = "form-group",
            tags$input(
              id = "busqueda_termino",
              type = "text",
              class = "form-control",
              placeholder = "Ej: presidente, boric…",
              autocomplete = "off",
              style = "width: 100%;"
            )
          ),
            uiOutput("frecuencia_termino")
          )
        )
      )
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
          div(style = "margin-bottom: 2.5rem;",
            plotlyOutput("grafico_evolucion", height = "380px")
          ),
          h4("Top 30 términos del período", style = "margin-top: 0.5rem;"),
          plotlyOutput("grafico_top_terminos", height = "520px"),
          hr(),
          div(class = "seccion-ultimas-noticias",
            h4("Últimas noticias"),
            div(class = "busqueda-noticias",
              textInput("busqueda_titulo", label = NULL, placeholder = "Buscar en titulares…", width = "100%")
            ),
            uiOutput("paginacion_noticias"),
            div(style = "overflow-x: auto; margin-top: 8px;", tableOutput("tabla_noticias")),
            uiOutput("controles_pagina")
          )
        ),
        tabPanel(
          "Volumen de noticias",
          h4("Noticias por día"),
          plotlyOutput("grafico_volumen_tiempo", height = "320px"),
          hr(),
          h4("Distribución por medio"),
          plotlyOutput("grafico_por_medio", height = "380px")
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

  # Misma lista de stopwords que run_analisis_titulos.R (filtrar en gráficos y tarjetas de términos)
  STOPWORDS_GRAFICOS <- c(
    "el", "la", "los", "las", "un", "una", "unos", "unas",
    "y", "o", "pero", "que", "en", "a", "de", "del", "al", "a la",
    "por", "para", "con", "sin", "sobre", "entre", "hasta", "desde",
    "su", "sus", "se", "lo", "le", "como", "más", "menos", "muy",
    "este", "esta", "estos", "estas", "ese", "esa", "eso", "aquél", "aquella",
    "qué", "cuál", "cómo", "cuándo", "dónde", "quién", "cuánto",
    "ser", "es", "son", "fue", "fueron", "ha", "han", "hay", "está", "están",
    "también", "solo", "sólo", "después", "antes", "durante", "tras",
    "según", "contra", "mediante", "excepto", "hacia",
    "no", "ni", "nos", "nosotros", "ante", "bajo", "tras",
    "años", "año", "mes", "meses", "día", "días", "hora", "horas",
    "mil", "nuevo", "nueva", "nuevos", "nuevas", "dos", "tres", "uno",
    "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "ciento", "cientos",
    "primera", "primero", "primer", "segunda", "segundo", "tercera", "tercero", "cuarta", "quinto",
    "otro", "otra", "otros", "otras", "mismo", "misma", "mismos", "mismas",
    "todo", "toda", "todos", "todas", "algo", "alguno", "alguna", "algunos", "algunas",
    "cada", "cual", "cuales", "cualquier", "cualesquiera",
    "puede", "pueden", "poder", "debe", "deben", "deber",
    "sido", "estado", "será", "serán", "había", "habían", "habrá", "habrán",
    "revisa", "cable", "aquí", "tiene", "pide",
    "quot", "amp", "lt", "gt", "nbsp", "mdash", "ndash", "rsquo", "lsquo", "hellip",
    "así"
  )
  n_sw <- length(STOPWORDS_GRAFICOS)
  ph_sw <- paste(sprintf("$%d", seq(3L, length.out = n_sw)), collapse = ", ")

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

  # por_ano: > 2 años → vista año a año; ≤ 2 años → vista mes a mes
  fechas <- reactive({
    req(input$fechas)
    start <- as.Date(input$fechas[1])
    end   <- as.Date(input$fechas[2])
    anos  <- as.numeric(difftime(end, start, units = "days")) / 365.25
    list(start = start, end = end, anos = anos, por_ano = anos > 2)
  })

  # Términos distintos en el período (entero para evitar integer64). Excluye stopwords como en el análisis.
  n_terminos <- reactive({
    f <- fechas()
    q <- paste0("SELECT COUNT(DISTINCT termino) AS n FROM titulos_terminos_diarios WHERE fecha >= $1 AND fecha <= $2 AND termino NOT IN (", ph_sw, ")")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(STOPWORDS_GRAFICOS)))
    as.integer(as.numeric(out$n))
  })

  # Término más frecuente (nombre + frecuencia). total como entero. Excluye stopwords.
  termino_top <- reactive({
    f <- fechas()
    q <- paste0("
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2 AND termino NOT IN (", ph_sw, ")
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 1
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(STOPWORDS_GRAFICOS)))
    if (nrow(out) > 0L) out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Top 10 términos para el selector y gráfico de evolución (elegir 1 a 10). total como entero. Excluye stopwords.
  top_10_evol <- reactive({
    f <- fechas()
    q <- paste0("
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2 AND termino NOT IN (", ph_sw, ")
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 10
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(STOPWORDS_GRAFICOS)))
    if (nrow(out) > 0L) out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Top 30 términos para el gráfico de barras. total como numérico para evitar integer64 en plotly. Excluye stopwords.
  top_30_df <- reactive({
    f <- fechas()
    q <- paste0("
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2 AND termino NOT IN (", ph_sw, ")
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 30
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(STOPWORDS_GRAFICOS)))
    if (nrow(out) > 0L) out$total <- as.numeric(as.integer(out$total))
    out
  })

  # Datos de evolución para términos seleccionados (frecuencia como entero para evitar integer64 en plotly)
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
    out <- dbGetQuery(pool, q, params = params)
    out$fecha <- as.Date(out$fecha)
    out$frecuencia <- as.integer(as.numeric(out$frecuencia))
    out
  })

  # Volumen (total noticias, promedio). Valores numéricos para evitar integer64.
  volumen <- reactive({
    f <- fechas()
    q <- "
      SELECT COALESCE(SUM(total_noticias), 0) AS total,
             COALESCE(COUNT(*), 0) AS dias
      FROM metricas_titulos_diarias
      WHERE fecha >= $1 AND fecha <= $2
    "
    r <- dbGetQuery(pool, q, params = list(f$start, f$end))
    total <- as.numeric(as.integer(r$total))
    dias <- as.numeric(as.integer(r$dias))
    list(total = total, dias = dias, promedio = if (dias > 0) round(total / dias, 1) else 0)
  })

  # Total de noticias en el rango (con filtro de búsqueda en titulares). Siempre entero para paginación.
  total_noticias_rango <- reactive({
    f <- fechas()
    busq <- trimws(if (is.null(input$busqueda_titulo)) "" else input$busqueda_titulo)
    if (nchar(busq) == 0L) {
      q <- "SELECT COUNT(*) AS n FROM noticias WHERE fecha >= $1 AND fecha <= $2"
      out <- dbGetQuery(pool, q, params = list(f$start, f$end))
    } else {
      q <- "SELECT COUNT(*) AS n FROM noticias WHERE fecha >= $1 AND fecha <= $2 AND titulo ILIKE $3"
      out <- dbGetQuery(pool, q, params = list(f$start, f$end, paste0("%", busq, "%")))
    }
    as.integer(as.numeric(out$n))
  })

  page_noticias <- reactiveVal(1L)
  observeEvent(fechas(), { page_noticias(1L) })
  observeEvent(input$busqueda_titulo, { page_noticias(1L) }, ignoreInit = TRUE)

  # Tabla últimas noticias (5 por página; filtrada por búsqueda si hay texto)
  tabla_noticias <- reactive({
    f <- fechas()
    pg <- max(1L, as.integer(page_noticias()))
    busq <- trimws(if (is.null(input$busqueda_titulo)) "" else input$busqueda_titulo)
    if (nchar(busq) == 0L) {
      q <- "
        SELECT titulo, fecha, medio, url
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2
        ORDER BY fecha DESC
        LIMIT 5 OFFSET $3
      "
      params <- list(f$start, f$end, (pg - 1L) * 5L)
    } else {
      q <- "
        SELECT titulo, fecha, medio, url
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2 AND titulo ILIKE $3
        ORDER BY fecha DESC
        LIMIT 5 OFFSET $4
      "
      params <- list(f$start, f$end, paste0("%", busq, "%"), (pg - 1L) * 5L)
    }
    dbGetQuery(pool, q, params = params)
  })

  # Serie temporal: noticias por día (para pestaña Volumen)
  volumen_por_dia <- reactive({
    f <- fechas()
    q <- "
      SELECT fecha, total_noticias
      FROM metricas_titulos_diarias
      WHERE fecha >= $1 AND fecha <= $2
      ORDER BY fecha
    "
    out <- dbGetQuery(pool, q, params = list(f$start, f$end))
    if (nrow(out) > 0L) out$total_noticias <- as.integer(as.numeric(out$total_noticias))
    out
  })

  # Volumen por día y por medio (para gráfico de área apilada)
  volumen_por_dia_por_medio <- reactive({
    f <- fechas()
    q <- "
      SELECT fecha, medio, COUNT(*) AS total
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY fecha, medio
      ORDER BY fecha, medio
    "
    out <- dbGetQuery(pool, q, params = list(f$start, f$end))
    if (nrow(out) == 0) return(out)
    out$fecha <- as.Date(out$fecha)
    out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Distribución por medio (para pestaña Volumen). total como numérico para evitar integer64 en plotly.
  noticias_por_medio <- reactive({
    f <- fechas()
    q <- "
      SELECT medio, COUNT(*) AS total
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY medio
      ORDER BY total DESC
    "
    out <- dbGetQuery(pool, q, params = list(f$start, f$end))
    if (nrow(out) > 0L) out$total <- as.numeric(as.integer(out$total))
    out
  })

  # ---- Outputs ----
  output$card_terminos_distintos <- renderUI({
    n <- n_terminos()
    if (is.na(n)) n <- 0
    div(class = "card-box",
        h5("Términos distintos en el período"),
        div(class = "valor", format(as.integer(n), big.mark = ".", decimal.mark = ","))
    )
  })

  output$card_termino_top <- renderUI({
    tt <- termino_top()
    if (nrow(tt) == 0) {
      val <- "—"
      sub <- "Sin datos"
    } else {
      val <- tt$termino
      sub <- paste(format(as.integer(tt$total), big.mark = ".", decimal.mark = ","), "apariciones")
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
      tags$strong(format(round(v$total), big.mark = ".", decimal.mark = ",")), " noticias en el período · ",
      "promedio ", tags$strong(v$promedio), " por día"
    )
  })

  output$selector_terminos_evol <- renderUI({
    top <- top_10_evol()
    if (nrow(top) == 0) return(NULL)
    choices <- setNames(top$termino, top$termino)
    div(class = "term-chips-box",
      tags$label(class = "control-label", style = "display: block; margin-bottom: 6px;",
        "Términos para el gráfico de evolución"
      ),
      checkboxGroupInput(
        "terminos_evolucion",
        label = NULL,
        choices = choices,
        selected = head(top$termino, 2),
        inline = TRUE
      )
    )
  })

  # Búsqueda parcial: términos que contienen el texto (ILIKE), con su frecuencia en el rango
  resultados_busqueda_termino <- reactive({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0) return(NULL)
    f <- fechas()
    # Patrón: contiene el texto (escapar % y _ para que sean literales)
    patron <- paste0("%", gsub("\\\\", "\\\\\\\\", gsub("_", "\\\\_", gsub("%", "\\\\%", busq, fixed = TRUE), fixed = TRUE), fixed = TRUE), "%")
    q <- "
      SELECT termino, SUM(frecuencia) AS total
      FROM titulos_terminos_diarios
      WHERE fecha >= $1 AND fecha <= $2 AND termino ILIKE $3
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 25
    "
    out <- dbGetQuery(pool, q, params = list(f$start, f$end, patron))
    if (nrow(out) == 0) return(out)
    out$total <- as.integer(as.numeric(out$total))
    out
  })

  output$frecuencia_termino <- renderUI({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0)
      return(tags$p(class = "frecuencia-termino-msg", "Escribe un término arriba para ver coincidencias y su frecuencia en el período."))
    res <- resultados_busqueda_termino()
    if (is.null(res) || nrow(res) == 0)
      return(tags$p(class = "frecuencia-termino-msg", paste0("No se encontraron términos que coincidan con \"", busq, "\".")))
    items <- lapply(seq_len(nrow(res)), function(i) {
      tags$div(
        class = "frecuencia-termino-item",
        tags$span(class = "frecuencia-termino-nombre", res$termino[i]),
        tags$span(class = "frecuencia-termino-num", paste0("(", format(res$total[i], big.mark = ".", decimal.mark = ","), ")"))
      )
    })
    tags$div(class = "frecuencia-termino-lista", items)
  })

  output$grafico_evolucion <- renderPlotly({
    d <- datos_evolucion()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        layout(
          title = list(text = "Selecciona al menos un término en el panel izquierdo.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    d <- mutate(d, fecha = as.Date(fecha), frecuencia = as.numeric(frecuencia))
    por_ano <- fechas()$por_ano
    paleta <- c("#0d6efd", "#e74c3c", "#2ecc71", "#f1c40f", "#9b59b6", "#1abc9c", "#e67e22", "#3498db")
    if (por_ano) {
      d <- d %>% mutate(ano = as.integer(format(fecha, "%Y"))) %>%
        group_by(ano, termino) %>% summarise(frecuencia = sum(frecuencia), .groups = "drop")
      terminos <- unique(d$termino)
      p <- plot_ly()
      for (i in seq_along(terminos)) {
        sub <- d %>% filter(termino == terminos[i]) %>% arrange(ano)
        p <- p %>% add_trace(
          x = sub$ano, y = sub$frecuencia,
          type = "scatter", mode = "lines+markers",
          name = terminos[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 6, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Año: %{x}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos[i], "<extra></extra>")
        )
      }
      anos <- sort(unique(d$ano))
      p <- p %>% layout(
        xaxis = list(title = "Año", tickvals = anos, zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "h", y = 1.02, x = 0.5, xanchor = "center", yanchor = "bottom", title = list(text = "Término")),
        margin = list(b = 70, t = 70, l = 60, r = 50)
      )
    } else {
      # Eje X adaptado al rango: ≤7 días → un tick por día; ≤31 días → cada 2 días; >31 días → mensual
      rango <- range(d$fecha)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      if (dias_rango <= 7) {
        breaks_x <- seq(rango[1], rango[2], by = "1 day")
        tick_fmt <- "%d %b"
      } else if (dias_rango <= 31) {
        breaks_x <- seq(rango[1], rango[2], by = "2 days")
        tick_fmt <- "%d %b"
      } else {
        breaks_x <- seq(rango[1], rango[2], by = "1 month")
        tick_fmt <- "%b %Y"
      }
      terminos <- unique(d$termino)
      p <- plot_ly()
      for (i in seq_along(terminos)) {
        sub <- d %>% filter(termino == terminos[i]) %>% arrange(fecha)
        p <- p %>% add_trace(
          x = sub$fecha, y = sub$frecuencia,
          type = "scatter", mode = "lines+markers",
          name = terminos[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 6, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Fecha: %{x|%d/%m/%Y}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos[i], "<extra></extra>")
        )
      }
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      p <- p %>% layout(
        xaxis = list(
          type = "date",
          title = list(text = "Fecha", standoff = 12),
          tickmode = "array",
          tickvals = tickvals_ms,
          ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45,
          tickfont = list(size = 11),
          zeroline = FALSE,
          showgrid = TRUE,
          range = as.numeric(as.POSIXct(rango, tz = "UTC")) * 1000
        ),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "h", y = 1.02, x = 0.5, xanchor = "center", yanchor = "bottom", title = list(text = "Término")),
        margin = list(b = 130, t = 70, l = 60, r = 50)
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  output$grafico_top_terminos <- renderPlotly({
    top <- top_30_df()
    if (nrow(top) == 0) {
      return(plot_ly() %>%
        layout(
          title = list(text = "No hay datos para el rango elegido.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    top <- mutate(top, termino = reorder(termino, total))
    gg <- ggplot(top, aes(x = total, y = termino,
                          text = paste0("Término: ", termino, "<br>Frecuencia: ", format(round(total), big.mark = ".", decimal.mark = ",")))) +
      geom_col(fill = "#0d6efd") +
      labs(x = "Frecuencia total", y = NULL) +
      theme_minimal(base_size = 12)
    p <- suppressWarnings(ggplotly(gg, tooltip = "text"))
    n_t <- length(p$x$data)
    if (n_t > 0L) p <- style(p, mode = "markers", traces = seq_len(n_t))
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  output$paginacion_noticias <- renderUI({
    total <- total_noticias_rango()
    if (total == 0) return(tags$p("No hay noticias en el rango elegido."))
    pg <- page_noticias()
    desde <- (pg - 1L) * 5L + 1L
    hasta <- as.integer(min(pg * 5L, total))
    tags$p(class = "small-metric", paste0("Mostrando ", desde, "-", hasta, " de ", format(as.integer(total), big.mark = ".", decimal.mark = ","), " (5 por página)"))
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

  # Gráfico: volumen de noticias por medio (área apilada) + línea total. ≤3 años mes a mes, >3 años año a año
  output$grafico_volumen_tiempo <- renderPlotly({
    d <- volumen_por_dia_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        layout(
          title = list(text = "No hay datos para el rango elegido.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    por_ano <- fechas()$por_ano
    medios_orden <- noticias_por_medio()$medio
    if (length(medios_orden) == 0) medios_orden <- unique(d$medio)
    # Paleta por medio (estilo referencia: rojo, azul, verde, amarillo, etc.)
    paleta <- c("#e74c3c", "#3498db", "#2ecc71", "#f1c40f", "#9b59b6", "#1abc9c", "#e67e22", "#95a5a6")
    if (por_ano) {
      d <- d %>% mutate(ano = as.integer(format(fecha, "%Y"))) %>%
        group_by(ano, medio) %>% summarise(total = sum(total), .groups = "drop")
      anos <- unique(d$ano)
      grid <- expand.grid(ano = anos, medio = medios_orden, stringsAsFactors = FALSE)
      d <- left_join(grid, d, by = c("ano", "medio")) %>%
        mutate(total = as.numeric(coalesce(total, 0L)))
      # Una traza por medio (apilada) + línea total
      p <- plot_ly()
      for (i in seq_along(medios_orden)) {
        m <- medios_orden[i]
        sub <- d %>% filter(medio == m) %>% arrange(ano)
        if (nrow(sub) == 0) next
        p <- p %>% add_trace(
          x = sub$ano, y = sub$total,
          type = "scatter", mode = "lines",
          fill = "tonexty", stackgroup = "one",
          name = m,
          line = list(width = 0.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Año: %{x}<br>", m, ": %{y:.0f}<extra></extra>")
        )
      }
      total_serie <- d %>% group_by(ano) %>% summarise(total = sum(total), .groups = "drop")
      p <- p %>% add_trace(
        x = total_serie$ano, y = total_serie$total,
        type = "scatter", mode = "lines",
        name = "Total",
        line = list(color = "#1a1a2e", width = 2),
        fill = "none",
        hovertemplate = "Año: %{x}<br>Total: %{y:.0f}<extra></extra>"
      )
      p <- p %>% layout(
        title = list(text = "VOLUMEN DE NOTICIAS POR MEDIO", font = list(size = 16, color = "#1a1a2e")),
        xaxis = list(title = "Año", tickvals = anos, zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Noticias", zeroline = FALSE, showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)"),
        legend = list(orientation = "h", y = 1.08, x = 0.5, xanchor = "center", yanchor = "bottom"),
        margin = list(t = 80, b = 60, l = 60, r = 40),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
    } else {
      fechas_completas <- seq(min(d$fecha), max(d$fecha), by = "1 day")
      grid <- expand.grid(fecha = fechas_completas, medio = medios_orden, stringsAsFactors = FALSE)
      d <- left_join(grid, d, by = c("fecha", "medio")) %>%
        mutate(total = as.numeric(coalesce(total, 0L)))
      p <- plot_ly()
      for (i in seq_along(medios_orden)) {
        m <- medios_orden[i]
        sub <- d %>% filter(medio == m) %>% arrange(fecha)
        if (nrow(sub) == 0) next
        p <- p %>% add_trace(
          x = sub$fecha, y = sub$total,
          type = "scatter", mode = "lines",
          fill = "tonexty", stackgroup = "one",
          name = m,
          line = list(width = 0.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Fecha: %{x|%d/%m/%Y}<br>", m, ": %{y:.0f}<extra></extra>")
        )
      }
      total_serie <- d %>% group_by(fecha) %>% summarise(total = sum(total), .groups = "drop")
      p <- p %>% add_trace(
        x = total_serie$fecha, y = total_serie$total,
        type = "scatter", mode = "lines",
        name = "Total",
        line = list(color = "#1a1a2e", width = 2),
        fill = "none",
        hovertemplate = "Fecha: %{x|%d/%m/%Y}<br>Total: %{y:.0f}<extra></extra>"
      )
      rango <- range(d$fecha)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      if (dias_rango <= 7) {
        breaks_x <- seq(rango[1], rango[2], by = "1 day")
        tick_fmt <- "%d %b"
      } else if (dias_rango <= 31) {
        breaks_x <- seq(rango[1], rango[2], by = "2 days")
        tick_fmt <- "%d %b"
      } else {
        breaks_x <- seq(rango[1], rango[2], by = "1 month")
        tick_fmt <- "%b %Y"
      }
      p <- p %>% layout(
        title = list(text = "VOLUMEN DE NOTICIAS POR MEDIO", font = list(size = 16, color = "#1a1a2e")),
        xaxis = list(
          type = "date",
          title = "Fecha",
          tickmode = "array",
          tickvals = as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000,
          ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45,
          zeroline = FALSE,
          showgrid = TRUE
        ),
        yaxis = list(title = "Noticias", zeroline = FALSE, showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)"),
        legend = list(orientation = "h", y = 1.08, x = 0.5, xanchor = "center", yanchor = "bottom"),
        margin = list(t = 80, b = 100, l = 60, r = 40),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  # Gráfico: distribución por medio (pestaña Volumen)
  output$grafico_por_medio <- renderPlotly({
    d <- noticias_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        layout(
          title = list(text = "No hay datos para el rango elegido.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    d <- mutate(d, medio = reorder(medio, total))
    gg <- ggplot(d, aes(x = total, y = medio, fill = total,
                        text = paste0("Medio: ", medio, "<br>Noticias: ", format(as.integer(total), big.mark = ".", decimal.mark = ",")))) +
      geom_col() +
      scale_fill_gradient(low = "#6c9bd1", high = "#0d6efd") +
      labs(x = "Número de noticias", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    p <- suppressWarnings(ggplotly(gg, tooltip = "text"))
    n_t <- length(p$x$data)
    if (n_t > 0L) p <- style(p, mode = "markers", traces = seq_len(n_t))
    p %>% config(displayModeBar = TRUE, locale = "es")
  })
}

# ------------------------------------------------------------------------------
# Lanzar app (desde R o RStudio, o: Rscript -e "shiny::runApp('dashboard', port=3838)")
# Desde la carpeta noticias: runApp("dashboard", port = 3838)
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
