# Dashboard de noticias — etiquetas en titulares
# Requiere: shiny, plotly, pool, DBI, RPostgres, ggplot2, dplyr
# Variables: PGHOST, PGPORT, PGUSER_NOTICIAS/PGPASSWORD_NOTICIAS/PGDATABASE_NOTICIAS (o PGUSER/...)
# Lee .env en dashboard/ o en raíz del proyecto (Paginaweb). Datos desde 2018; tabla noticias usa columna fuente (medio).

library(shiny)
library(plotly)
library(pool)
library(DBI)
library(RPostgres)
library(ggplot2)
library(dplyr)
if (requireNamespace("igraph", quietly = TRUE)) library(igraph)

# ------------------------------------------------------------------------------
# Cargar .env desde dashboard/ o raíz (Paginaweb) y configurar pool
# ------------------------------------------------------------------------------
FECHA_DESDE_DASHBOARD <- as.Date("2018-01-01")  # alineado a run_analisis_titulos.R

env_candidates <- c(
  file.path(getwd(), ".env"),
  file.path(getwd(), "..", ".env"),
  file.path(getwd(), "..", "..", ".env")
)
for (env_file in env_candidates) {
  if (file.exists(env_file)) {
    env_lines <- readLines(env_file, warn = FALSE)
    for (line in env_lines) {
      line <- trimws(line)
      if (!nzchar(line) || startsWith(line, "#")) next
      eq_pos <- regexpr("=", line, fixed = TRUE)
      if (eq_pos < 2L) next
      key <- substr(line, 1L, eq_pos - 1L)
      val <- substr(line, eq_pos + 1L, nchar(line))
      val <- gsub('^["\']|["\']$', "", val)
      if (key == "PGHOST")               Sys.setenv(PGHOST               = val)
      if (key == "PGPORT")               Sys.setenv(PGPORT               = val)
      if (key == "PGUSER_NOTICIAS")      Sys.setenv(PGUSER_NOTICIAS      = val)
      if (key == "PGPASSWORD_NOTICIAS")  Sys.setenv(PGPASSWORD_NOTICIAS  = val)
      if (key == "PGDATABASE_NOTICIAS")  Sys.setenv(PGDATABASE_NOTICIAS  = val)
      if (key == "PGUSER")               Sys.setenv(PGUSER               = val)
      if (key == "PGPASSWORD")           Sys.setenv(PGPASSWORD           = val)
      if (key == "PGDATABASE")           Sys.setenv(PGDATABASE           = val)
    }
    break
  }
}

pg_host     <- Sys.getenv("PGHOST", "localhost")
pg_port     <- as.integer(Sys.getenv("PGPORT", "5432"))
pg_user     <- Sys.getenv("PGUSER_NOTICIAS", Sys.getenv("PGUSER", "noticias"))
pg_password <- Sys.getenv("PGPASSWORD_NOTICIAS", Sys.getenv("PGPASSWORD", ""))
pg_db       <- Sys.getenv("PGDATABASE_NOTICIAS", Sys.getenv("PGDATABASE", "noticias_chile"))

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
    con_tmp <- poolCheckout(pool_global)
    tryCatch({
      dbExecute(con_tmp, "
        CREATE OR REPLACE VIEW terminos_unificados_diarios AS
          SELECT termino, fecha, frecuencia FROM titulos_terminos_diarios
          UNION ALL
          SELECT ngrama AS termino, fecha, n AS frecuencia FROM titulos_ngramas_diarios
      ")
      dbExecute(con_tmp, "
        CREATE OR REPLACE VIEW terminos_unificados_por_medio AS
          SELECT termino, fecha, fuente, frecuencia FROM titulos_terminos_por_medio
          UNION ALL
          SELECT ngrama AS termino, fecha, fuente, n AS frecuencia FROM titulos_ngramas_por_medio
      ")
    }, error = function(e) message("Aviso: no se pudieron crear las vistas unificadas: ", e$message))
    poolReturn(con_tmp)
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
    tags$title("Monitor de Noticias Chile — Seguimiento de tendencias en prensa (2018-2026)"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap"),
    tags$script(HTML("
      function syncTabClass() {
        var active = $('.nav-tabs li.active a').attr('data-value') || '';
        if (active === 'M\\u00e1s informaci\\u00f3n') {
          $('body').addClass('tab-mas-info');
        } else {
          $('body').removeClass('tab-mas-info');
        }
      }
      $(document).on('shiny:connected', function() { setTimeout(syncTabClass, 200); });
      $(document).on('click', '.nav-tabs a', function() { setTimeout(syncTabClass, 50); });
      // Forzar resize de Plotly al mostrar cualquier tab (corrige gráficos cortados)
      $(document).on('shown.bs.tab', function() {
        setTimeout(function() { $(window).trigger('resize'); }, 50);
      });
    ")),
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
      .titulo-row {
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        gap: 16px;
        padding-bottom: 0.6rem;
        border-bottom: 3px solid #0d6efd;
        margin-bottom: 0.4rem;
      }
      .titulo-row .dashboard-title {
        border-bottom: none !important;
        padding-bottom: 0 !important;
        margin-bottom: 0 !important;
      }
      .btn-volver-escritorio {
        display: none;
      }
      .standalone .btn-volver-escritorio {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        flex-shrink: 0;
        background: #1a1a2e;
        color: #fff !important;
        border-radius: 6px;
        padding: 7px 14px;
        font-size: 0.8rem;
        font-weight: 600;
        text-decoration: none !important;
        letter-spacing: 0.03em;
        white-space: nowrap;
        transition: background 0.15s;
        margin-bottom: 3px;
      }
      .btn-volver-escritorio:hover {
        background: #0d6efd;
        color: #fff !important;
        text-decoration: none !important;
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
      .sidebar-medio-destacados {
        border-top: 2px solid #dee2e6;
        padding-top: 1rem;
        margin-top: 0.25rem;
      }
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
        cursor: pointer; transition: background 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
      }
      .frecuencia-termino-item:hover {
        background: #cfe2ff; border-color: #0d6efd; box-shadow: 0 0 0 1px rgba(13, 110, 253, 0.25);
      }
      .frecuencia-termino-nombre { font-weight: 500; word-break: break-word; }
      .frecuencia-termino-num { font-weight: 700; white-space: nowrap; }
      .busqueda-noticias { margin-bottom: 0.75rem; }
      .busqueda-noticias .form-group { margin-bottom: 0; }
      .busqueda-noticias .form-control {
        padding: 8px 14px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        font-size: 1.05rem;
      }
      .busqueda-noticias .form-control:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.2);
      }
      .busqueda-noticias .selectize-control { margin-bottom: 0; }
      .busqueda-noticias .selectize-control .selectize-input {
        border: 1px solid #dee2e6 !important;
        border-radius: 8px !important;
        padding: 8px 14px !important;
        font-size: 1.05rem !important;
        box-shadow: none !important;
        min-height: unset !important;
        line-height: 1.5 !important;
      }
      .busqueda-noticias .selectize-control .selectize-input.focus {
        border-color: #0d6efd !important;
        box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.2) !important;
      }
      .seccion-ultimas-noticias h4 { margin-bottom: 0.5rem; }
      .insights-tab {
        width: 100%; max-width: 100%; padding: 0;
        font-size: 1.4rem; line-height: 1.6; color: #2d3748;
      }
      .insights-tab h4 { font-size: 1.75rem; font-weight: 600; color: #1a1a2e; margin-bottom: 1rem; letter-spacing: -0.02em; }
      .insights-subtitle {
        font-size: 1.4rem; color: #4a5568; margin-bottom: 2rem; line-height: 1.6;
      }
      .insight-contenido {
        padding: 0; margin: 0;
        font-size: 1.4rem; line-height: 1.65; color: #2d3748;
      }
      .insight-contenido .insight-titulo {
        font-size: 1.75rem; font-weight: 600; color: #1a1a2e;
        margin: 0 0 1.25rem 0; letter-spacing: -0.02em; padding-bottom: 0.75rem;
        border-bottom: 2px solid #e2e8f0;
      }
      .insight-contenido .insight-texto {
        margin: 0 0 1rem 0; font-size: 1.4rem; color: #2d3748; line-height: 1.65;
      }
      .insight-contenido .insight-texto:last-child { margin-bottom: 0; }
      .insight-contenido a.insight-link { color: #0d6efd; text-decoration: none; border-bottom: 1px solid #0d6efd; }
      .insight-contenido a.insight-link:hover { text-decoration: underline; }
      .insight-contenido .insight-imagen { max-width: 100%; height: auto; display: block; margin: 1rem 0; border-radius: 8px; }
      .insight-contenido .insight-imagenes-fila { display: flex; flex-wrap: wrap; gap: 1rem; margin: 1rem 0; align-items: flex-start; }
      .insight-contenido .insight-imagenes-fila .insight-imagen { flex: 1 1 280px; max-width: 100%; margin: 0; }
      .sidebar-insights { margin-bottom: 0; }
      .sidebar-insights .shiny-options-group { display: flex; flex-direction: column; gap: 0; }
      .sidebar-insights .radio { margin: 0; }
      .sidebar-insights input[type='radio'] { display: none; }
      .sidebar-insights .radio label {
        padding: 0.75rem 0; margin: 0;
        border: none; border-bottom: 1px solid #e2e8f0;
        background: transparent; cursor: pointer; display: block;
        font-size: 1.25rem; font-weight: 400; color: #475569;
        transition: color 0.2s ease, font-weight 0.2s ease;
      }
      .sidebar-insights .radio label:last-of-type { border-bottom: none; }
      .sidebar-insights .radio label:hover { color: #1e293b; }
      .sidebar-insights .radio input:checked + label,
      .sidebar-insights .radio label:has(input:checked) {
        color: #0d6efd; font-weight: 600; background: transparent;
      }
      .mi-header { background: linear-gradient(135deg, #1a1a2e 0%, #0d3b7a 100%); border-radius: 12px; padding: 2.5rem 2rem; margin-bottom: 2rem; color: #fff; }
      .mi-header h1 { font-size: 2rem; font-weight: 700; margin: 0 0 0.5rem 0; color: #fff; letter-spacing: -0.02em; }
      .mi-header p { font-size: 1.05rem; color: rgba(255,255,255,0.8); margin: 0 0 1.25rem 0; }
      .mi-badges { display: flex; flex-wrap: wrap; gap: 8px; }
      .mi-badge { background: rgba(255,255,255,0.15); border: 1px solid rgba(255,255,255,0.3); border-radius: 20px; padding: 4px 14px; font-size: 0.85rem; color: #fff; }
      .mi-card { background: #fff; border: 1px solid #e2e8f0; border-radius: 10px; padding: 1.5rem; margin-bottom: 1.5rem; box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
      .mi-card h3 { font-size: 1.1rem; font-weight: 700; color: #1a1a2e; margin: 0 0 1rem 0; padding-bottom: 0.6rem; border-bottom: 2px solid #e2e8f0; }
      .mi-feature { display: flex; align-items: flex-start; gap: 12px; margin-bottom: 0.85rem; }
      .mi-feature:last-child { margin-bottom: 0; }
      .mi-feature-icon { font-size: 1.4rem; flex-shrink: 0; width: 32px; text-align: center; }
      .mi-feature-text strong { display: block; font-size: 0.95rem; color: #1a1a2e; margin-bottom: 2px; }
      .mi-feature-text span { font-size: 0.88rem; color: #64748b; }
      .mi-chips { display: flex; flex-wrap: wrap; gap: 6px; }
      .mi-chip { font-size: 0.8rem; padding: 3px 10px; border-radius: 20px; border: 1px solid #dee2e6; background: #f8f9fa; color: #374151; }
      .mi-chip-tv { background: #e7f1ff; border-color: #b6d4fe; color: #0d4ea6; }
      .mi-chip-radio { background: #e8f5e9; border-color: #a5d6a7; color: #1b5e20; }
      .mi-chip-print { background: #fff3e0; border-color: #ffcc80; color: #e65100; }
      .mi-chip-digital { background: #f3e8ff; border-color: #d8b4fe; color: #6b21a8; }
      .mi-group-label { font-size: 0.75rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.06em; color: #94a3b8; margin: 0.75rem 0 0.4rem 0; display: block; }
      .mi-step { display: flex; align-items: flex-start; gap: 14px; margin-bottom: 0.9rem; }
      .mi-step:last-child { margin-bottom: 0; }
      .mi-step-num { background: #0d6efd; color: #fff; border-radius: 50%; width: 26px; height: 26px; font-size: 0.8rem; font-weight: 700; display: flex; align-items: center; justify-content: center; flex-shrink: 0; margin-top: 1px; }
      .mi-step-text { font-size: 0.92rem; color: #374151; line-height: 1.5; }
      .mi-footer { display: flex; flex-wrap: wrap; gap: 12px; padding-top: 1rem; border-top: 1px solid #e2e8f0; margin-top: 0.5rem; }
      .mi-footer a { font-size: 0.85rem; color: #0d6efd; text-decoration: none; display: flex; align-items: center; gap: 5px; }
      .mi-footer a:hover { text-decoration: underline; }
      /* Ocultar sidebar en tab Más información */
      body.tab-mas-info .col-sm-3 { display: none !important; }
      body.tab-mas-info .col-sm-9 { width: 100% !important; max-width: 100% !important; padding: 0 1rem !important; }
      body.tab-mas-info .tab-content { width: 100%; }
      /* Win95 aesthetic para Más información */
      .mi-w95 {
        font-family: 'VT323', monospace;
        font-size: 20px;
        color: #000;
        padding: 1.25rem;
        line-height: 1.4;
        width: 100%;
        box-sizing: border-box;
        image-rendering: pixelated;
      }
      .mi-w95-win {
        border: 2px solid #fff;
        border-right-color: #808080;
        border-bottom-color: #808080;
        background: #c0c0c0;
        margin-bottom: 1rem;
      }
      .mi-w95-titlebar {
        background: #000080;
        color: #fff;
        font-family: 'Press Start 2P', monospace;
        font-size: 8px;
        padding: 4px 8px;
        display: flex;
        align-items: center;
        gap: 6px;
        user-select: none;
        line-height: 1.6;
      }
      .mi-w95-titlebar-credit { background: linear-gradient(90deg, #000080 0%, #1084d0 60%, #000080 100%); }
      .mi-w95-body {
        padding: 10px 14px;
        border: 2px solid #808080;
        border-right-color: #fff;
        border-bottom-color: #fff;
        margin: 4px;
      }
      .mi-w95 p { margin: 0 0 5px 0; font-family: 'VT323', monospace; font-size: 20px; color: #000; }
      .mi-w95 a { color: #000080; text-decoration: underline; font-family: 'VT323', monospace; font-size: 20px; }
      .mi-w95 strong { font-weight: bold; }
      .mi-w95-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; }
      .mi-w95-badges { display: flex; flex-wrap: wrap; gap: 6px; margin-top: 8px; }
      .mi-w95-badge {
        background: #000080; color: #fff;
        font-family: 'Press Start 2P', monospace; font-size: 6px;
        padding: 3px 8px;
        border: 2px solid #fff; border-right-color: #808080; border-bottom-color: #808080;
      }
      .mi-w95-blink { animation: blink95 1s step-start infinite; }
      @keyframes blink95 { 0%,100%{opacity:1} 50%{opacity:0} }
      .mi-w95-chips { display: flex; flex-wrap: wrap; gap: 4px; margin-top: 4px; }
      .mi-w95-chip {
        font-family: 'VT323', monospace; font-size: 17px; padding: 1px 8px;
        border: 2px solid #fff; border-right-color: #808080; border-bottom-color: #808080;
        background: #c0c0c0; color: #000;
      }
      .mi-w95-chip-tv    { background: #cce0ff; }
      .mi-w95-chip-radio { background: #ccf0cc; }
      .mi-w95-chip-print { background: #fff0cc; }
      .mi-w95-chip-dig   { background: #f0ccff; }
      .mi-w95-group-label {
        font-family: 'Press Start 2P', monospace; font-size: 7px;
        color: #000080; margin: 10px 0 4px 0; display: block;
      }
      .mi-w95-feature { display: flex; gap: 8px; margin-bottom: 6px; align-items: flex-start; }
      .mi-w95-feature-bullet { color: #000080; font-family: 'Press Start 2P', monospace; font-size: 8px; flex-shrink: 0; padding-top: 4px; }
      .mi-w95-step { display: flex; gap: 8px; margin-bottom: 8px; align-items: flex-start; }
      .mi-w95-step-num {
        background: #000080; color: #fff;
        font-family: 'Press Start 2P', monospace; font-size: 7px;
        min-width: 20px; height: 20px;
        display: flex; align-items: center; justify-content: center; flex-shrink: 0;
        border: 2px solid #fff; border-right-color: #808080; border-bottom-color: #808080;
      }
      .mi-w95-statusbar {
        background: #c0c0c0;
        border-top: 2px solid #808080; border-bottom: 2px solid #fff;
        padding: 3px 10px;
        display: flex; gap: 1rem; flex-wrap: wrap; align-items: center;
        font-family: 'VT323', monospace; font-size: 16px; color: #000;
      }
      .mi-w95-statusbar a { color: #000080; text-decoration: underline; font-size: 16px; }
      .mi-w95-statusbar span { color: #444; margin-left: auto; }
      @media (max-width: 768px) { .mi-w95-grid { grid-template-columns: 1fr; } }
    ")),
    tags$script(HTML("
      (function() {
        if (window.self === window.top) {
          document.documentElement.classList.add('standalone');
        }
      })();
    "))
  ),
  div(style = "padding: 1rem 1rem 0;"),
  div(class = "titulo-row",
    h1(class = "dashboard-title", "Monitor de Noticias Chile — Seguimiento de tendencias en prensa (2018-2026)"),
    tags$a(href = "/", class = "btn-volver-escritorio", "\u229e Escritorio")
  ),
  p(class = "dashboard-subtitle", "Análisis de los temas que dominan los titulares de los principales medios chilenos"),
  div(id = "main-layout-wrapper",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = "input.tabs !== 'Más información'",
          div(class = "sidebar-seccion",
        tags$label(class = "control-label", "Rango de fechas"),
        dateRangeInput(
          "fechas",
          label = NULL,
          start = max(Sys.Date() - 90, as.Date("2018-01-01")),
          end = Sys.Date(),
          min = as.Date("2018-01-01"),
          max = Sys.Date(),
          format = "yyyy-mm-dd",
          language = "es",
          separator = " a "
        ),
        div(class = "preset-buttons",
          actionButton("preset_7",  "Últimos 7 días",  class = "btn-sm"),
          actionButton("preset_30", "Último mes",     class = "btn-sm"),
          actionButton("preset_365", "Último año", class = "btn-sm")
        ),
        conditionalPanel(
          condition = "!(input.tabs === 'Medios' && input.tabs_medios === 'Volumen de datos')",
          div(class = "sidebar-seccion", style = "margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
            tags$label(class = "control-label", "Excluir palabras de los gráficos"),
            tags$div(class = "form-group",
              tags$input(id = "palabras_excluir", type = "text", class = "form-control",
                placeholder = "Ej: chilena, video, esto", autocomplete = "off", style = "width: 100%;")
            ),
            div(style = "margin-top: 6px; display: flex; gap: 6px;",
              actionButton("btn_excluir_aplicar", "Aplicar", class = "btn-sm btn-outline-secondary"),
              actionButton("btn_excluir_limpiar", "Limpiar", class = "btn-sm btn-outline-secondary")
            ),
            uiOutput("palabras_excluidas_chips")
          )
        )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'Más información'",
          div(class = "sidebar-seccion sidebar-insights",
            tags$p(style = "font-size:0.85em; color:#6c757d; padding: 4px 8px;", "Detalles técnicos del proyecto.")
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'Tendencias'",
        div(class = "sidebar-seccion sidebar-terminos",
          uiOutput("selector_terminos_evol"),
          div(class = "busqueda-termino",
            tags$label(class = "control-label", "Buscar una palabra"),
            tags$div(
            class = "form-group",
            tags$input(
              id = "busqueda_termino",
              type = "text",
              class = "form-control",
              placeholder = "Ej: presidente, economía…",
              autocomplete = "off",
              style = "width: 100%;"
            )
          ),
            uiOutput("frecuencia_termino")
          )
        )
      ),
        conditionalPanel(
          condition = "input.tabs === 'Medios' && input.tabs_medios === 'Conceptos por medio'",
          div(class = "sidebar-seccion sidebar-terminos",
            uiOutput("selector_terminos_medios"),
            div(class = "busqueda-termino",
              tags$label(class = "control-label", "Buscar una palabra"),
              tags$div(
                class = "form-group",
                tags$input(
                  id = "busqueda_termino_medios",
                  type = "text",
                  class = "form-control",
                  placeholder = "Ej: presidente, economía…",
                  autocomplete = "off",
                  style = "width: 100%;"
                )
              ),
              uiOutput("frecuencia_termino_medios")
            ),
            div(class = "sidebar-seccion", style = "margin-top: 20rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", "Medio para evolución temporal"),
              selectInput("medio_evolucion_concepto", label = NULL, choices = character(0), selected = NULL)
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'Medios' && input.tabs_medios === 'Términos destacados'",
          div(class = "sidebar-medio-destacados",
            tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", "Elegir medio"),
            uiOutput("selector_medio_terminos"),
            div(style = "margin-top: 27rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", "Términos en el gráfico"),
              uiOutput("selector_evol_terminos_medio")
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'Medios' && input.tabs_medios === 'Volumen de datos'",
          div(class = "sidebar-medio-destacados",
            div(style = "margin-top: 65rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", "Medios en el gráfico"),
              uiOutput("selector_medios_evol_volumen")
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'Medios' && input.tabs_medios === 'Red de palabras'",
          div(class = "sidebar-seccion",
            div(class = "form-group",
              tags$label(class = "control-label", "Medio"),
              selectInput("fuente_red", label = NULL, choices = character(0), selected = NULL)
            ),
            sliderInput("umbral_coocurrencia", "Co-ocurrencia mínima", min = 2L, max = 50L, value = 5L, step = 1L),
            sliderInput("max_nodos_red", "Máximo de nodos", min = 10L, max = 150L, value = 50L, step = 10L)
          )
        )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Tendencias",
          fluidRow(
            column(6, uiOutput("card_terminos_distintos")),
            column(6, uiOutput("card_termino_top"))
          ),
          tags$p(class = "small-metric", uiOutput("texto_volumen")),
          hr(),
          h4("¿Cómo han cambiado los temas con el tiempo?"),
          p(class = "small-metric", "Selecciona palabras en el panel izquierdo para ver cómo evolucionó su presencia en los titulares."),
          div(style = "margin-bottom: 2.5rem;",
            plotlyOutput("grafico_evolucion", height = "380px")
          ),
          h4("Las 30 palabras más mencionadas en el período", style = "margin-top: 0.5rem;"),
          plotlyOutput("grafico_top_terminos", height = "520px"),
          hr(),
          div(class = "seccion-ultimas-noticias",
            h4("Noticias recientes"),
            div(style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 0.5rem;",
              div(class = "busqueda-noticias", style = "flex: 2; min-width: 0;",
                textInput("busqueda_titulo", label = NULL, placeholder = "Buscar en titulares…", width = "100%")
              ),
              div(class = "busqueda-noticias", style = "flex: 1; min-width: 120px;",
                selectInput("filtro_medio_noticias", label = NULL, choices = c("Todos"), selected = "Todos", width = "100%")
              )
            ),
            uiOutput("paginacion_noticias"),
            div(style = "overflow-x: auto; margin-top: 8px;", tableOutput("tabla_noticias")),
            uiOutput("controles_pagina")
          )
        ),
        tabPanel(
          "Medios",
          tabsetPanel(id = "tabs_medios",
            tabPanel(
              "Conceptos por medio",
              h4("¿Qué palabras usa cada medio?"),
              p(class = "small-metric", "Cuántas noticias de cada medio mencionan el término buscado en el titular, dentro del período seleccionado."),
              plotlyOutput("grafico_conceptos_por_medio", height = "600px"),
              hr(),
              h4("¿Cómo evolucionó este concepto en cada medio?"),
              p(class = "small-metric", "Evolución temporal del término buscado por medio de comunicación (top 8 medios por frecuencia total)."),
              plotlyOutput("grafico_evolucion_concepto_por_medio", height = "420px")
            ),
            tabPanel(
              "Términos destacados",
              h4("Los temas favoritos de cada medio"),
              p(class = "small-metric", "Los términos que más aparecen en los titulares del medio seleccionado."),
              plotlyOutput("grafico_terminos_por_medio", height = "500px"),
              hr(),
              h4("¿Cuándo fueron más mencionados estos temas?"),
              p(class = "small-metric", "Evolución temporal de los términos más frecuentes del medio seleccionado. Selecciona los términos en el panel izquierdo."),
              plotlyOutput("grafico_evolucion_terminos_por_medio", height = "380px")
            ),
            tabPanel(
              "Volumen de datos",
              h4("Volumen de noticias por medio"),
              p(class = "small-metric", "Top de medios ordenados por volumen total de noticias publicadas en el período seleccionado."),
              plotlyOutput("grafico_por_medio", height = "700px"),
              hr(),
              h4("Evolución del volumen en el tiempo"),
              p(class = "small-metric", "Noticias publicadas por mes y medio. Selecciona los medios en el panel izquierdo. Vista anual cuando el rango supera los 2 años."),
              plotlyOutput("grafico_evolucion_volumen_por_medio", height = "420px")
            ),
            tabPanel(
              "Red de palabras",
              h4("Palabras que aparecen juntas en los titulares"),
              p(class = "small-metric", "Términos que aparecen en el mismo titular con frecuencia. Cuanto más grueso el nodo, más conexiones tiene."),
              plotlyOutput("red_coocurrencia_plotly", height = "500px")
            ),
          )
        ),
        tabPanel(
          "Más información",
          div(class = "insights-tab",
            uiOutput("mas_informacion_contenido")
          )
        )
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

  # Stopwords centralizadas (fuente única: stopwords.R en raíz del proyecto)
  source("../stopwords.R")
  STOPWORDS_GRAFICOS <- STOPWORDS

  # Presets de fechas
  observeEvent(input$preset_7, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 6, end = Sys.Date())
  })
  observeEvent(input$preset_30, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 29, end = Sys.Date())
  })
  observeEvent(input$preset_365, {
    updateDateRangeInput(session, "fechas", start = Sys.Date() - 364, end = Sys.Date())
  })

  palabras_excluidas_custom <- reactiveVal(character(0))

  observeEvent(input$btn_excluir_aplicar, {
    raw <- trimws(if (is.null(input$palabras_excluir)) "" else input$palabras_excluir)
    if (!nzchar(raw)) return()
    nuevas <- unique(tolower(trimws(strsplit(raw, ",")[[1]])))
    nuevas <- nuevas[nzchar(nuevas)]
    palabras_excluidas_custom(unique(c(palabras_excluidas_custom(), nuevas)))
  })

  observeEvent(input$btn_excluir_limpiar, {
    palabras_excluidas_custom(character(0))
  })

  output$palabras_excluidas_chips <- renderUI({
    words <- palabras_excluidas_custom()
    if (length(words) == 0) return(NULL)
    tags$div(style = "margin-top: 6px; display: flex; flex-wrap: wrap; gap: 4px;",
      lapply(words, function(w) {
        tags$span(style = "background: #e9ecef; border-radius: 4px; padding: 2px 8px; font-size: 0.85em;", w)
      })
    )
  })

  all_stopwords <- reactive({
    unique(c(STOPWORDS_GRAFICOS, palabras_excluidas_custom()))
  })

  tabla_config <- reactive({
    list(tabla = "titulos_terminos_diarios", col_t = "termino", col_f = "frecuencia",
         filtro_tipo = "", tiene_pm = TRUE,
         tabla_pm = "titulos_terminos_por_medio", col_t_pm = "termino", col_f_pm = "frecuencia", filtro_tipo_pm = "")
  })

  # Reverse lookup: canonical lema → variants that map to it (for showing chips in search)
  LEMAS_INVERSO <- local({
    lemas_vec <- c(
      "delincuente" = "delincuencia", "delincuentes" = "delincuencia",
      "criminal" = "crimen", "criminales" = "crimen",
      "corrupto" = "corrupción", "corruptos" = "corrupción",
      "corrupta" = "corrupción", "corruptas" = "corrupción",
      "migrante" = "migración", "migrantes" = "migración",
      "migratorio" = "migración", "migratoria" = "migración",
      "económico" = "economía", "económica" = "economía",
      "económicos" = "economía", "económicas" = "economía",
      "manifestante" = "manifestación", "manifestantes" = "manifestación",
      "protestas" = "protesta",
      "constituyente" = "constitución", "constituyentes" = "constitución",
      "constitucional" = "constitución",
      "pensionado" = "pensión", "pensionados" = "pensión",
      "pensionada" = "pensión", "pensionadas" = "pensión",
      "violento" = "violencia", "violenta" = "violencia",
      "violentos" = "violencia", "violentas" = "violencia",
      "elecciones" = "elección", "electoral" = "elección", "electorales" = "elección",
      "sanitario" = "salud", "sanitaria" = "salud",
      "sanitarios" = "salud", "sanitarias" = "salud",
      "educativo" = "educación", "educativa" = "educación",
      "educativos" = "educación", "educativas" = "educación",
      "terremoto" = "sismo", "terremotos" = "sismo", "sismos" = "sismo"
    )
    inv <- list()
    for (variante in names(lemas_vec)) {
      canon <- lemas_vec[[variante]]
      inv[[canon]] <- c(inv[[canon]], variante)
    }
    inv
  })

  # por_ano: > 2 años → vista año a año; ≤ 2 años → vista mes a mes
  fechas <- reactive({
    req(input$fechas)
    parse_fecha <- function(x) {
      d <- suppressWarnings(as.Date(x))
      if (!is.na(d)) return(d)
      suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
    }
    start <- parse_fecha(input$fechas[1])
    end   <- parse_fecha(input$fechas[2])
    req(!is.na(start), !is.na(end))
    anos  <- as.numeric(difftime(end, start, units = "days")) / 365.25
    list(start = start, end = end, anos = anos, por_ano = anos > 2)
  })

  # Términos distintos en el período (entero para evitar integer64). Excluye stopwords como en el análisis.
  n_terminos <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(3L, length.out = length(sw))), collapse = ", ")
    q <- paste0("SELECT COUNT(DISTINCT ", cfg$col_t, ") AS n FROM ", cfg$tabla, " WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo)
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(sw)))
    as.integer(as.numeric(out$n))
  })

  # Término más frecuente (nombre + frecuencia). total como entero. Excluye stopwords.
  termino_top <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(3L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 1
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(sw)))
    if (nrow(out) > 0L) out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Top 10 términos para el selector y gráfico de evolución (elegir 1 a 10). total como entero. Excluye stopwords.
  top_10_evol <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(3L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 10
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(sw)))
    if (nrow(out) > 0L) out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Términos añadidos por clic desde el buscador de frecuencias (solo esos suben a la lista de evolución)
  terminos_anadidos_por_clic <- reactiveVal(character(0))

  # Términos disponibles para el gráfico de evolución: top 10 + solo los que el usuario ha añadido con clic
  terminos_evolucion_disponibles <- reactive({
    top <- top_10_evol()
    anadidos <- terminos_anadidos_por_clic()
    if (nrow(top) == 0 && length(anadidos) == 0) return(character(0))
    base <- if (nrow(top) > 0) top$termino else character(0)
    unique(c(base, anadidos))
  })

  # Top 30 términos para el gráfico de barras. total como numérico para evitar integer64 en plotly. Excluye stopwords.
  top_30_df <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(3L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 30
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(sw)))
    if (nrow(out) > 0L) out$total <- as.numeric(as.integer(out$total))
    out
  })

  # Datos de evolución para términos seleccionados (frecuencia como entero para evitar integer64 en plotly)
  datos_evolucion <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    terms <- input$terminos_evolucion
    if (is.null(terms) || length(terms) == 0L)
      return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    ph <- paste(sprintf("$%d", seq(3L, length.out = length(terms))), collapse = ", ")
    q <- paste0("
      SELECT fecha, ", cfg$col_t, " AS termino, ", cfg$col_f, " AS frecuencia
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " IN (", ph, ")", cfg$filtro_tipo, "
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

  # Fechas para la tabla de noticias: igual al rango global
  fechas_noticias <- reactive({ fechas() })

  # Dirección de orden para la columna Fecha
  orden_fecha <- reactiveVal("DESC")

  # Total de noticias en el rango (desde 2018; con filtro de búsqueda en titulares y medio)
  total_noticias_rango <- reactive({
    f <- fechas_noticias()
    start <- f$start
    busq <- trimws(if (is.null(input$busqueda_titulo)) "" else input$busqueda_titulo)
    medio <- if (is.null(input$filtro_medio_noticias) || input$filtro_medio_noticias == "Todos") NULL else input$filtro_medio_noticias
    filtro_medio_sql <- if (!is.null(medio)) " AND fuente = $3" else ""
    filtro_busq_sql  <- if (nchar(busq) > 0L) paste0(" AND titulo ~* $", if (!is.null(medio)) 4L else 3L) else ""
    q <- paste0("SELECT COUNT(*) AS n FROM noticias WHERE fecha >= $1 AND fecha <= $2", filtro_medio_sql, filtro_busq_sql)
    params <- list(start, f$end)
    if (!is.null(medio)) params <- c(params, list(medio))
    if (nchar(busq) > 0L) {
      busq_esc <- gsub("([.+*?\\[\\](){}|^$\\\\])", "\\\\\\1", busq, perl = TRUE)
      params <- c(params, list(paste0("\\y", busq_esc, "\\y")))
    }
    out <- dbGetQuery(pool, q, params = params)
    as.integer(as.numeric(out$n))
  })

  page_noticias <- reactiveVal(1L)
  observeEvent(fechas(), { page_noticias(1L) })
  observeEvent(input$busqueda_titulo, { page_noticias(1L) }, ignoreInit = TRUE)
  observeEvent(input$filtro_medio_noticias, { page_noticias(1L) }, ignoreInit = TRUE)
  observe({
    medios <- noticias_por_medio()$medio
    updateSelectInput(session, "filtro_medio_noticias",
      choices = c("Todos", sort(medios)), selected = "Todos")
  })
  observeEvent(input$toggle_fecha_orden, {
    orden_fecha(if (orden_fecha() == "DESC") "ASC" else "DESC")
    page_noticias(1L)
  }, ignoreInit = TRUE)

  # Tabla últimas noticias (5 por página; desde 2018; cada noticia con su medio/fuente)
  tabla_noticias <- reactive({
    f <- fechas_noticias()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    pg <- max(1L, as.integer(page_noticias()))
    dir <- if (orden_fecha() == "ASC") "ASC" else "DESC"
    busq <- trimws(if (is.null(input$busqueda_titulo)) "" else input$busqueda_titulo)
    medio <- if (is.null(input$filtro_medio_noticias) || input$filtro_medio_noticias == "Todos") NULL else input$filtro_medio_noticias
    # Construir cláusulas dinámicamente
    filtro_medio_sql <- if (!is.null(medio)) " AND fuente = $3" else ""
    idx_busq <- if (!is.null(medio)) 4L else 3L
    filtro_busq_sql  <- if (nchar(busq) > 0L) paste0(" AND titulo ~* $", idx_busq) else ""
    idx_offset <- idx_busq + if (nchar(busq) > 0L) 1L else 0L
    q <- paste0(
      "SELECT titulo, fecha, fuente AS medio, url FROM noticias",
      " WHERE fecha >= $1 AND fecha <= $2",
      filtro_medio_sql, filtro_busq_sql,
      " ORDER BY fecha ", dir,
      " LIMIT 5 OFFSET $", idx_offset
    )
    params <- list(start, f$end)
    if (!is.null(medio)) params <- c(params, list(medio))
    if (nchar(busq) > 0L) {
      busq_esc <- gsub("([.+*?\\[\\](){}|^$\\\\])", "\\\\\\1", busq, perl = TRUE)
      params <- c(params, list(paste0("\\y", busq_esc, "\\y")))
    }
    params <- c(params, list((pg - 1L) * 5L))
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

  # Evolución de volumen por medio en el tiempo (pestaña Volumen de datos)
  volumen_por_medio_tiempo <- reactive({
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    if (f$por_ano) {
      q <- "
        SELECT EXTRACT(YEAR FROM fecha)::int AS periodo, fuente AS medio, COUNT(*) AS total
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2
        GROUP BY periodo, fuente
        ORDER BY periodo, fuente
      "
    } else {
      q <- "
        SELECT DATE_TRUNC('month', fecha)::date AS periodo, fuente AS medio, COUNT(*) AS total
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2
        GROUP BY periodo, fuente
        ORDER BY periodo, fuente
      "
    }
    tryCatch({
      r <- dbGetQuery(pool, q, params = list(start, f$end))
      if (nrow(r) == 0) return(r)
      r$total <- as.integer(as.numeric(r$total))
      r
    }, error = function(e) data.frame(periodo = character(), medio = character(), total = integer()))
  })

  output$selector_medios_evol_volumen <- renderUI({
    d <- volumen_por_medio_tiempo()
    if (nrow(d) == 0) return(NULL)
    todos <- d %>% group_by(medio) %>% summarise(total = sum(total), .groups = "drop") %>%
      arrange(desc(total)) %>% pull(medio)
    top3 <- head(todos, 3L)
    current <- isolate(input$medios_evol_volumen_sel)
    sel <- if (!is.null(current) && length(current) > 0) intersect(current, todos) else top3
    if (length(sel) == 0) sel <- top3
    div(class = "term-chips-box", style = "margin-bottom: 0.75rem;",
      checkboxGroupInput(
        "medios_evol_volumen_sel",
        label = NULL,
        choices = setNames(todos, todos),
        selected = sel,
        inline = TRUE
      )
    )
  })

  output$grafico_evolucion_volumen_por_medio <- renderPlotly({
    d <- volumen_por_medio_tiempo()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No hay datos para el rango elegido.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    sel_medios <- input$medios_evol_volumen_sel
    if (!is.null(sel_medios) && length(sel_medios) > 0) d <- d[d$medio %in% sel_medios, ]
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "Selecciona al menos un medio arriba.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 13, color = "#6c757d")) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    por_ano <- fechas()$por_ano
    medios <- unique(d$medio)
    paleta <- colorRampPalette(c(
      "#0d6efd","#e74c3c","#2ecc71","#f1c40f","#9b59b6",
      "#1abc9c","#e67e22","#3498db","#e91e63","#00bcd4",
      "#8bc34a","#ff5722","#795548","#607d8b","#673ab7"
    ))(max(length(medios), 1L))
    p <- plot_ly()
    if (por_ano) {
      for (i in seq_along(medios)) {
        sub <- d %>% filter(medio == medios[i]) %>% arrange(periodo)
        p <- p %>% add_trace(
          x = sub$periodo, y = sub$total, type = "scatter", mode = "lines",
          name = medios[i],
          fill = "tozeroy",
          fillcolor = paste0(paleta[i], "30"),
          line = list(width = 2, color = paleta[i]),
          hovertemplate = paste0("Año: %{x}<br>Noticias: %{y:,.0f}<br>Medio: ", medios[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        xaxis = list(title = "Año", tickvals = sort(unique(d$periodo)), zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Noticias publicadas", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff"
      )
    } else {
      d$periodo <- as.Date(d$periodo)
      rango <- range(d$periodo)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      breaks_x <- seq(rango[1], rango[2], by = "1 month")
      tick_fmt <- if (dias_rango <= 90) "%b %Y" else "%b %Y"
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      for (i in seq_along(medios)) {
        sub <- d %>% filter(medio == medios[i]) %>% arrange(periodo)
        p <- p %>% add_trace(
          x = sub$periodo, y = sub$total, type = "scatter", mode = "lines+markers",
          name = medios[i],
          fill = "tozeroy",
          fillcolor = paste0(paleta[i], "30"),
          line = list(width = 2, color = paleta[i]),
          hovertemplate = paste0("Mes: %{x|%b %Y}<br>Noticias: %{y:,.0f}<br>Medio: ", medios[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        xaxis = list(type = "date", title = list(text = "Mes", standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = -45, tickfont = list(size = 10),
          zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Noticias publicadas", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff"
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  # Distribución por medio (pestaña Medios; columna fuente = medio, desde 2018)
  noticias_por_medio <- reactive({
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    q <- "
      SELECT fuente AS medio, COUNT(*) AS total
      FROM noticias
      WHERE fecha >= $1 AND fecha <= $2
      GROUP BY fuente
      ORDER BY total DESC
    "
    out <- dbGetQuery(pool, q, params = list(start, f$end))
    if (nrow(out) > 0L) out$total <- as.numeric(as.integer(out$total))
    out
  })

  # ---- Pestaña Medios: conceptos por medio ----
  terminos_anadidos_por_clic_medios <- reactiveVal(character(0))

  # Top 10 términos para el selector de Medios (misma lógica que Tendencias)
  top_10_medios <- reactive({
    f <- fechas()
    cfg <- tabla_config()
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(3L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 10
    ")
    out <- dbGetQuery(pool, q, params = c(list(f$start, f$end), as.list(sw)))
    if (nrow(out) > 0L) out$total <- as.integer(as.numeric(out$total))
    out
  })

  terminos_medios_disponibles <- reactive({
    top <- top_10_medios()
    anadidos <- terminos_anadidos_por_clic_medios()
    if (nrow(top) == 0 && length(anadidos) == 0) return(character(0))
    base <- if (nrow(top) > 0) top$termino else character(0)
    unique(c(base, anadidos))
  })

  # Búsqueda de términos para añadir al gráfico Medios
  resultados_busqueda_termino_medios <- reactive({
    busq <- trimws(if (is.null(input$busqueda_termino_medios)) "" else input$busqueda_termino_medios)
    if (nchar(busq) == 0) return(NULL)
    f <- fechas()
    cfg <- tabla_config()
    patron <- paste0("%", gsub("\\\\", "\\\\\\\\", gsub("_", "\\\\_", gsub("%", "\\\\%", busq, fixed = TRUE), fixed = TRUE), fixed = TRUE), "%")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " ILIKE $3", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 25
    ")
    out <- dbGetQuery(pool, q, params = list(f$start, f$end, patron))
    if (nrow(out) == 0) return(out)
    out$total <- as.integer(as.numeric(out$total))
    out
  })

  # Por cada término seleccionado: sumar frecuencias por medio desde la tabla precalculada.
  datos_conceptos_por_medio <- reactive({
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    cfg <- tabla_config()
    terms <- input$terminos_medios
    if (is.null(terms) || length(terms) == 0L) {
      top <- top_10_medios()
      if (nrow(top) == 0) return(data.frame(medio = character(), termino = character(), n = integer()))
      terms <- head(top$termino, 2)
    }
    medios <- noticias_por_medio()$medio
    if (length(medios) == 0L) return(data.frame(medio = character(), termino = character(), n = integer()))

    if (!cfg$tiene_pm) return(data.frame(medio = character(), termino = character(), n = integer()))

    out_list <- vector("list", length(terms))
    for (i in seq_along(terms)) {
      q <- paste0("
        SELECT fuente AS medio, SUM(", cfg$col_f_pm, ") AS n
        FROM ", cfg$tabla_pm, "
        WHERE fecha >= $1 AND fecha <= $2 AND lower(", cfg$col_t_pm, ") = lower($3)", cfg$filtro_tipo_pm, "
        GROUP BY fuente
      ")
      r <- tryCatch(
        dbGetQuery(pool, q, params = list(start, f$end, terms[i])),
        error = function(e) data.frame(medio = character(), n = integer())
      )
      r$termino <- terms[i]
      r$n <- as.integer(as.numeric(r$n))
      grid <- expand.grid(medio = medios, termino = terms[i], stringsAsFactors = FALSE)
      r <- left_join(grid, r, by = c("medio", "termino")) %>% mutate(n = coalesce(n, 0L))
      out_list[[i]] <- r
    }
    bind_rows(out_list)
  })

  output$mas_informacion_contenido <- renderUI({
    repo_prensa   <- "https://github.com/bastianolea/prensa_chile"
    repo_analisis <- "https://github.com/jahadd/analisis-noticias"

    win <- function(title, icon = "\U0001f4c4", ...) {
      tags$div(class = "mi-w95-win",
        tags$div(class = "mi-w95-titlebar", tags$span(icon), tags$span(title)),
        tags$div(class = "mi-w95-body", ...)
      )
    }
    win_credit <- function(title, icon = "\U0001f4c4", ...) {
      tags$div(class = "mi-w95-win",
        tags$div(class = "mi-w95-titlebar mi-w95-titlebar-credit", tags$span(icon), tags$span(title)),
        tags$div(class = "mi-w95-body", ...)
      )
    }

    chip <- function(n, tipo = "") tags$span(class = paste("mi-w95-chip", tipo), n)

    feat <- function(title, desc) {
      tags$div(class = "mi-w95-feature",
        tags$span(class = "mi-w95-feature-bullet", ">"),
        tags$div(tags$strong(title), tags$br(), tags$span(style="color:#333;", desc))
      )
    }

    step <- function(n, title, desc) {
      tags$div(class = "mi-w95-step",
        tags$div(class = "mi-w95-step-num", n),
        tags$div(tags$strong(title), tags$span(style="color:#333;", paste0(" — ", desc)))
      )
    }

    tags$div(class = "mi-w95",

      # ── Barra de título principal ──────────────────────────────────────────
      win("\U0001f4f0  MONITOR DE NOTICIAS CHILE  —  Seguimiento de titulares 2018-2026", icon = "",
        tags$div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px;",
          tags$div(
            tags$p(style="font-size:22px; margin:0 0 6px 0;",
              "Un vistazo en tiempo real a los temas que dominan la prensa chilena."),
            tags$div(class = "mi-w95-badges",
              tags$span(class = "mi-w95-badge", "2018 \u2014 2026"),
              tags$span(class = "mi-w95-badge", "28 MEDIOS"),
              tags$span(class = "mi-w95-badge mi-w95-blink", "EN LINEA")
            )
          )
        )
      ),

      # ── CRÉDITOS ────────────────────────────────────────────────────────────
      win_credit("!!  CREDITOS  !!", icon = "\u2605",
        tags$p(style="font-size:21px; margin-bottom:8px;",
          tags$strong("Datos y scraping: "),
          tags$a(href = repo_prensa, target = "_blank", rel = "noopener",
            "prensa_chile"),
          " de Bastian Olea Herrera. Base de datos con más de 880.000 noticias recopiladas desde 2018."
        ),
        tags$p(style="font-size:21px; margin:0;",
          tags$strong("Pipeline, análisis y dashboard: "),
          tags$a(href = repo_analisis, target = "_blank", rel = "noopener",
            "analisis-noticias"),
          ". Tokenización, frecuencias, co-ocurrencias, sentimiento y visualización interactiva."
        )
      ),

      # ── Dos columnas: ¿Qué es? + ¿Qué puedo hacer? ─────────────────────────
      tags$div(class = "mi-w95-grid",

        win("¿Qué es esto?", icon = "\u2139",
          tags$p("Este dashboard rastrea qué palabras y temas aparecen en los titulares de los principales medios chilenos."),
          tags$p("Permite ver cuándo un tema se volvió tendencia, cómo distintos medios cubren los mismos eventos y qué conceptos aparecen juntos con más frecuencia."),
          tags$p("Los datos se recopilan automáticamente y se procesan para explorar sin leer miles de artículos.")
        ),

        win("¿Qué puedo hacer aquí?", icon = "\u2756",
          feat("Ver tendencias", "Palabras más mencionadas y cómo cambiaron en el tiempo."),
          feat("Comparar medios", "Si distintos medios priorizan los mismos temas."),
          feat("Buscar conceptos", "Cuántas veces apareció una palabra, en qué período y medios."),
          feat("Explorar conexiones", "Términos que aparecen juntos en titulares."),
          feat("Seguir el volumen", "Cuántas noticias publica cada medio y cómo ha evolucionado.")
        )
      ),

      # ── Fuentes cubiertas (ancho completo) ──────────────────────────────────
      win("Fuentes cubiertas — 28 medios chilenos", icon = "\U0001f4f0",
        tags$div(class = "mi-w95-group-label", "\u25ba  TELEVISION"),
        tags$div(class = "mi-w95-chips",
          chip("24horas", "mi-w95-chip-tv"), chip("CHV Noticias", "mi-w95-chip-tv"),
          chip("CNN Chile", "mi-w95-chip-tv"), chip("Meganoticias", "mi-w95-chip-tv"),
          chip("T13", "mi-w95-chip-tv")
        ),
        tags$div(class = "mi-w95-group-label", "\u25ba  RADIO"),
        tags$div(class = "mi-w95-chips",
          chip("ADN Radio", "mi-w95-chip-radio"), chip("Agricultura", "mi-w95-chip-radio"),
          chip("Cooperativa", "mi-w95-chip-radio"), chip("Radio Uchile", "mi-w95-chip-radio")
        ),
        tags$div(class = "mi-w95-group-label", "\u25ba  PRENSA ESCRITA"),
        tags$div(class = "mi-w95-chips",
          chip("Diario Financiero", "mi-w95-chip-print"), chip("El Siglo", "mi-w95-chip-print"),
          chip("Emol", "mi-w95-chip-print"), chip("La Cuarta", "mi-w95-chip-print"),
          chip("La Nación", "mi-w95-chip-print"), chip("La Tercera", "mi-w95-chip-print"),
          chip("Publimetro", "mi-w95-chip-print")
        ),
        tags$div(class = "mi-w95-group-label", "\u25ba  MEDIOS DIGITALES"),
        tags$div(class = "mi-w95-chips",
          chip("Bío Bío", "mi-w95-chip-dig"), chip("CIPER", "mi-w95-chip-dig"),
          chip("El Ciudadano", "mi-w95-chip-dig"), chip("El Desconcierto", "mi-w95-chip-dig"),
          chip("El Dinamo", "mi-w95-chip-dig"), chip("El Mostrador", "mi-w95-chip-dig"),
          chip("Exante", "mi-w95-chip-dig"), chip("Izquierda Diario", "mi-w95-chip-dig"),
          chip("La Hora", "mi-w95-chip-dig"), chip("Quinto Poder", "mi-w95-chip-dig"),
          chip("RedGol", "mi-w95-chip-dig"), chip("The Clinic", "mi-w95-chip-dig")
        )
      ),

      # ── Cómo funciona (ancho completo) ──────────────────────────────────────
      win("¿Cómo funciona?", icon = "\u2699",
        tags$div(class = "mi-w95-grid",
          tags$div(
            step("1", "Recolección", "Un sistema visita periódicamente los 28 medios y guarda sus titulares."),
            step("2", "Identificación", "Cada titular se descompone en palabras clave. Se fusionan nombres de personas, instituciones y partidos.")
          ),
          tags$div(
            step("3", "Análisis", "Se cuenta cuántas veces aparece cada término por día y por medio para detectar tendencias."),
            step("4", "Visualización", "Resultados presentados en este dashboard interactivo. Filtra, busca y explora libremente.")
          )
        )
      ),

      # ── Barra de estado footer ───────────────────────────────────────────────
      tags$div(class = "mi-w95-statusbar",
        tags$a(href = repo_prensa,   target = "_blank", rel = "noopener", "Datos: prensa_chile"),
        tags$span("|"),
        tags$a(href = repo_analisis, target = "_blank", rel = "noopener", "Pipeline: analisis-noticias"),
        tags$span("Licencia MIT \u00b7 Datos sujetos a TOS de cada medio.")
      )
    )
  })

  # ---- Outputs ----
  output$card_terminos_distintos <- renderUI({
    n <- n_terminos()
    if (is.na(n)) n <- 0
    div(class = "card-box",
        h5("Palabras únicas en el período"),
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
        h5("Palabra más mencionada"),
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
    disponibles <- terminos_evolucion_disponibles()
    if (length(disponibles) == 0) return(NULL)
    choices <- setNames(disponibles, disponibles)
    top <- top_10_evol()
    # Selección solo por defecto o lo que el usuario tenga elegido; no auto-añadir términos de la búsqueda
    current_input <- isolate(input$terminos_evolucion)
    if (!is.null(current_input) && length(current_input) > 0) {
      selected <- intersect(current_input, disponibles)
    } else {
      selected <- if (nrow(top) >= 2) head(top$termino, 2) else if (nrow(top) == 1) top$termino else head(disponibles, min(2L, length(disponibles)))
    }
    if (length(selected) == 0 && length(disponibles) > 0)
      selected <- head(disponibles, min(2L, length(disponibles)))
    div(class = "term-chips-box",
      tags$label(class = "control-label", style = "display: block; margin-bottom: 6px;",
        "Términos para comparar"
      ),
      checkboxGroupInput(
        "terminos_evolucion",
        label = NULL,
        choices = choices,
        selected = selected,
        inline = TRUE
      )
    )
  })

  # Búsqueda parcial: términos que contienen el texto (ILIKE), con su frecuencia en el rango
  resultados_busqueda_termino <- reactive({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0) return(NULL)
    f <- fechas()
    cfg <- tabla_config()
    patron <- paste0("%", gsub("\\\\", "\\\\\\\\", gsub("_", "\\\\_", gsub("%", "\\\\%", busq, fixed = TRUE), fixed = TRUE), fixed = TRUE), "%")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " ILIKE $3", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY total DESC
      LIMIT 25
    ")
    out <- dbGetQuery(pool, q, params = list(f$start, f$end, patron))
    if (nrow(out) == 0) return(out)
    out$total <- as.integer(as.numeric(out$total))
    out
  })

  output$frecuencia_termino <- renderUI({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0)
      return(tags$p(class = "frecuencia-termino-msg", "Escribe una palabra arriba para ver cuántas veces aparece en los titulares y sus variantes. Haz clic para añadirla al gráfico."))
    res <- resultados_busqueda_termino()
    if (is.null(res) || nrow(res) == 0)
      return(tags$p(class = "frecuencia-termino-msg", paste0("No se encontraron términos que coincidan con \"", busq, "\".")))
    items <- lapply(seq_len(nrow(res)), function(i) {
      term_esc <- gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", res$termino[i], fixed = TRUE), fixed = TRUE)
      onclick_js <- sprintf("Shiny.setInputValue('termo_añadir_evol', '%s', {priority: 'event'})", term_esc)
      nombre_node <- local({
        t <- res$termino[i]
        variantes_lema <- LEMAS_INVERSO[[t]]
        if (grepl(" ", t, fixed = TRUE)) {
          parts <- strsplit(t, " ", fixed = TRUE)[[1]]
          tagList(
            tags$span(class = "frecuencia-termino-nombre", t),
            tags$span(
              style = "display: inline-flex; gap: 3px; flex-wrap: wrap; margin-left: 5px;",
              lapply(parts, function(p) tags$span(
                style = "font-size: 0.7em; background: #cfe2ff; color: #084298; border-radius: 3px; padding: 1px 5px;",
                p
              ))
            )
          )
        } else if (length(variantes_lema) > 0L) {
          tagList(
            tags$span(class = "frecuencia-termino-nombre", t),
            tags$span(
              style = "display: inline-flex; gap: 3px; flex-wrap: wrap; margin-left: 5px;",
              lapply(variantes_lema, function(v) tags$span(
                style = "font-size: 0.7em; background: #d1e7dd; color: #0a3622; border-radius: 3px; padding: 1px 5px;",
                v
              ))
            )
          )
        } else {
          tags$span(class = "frecuencia-termino-nombre", t)
        }
      })
      tags$div(
        class = "frecuencia-termino-item",
        onclick = onclick_js,
        role = "button",
        nombre_node,
        tags$span(class = "frecuencia-termino-num", paste0("(", format(res$total[i], big.mark = ".", decimal.mark = ","), ")"))
      )
    })
    tags$div(
      tags$p(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 4px;", paste0("Variantes encontradas para '", busq, "':")),
      tags$div(class = "frecuencia-termino-lista", items)
    )
  })

  # Al hacer clic en una casilla de resultado de búsqueda, se añade solo ese término a la lista y al gráfico de evolución
  observeEvent(input$termo_añadir_evol, {
    term <- input$termo_añadir_evol
    if (is.null(term) || !nzchar(trimws(term))) return()
    terminos_anadidos_por_clic(unique(c(terminos_anadidos_por_clic(), term)))
    current <- input$terminos_evolucion
    if (is.null(current)) current <- character(0)
    updateCheckboxGroupInput(session, "terminos_evolucion", selected = unique(c(current, term)))
  })

  output$grafico_evolucion <- renderPlotly({
    d <- datos_evolucion()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
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
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      title = list(text = "Término"), font = list(size = 10, family = "Arial, sans-serif"),
                      tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30)
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
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      title = list(text = "Término"), font = list(size = 10, family = "Arial, sans-serif"),
                      tracegroupgap = 0),
        margin = list(b = 60, t = 30, l = 60, r = 30)
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  output$grafico_top_terminos <- renderPlotly({
    top <- top_30_df()
    if (nrow(top) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No hay datos para el rango elegido.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    top <- top[order(top$total), ]
    plot_ly(
      x = top$total,
      y = factor(top$termino, levels = top$termino),
      type = "bar", orientation = "h",
      marker = list(
        color = colorRampPalette(c("#6c9bd1", "#0d6efd"))(nrow(top)),
        line = list(color = "rgba(255,255,255,0)", width = 0)
      ),
      hovertemplate = "<b>%{y}</b><br>Frecuencia: %{x:,.0f}<extra></extra>"
    ) %>%
    layout(
      xaxis = list(title = "Frecuencia total", zeroline = FALSE, showgrid = TRUE, gridcolor = "#eee"),
      yaxis = list(title = NULL, tickfont = list(size = 11)),
      margin = list(l = 140, r = 30, t = 20, b = 50),
      plot_bgcolor = "#fff",
      paper_bgcolor = "#fff"
    ) %>%
    config(displayModeBar = TRUE, locale = "es")
  })

  output$paginacion_noticias <- renderUI({
    total <- total_noticias_rango()
    dir <- orden_fecha()
    flecha <- if (dir == "DESC") "\u2193" else "\u2191"
    sort_link <- actionLink("toggle_fecha_orden",
      label = paste0("Fecha ", flecha),
      style = "font-size: 0.85em; cursor: pointer;"
    )
    if (total == 0) return(tagList(sort_link, tags$p("No hay noticias en el rango elegido.")))
    pg <- page_noticias()
    desde <- (pg - 1L) * 5L + 1L
    hasta <- as.integer(min(pg * 5L, total))
    tagList(
      tags$div(style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 4px;",
        sort_link,
        tags$span(class = "small-metric", paste0("Mostrando ", desde, "-", hasta, " de ", format(as.integer(total), big.mark = ".", decimal.mark = ","), " (5 por página)"))
      )
    )
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
    names(t)[names(t) == "medio"] <- "Medio"
    t
  }, striped = TRUE, hover = TRUE, sanitize.text.function = function(x) x)

  # ---- UI y gráfico pestaña Medios: panel de términos ----
  output$selector_terminos_medios <- renderUI({
    disponibles <- terminos_medios_disponibles()
    if (length(disponibles) == 0) return(NULL)
    choices <- setNames(disponibles, disponibles)
    top <- top_10_medios()
    current <- isolate(input$terminos_medios)
    if (!is.null(current) && length(current) > 0) {
      selected <- intersect(current, disponibles)
    } else {
      selected <- if (nrow(top) >= 2) head(top$termino, 2) else if (nrow(top) == 1) top$termino else head(disponibles, min(2L, length(disponibles)))
    }
    if (length(selected) == 0) selected <- head(disponibles, min(2L, length(disponibles)))
    div(class = "term-chips-box",
      tags$label(class = "control-label", style = "display: block; margin-bottom: 6px;",
        "Términos para el gráfico por medio"
      ),
      checkboxGroupInput(
        "terminos_medios",
        label = NULL,
        choices = choices,
        selected = selected,
        inline = TRUE
      )
    )
  })

  output$frecuencia_termino_medios <- renderUI({
    busq <- trimws(if (is.null(input$busqueda_termino_medios)) "" else input$busqueda_termino_medios)
    if (nchar(busq) == 0)
      return(tags$p(class = "frecuencia-termino-msg", "Escribe una palabra arriba para ver cuántas veces aparece en los titulares y sus variantes. Haz clic para añadirla al gráfico."))
    res <- resultados_busqueda_termino_medios()
    if (is.null(res) || nrow(res) == 0)
      return(tags$p(class = "frecuencia-termino-msg", paste0("No se encontraron términos que coincidan con \"", busq, "\".")))
    items <- lapply(seq_len(nrow(res)), function(i) {
      term_esc <- gsub("\\\\", "\\\\\\\\", gsub("'", "\\\\'", res$termino[i], fixed = TRUE), fixed = TRUE)
      onclick_js <- sprintf("Shiny.setInputValue('termo_añadir_medios', '%s', {priority: 'event'})", term_esc)
      nombre_node <- local({
        t <- res$termino[i]
        variantes_lema <- LEMAS_INVERSO[[t]]
        if (grepl(" ", t, fixed = TRUE)) {
          parts <- strsplit(t, " ", fixed = TRUE)[[1]]
          tagList(
            tags$span(class = "frecuencia-termino-nombre", t),
            tags$span(
              style = "display: inline-flex; gap: 3px; flex-wrap: wrap; margin-left: 5px;",
              lapply(parts, function(p) tags$span(
                style = "font-size: 0.7em; background: #cfe2ff; color: #084298; border-radius: 3px; padding: 1px 5px;",
                p
              ))
            )
          )
        } else if (length(variantes_lema) > 0L) {
          tagList(
            tags$span(class = "frecuencia-termino-nombre", t),
            tags$span(
              style = "display: inline-flex; gap: 3px; flex-wrap: wrap; margin-left: 5px;",
              lapply(variantes_lema, function(v) tags$span(
                style = "font-size: 0.7em; background: #d1e7dd; color: #0a3622; border-radius: 3px; padding: 1px 5px;",
                v
              ))
            )
          )
        } else {
          tags$span(class = "frecuencia-termino-nombre", t)
        }
      })
      tags$div(
        class = "frecuencia-termino-item",
        onclick = onclick_js,
        role = "button",
        nombre_node,
        tags$span(class = "frecuencia-termino-num", paste0("(", format(res$total[i], big.mark = ".", decimal.mark = ","), ")"))
      )
    })
    tags$div(
      tags$p(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 4px;", paste0("Variantes encontradas para '", busq, "':")),
      tags$div(class = "frecuencia-termino-lista", items)
    )
  })

  observeEvent(input$termo_añadir_medios, {
    term <- input$termo_añadir_medios
    if (is.null(term) || !nzchar(trimws(term))) return()
    terminos_anadidos_por_clic_medios(unique(c(terminos_anadidos_por_clic_medios(), term)))
    current <- input$terminos_medios
    if (is.null(current)) current <- character(0)
    updateCheckboxGroupInput(session, "terminos_medios", selected = unique(c(current, term)))
  })

  # Gráfico: repetición de conceptos por medio (barras agrupadas)
  output$grafico_conceptos_por_medio <- renderPlotly({
    d <- datos_conceptos_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
        layout(
          title = list(text = "Selecciona al menos un término en el panel izquierdo.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    terminos <- unique(d$termino)
    medios_orden <- noticias_por_medio()$medio
    paleta <- c("#0d6efd", "#e74c3c", "#2ecc71", "#f1c40f", "#9b59b6", "#1abc9c", "#e67e22", "#3498db")
    p <- plot_ly()
    for (i in seq_along(terminos)) {
      sub <- d %>% filter(termino == terminos[i]) %>% arrange(match(medio, medios_orden))
      p <- p %>% add_trace(
        data = sub,
        x = ~medio,
        y = ~n,
        type = "bar",
        name = terminos[i],
        marker = list(color = paleta[(i - 1L) %% length(paleta) + 1L]),
        hovertemplate = paste0("%{x}<br>", terminos[i], ": %{y:.0f}<extra></extra>")
      )
    }
    metrica_label <- "Noticias con el término en el titular"
    p <- p %>% layout(
      barmode = "group",
      xaxis = list(title = "Medio", tickangle = -45, categoryorder = "array", categoryarray = medios_orden),
      yaxis = list(title = metrica_label, zeroline = FALSE),
      legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                    title = list(text = "Concepto"), font = list(size = 10, family = "Arial, sans-serif"),
                    tracegroupgap = 0),
      margin = list(b = 80, t = 30, l = 60, r = 30)
    )
    p %>% config(displayModeBar = TRUE, locale = "es")
  })
  outputOptions(output, "grafico_conceptos_por_medio", suspendWhenHidden = FALSE)

  # Gráfico: distribución por medio (pestaña Medios)
  output$grafico_por_medio <- renderPlotly({
    d <- noticias_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
        layout(
          title = list(text = "No hay datos para el rango elegido.", font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    d <- d[order(d$total), ]
    plot_ly(
      x = d$total,
      y = factor(d$medio, levels = d$medio),
      type = "bar", orientation = "h",
      marker = list(
        color = colorRampPalette(c("#6c9bd1", "#0d6efd"))(nrow(d)),
        line = list(color = "rgba(255,255,255,0)", width = 0)
      ),
      hovertemplate = "<b>%{y}</b><br>Noticias: %{x:,.0f}<extra></extra>"
    ) %>%
    layout(
      xaxis = list(title = "Número de noticias", zeroline = FALSE, showgrid = TRUE, gridcolor = "#eee"),
      yaxis = list(title = NULL, tickfont = list(size = 11)),
      margin = list(l = 130, r = 30, t = 20, b = 50),
      plot_bgcolor = "#fff",
      paper_bgcolor = "#fff"
    ) %>%
    config(displayModeBar = TRUE, locale = "es")
  })

  # ---- Términos más repetidos por medio (gráfico de barras) ----
  selected_medio_terminos <- reactiveVal(NULL)

  observeEvent(noticias_por_medio(), {
    medios <- noticias_por_medio()$medio
    if (length(medios) == 0) return()
    actual <- selected_medio_terminos()
    if (is.null(actual) || !actual %in% medios)
      selected_medio_terminos(medios[1])
  }, priority = 10)

  observeEvent(input$medio_terminos_seleccionado, {
    if (is.null(input$medio_terminos_seleccionado) || !nzchar(trimws(input$medio_terminos_seleccionado))) return()
    selected_medio_terminos(input$medio_terminos_seleccionado)
  })

  output$selector_medio_terminos <- renderUI({
    medios <- noticias_por_medio()$medio
    if (length(medios) == 0) return(tags$p(class = "small-metric", "No hay medios en el período."))
    sel <- selected_medio_terminos()
    if (is.null(sel)) sel <- medios[1]
    selectInput("medio_terminos_seleccionado", label = NULL, choices = medios, selected = sel)
  })

  # Actualizar selector de fuente para la red de co-ocurrencia y evolución por concepto
  observe({
    medios <- noticias_por_medio()$medio
    if (length(medios) == 0L) return()
    updateSelectInput(session, "fuente_red", choices = medios, selected = medios[1L])
    updateSelectInput(session, "medio_evolucion_concepto", choices = medios, selected = medios[1L])
  })

  # Reactivo: datos de la red de co-ocurrencia
  coocurrencia_red <- reactive({
    req(input$fuente_red, input$umbral_coocurrencia, input$max_nodos_red)
    f <- fechas()
    sw <- all_stopwords()
    ph_sw <- paste(sprintf("$%d", seq(6L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      WITH sw_list(w) AS (SELECT unnest(ARRAY[", ph_sw, "]::text[])),
      top_terminos AS (
        SELECT termino_a AS t FROM titulos_coocurrencia
          WHERE fuente = $1 AND fecha BETWEEN $2 AND $3
          AND termino_a NOT IN (SELECT w FROM sw_list)
          AND termino_b NOT IN (SELECT w FROM sw_list)
        UNION ALL
        SELECT termino_b FROM titulos_coocurrencia
          WHERE fuente = $1 AND fecha BETWEEN $2 AND $3
          AND termino_a NOT IN (SELECT w FROM sw_list)
          AND termino_b NOT IN (SELECT w FROM sw_list)
      ),
      nodos_top AS (
        SELECT t, COUNT(*) AS freq FROM top_terminos GROUP BY t ORDER BY freq DESC LIMIT $5
      )
      SELECT tc.termino_a, tc.termino_b, SUM(tc.n) AS peso
      FROM titulos_coocurrencia tc
      JOIN nodos_top na ON na.t = tc.termino_a
      JOIN nodos_top nb ON nb.t = tc.termino_b
      WHERE tc.fuente = $1 AND tc.fecha BETWEEN $2 AND $3
      AND tc.termino_a NOT IN (SELECT w FROM sw_list)
      AND tc.termino_b NOT IN (SELECT w FROM sw_list)
      GROUP BY tc.termino_a, tc.termino_b
      HAVING SUM(tc.n) >= $4
      ORDER BY peso DESC
      LIMIT 500
    ")
    tryCatch(
      dbGetQuery(pool, q, params = c(
        list(input$fuente_red, f$start, f$end,
             as.integer(input$umbral_coocurrencia),
             as.integer(input$max_nodos_red)),
        as.list(sw)
      )),
      error = function(e) data.frame(termino_a = character(), termino_b = character(), peso = integer())
    )
  })

  output$red_coocurrencia_plotly <- renderPlotly({
    datos <- coocurrencia_red()
    if (nrow(datos) == 0L) {
      return(plot_ly() %>%
        add_annotations(
          text = "Sin datos para este período o umbral seleccionado.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    if (!requireNamespace("igraph", quietly = TRUE)) {
      return(plot_ly() %>%
        add_annotations(
          text = "Instalar igraph: install.packages('igraph')",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    todos_nodos <- unique(c(datos$termino_a, datos$termino_b))
    g <- igraph::graph_from_data_frame(
      datos[, c("termino_a", "termino_b", "peso")],
      directed = FALSE,
      vertices = data.frame(name = todos_nodos, stringsAsFactors = FALSE)
    )
    set.seed(42L)
    coords <- igraph::layout_with_fr(g)
    nodo_ids <- igraph::V(g)$name
    grado <- igraph::degree(g)
    edge_x <- numeric(0)
    edge_y <- numeric(0)
    for (k in seq_len(igraph::ecount(g))) {
      ends <- igraph::ends(g, k)
      i1 <- match(ends[1], nodo_ids)
      i2 <- match(ends[2], nodo_ids)
      edge_x <- c(edge_x, coords[i1, 1], coords[i2, 1], NA)
      edge_y <- c(edge_y, coords[i1, 2], coords[i2, 2], NA)
    }
    plot_ly() %>%
      add_trace(
        x = edge_x, y = edge_y,
        type = "scatter", mode = "lines",
        line = list(color = "rgba(160,160,190,0.4)", width = 1),
        hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        x = coords[, 1], y = coords[, 2],
        type = "scatter", mode = "markers+text",
        text = nodo_ids,
        textposition = "top center",
        textfont = list(size = 9, color = "#333"),
        marker = list(
          size = 7 + 14 * (grado / max(grado, 1L)),
          color = "#0d6efd",
          opacity = 0.8,
          line = list(color = "#ffffff", width = 1.5)
        ),
        hovertemplate = paste0(nodo_ids, "<br>Conexiones: ", grado, "<extra></extra>"),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        plot_bgcolor = "#fafafa",
        paper_bgcolor = "#fafafa"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Reactivo: sentimiento por fuente
  sentimiento_por_fuente <- reactive({
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    q <- "
      SELECT n.fuente AS medio, ns.sentimiento, COUNT(*) AS n
      FROM noticias n
      JOIN noticias_sentimiento ns ON ns.id = n.id
      WHERE n.fecha >= $1 AND n.fecha <= $2
      GROUP BY n.fuente, ns.sentimiento
      ORDER BY n.fuente, ns.sentimiento
    "
    tryCatch(
      {
        r <- dbGetQuery(pool, q, params = list(start, f$end))
        if (nrow(r) > 0L) r$n <- as.integer(as.numeric(r$n))
        r
      },
      error = function(e) data.frame(medio = character(), sentimiento = character(), n = integer())
    )
  })

  output$grafico_sentimiento_por_fuente <- renderPlotly({
    d <- sentimiento_por_fuente()
    if (nrow(d) == 0L) {
      return(plot_ly() %>%
        add_annotations(
          text = "Sin datos de sentimiento. Ejecutar run_sentimiento.R primero.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    colores_sentimiento <- c("positivo" = "#2ecc71", "neutral" = "#95a5a6", "negativo" = "#e74c3c")
    sentimientos <- unique(d$sentimiento)
    p <- plot_ly()
    for (sent in sentimientos) {
      sub <- d[d$sentimiento == sent, ]
      p <- p %>% add_trace(
        data = sub, x = ~medio, y = ~n,
        type = "bar", name = sent,
        marker = list(color = unname(colores_sentimiento[sent])),
        hovertemplate = paste0("%{x}<br>", sent, ": %{y:.0f}<extra></extra>")
      )
    }
    p <- p %>% layout(
      barmode = "stack",
      xaxis = list(title = "Medio", tickangle = -45),
      yaxis = list(title = "Número de artículos", zeroline = FALSE),
      legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                    title = list(text = "Sentimiento"), font = list(size = 10, family = "Arial, sans-serif"),
                    tracegroupgap = 0),
      margin = list(b = 60, t = 30, l = 60, r = 30)
    )
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  # Poblar selector de medio para gráfico de actores (usa medios ya disponibles en sentimiento_por_fuente)
  observe({
    medios_disp <- sort(unique(sentimiento_por_fuente()$medio))
    if (length(medios_disp) == 0L) medios_disp <- character(0L)
    updateSelectInput(session, "medio_actor_sentimiento",
      choices = medios_disp,
      selected = if (length(medios_disp) > 0L) medios_disp[1] else NULL)
  })

  # Reactivo: sentimiento por actor político para el medio seleccionado
  sentimiento_por_actor <- reactive({
    req(input$medio_actor_sentimiento)
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    tipos <- if (!is.null(input$tipo_actor_filtro) && length(input$tipo_actor_filtro) > 0)
      input$tipo_actor_filtro else c("manual", "auto")
    ph_tipos <- paste(sprintf("$%d", seq(4L, length.out = length(tipos))), collapse = ", ")
    tryCatch({
      r <- dbGetQuery(pool, paste0("
        SELECT actor, sentimiento, SUM(n)::integer AS n
        FROM actores_sentimiento
        WHERE fuente = $1 AND fecha >= $2 AND fecha <= $3 AND tipo IN (", ph_tipos, ")
        GROUP BY actor, sentimiento
        ORDER BY actor, sentimiento
      "), params = c(list(input$medio_actor_sentimiento, start, f$end), as.list(tipos)))
      if (nrow(r) > 0L) r$n <- as.integer(as.numeric(r$n))
      r
    },
    error = function(e) data.frame(actor = character(), sentimiento = character(), n = integer()))
  })

  output$grafico_sentimiento_por_actor <- renderPlotly({
    d <- sentimiento_por_actor()
    sin_datos_msg <- function(msg) {
      plot_ly() %>%
        add_annotations(text = msg, x = 0.5, y = 0.5,
          xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14, color = "#6c757d")) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE)
    }
    if (nrow(d) == 0L) {
      return(sin_datos_msg("Sin datos. Ejecutar run_actores_sentimiento.R primero."))
    }

    # ── Proporciones por actor ────────────────────────────────────────────────
    totales <- aggregate(n ~ actor, data = d, FUN = sum)
    names(totales)[2] <- "total"
    d <- merge(d, totales, by = "actor")
    d$pct <- round(100 * d$n / d$total, 1)

    # Pivot a wide (positivo, negativo)
    pos_d <- d[d$sentimiento == "positivo", c("actor", "pct", "n")]
    neg_d <- d[d$sentimiento == "negativo", c("actor", "pct", "n")]
    names(pos_d)[2:3] <- c("pct_pos", "n_pos")
    names(neg_d)[2:3] <- c("pct_neg", "n_neg")
    wide <- merge(totales, pos_d, by = "actor", all.x = TRUE)
    wide <- merge(wide, neg_d, by = "actor", all.x = TRUE)
    for (col in c("pct_pos", "n_pos", "pct_neg", "n_neg")) {
      wide[[col]][is.na(wide[[col]])] <- 0
    }

    # Ordenar por sentimiento neto (más positivo arriba)
    wide$net <- wide$pct_pos - wide$pct_neg
    wide <- wide[order(wide$net), ]
    wide$actor <- factor(wide$actor, levels = wide$actor)

    # Etiquetas: solo mostrar si barra >= 8 %
    wide$label_pos <- ifelse(wide$pct_pos >= 8, paste0(wide$pct_pos, "%"), "")
    wide$label_neg <- ifelse(wide$pct_neg >= 8, paste0(wide$pct_neg, "%"), "")

    # ── Gráfico divergente ────────────────────────────────────────────────────
    plot_ly(wide) %>%
      add_trace(
        y = ~actor, x = ~pct_pos,
        type = "bar", orientation = "h",
        name = "Positivo",
        marker = list(color = "#27ae60", line = list(width = 0)),
        text = ~label_pos,
        textposition = "inside",
        insidetextanchor = "middle",
        textfont = list(color = "white", size = 11, family = "Arial"),
        hovertemplate = "<b>%{y}</b><br>Positivo: %{x:.1f}%<extra></extra>"
      ) %>%
      add_trace(
        y = ~actor, x = ~-pct_neg,
        type = "bar", orientation = "h",
        name = "Negativo",
        marker = list(color = "#e74c3c", line = list(width = 0)),
        text = ~label_neg,
        textposition = "inside",
        insidetextanchor = "middle",
        textfont = list(color = "white", size = 11, family = "Arial"),
        customdata = ~pct_neg,
        hovertemplate = "<b>%{y}</b><br>Negativo: %{customdata:.1f}%<extra></extra>"
      ) %>%
      layout(
        barmode       = "overlay",
        xaxis         = list(
          title       = list(text = "\u2190 Negativo  |  Positivo \u2192", font = list(size = 12)),
          ticksuffix  = "%",
          tickvals    = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
          ticktext    = c("100%", "75%", "50%", "25%", "0", "25%", "50%", "75%", "100%"),
          zeroline    = TRUE, zerolinecolor = "#333333", zerolinewidth = 2,
          range       = c(-105, 105),
          gridcolor   = "#eeeeee",
          automargin  = TRUE
        ),
        yaxis         = list(
          title       = "",
          automargin  = TRUE,
          tickfont    = list(size = 12)
        ),
        legend        = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08, yanchor = "top",
                             traceorder = "normal",
                             font = list(size = 10, family = "Arial, sans-serif"),
                             tracegroupgap = 0),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(l = 10, r = 20, t = 20, b = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Términos más repetidos por medio: lee de la tabla precalculada, excluyendo stopwords
  terminos_por_medio_df <- reactive({
    req(selected_medio_terminos())
    cfg <- tabla_config()
    if (!cfg$tiene_pm) return(data.frame(termino = character(), freq = integer()))
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    sw <- all_stopwords()
    ph_dyn <- paste(sprintf("$%d", seq(4L, length.out = length(sw))), collapse = ", ")
    q <- paste0("
      SELECT ", cfg$col_t_pm, " AS termino, SUM(", cfg$col_f_pm, ") AS freq
      FROM ", cfg$tabla_pm, "
      WHERE fecha >= $1 AND fecha <= $2 AND fuente = $3 AND ", cfg$col_t_pm, " NOT IN (", ph_dyn, ")", cfg$filtro_tipo_pm, "
      GROUP BY termino
      ORDER BY freq DESC
      LIMIT 25
    ")
    r <- tryCatch(
      dbGetQuery(pool, q, params = c(list(start, f$end, selected_medio_terminos()), as.list(sw))),
      error = function(e) data.frame(termino = character(), freq = integer())
    )
    if (is.null(r) || nrow(r) == 0) return(data.frame(termino = character(), freq = integer()))
    r$freq <- as.integer(as.numeric(r$freq))
    r
  })

  # Evolución temporal de los top términos del medio seleccionado
  evolucion_terminos_por_medio <- reactive({
    req(selected_medio_terminos())
    cfg <- tabla_config()
    if (!cfg$tiene_pm) return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    top <- terminos_por_medio_df()
    if (nrow(top) == 0) return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    terms <- head(top$termino, 30L)
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    ph <- paste(sprintf("$%d", seq(4L, length.out = length(terms))), collapse = ", ")
    q <- paste0("
      SELECT fecha, ", cfg$col_t_pm, " AS termino, SUM(", cfg$col_f_pm, ") AS frecuencia
      FROM ", cfg$tabla_pm, "
      WHERE fecha >= $1 AND fecha <= $2 AND fuente = $3 AND ", cfg$col_t_pm, " IN (", ph, ")", cfg$filtro_tipo_pm, "
      GROUP BY fecha, termino
      ORDER BY fecha
    ")
    tryCatch(
      {
        r <- dbGetQuery(pool, q, params = c(list(start, f$end, selected_medio_terminos()), as.list(terms)))
        r$fecha <- as.Date(r$fecha)
        r$frecuencia <- as.integer(as.numeric(r$frecuencia))
        r
      },
      error = function(e) data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer())
    )
  })

  evolucion_concepto_por_medio <- reactive({
    cfg <- tabla_config()
    if (!cfg$tiene_pm) return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    terms <- input$terminos_medios
    if (is.null(terms) || length(terms) == 0)
      return(data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
    medio_sel <- trimws(if (is.null(input$medio_evolucion_concepto)) "" else input$medio_evolucion_concepto)
    req(nchar(medio_sel) > 0)
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    ph <- paste(sprintf("lower($%d)", seq(4L, length.out = length(terms))), collapse = ", ")
    q <- paste0("
      SELECT fecha, ", cfg$col_t_pm, " AS termino, SUM(", cfg$col_f_pm, ") AS frecuencia
      FROM ", cfg$tabla_pm, "
      WHERE fecha >= $1 AND fecha <= $2 AND fuente = $3 AND lower(", cfg$col_t_pm, ") IN (", ph, ")", cfg$filtro_tipo_pm, "
      GROUP BY fecha, termino
      ORDER BY fecha, termino
    ")
    tryCatch({
      r <- dbGetQuery(pool, q, params = c(list(start, f$end, medio_sel), as.list(terms)))
      if (nrow(r) == 0) return(r)
      r$fecha <- as.Date(r$fecha)
      r$frecuencia <- as.integer(as.numeric(r$frecuencia))
      r
    }, error = function(e) data.frame(fecha = as.Date(character()), termino = character(), frecuencia = integer()))
  })

  output$grafico_evolucion_concepto_por_medio <- renderPlotly({
    terms <- input$terminos_medios
    medio_sel <- trimws(if (is.null(input$medio_evolucion_concepto)) "" else input$medio_evolucion_concepto)
    if (is.null(terms) || length(terms) == 0 || nchar(medio_sel) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "Selecciona un término y un medio para ver la evolución temporal.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 13, color = "#6c757d")) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    d <- evolucion_concepto_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(text = paste0("Sin datos para los términos seleccionados en \"", medio_sel, "\" en este período."),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 13)) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    d <- mutate(d, fecha = as.Date(fecha), frecuencia = as.numeric(frecuencia))
    por_ano <- fechas()$por_ano
    terminos_grafico <- unique(d$termino)
    paleta <- c("#0d6efd", "#e74c3c", "#2ecc71", "#f1c40f", "#9b59b6", "#1abc9c", "#e67e22", "#3498db")
    if (por_ano) {
      d <- d %>% mutate(ano = as.integer(format(fecha, "%Y"))) %>%
        group_by(ano, termino) %>% summarise(frecuencia = sum(frecuencia), .groups = "drop")
      p <- plot_ly()
      for (i in seq_along(terminos_grafico)) {
        sub <- d %>% filter(termino == terminos_grafico[i]) %>% arrange(ano)
        p <- p %>% add_trace(
          x = sub$ano, y = sub$frecuencia, type = "scatter", mode = "lines+markers",
          name = terminos_grafico[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 6, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Año: %{x}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos_grafico[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        title = list(text = paste0("Evolución en \"", medio_sel, "\""), font = list(size = 13), x = 0.5, xanchor = "center"),
        xaxis = list(title = "Año", tickvals = sort(unique(d$ano)), zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 50, l = 60, r = 30),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff"
      )
    } else {
      rango <- range(d$fecha)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      breaks_x <- if (dias_rango <= 7) seq(rango[1], rango[2], by = "1 day") else
                  if (dias_rango <= 31) seq(rango[1], rango[2], by = "2 days") else
                  seq(rango[1], rango[2], by = "1 month")
      tick_fmt <- if (dias_rango <= 31) "%d %b" else "%b %Y"
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      p <- plot_ly()
      for (i in seq_along(terminos_grafico)) {
        sub <- d %>% filter(termino == terminos_grafico[i]) %>% arrange(fecha)
        p <- p %>% add_trace(
          x = sub$fecha, y = sub$frecuencia, type = "scatter", mode = "lines+markers",
          name = terminos_grafico[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Fecha: %{x|%d/%m/%Y}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos_grafico[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        title = list(text = paste0("Evolución en \"", medio_sel, "\""), font = list(size = 13), x = 0.5, xanchor = "center"),
        xaxis = list(type = "date", title = list(text = "Fecha", standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45, tickfont = list(size = 11),
          zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 60, t = 50, l = 60, r = 30),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff"
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  output$grafico_terminos_por_medio <- renderPlotly({
    d <- terminos_por_medio_df()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "Sin datos para el medio elegido en este período.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    d <- head(d[order(d$freq, decreasing = TRUE), ], 15L)
    colores_pie <- c("#a8c8e8","#f4a9a8","#a8d8a8","#f9e4a8","#c8b4e8",
                     "#a8e0d4","#f4c88c","#a8c4e0","#f4b8d0","#a8dce8",
                     "#c8e0a8","#f4c4a8","#c4b8b0","#b8c8d4","#c4b8e0")
    plot_ly(
      labels = d$termino,
      values = d$freq,
      type = "pie",
      textposition = "inside",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hovertemplate = "<b>%{label}</b><br>Frecuencia: %{value:,.0f}<br>%{percent}<extra></extra>",
      marker = list(
        colors = colores_pie[seq_len(nrow(d))],
        line = list(color = "#fff", width = 1.5)
      ),
      showlegend = FALSE
    ) %>%
    layout(
      title = list(
        text = paste0("Top 15 términos — ", selected_medio_terminos()),
        font = list(size = 13), x = 0.5, xanchor = "center"
      ),
      margin = list(l = 20, r = 20, t = 50, b = 20),
      paper_bgcolor = "#fff"
    ) %>%
    config(displayModeBar = TRUE, locale = "es")
  })

  output$selector_evol_terminos_medio <- renderUI({
    top <- terminos_por_medio_df()
    if (nrow(top) == 0) return(NULL)
    terminos <- head(top$termino, 30L)
    current <- isolate(input$terminos_evol_medio_sel)
    sel <- if (!is.null(current) && length(current) > 0) intersect(current, terminos) else head(terminos, 5L)
    if (length(sel) == 0) sel <- head(terminos, 5L)
    div(class = "term-chips-box", style = "margin-bottom: 0.75rem;",
      checkboxGroupInput(
        "terminos_evol_medio_sel",
        label = NULL,
        choices = setNames(terminos, terminos),
        selected = sel,
        inline = TRUE
      )
    )
  })

  output$grafico_evolucion_terminos_por_medio <- renderPlotly({
    d <- evolucion_terminos_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "Sin datos para el medio elegido en este período.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    sel_terms <- input$terminos_evol_medio_sel
    if (!is.null(sel_terms) && length(sel_terms) > 0) d <- d[d$termino %in% sel_terms, ]
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "Selecciona al menos un término arriba.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 13, color = "#6c757d")
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
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
          type = "scatter", mode = "lines+markers", name = terminos[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 6, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Año: %{x}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos[i], "<extra></extra>")
        )
      }
      anos <- sort(unique(d$ano))
      p <- p %>% layout(
        xaxis = list(title = "Año", tickvals = anos, zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30)
      )
    } else {
      rango <- range(d$fecha)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      breaks_x <- if (dias_rango <= 7) seq(rango[1], rango[2], by = "1 day") else if (dias_rango <= 31) seq(rango[1], rango[2], by = "2 days") else seq(rango[1], rango[2], by = "1 month")
      tick_fmt <- if (dias_rango <= 31) "%d %b" else "%b %Y"
      terminos <- unique(d$termino)
      p <- plot_ly()
      for (i in seq_along(terminos)) {
        sub <- d %>% filter(termino == terminos[i]) %>% arrange(fecha)
        p <- p %>% add_trace(
          x = sub$fecha, y = sub$frecuencia,
          type = "scatter", mode = "lines+markers", name = terminos[i],
          line = list(width = 1.5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          marker = list(size = 5, color = paleta[(i - 1L) %% length(paleta) + 1L]),
          hovertemplate = paste0("Fecha: %{x|%d/%m/%Y}<br>Frecuencia: %{y:.0f}<br>Término: ", terminos[i], "<extra></extra>")
        )
      }
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      p <- p %>% layout(
        xaxis = list(
          type = "date", title = list(text = "Fecha", standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45, tickfont = list(size = 11),
          zeroline = FALSE, showgrid = TRUE
        ),
        yaxis = list(title = "Frecuencia", zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 60, t = 30, l = 60, r = 30)
      )
    }
    p %>% config(displayModeBar = TRUE, locale = "es")
  })

  outputOptions(output, "grafico_evolucion_concepto_por_medio", suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_evolucion_volumen_por_medio",  suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_terminos_por_medio",           suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_evolucion_terminos_por_medio", suspendWhenHidden = FALSE)
}

# ------------------------------------------------------------------------------
# Lanzar app (desde R o RStudio, o: Rscript -e "shiny::runApp('dashboard', port=3838)")
# Desde la carpeta noticias: runApp("dashboard", port = 3838)
# Si no se actualizan gráficos o selectores: reiniciar la app (Ctrl+C y volver a runApp)
# y/o vaciar caché del navegador (Ctrl+Shift+R o Cmd+Shift+R para recarga forzada).
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
