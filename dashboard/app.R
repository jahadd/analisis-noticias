# Dashboard de noticias — etiquetas en titulares
# Requiere: shiny, plotly, pool, DBI, RPostgres, ggplot2, dplyr
# Variables: PGHOST, PGPORT, PGUSER_NOTICIAS/PGPASSWORD_NOTICIAS/PGDATABASE_NOTICIAS (o PGUSER/...)
# Lee .env en dashboard/ o en raíz del proyecto (Paginaweb). Datos desde 2018; tabla noticias usa columna fuente (medio).

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(pool)
  library(DBI)
  library(RPostgres)
  library(ggplot2)
  library(dplyr)
  if (requireNamespace("igraph", quietly = TRUE)) library(igraph)
  if (requireNamespace("httr2", quietly = TRUE)) library(httr2)
})

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
      dbname = pg_db,
      onCreate = function(con) {
        tryCatch(dbExecute(con, "SET work_mem = '64MB'"), error = function(e) NULL)
      }
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

source("i18n.R")

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
# ui es function(request) (no un fluidPage estatico) para poder leer la
# cookie babelos_lang y acertar el idioma en el primer render — el dashboard
# vive en el mismo origen que el sitio padre (via /shiny/), asi que el
# navegador ya la envia. El cambio de idioma EN CALIENTE (con el dashboard
# ya abierto) lo hace el shim JS de mas abajo via data-i18n, sin depender de
# este primer render.
ui <- function(request) {
lang <- i18n_lang_from_request(request)
fluidPage(
  tags$head(
    tags$title(i18n_tr("page.title", lang)),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&family=Inter:wght@400;500;600;700&display=swap"),
    tags$script(HTML("
      function syncTabClass() {
        var active = $('.nav-tabs li.active a').attr('data-value') || '';
        if (active === 'mas_info') {
          $('body').addClass('tab-mas-info');
        } else {
          $('body').removeClass('tab-mas-info');
        }
      }
      // En celular, #sidebar-tab-filtros (selector de terminos/medio/sliders
      // de la pestaña activa) se reubica para quedar entre las pestañas y su
      // contenido, en vez de al final de todo junto con el resto del sidebar.
      // Mueve el nodo real (no lo clona) — el id sigue existiendo una sola
      // vez en la pagina, asi que no rompe el binding de Shiny. Selector con
      // hijos directos (.col-sm-9 > .tabbable > .tab-content) para alcanzar
      // solo el tabsetPanel principal, no el anidado 'tabs_medios'.
      function reubicarFiltrosMobile() {
        if (window.innerWidth > 767) return;
        var $filtros = $('#sidebar-tab-filtros');
        var $tabContent = $('#main-layout-wrapper > .row > .col-sm-9 > .tabbable > .tab-content');
        if ($filtros.length && $tabContent.length) {
          $filtros.insertBefore($tabContent);
        }
      }
      $(document).on('shiny:connected', function() { setTimeout(syncTabClass, 200); setTimeout(reubicarFiltrosMobile, 200); });
      $(document).on('click', '.nav-tabs a', function() { setTimeout(syncTabClass, 50); });
      // Forzar resize de Plotly al mostrar cualquier tab (corrige gráficos cortados)
      $(document).on('shown.bs.tab', function() {
        setTimeout(function() { $(window).trigger('resize'); }, 50);
      });
      Shiny.addCustomMessageHandler('toggle_btn_ia', function(msg) {
        var btn = document.getElementById('btn_generar_resumen');
        if (!btn) return;
        btn.disabled = msg.disabled;
        btn.textContent = msg.label;
      });

      /* Traduccion del shell estatico (lo que no re-renderiza Shiny via
         renderUI): titulo, subtitulo, boton de escritorio, labels del
         sidebar y nombres de pestaña. Mantener en sync con dashboard/i18n.R
         (mismas claves) — el contenido dinamico (renderUI/renderPlotly) se
         traduce del lado del servidor via current_lang()/tr(). */
      var BABEL_SHELL = {
        'page.title': {es: 'Monitor de Noticias Chile \\u2014 Seguimiento de tendencias en prensa (2018-2026)', en: 'Chile News Monitor \\u2014 Tracking Press Trends (2018-2026)'},
        'page.subtitle': {es: 'An\\u00e1lisis de los temas que dominan los titulares de los principales medios chilenos', en: 'Analysis of the topics dominating headlines across major Chilean media outlets'},
        'nav.desktop_link': {es: 'Escritorio', en: 'Desktop'},
        'sidebar.date_range': {es: 'Rango de fechas', en: 'Date range'}
      };
      var BABEL_TAB_LABELS = {
        'tendencias': {es: 'Tendencias', en: 'Trends'},
        'medios': {es: 'Medios', en: 'Media'},
        'sentimiento': {es: 'Sentimiento', en: 'Sentiment'},
        'mas_info': {es: 'M\\u00e1s informaci\\u00f3n', en: 'More information'},
        'conceptos_por_medio': {es: 'Conceptos por medio', en: 'Concepts by outlet'},
        'terminos_destacados': {es: 'T\\u00e9rminos destacados', en: 'Top terms'},
        'volumen_datos': {es: 'Volumen de datos', en: 'Data volume'},
        'red_palabras': {es: 'Red de palabras', en: 'Word network'}
      };

      function applyBabelLanguage(lang) {
        if (lang !== 'es' && lang !== 'en') return;
        document.documentElement.setAttribute('lang', lang);
        var titleEntry = BABEL_SHELL['page.title'];
        if (titleEntry) document.title = titleEntry[lang];
        document.querySelectorAll('[data-i18n]').forEach(function(el) {
          var entry = BABEL_SHELL[el.getAttribute('data-i18n')];
          if (entry && entry[lang]) el.textContent = entry[lang];
        });
        document.querySelectorAll('.nav-tabs a[data-value]').forEach(function(a) {
          var entry = BABEL_TAB_LABELS[a.getAttribute('data-value')];
          if (entry && entry[lang]) a.textContent = entry[lang];
        });
        var options = document.querySelectorAll('.news-language-switcher [data-babel-lang]');
        options.forEach(function(btn) {
          btn.classList.toggle('active', btn.getAttribute('data-babel-lang') === lang);
        });
        if (window.Shiny) Shiny.setInputValue('babel_lang', lang);
      }

      function resolveBabelLanguage() {
        try {
          var stored = window.localStorage.getItem('babelos-language');
          if (stored === 'es' || stored === 'en') return stored;
        } catch (e) {}
        return document.documentElement.getAttribute('lang') || 'es';
      }

      $(document).on('shiny:connected', function() {
        applyBabelLanguage(resolveBabelLanguage());
      });
      $(document).on('click', '.news-language-switcher [data-babel-lang]', function() {
        var lang = this.getAttribute('data-babel-lang');
        try { window.localStorage.setItem('babelos-language', lang); } catch (e) {}
        applyBabelLanguage(lang);
      });
      window.addEventListener('message', function(event) {
        if (event.data && event.data.type === 'babelos-language') {
          applyBabelLanguage(event.data.language);
        }
      });
      window.addEventListener('storage', function(event) {
        if (event.key === 'babelos-language') applyBabelLanguage(event.newValue);
      });
    ")),
    tags$style(HTML("
      /* === BASE === */
      body {
        background-color: #f0f4f8;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        font-size: 14px;
      }
      /* === HEADER === */
      .dashboard-title {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        font-size: 3.3rem;
        font-weight: 800;
        color: #0f172a;
        letter-spacing: -0.04em;
        line-height: 1.15;
        margin: 0 0 0.3rem 0;
        padding-bottom: 0;
        border-bottom: none;
      }
      .titulo-row {
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        gap: 16px;
        padding-bottom: 0.9rem;
        border-bottom: 3px solid #0d6efd;
        margin-bottom: 0.75rem;
      }
      .titulo-row .dashboard-title { border-bottom: none !important; padding-bottom: 0 !important; margin-bottom: 0 !important; }
      .dashboard-subtitle {
        font-size: 1.5rem;
        color: #64748b;
        margin: 0 0 1rem 0;
        font-weight: 400;
        letter-spacing: 0.01em;
      }
      .btn-volver-escritorio { display: none; }
      /* Botón Escritorio anclado a la esquina superior derecha, fuera del flujo
         del título. El padding-right se reserva SIEMPRE (también en modo iframe):
         la página wrapper /analisis-noticias-chile superpone su propio botón fijo
         en esa esquina y el título no debe quedar debajo. */
      .titulo-row { position: relative; padding-right: 150px; }
      .standalone .btn-volver-escritorio {
        display: inline-flex; align-items: center; gap: 6px;
        position: absolute; top: 0; right: 0;
        background: #0f172a; color: #fff !important; border-radius: 7px;
        padding: 7px 14px; font-size: 1.38rem; font-weight: 600;
        text-decoration: none !important; letter-spacing: 0.03em;
        white-space: nowrap; transition: background 0.15s;
      }
      .btn-volver-escritorio:hover { background: #0d6efd; color: #fff !important; text-decoration: none !important; }
      .news-language-switcher { display: flex; gap: 4px; }
      .news-language-switcher .lang-btn {
        border: 1px solid #e2e8f0; background: #fff; color: #64748b;
        border-radius: 7px; padding: 5px 10px; font-size: 1.2rem; font-weight: 700;
        letter-spacing: 0.03em; cursor: pointer; transition: all 0.14s;
      }
      .news-language-switcher .lang-btn:hover { border-color: #cbd5e1; color: #0f172a; }
      .news-language-switcher .lang-btn.active { background: #0d6efd; border-color: #0d6efd; color: #fff; }
      /* === LAYOUT === */
      .container-fluid { padding-left: 2rem !important; padding-right: 2rem !important; }
      /* Main content column: breathing room from sidebar */
      .col-sm-9 { padding-left: 2rem !important; padding-right: 2.5rem !important; }
      /* Sidebar left margin */
      .col-sm-3 { padding-left: 0 !important; padding-right: 0 !important; }
      /* === SIDEBAR === */
      .well {
        background: #ffffff !important;
        border: none !important;
        border-right: 1px solid #e8edf3 !important;
        border-radius: 0 !important;
        box-shadow: 3px 0 20px rgba(15,23,42,0.06) !important;
        padding: 1.5rem 1.1rem 2rem 1.2rem !important;
        min-height: 100vh;
        margin-bottom: 0 !important;
      }
      .sidebar-seccion { margin-bottom: 1.1rem; }
      .sidebar-seccion .control-label {
        font-weight: 600; font-size: 1.32rem; letter-spacing: 0.07em;
        text-transform: uppercase; margin-bottom: 8px;
        display: block; color: #64748b;
      }
      .sidebar-terminos { margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #e2e8f0; }
      .sidebar-medio-destacados { border-top: 1px solid #e2e8f0; padding-top: 1rem; margin-top: 0.25rem; }
      /* === PRESET BUTTONS === */
      .preset-buttons { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 5px; margin: 8px 0 0 0; }
      .preset-buttons .btn {
        font-size: 1.32rem; font-weight: 500; padding: 8px 4px;
        background: #f8fafc; border: 1px solid #e2e8f0; color: #475569;
        border-radius: 6px; text-align: center; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
        transition: all 0.14s;
      }
      .preset-buttons .btn:hover { background: #e2e8f0; border-color: #cbd5e1; color: #0f172a; }
      /* === IA SIDEBAR BUTTON === */
      .btn-ia-sidebar {
        width: 100%; display: flex; align-items: center; justify-content: center;
        gap: 8px;
        background: linear-gradient(135deg, #ede9fe 0%, #e0e7ff 100%);
        border: 1.5px solid #c4b5fd; color: #5b21b6;
        border-radius: 10px; padding: 13px 0;
        font-size: 1.19rem; font-weight: 600; cursor: pointer;
        transition: all 0.18s; line-height: 1.4;
        box-shadow: 0 2px 8px rgba(124,58,237,0.10);
      }
      .btn-ia-sidebar:hover, .btn-ia-sidebar:focus {
        background: linear-gradient(135deg, #ddd6fe 0%, #c7d2fe 100%);
        border-color: #7c3aed; color: #4c1d95; outline: none;
        box-shadow: 0 4px 14px rgba(124,58,237,0.18);
        transform: translateY(-1px);
      }
      /* === TAB NAVIGATION === */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0 !important;
        margin-bottom: 0 !important;
        gap: 0;
      }
      .nav-tabs > li > a {
        border: none !important;
        border-bottom: 2px solid transparent !important;
        border-radius: 0 !important;
        padding: 0.85rem 1.4rem !important;
        color: #64748b !important;
        font-weight: 500 !important;
        font-size: 1.5rem !important;
        letter-spacing: 0.01em;
        margin-bottom: -2px !important;
        background: transparent !important;
        transition: color 0.15s, border-color 0.15s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #0d6efd !important;
        border-bottom: 2px solid #0d6efd !important;
        background: transparent !important;
        font-weight: 600 !important;
      }
      .nav-tabs > li > a:hover {
        color: #0f172a !important;
        border-bottom: 2px solid #cbd5e1 !important;
        background: transparent !important;
      }
      .tab-content { padding-top: 1.5rem; }
      /* === SECTION HEADINGS === */
      .tab-content h4 {
        font-size: 1.73rem;
        font-weight: 700;
        color: #0f172a;
        letter-spacing: -0.01em;
        margin: 0 0 0.25rem 0;
        padding: 0;
      }
      /* === SECTION CARDS (chart wrappers) === */
      .chart-card {
        background: #ffffff;
        border: 1px solid #e8edf3;
        border-radius: 14px;
        padding: 1.4rem 1.4rem 1.1rem 1.4rem;
        margin-bottom: 1.4rem;
        box-shadow: 0 2px 14px rgba(15,23,42,0.07), 0 1px 3px rgba(15,23,42,0.04);
        overflow: visible;
      }
      .chart-card h4 { margin-bottom: 0.2rem; }
      /* === METRIC CARDS === */
      .small-metric { font-size: 1.47rem; color: #64748b; margin: 4px 0 0.75rem 0; }
      .card-box {
        padding: 20px 22px;
        border-radius: 14px;
        background: #ffffff;
        border: 1px solid #e8edf3;
        box-shadow: 0 2px 14px rgba(15,23,42,0.07), 0 1px 3px rgba(15,23,42,0.04);
      }
      .card-box .valor {
        font-size: 3.15rem;
        font-weight: 800;
        color: #0d6efd;
        letter-spacing: -0.04em;
        line-height: 1;
        display: block;
        margin-bottom: 4px;
      }
      .card-box .etiqueta {
        font-size: 1.32rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #94a3b8;
      }
      /* === SENTIMENT === */
      .sent-kpi { position: relative; overflow: hidden; }
      .sent-kpi::before { content:''; position:absolute; top:0; left:0; right:0; height:4px; background: var(--c, #0d6efd); }
      .sent-kpi .valor { color: var(--c, #0d6efd); }
      .sent-kpi.neg { --c:#dc2626; } .sent-kpi.neu { --c:#94a3b8; } .sent-kpi.pos { --c:#16a34a; } .sent-kpi.net { --c:#0d6efd; }
      .sent-kpi .delta { font-size:1.3rem; font-weight:700; margin-left:7px; letter-spacing:0; }
      .sent-kpi .delta.up { color:#16a34a; } .sent-kpi .delta.down { color:#dc2626; } .sent-kpi .delta.flat { color:#94a3b8; }
      .sent-bar { display:flex; height:38px; border-radius:10px; overflow:hidden; border:1px solid #e8edf3; box-shadow: inset 0 1px 2px rgba(15,23,42,0.05); }
      .sent-bar .seg { display:flex; align-items:center; justify-content:center; color:#fff; font-weight:700; font-size:1.4rem; min-width:0; text-shadow:0 1px 2px rgba(0,0,0,0.12); transition:flex-basis .25s ease; }
      .sent-bar .seg.neg { background:#dc2626; } .sent-bar .seg.neu { background:#94a3b8; } .sent-bar .seg.pos { background:#16a34a; }
      .sent-legend { display:flex; gap:18px; margin-top:10px; font-size:1.3rem; color:#64748b; }
      .sent-legend span { display:inline-flex; align-items:center; gap:6px; }
      .sent-legend i { width:11px; height:11px; border-radius:3px; display:inline-block; }
      .sent-chip { display:inline-block; padding:3px 12px; border-radius:20px; font-size:1.28rem; font-weight:600; white-space:nowrap; }
      .sent-chip.neg { background:#fef2f2; color:#b91c1c; border:1px solid #fecaca; }
      .sent-chip.neu { background:#f1f5f9; color:#475569; border:1px solid #e2e8f0; }
      .sent-chip.pos { background:#f0fdf4; color:#15803d; border:1px solid #bbf7d0; }
      .sent-tabla { width:100%; border-collapse:collapse; }
      .sent-tabla td { padding:11px 8px; border-bottom:1px solid #f1f5f9; font-size:1.46rem; color:#1e293b; vertical-align:middle; line-height:1.35; }
      .sent-tabla tr:hover td { background:#f8fafc; }
      .sent-tabla .meta-celda { white-space:nowrap; }
      .sent-tabla .fecha { color:#94a3b8; font-size:1.25rem; display:block; }
      .sent-tabla .medio { color:#64748b; font-size:1.25rem; font-weight:600; display:block; }
      .sent-tabla .conf { color:#94a3b8; font-size:1.22rem; white-space:nowrap; text-align:right; }
      .sent-meta { font-size:1.34rem; color:#94a3b8; margin:2px 4px 1.4rem 4px; }
      .sent-meta b { color:#475569; font-weight:700; }
      .sent-aviso-rango {
        display:flex; align-items:baseline; gap:9px;
        background:#fffbeb; border:1px solid #fde68a; border-radius:10px;
        padding:10px 16px; margin-bottom:1.4rem;
        font-size:1.36rem; color:#92400e; line-height:1.45;
      }
      .sent-aviso-rango b { font-weight:700; }
      /* sentimiento: barra interactiva + control de medio */
      .sent-bar .seg { cursor:pointer; transition:flex-basis .25s ease, opacity .15s ease, filter .15s ease; }
      .sent-bar .seg:hover { filter:brightness(1.09); }
      .sent-bar.has-sel .seg { opacity:0.38; }
      .sent-bar.has-sel .seg.active { opacity:1; box-shadow: inset 0 0 0 3px rgba(255,255,255,0.9); }
      .sent-filtro-activo { margin-top:11px; font-size:1.34rem; color:#475569; display:flex; align-items:center; gap:9px; flex-wrap:wrap; }
      .sent-filtro-activo .limpiar { color:#0d6efd; cursor:pointer; font-weight:600; text-decoration:none; border:none; background:none; padding:0; font-size:1.34rem; }
      .sent-filtro-activo .limpiar:hover { text-decoration:underline; }
      .sent-control { display:flex; align-items:flex-end; gap:14px; flex-wrap:wrap; margin-bottom:1.4rem; padding:1.1rem 1.4rem; }
      .sent-control .titulo { flex:1; min-width:200px; }
      .sent-control .titulo h4 { margin:0 0 2px 0; }
      .sent-control .selector { min-width:230px; }
      .sent-control .selector .control-label { font-weight:600; font-size:1.2rem; letter-spacing:0.07em; text-transform:uppercase; color:#64748b; margin-bottom:5px; display:block; }
      .sent-hint { font-size:1.28rem; color:#94a3b8; margin:3px 0 0 0; }
      /* sentimiento: fila superior — tono neto (card aparte) + distribución con selector */
      .sent-toprow { display:flex; flex-wrap:wrap; gap:1.4rem; margin-bottom:1.4rem; align-items:stretch; }
      .sent-net-card { flex:1 1 280px; display:flex; flex-direction:column; justify-content:center; border-radius:14px; border:1px solid #e8edf3; padding:1.7rem 1.9rem; box-shadow:0 2px 14px rgba(15,23,42,0.07),0 1px 3px rgba(15,23,42,0.04); }
      .sent-net-card .lbl { font-size:1.22rem; font-weight:700; text-transform:uppercase; letter-spacing:0.11em; opacity:0.72; }
      .sent-net-card .big { font-size:4.6rem; font-weight:800; line-height:1; letter-spacing:-0.045em; margin:8px 0 7px; }
      .sent-net-card .desc { font-size:1.5rem; font-weight:600; opacity:0.9; }
      .sent-net-card .vs { font-size:1.16rem; font-weight:600; text-transform:uppercase; letter-spacing:0.07em; margin-top:11px; opacity:0.6; }
      .sent-net-card.neg { background:linear-gradient(135deg,#fff5f5,#fcdcdc); color:#b91c1c; border-color:#f7c9c9; }
      .sent-net-card.pos { background:linear-gradient(135deg,#f2fdf5,#d3f4dd); color:#15803d; border-color:#bfe9cb; }
      .sent-net-card.neu { background:linear-gradient(135deg,#fbfcfe,#eaeff5); color:#475569; }
      .sent-distrib-card { flex:2 1 430px; margin-bottom:0; }
      .sent-card-head { display:flex; justify-content:space-between; align-items:center; gap:16px; flex-wrap:wrap; margin-bottom:16px; }
      .sent-card-head h4 { margin:0; }
      .sent-medio { display:flex; align-items:center; gap:11px; }
      .sent-medio .lbl { font-size:1.16rem; font-weight:700; text-transform:uppercase; letter-spacing:0.08em; color:#94a3b8; white-space:nowrap; }
      .sent-medio .form-group { margin:0; }
      .sent-medio .selectize-control { margin:0; min-width:200px; }
      .sent-medio .selectize-input { border-radius:9px; border:1.5px solid #e2e8f0; padding:9px 14px; font-size:1.4rem; font-weight:600; color:#0f172a; box-shadow:none; }
      .sent-medio .selectize-input.focus { border-color:#0d6efd; box-shadow:0 0 0 3px rgba(13,110,253,0.12); }
      .sent-legend.vals span { font-weight:600; color:#475569; font-size:1.32rem; }
      .sent-paginacion .btn-sm { font-size:1.28rem; border-radius:8px; }
      /* === HR SEPARATORS === */
      hr { border: none; border-top: 1px solid #e2e8f0; margin: 1.25rem 0; }
      /* === FORMS & INPUTS === */
      .form-control {
        border-radius: 7px !important;
        border-color: #e2e8f0 !important;
        font-size: 1.5rem !important;
        color: #0f172a !important;
        box-shadow: none !important;
      }
      .form-control:focus {
        border-color: #0d6efd !important;
        box-shadow: 0 0 0 3px rgba(13,110,253,0.1) !important;
      }
      .busqueda-termino { margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #e2e8f0; }
      .busqueda-termino .control-label {
        font-weight: 600; font-size: 1.32rem; letter-spacing: 0.07em;
        text-transform: uppercase; margin-bottom: 8px; display: block; color: #64748b;
      }
      .busqueda-termino .form-group { margin-bottom: 0.5rem; }
      .busqueda-termino .form-control {
        width: 100%; max-width: 100%; padding: 9px 13px;
        border-radius: 7px; border: 1px solid #e2e8f0; font-size: 1.5rem;
      }
      .busqueda-termino .form-control:focus {
        border-color: #0d6efd; box-shadow: 0 0 0 3px rgba(13,110,253,0.1); outline: none;
      }
      .busqueda-termino .form-control::placeholder { color: #94a3b8; }
      /* === TERM CHIPS === */
      .term-chips-box { padding: 6px 0; }
      .term-chips-box .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
      .term-chips-box .checkbox { margin: 0; }
      .term-chips-box .checkbox-inline {
        margin: 0; padding: 7px 16px; border-radius: 20px;
        border: 1.5px solid #e2e8f0; background: #f8fafc; cursor: pointer;
        font-size: 1.43rem; font-weight: 500; color: #475569;
        transition: all 0.14s;
      }
      .term-chips-box .checkbox-inline:hover { background: #f1f5f9; border-color: #cbd5e1; }
      .term-chips-box label.checkbox-inline:has(input:checked) {
        background: #0d6efd; border-color: #0d6efd; color: #fff;
      }
      .term-chips-box label { cursor: pointer; }
      .term-chips-box input[type='checkbox'] { display: none; }
      /* === FREQUENCY ITEMS === */
      .frecuencia-termino-valor {
        margin-top: 8px; padding: 10px 14px;
        border-radius: 8px; background: #eff6ff; border: 1px solid #bfdbfe;
        font-size: 1.5rem; color: #1d4ed8;
      }
      .frecuencia-termino-valor .numero { font-size: 1.88rem; font-weight: 700; }
      .frecuencia-termino-msg { font-size: 1.43rem; color: #94a3b8; margin: 8px 0 0 0; }
      .frecuencia-termino-lista { margin-top: 8px; max-height: 320px; overflow-y: auto; }
      .frecuencia-termino-item {
        padding: 8px 12px; margin-bottom: 4px; border-radius: 7px;
        background: #eff6ff; border: 1px solid #bfdbfe;
        font-size: 1.46rem; color: #1d4ed8;
        display: flex; justify-content: space-between; align-items: baseline; gap: 8px;
        cursor: pointer; transition: all 0.13s;
      }
      .frecuencia-termino-item:hover {
        background: #dbeafe; border-color: #0d6efd;
        box-shadow: 0 0 0 1px rgba(13,110,253,0.2);
      }
      .frecuencia-termino-nombre { font-weight: 500; word-break: break-word; }
      .frecuencia-termino-num { font-weight: 700; white-space: nowrap; }
      /* === NEWS SEARCH === */
      .busqueda-noticias { margin-bottom: 0.6rem; }
      .busqueda-noticias .form-group { margin-bottom: 0; }
      .busqueda-noticias .form-control {
        padding: 9px 15px; border-radius: 7px; border: 1px solid #e2e8f0; font-size: 1.19rem;
      }
      .busqueda-noticias .form-control:focus { border-color: #0d6efd; box-shadow: 0 0 0 3px rgba(13,110,253,0.1); }
      .busqueda-noticias .selectize-control { margin-bottom: 0; }
      .busqueda-noticias .selectize-control .selectize-input {
        border: 1px solid #e2e8f0 !important; border-radius: 7px !important;
        padding: 9px 15px !important; font-size: 1.19rem !important;
        box-shadow: none !important; min-height: unset !important; line-height: 1.5 !important;
      }
      .busqueda-noticias .selectize-control .selectize-input.focus {
        border-color: #0d6efd !important; box-shadow: 0 0 0 3px rgba(13,110,253,0.1) !important;
      }
      /* === NEWS SECTION === */
      .seccion-ultimas-noticias { background: #fff; border: 1px solid #e8edf3; border-radius: 14px; padding: 1.4rem 1.4rem 1rem; box-shadow: 0 2px 14px rgba(15,23,42,0.07), 0 1px 3px rgba(15,23,42,0.04); }
      .seccion-ultimas-noticias h4 { margin-bottom: 0.75rem; }
      /* === TABLE === */
      .shiny-output-container .table { font-size: 1.5rem; border-collapse: separate; border-spacing: 0; }
      .shiny-output-container .table thead th {
        font-weight: 600; font-size: 1.32rem; text-transform: uppercase;
        letter-spacing: 0.07em; color: #64748b; border-bottom: 2px solid #e2e8f0;
        padding: 10px 12px; background: #f8fafc;
      }
      .shiny-output-container .table tbody tr td { padding: 9px 12px; border-bottom: 1px solid #f1f5f9; vertical-align: middle; }
      .shiny-output-container .table tbody tr:last-child td { border-bottom: none; }
      .shiny-output-container .table-striped tbody tr:nth-of-type(odd) td { background-color: #f8fafc; }
      .shiny-output-container .table-hover tbody tr:hover td { background-color: #eff6ff; }
      /* === PAGINATION === */
      .shiny-output-container .btn-sm { font-size: 1.43rem; border-radius: 6px; }
      /* === IA GENERATE BUTTON === */
      .btn-generar-ia {
        background: #0d6efd !important; border: none !important; color: #fff !important;
        font-size: 1.5rem !important; padding: 10px 24px !important; border-radius: 7px !important;
        font-weight: 600 !important; white-space: nowrap; transition: background 0.15s !important;
      }
      .btn-generar-ia:hover { background: #0b5ed7 !important; }
      .btn-generar-ia:disabled, .btn-generar-ia[disabled] { background: #9ca3af !important; cursor: not-allowed !important; }
      /* === CHAT AGENT === */
      .chat-messages {
        height: 420px; overflow-y: auto; padding: 16px 8px;
        display: flex; flex-direction: column; gap: 16px;
        scroll-behavior: smooth;
      }
      .chat-welcome {
        text-align: center; color: #64748b; font-size: 1rem; margin-bottom: 14px;
      }
      .chat-chips { display: flex; flex-wrap: wrap; gap: 8px; justify-content: center; }
      .chip-btn {
        background: #f0f4f8; border: 1.5px solid #e2e8f0; border-radius: 20px;
        padding: 7px 16px; font-size: 0.88rem; color: #475569; cursor: pointer;
        transition: all 0.15s; line-height: 1.4;
      }
      .chip-btn:hover { background: #e0e7ff; border-color: #818cf8; color: #3730a3; }
      .chat-msg { display: flex; align-items: flex-end; gap: 8px; }
      .chat-msg.user { flex-direction: row-reverse; }
      .chat-avatar {
        width: 30px; height: 30px; border-radius: 50%; flex-shrink: 0;
        background: linear-gradient(135deg, #6610f2, #0d6efd);
        display: flex; align-items: center; justify-content: center;
        color: #fff; font-size: 0.78rem;
      }
      .chat-bubble {
        max-width: 78%; padding: 10px 14px; line-height: 1.55;
        font-size: 0.95rem; word-break: break-word;
      }
      .chat-msg.user .chat-bubble {
        background: #0d6efd; color: #fff;
        border-radius: 16px 16px 4px 16px;
      }
      .chat-msg.assistant .chat-bubble {
        background: #f0f4f8; color: #1a2332;
        border-radius: 16px 16px 16px 4px;
      }
      .chat-typing { display: flex; gap: 6px; align-items: center; padding: 14px 18px; }
      .chat-typing span {
        width: 9px; height: 9px; border-radius: 50%; background: #94a3b8;
        animation: ia-dot-pulse 1.4s ease-in-out infinite;
      }
      .chat-typing span:nth-child(2) { animation-delay: 0.22s; }
      .chat-typing span:nth-child(3) { animation-delay: 0.44s; }
      @keyframes ia-dot-pulse {
        0%, 100% { opacity: 0.25; transform: scale(0.7); }
        50%       { opacity: 1;   transform: scale(1);   }
      }
      .chat-sql-toggle { font-size: 0.75rem; color: #94a3b8; cursor: pointer; margin-top: 4px; user-select: none; }
      .chat-sql-toggle:hover { color: #64748b; }
      .chat-sql-code {
        font-size: 0.75rem; font-family: monospace; background: #f8f9fa;
        border: 1px solid #e2e8f0; border-radius: 6px; padding: 8px 10px;
        margin-top: 4px; white-space: pre-wrap; word-break: break-all; color: #475569;
      }
      .chat-input-area {
        display: flex; gap: 8px; padding: 12px 8px 0 8px;
        border-top: 1px solid #e2e8f0; align-items: center;
      }
      .chat-input-area .form-group { margin: 0; flex: 1; }
      .chat-input-area .form-control { border-radius: 22px !important; padding: 9px 16px !important; font-size: 0.95rem !important; }
      .chat-send-btn {
        width: 38px; height: 38px; border-radius: 50%; padding: 0;
        display: flex; align-items: center; justify-content: center;
        background: #0d6efd; border: none; color: #fff; flex-shrink: 0;
        cursor: pointer; transition: background 0.15s;
      }
      .chat-send-btn:hover { background: #0b5ed7; }
      .chat-send-btn:disabled { background: #cbd5e1; cursor: not-allowed; }
      /* === MÁS INFORMACIÓN (non-Win95) === */
      .insights-tab { width: 100%; max-width: 100%; padding: 0; font-size: 1.4rem; line-height: 1.6; color: #2d3748; }
      .insights-tab h4 { font-size: 1.75rem; font-weight: 600; color: #1a1a2e; margin-bottom: 1rem; letter-spacing: -0.02em; }
      .insights-subtitle { font-size: 1.4rem; color: #4a5568; margin-bottom: 2rem; line-height: 1.6; }
      .insight-contenido { padding: 0; margin: 0; font-size: 1.4rem; line-height: 1.65; color: #2d3748; }
      .insight-contenido .insight-titulo { font-size: 1.75rem; font-weight: 600; color: #1a1a2e; margin: 0 0 1.25rem 0; letter-spacing: -0.02em; padding-bottom: 0.75rem; border-bottom: 2px solid #e2e8f0; }
      .insight-contenido .insight-texto { margin: 0 0 1rem 0; font-size: 1.4rem; color: #2d3748; line-height: 1.65; }
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
      .sidebar-insights .radio label { padding: 0.75rem 0; margin: 0; border: none; border-bottom: 1px solid #e2e8f0; background: transparent; cursor: pointer; display: block; font-size: 1.25rem; font-weight: 400; color: #475569; transition: color 0.2s ease, font-weight 0.2s ease; }
      .sidebar-insights .radio label:last-of-type { border-bottom: none; }
      .sidebar-insights .radio label:hover { color: #1e293b; }
      .sidebar-insights .radio input:checked + label, .sidebar-insights .radio label:has(input:checked) { color: #0d6efd; font-weight: 600; background: transparent; }
      .mi-header { background: linear-gradient(135deg, #1a1a2e 0%, #0d3b7a 100%); border-radius: 12px; padding: 2.5rem 2rem; margin-bottom: 2rem; color: #fff; }
      .mi-header h1 { font-size: 1.5rem; font-weight: 700; margin: 0 0 0.5rem 0; color: #fff; letter-spacing: -0.02em; }
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
      /* ══════════════════════════════════════════════════════════════════
         MOBILE — el sidebarLayout de Bootstrap 3 ya apila sidebar→contenido
         de forma nativa a los 767px (col-sm-* sin col-xs-*). Todo lo de
         abajo depende de que ese apilado ya haya ocurrido, por eso usa el
         mismo punto de corte exacto en vez de uno propio (si no, queda una
         franja donde el sidebar ya se apiló pero nada de esto entró todavía).
         ══════════════════════════════════════════════════════════════════ */
      @media (max-width: 767px) {
        .mi-w95-grid { grid-template-columns: 1fr; }

        /* Sin esto el sidebar apilado ocupa el viewport entero — estaba
           pensado para cuando vivía al costado, a toda la altura de la
           pantalla — y el contenido principal queda empujado fuera de vista. */
        .well { min-height: auto; }

        .container-fluid { padding-left: 0.9rem !important; padding-right: 0.9rem !important; }
        .col-sm-9 { padding-left: 0.9rem !important; padding-right: 0.9rem !important; }

        /* Pestañas + contenido antes que los filtros por pestaña: sin esto,
           sidebarPanel (que Shiny siempre renderiza antes que mainPanel en
           el DOM) deja las pestañas enterradas debajo de toda la columna de
           filtros. Selector con hijo directo (#main-layout-wrapper > .row)
           a propósito, para no afectar ningun otro fluidRow()/column() del
           resto del archivo (ej. las tarjetas metricas de Tendencias). */
        #main-layout-wrapper > .row { display: flex; flex-direction: column; }
        #main-layout-wrapper > .row > .col-sm-9 { order: 1; }
        #main-layout-wrapper > .row > .col-sm-3 { order: 2; }

        /* #sidebar-tab-filtros (selector de términos / medio / sliders de
           la pestaña activa) lo reubica el JS de mas abajo para que quede
           entre las pestañas y el contenido, en vez de al final de todo
           junto con el resto del sidebar. Estilo propio porque deja de
           estar dentro de la columna gris del sidebar. */
        #sidebar-tab-filtros {
          background: #fff; border: 1px solid #e8edf3; border-radius: 10px;
          padding: 1rem 1.1rem; margin: 0.9rem 0;
          box-shadow: 0 2px 14px rgba(15,23,42,0.05);
        }

        /* Los 3 controles de sidebar que en escritorio se empujaban con
           margin-top gigante (20/27/65rem) para alinearse con contenido de
           la columna de al lado — en mobile esa columna ya no está al costado.
           Selectores que ya distinguen exactamente esos 3 casos sin tocar
           el markup: el único .sidebar-seccion anidado dentro de
           .sidebar-terminos (tab Medios→Conceptos por medio) y el último
           hijo de .sidebar-medio-destacados (Términos destacados / Volumen
           de datos). */
        .sidebar-terminos .sidebar-seccion,
        .sidebar-medio-destacados > div:last-child {
          margin-top: 1rem !important;
        }

        /* Pestañas (tabs principales + tabs_medios anidado, 8 etiquetas de
           texto largo en total) no entran en ~360-400px. .nav-tabs de
           Bootstrap 3 usa float, no flex — se convierte para poder armar una
           tira horizontal scrolleable en vez de romper el layout con wrap. */
        .nav-tabs {
          display: flex !important;
          flex-wrap: nowrap !important;
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
        }
        .nav-tabs > li { float: none; flex: 0 0 auto; }
        .nav-tabs > li > a {
          font-size: 1.05rem !important;
          padding: 0.65rem 0.9rem !important;
          white-space: nowrap;
        }

        .preset-buttons { grid-template-columns: 1fr 1fr; }
        .preset-buttons .btn { font-size: 1.1rem; }

        /* Fila de búsqueda+filtro de noticias (flex fijo 2:1, sin clase en
           el contenedor): :has() llega al padre sin tocar el markup R.
           Tambien alcanza (inofensivo) al selector de medio de Sentimiento,
           que ya tenia flex-wrap propio. */
        div:has(> .busqueda-noticias) { flex-wrap: wrap !important; }
        .busqueda-noticias { flex: 1 1 100% !important; min-width: 0 !important; }

        /* Alto de los graficos Plotly: vienen fijos en px como argumento
           del lado R (no es una clase CSS reutilizable), pensados para
           pantallas grandes. El ancho ya es responsive al contenedor — el
           propio archivo ya fuerza un resize de Plotly al cambiar de tab
           (ver 'shown.bs.tab' mas abajo), asi que el override de alto por
           CSS es suficiente sin tocar la firma de plotlyOutput(). */
        #grafico_por_medio, #grafico_por_medio .js-plotly-plot,
        #red_coocurrencia_plotly, #red_coocurrencia_plotly .js-plotly-plot,
        #grafico_conceptos_por_medio, #grafico_conceptos_por_medio .js-plotly-plot,
        #grafico_sentimiento_por_fuente, #grafico_sentimiento_por_fuente .js-plotly-plot {
          height: 420px !important;
        }
        #grafico_top_terminos, #grafico_top_terminos .js-plotly-plot,
        #grafico_terminos_por_medio, #grafico_terminos_por_medio .js-plotly-plot {
          height: 400px !important;
        }

        /* Modal Agente de datos (size='l', ~900px) y su lista de mensajes
           a alto fijo — sin tratamiento de mobile por defecto en Bootstrap 3. */
        .modal-dialog { max-width: calc(100vw - 24px) !important; margin: 12px auto !important; }
        .chat-messages { height: auto !important; max-height: 60dvh !important; }
      }

      /* Objetivos tactiles: por tipo de dispositivo, no por ancho — mismo
         criterio que ya usa el resto del sitio (static/style.css,
         ultimatum_play.html) para las apps del escritorio BabelOS. */
      @media (pointer: coarse), (hover: none) {
        .btn, .form-control, .selectize-input { min-height: 44px; }
        .preset-buttons .btn { min-height: 40px; }
        /* sliderInput (ion.rangeSlider): thumb chico por defecto. */
        .irs-handle { width: 26px !important; height: 26px !important; top: 22px !important; }
        .irs-line, .irs-bar { height: 6px !important; }
      }
      /* === RED DE PALABRAS: overlay de carga === */
      .red-spinner-wrap { position: relative; }
      .red-loading-overlay {
        display: none;
        position: absolute;
        inset: 0;
        background: rgba(255,255,255,0.88);
        border-radius: 14px;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 18px;
        z-index: 20;
        pointer-events: none;
      }
      .red-loading-overlay.visible { display: flex; }
    ")),
    tags$script(HTML("
      (function() {
        if (window.self === window.top) {
          document.documentElement.classList.add('standalone');
        }
      })();

      $(document).on('shiny:outputinvalidated', function(e) {
        if (e.target.id === 'red_coocurrencia_plotly') {
          $('#red-loading-overlay').addClass('visible');
        }
      });
      $(document).on('shiny:value shiny:error shiny:recalculated', function(e) {
        if (e.target.id === 'red_coocurrencia_plotly') {
          $('#red-loading-overlay').removeClass('visible');
        }
      });
    "))
  ),
  div(style = "padding: 1rem 1rem 0;"),
  div(class = "titulo-row",
    h1(class = "dashboard-title", `data-i18n` = "page.title", i18n_tr("page.title", lang)),
    tags$a(href = "/", class = "btn-volver-escritorio",
      HTML(paste0("\u229e <span data-i18n=\"nav.desktop_link\">", i18n_tr("nav.desktop_link", lang), "</span>"))),
    div(class = "news-language-switcher", role = "group", `aria-label` = "Idioma",
      tags$button(type = "button", class = "lang-btn", `data-babel-lang` = "es", "ES"),
      tags$button(type = "button", class = "lang-btn", `data-babel-lang` = "en", "EN")
    )
  ),
  p(class = "dashboard-subtitle", `data-i18n` = "page.subtitle", i18n_tr("page.subtitle", lang)),
  div(id = "main-layout-wrapper",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = "input.tabs !== 'mas_info'",
          div(class = "sidebar-seccion",
        tags$label(class = "control-label", `data-i18n` = "sidebar.date_range", i18n_tr("sidebar.date_range", lang)),
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
        div(style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #dee2e6;",
          actionButton("btn_abrir_ia",
                       label = tagList(icon("robot"), " Agente de datos"),
                       class = "btn-ia-sidebar")
        ),
        conditionalPanel(
          condition = "!(input.tabs === 'medios' && input.tabs_medios === 'volumen_datos')",
          div(class = "sidebar-seccion", style = "margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
            tags$label(class = "control-label", i18n_tr("sidebar.exclude_words.label", lang)),
            tags$div(class = "form-group",
              tags$input(id = "palabras_excluir", type = "text", class = "form-control",
                placeholder = i18n_tr("sidebar.exclude_words.placeholder", lang), autocomplete = "off", style = "width: 100%;")
            ),
            div(style = "margin-top: 6px; display: flex; gap: 6px;",
              actionButton("btn_excluir_aplicar", i18n_tr("sidebar.exclude_words.apply", lang), class = "btn-sm btn-outline-secondary"),
              actionButton("btn_excluir_limpiar", i18n_tr("sidebar.exclude_words.clear", lang), class = "btn-sm btn-outline-secondary")
            ),
            uiOutput("palabras_excluidas_chips")
          )
        )
          )
        ),
        div(id = "sidebar-tab-filtros",
        conditionalPanel(
          condition = "input.tabs === 'mas_info'",
          div(class = "sidebar-seccion sidebar-insights",
            tags$p(style = "font-size:0.85em; color:#6c757d; padding: 4px 8px;", i18n_tr("mas_info.sidebar.hint", lang))
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'tendencias'",
        div(class = "sidebar-seccion sidebar-terminos",
          uiOutput("selector_terminos_evol"),
          div(class = "busqueda-termino",
            tags$label(class = "control-label", i18n_tr("trends.sidebar.search_word_label", lang)),
            tags$div(
            class = "form-group",
            tags$input(
              id = "busqueda_termino",
              type = "text",
              class = "form-control",
              placeholder = i18n_tr("trends.sidebar.search_placeholder", lang),
              autocomplete = "off",
              style = "width: 100%;"
            )
          ),
            uiOutput("frecuencia_termino")
          ),
        )
      ),
        conditionalPanel(
          condition = "input.tabs === 'medios' && input.tabs_medios === 'conceptos_por_medio'",
          div(class = "sidebar-seccion sidebar-terminos",
            uiOutput("selector_terminos_medios"),
            div(class = "busqueda-termino",
              tags$label(class = "control-label", i18n_tr("trends.sidebar.search_word_label", lang)),
              tags$div(
                class = "form-group",
                tags$input(
                  id = "busqueda_termino_medios",
                  type = "text",
                  class = "form-control",
                  placeholder = i18n_tr("trends.sidebar.search_placeholder", lang),
                  autocomplete = "off",
                  style = "width: 100%;"
                )
              ),
              uiOutput("frecuencia_termino_medios")
            ),
            div(class = "sidebar-seccion", style = "margin-top: 20rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", i18n_tr("media.sidebar.outlet_for_evolution", lang)),
              selectInput("medio_evolucion_concepto", label = NULL, choices = character(0), selected = NULL)
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'medios' && input.tabs_medios === 'terminos_destacados'",
          div(class = "sidebar-medio-destacados",
            tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", i18n_tr("media.sidebar.choose_outlet", lang)),
            uiOutput("selector_medio_terminos"),
            div(style = "margin-top: 27rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", i18n_tr("media.sidebar.terms_in_chart", lang)),
              uiOutput("selector_evol_terminos_medio")
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'medios' && input.tabs_medios === 'volumen_datos'",
          div(class = "sidebar-medio-destacados",
            div(style = "margin-top: 65rem; padding-top: 1rem; border-top: 1px solid #dee2e6;",
              tags$label(class = "control-label", style = "display: block; margin-bottom: 6px; font-weight: 600;", i18n_tr("media.sidebar.outlets_in_chart", lang)),
              uiOutput("selector_medios_evol_volumen")
            )
          )
        ),
        conditionalPanel(
          condition = "input.tabs === 'medios' && input.tabs_medios === 'red_palabras'",
          div(class = "sidebar-seccion",
            div(class = "form-group",
              tags$label(class = "control-label", i18n_tr("media.chart.axis.outlet", lang)),
              selectInput("fuente_red", label = NULL, choices = character(0), selected = NULL)
            ),
            sliderInput("umbral_coocurrencia", i18n_tr("media.sidebar.min_cooccurrence", lang), min = 2L, max = 50L, value = 5L, step = 1L),
            sliderInput("max_nodos_red", i18n_tr("media.sidebar.max_nodes", lang), min = 10L, max = 150L, value = 50L, step = 10L),
            uiOutput("slider_semantico_red_ui")
          )
        )
        )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          i18n_tr("tab.tendencias", lang), value = "tendencias",
          fluidRow(
            column(6, uiOutput("card_terminos_distintos")),
            column(6, uiOutput("card_termino_top"))
          ),
          tags$p(class = "small-metric", uiOutput("texto_volumen")),
          div(class = "chart-card",
            h4(i18n_tr("trends.evolution.title", lang)),
            tags$p(class = "small-metric", i18n_tr("trends.evolution.hint", lang)),
            plotlyOutput("grafico_evolucion", height = "380px")
          ),
          div(class = "chart-card",
            h4(i18n_tr("trends.top_terms.title", lang)),
            plotlyOutput("grafico_top_terminos", height = "520px")
          ),
          div(class = "seccion-ultimas-noticias",
            h4(i18n_tr("trends.recent_news.title", lang)),
            div(style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 0.5rem;",
              div(class = "busqueda-noticias", style = "flex: 2; min-width: 0;",
                textInput("busqueda_titulo", label = NULL, placeholder = i18n_tr("search.placeholder_titles", lang), width = "100%")
              ),
              div(class = "busqueda-noticias", style = "flex: 1; min-width: 120px;",
                selectInput("filtro_medio_noticias", label = NULL,
                  choices = setNames("todos", i18n_tr("filter.all_media", lang)), selected = "todos", width = "100%")
              )
            ),
            uiOutput("paginacion_noticias"),
            div(style = "overflow-x: auto; margin-top: 8px;", tableOutput("tabla_noticias")),
            uiOutput("controles_pagina")
          )
        ),
        tabPanel(
          i18n_tr("tab.medios", lang), value = "medios",
          tabsetPanel(id = "tabs_medios",
            tabPanel(
              i18n_tr("tab.conceptos_por_medio", lang), value = "conceptos_por_medio",
              div(class = "chart-card",
                h4(i18n_tr("media.tab.concepts.chart1.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.concepts.chart1.hint", lang)),
                plotlyOutput("grafico_conceptos_por_medio", height = "600px")
              ),
              div(class = "chart-card",
                h4(i18n_tr("media.tab.concepts.chart2.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.concepts.chart2.hint", lang)),
                plotlyOutput("grafico_evolucion_concepto_por_medio", height = "420px")
              )
            ),
            tabPanel(
              i18n_tr("tab.terminos_destacados", lang), value = "terminos_destacados",
              div(class = "chart-card",
                h4(i18n_tr("media.tab.top_terms.chart1.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.top_terms.chart1.hint", lang)),
                plotlyOutput("grafico_terminos_por_medio", height = "500px")
              ),
              div(class = "chart-card",
                h4(i18n_tr("media.tab.top_terms.chart2.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.top_terms.chart2.hint", lang)),
                plotlyOutput("grafico_evolucion_terminos_por_medio", height = "380px")
              )
            ),
            tabPanel(
              i18n_tr("tab.volumen_datos", lang), value = "volumen_datos",
              div(class = "chart-card",
                h4(i18n_tr("media.tab.volume.chart1.title", lang)),
                tags$p(class = "small-metric", HTML(paste0(
                  i18n_tr("media.tab.volume.chart1.hint.prefix", lang),
                  "<span style='color:#f59e0b; font-weight:700;'>&#9632; ", i18n_tr("media.tab.volume.chart1.hint.yellow", lang), "</span>: ", i18n_tr("media.tab.volume.chart1.hint.yellow_desc", lang), " · ",
                  "<span style='color:#dc2626; font-weight:700;'>&#9632; ", i18n_tr("media.tab.volume.chart1.hint.red", lang), "</span>: ", i18n_tr("media.tab.volume.chart1.hint.red_desc", lang)))),
                plotlyOutput("grafico_por_medio", height = "700px")
              ),
              div(class = "chart-card",
                h4(i18n_tr("media.tab.volume.chart2.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.volume.chart2.hint", lang)),
                plotlyOutput("grafico_evolucion_volumen_por_medio", height = "420px")
              )
            ),
            tabPanel(
              i18n_tr("tab.red_palabras", lang), value = "red_palabras",
              div(class = "chart-card red-spinner-wrap",
                h4(i18n_tr("media.tab.network.title", lang)),
                tags$p(class = "small-metric", i18n_tr("media.tab.network.hint", lang)),
                plotlyOutput("red_coocurrencia_plotly", width = "100%", height = "640px"),
                div(id = "red-loading-overlay", class = "red-loading-overlay visible",
                  div(class = "ia-modal-dots",
                    tags$span(), tags$span(), tags$span()
                  ),
                  tags$p(class = "ia-modal-loading-msg", i18n_tr("media.tab.network.loading", lang))
                )
              )
            ),
          )
        ),
        tabPanel(
          i18n_tr("tab.sentimiento", lang), value = "sentimiento",
          # Aviso cuando el rango elegido excede la cobertura del análisis de sentimiento
          uiOutput("sent_aviso_rango"),
          # FILA SUPERIOR — tono neto (card aparte, acento) + distribución con selector de medio
          div(class = "sent-toprow",
            uiOutput("sent_hero_net"),
            div(class = "chart-card sent-distrib-card",
              div(class = "sent-card-head",
                h4(i18n_tr("sent.distribution.title", lang)),
                div(class = "sent-medio busqueda-noticias",
                  tags$span(class = "lbl", i18n_tr("media.chart.axis.outlet", lang)),
                  selectInput("sent_filtro_medio", label = NULL,
                    choices = setNames("todos", i18n_tr("sent.distribution.all_media", lang)), selected = "todos", width = "220px"))
              ),
              uiOutput("sent_barra_periodo")
            )
          ),
          # Evolución en el tiempo, y debajo el ranking de medios (cuando es "Todos")
          uiOutput("sent_bento"),
          # TABLA paginada (10 por página)
          div(class = "chart-card",
            h4(i18n_tr("sent.table.title", lang)),
            tags$p(class = "small-metric", i18n_tr("sent.table.hint", lang)),
            uiOutput("tabla_sent_titulares"),
            div(class = "sent-paginacion", uiOutput("sent_paginacion"))
          ),
          uiOutput("sent_meta")
        ),
        tabPanel(
          i18n_tr("tab.mas_info", lang), value = "mas_info",
          div(class = "insights-tab",
            uiOutput("mas_informacion_contenido")
          )
        )
      )
    )
  )
)
)
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Idioma en caliente: input$babel_lang lo fija el shim JS (postMessage /
  # localStorage / click en el switcher). El primer valor coincide con el
  # que ya se uso para el primer render (cookie, ver ui <- function(request)),
  # asi que no hay parpadeo al conectar.
  current_lang <- reactive({
    lang <- input$babel_lang
    if (is.null(lang) || !(lang %in% c("es", "en"))) "es" else lang
  })
  tr <- function(key) i18n_tr(key, current_lang())

  pool <- get_pool()

  rag_store <- tryCatch({
    if (requireNamespace("ragnar", quietly = TRUE) && file.exists("../datos/noticias_rag.duckdb")) {
      ragnar::ragnar_store_connect(
        location  = "../datos/noticias_rag.duckdb",
        read_only = TRUE
      )
    } else NULL
  }, error = function(e) NULL)

  store_disponible <- reactive({ !is.null(rag_store) })

  # Helper: enriquece resultados VSS con fecha/fuente/titulo desde PostgreSQL
  # El campo origin del store contiene el noticias.id
  enriquecer_vss <- function(res) {
    if (is.null(res) || nrow(res) == 0L) return(res)
    ids <- res$origin[!is.na(res$origin)]
    if (length(ids) == 0L) return(res)
    con <- poolCheckout(pool)
    meta <- tryCatch({
      ids_quoted <- paste(vapply(ids, function(x) DBI::dbQuoteLiteral(con, x), character(1L)), collapse = ", ")
      DBI::dbGetQuery(con,
        paste0("SELECT id, fecha, fuente, titulo FROM noticias WHERE id IN (", ids_quoted, ")")
      )
    }, error = function(e) data.frame(id=character(), fecha=as.Date(character()),
                                      fuente=character(), titulo=character()))
    poolReturn(con)
    res <- merge(res, meta, by.x = "origin", by.y = "id", all.x = TRUE)
    res$fecha <- as.Date(res$fecha)
    res
  }

  # Stopwords centralizadas (fuente única: stopwords.R en raíz del proyecto)
  source("../stopwords.R")
  STOPWORDS_GRAFICOS <- STOPWORDS

  # Presets de fechas
  aplicar_preset_fechas <- function(n_dias) {
    limite_fin <- Sys.Date()
    rango_actual <- isolate(input$fechas)
    if (length(rango_actual) >= 2) {
      fin_actual <- suppressWarnings(as.Date(rango_actual[2]))
      if (!is.na(fin_actual)) limite_fin <- fin_actual
    }
    fecha_fin <- limite_fin
    fecha_inicio <- max(as.Date("2018-01-01"), fecha_fin - (n_dias - 1))
    updateDateRangeInput(
      session, "fechas",
      start = fecha_inicio,
      end = fecha_fin,
      min = as.Date("2018-01-01"),
      max = limite_fin
    )
    # Fallback explícito para forzar ambos campos en el cliente.
    session$sendInputMessage("fechas", list(
      value = c(as.character(fecha_inicio), as.character(fecha_fin))
    ))
  }
  observeEvent(input$preset_7, {
    aplicar_preset_fechas(7)
  })
  observeEvent(input$preset_30, {
    aplicar_preset_fechas(30)
  })
  observeEvent(input$preset_365, {
    aplicar_preset_fechas(365)
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


  output$slider_semantico_red_ui <- renderUI({
    if (!store_disponible()) return(NULL)
    sliderInput("umbral_semantico", tr("media.network.slider.semantic_similarity"),
                min = 0.5, max = 0.95, value = 0.80, step = 0.05)
  })

  tabla_config <- reactive({
    f <- fechas()
    usa_mensual <- as.numeric(difftime(f$end, f$start, units = "days")) > 180
    list(
      tabla      = if (usa_mensual) "mv_terminos_mensuales"          else "titulos_terminos_diarios",
      col_t      = "termino", col_f = "frecuencia", filtro_tipo = "",
      tiene_pm   = TRUE,
      tabla_pm   = if (usa_mensual) "mv_terminos_por_medio_mensuales" else "titulos_terminos_por_medio",
      col_t_pm   = "termino", col_f_pm = "frecuencia", filtro_tipo_pm = ""
    )
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

  # Granularidad de las series temporales según el rango elegido: diaria hasta 62 días
  # (rangos cortos como "últimos 7 días" colapsaban a un solo punto mensual), mensual
  # hasta 2 años y anual después (mismo umbral que por_ano). Valores fijos → seguros
  # de interpolar en SQL.
  gran_tiempo <- function(f) {
    dias <- as.numeric(difftime(f$end, f$start, units = "days"))
    if (dias <= 62) "day" else if (f$por_ano) "year" else "month"
  }

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

    # Frecuencia literal de cada término por día (una línea por concepto).
    # Nota: NO se usa búsqueda semántica aquí. Con top_k alto, ragnar_retrieve_vss
    # devolvía casi todos los documentos para cualquier término, por lo que todas
    # las líneas quedaban idénticas (superpuestas) y el gráfico mostraba "un solo
    # concepto". La tabla precalculada de frecuencias da una serie distinta por
    # palabra, que es exactamente "cómo evolucionó su presencia en los titulares".
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

  # IDs semánticos para la tabla de noticias.
  # VSS desactivado: la búsqueda semántica solo devuelve artículos indexados en DuckDB.
  # Como el store puede estar desactualizado (Shiny mantiene un lock de lectura que
  # bloquea run_embeddings.R), los artículos recientes quedan invisibles. Además, la
  # búsqueda por embedding no garantiza coincidencia con la palabra exacta del titular.
  # El path de regex en PostgreSQL (título ~* \y<término>\y) cubre todos los artículos
  # en tiempo real y es el correcto para búsqueda por palabra clave.
  ids_semanticos_tabla <- reactive({ NULL })

  # Total de noticias en el rango (desde 2018; con filtro de búsqueda en titulares y medio)
  total_noticias_rango <- reactive({
    f <- fechas_noticias()
    start <- f$start
    busq <- trimws(if (is.null(input$busqueda_titulo)) "" else input$busqueda_titulo)
    medio <- if (is.null(input$filtro_medio_noticias) || input$filtro_medio_noticias == "todos") NULL else input$filtro_medio_noticias

    ids_sem <- ids_semanticos_tabla()
    if (!is.null(ids_sem)) {
      if (length(ids_sem) == 0L) return(0L)
      # DBI/RPostgres no bindea un vector como array PG (lo trata como batch);
      # expandir a placeholders individuales, como en el resto del archivo.
      ph_ids <- paste(sprintf("$%d", seq_along(ids_sem)), collapse = ", ")
      filtro_medio_sql <- if (!is.null(medio)) paste0(" AND fuente = $", length(ids_sem) + 1L) else ""
      q <- paste0("SELECT COUNT(*) AS n FROM noticias WHERE id = ANY(ARRAY[", ph_ids, "]::text[])", filtro_medio_sql)
      params <- as.list(ids_sem)
      if (!is.null(medio)) params <- c(params, list(medio))
      out <- dbGetQuery(pool, q, params = params)
      return(as.integer(as.numeric(out$n)))
    }

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
    medios <- sort(noticias_por_medio()$medio)
    updateSelectInput(session, "filtro_medio_noticias",
      choices = setNames(c("todos", medios), c(tr("filter.all_media"), medios)),
      selected = "todos")
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
    medio <- if (is.null(input$filtro_medio_noticias) || input$filtro_medio_noticias == "todos") NULL else input$filtro_medio_noticias

    ids_sem <- ids_semanticos_tabla()
    if (!is.null(ids_sem)) {
      if (length(ids_sem) == 0L) return(data.frame(titulo = character(), fecha = character(), medio = character(), url = character()))
      # DBI/RPostgres no bindea un vector como array PG (lo trata como batch);
      # expandir a placeholders individuales, como en el resto del archivo.
      ph_ids <- paste(sprintf("$%d", seq_along(ids_sem)), collapse = ", ")
      n_ids <- length(ids_sem)
      filtro_medio_sql <- if (!is.null(medio)) paste0(" AND fuente = $", n_ids + 1L) else ""
      idx_offset <- n_ids + if (!is.null(medio)) 2L else 1L
      q <- paste0(
        "SELECT titulo, fecha, fuente AS medio, url FROM noticias",
        " WHERE id = ANY(ARRAY[", ph_ids, "]::text[])", filtro_medio_sql,
        " ORDER BY fecha ", dir,
        " LIMIT 5 OFFSET $", idx_offset
      )
      params <- as.list(ids_sem)
      if (!is.null(medio)) params <- c(params, list(medio))
      params <- c(params, list((pg - 1L) * 5L))
      return(dbGetQuery(pool, q, params = params))
    }

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
      q <- paste0("
        SELECT DATE_TRUNC('", gran_tiempo(f), "', fecha)::date AS periodo, fuente AS medio, COUNT(*) AS total
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2
        GROUP BY periodo, fuente
        ORDER BY periodo, fuente
      ")
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
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(text = tr("trends.top_terms_chart.no_data"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    sel_medios <- input$medios_evol_volumen_sel
    if (!is.null(sel_medios) && length(sel_medios) > 0) d <- d[d$medio %in% sel_medios, ]
    if (nrow(d) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(text = tr("media.volume_evol.select_outlet"),
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
          hovertemplate = paste0(tr("trends.chart.hover.year"), "%{x}<br>", tr("media.chart.hover.news"), "%{y:,.0f}<br>", tr("media.chart.hover.outlet"), medios[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        xaxis = list(title = tr("trends.chart.axis.year"), tickvals = sort(unique(d$periodo)), zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("media.chart.axis.news_published"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
      )
    } else {
      d$periodo <- as.Date(d$periodo)
      rango <- range(d$periodo)
      dias_rango <- as.numeric(difftime(rango[2], rango[1], units = "days"))
      diario <- gran_tiempo(fechas()) == "day"
      # Eje X adaptado al rango, con el mismo criterio que el gráfico de volumen general
      if (!diario) {
        breaks_x <- seq(rango[1], rango[2], by = "1 month")
        tick_fmt <- "%b %Y"
      } else if (dias_rango <= 7) {
        breaks_x <- seq(rango[1], rango[2], by = "1 day")
        tick_fmt <- "%d %b"
      } else if (dias_rango <= 31) {
        breaks_x <- seq(rango[1], rango[2], by = "2 days")
        tick_fmt <- "%d %b"
      } else {
        breaks_x <- seq(rango[1], rango[2], by = "1 week")
        tick_fmt <- "%d %b"
      }
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      hover_x <- if (diario) paste0(tr("trends.chart.hover.date"), "%{x|%d %b %Y}") else paste0(tr("media.chart.hover.month"), "%{x|%b %Y}")
      for (i in seq_along(medios)) {
        sub <- d %>% filter(medio == medios[i]) %>% arrange(periodo)
        p <- p %>% add_trace(
          x = sub$periodo, y = sub$total, type = "scatter", mode = "lines+markers",
          name = medios[i],
          fill = "tozeroy",
          fillcolor = paste0(paleta[i], "30"),
          line = list(width = 2, color = paleta[i]),
          hovertemplate = paste0(hover_x, "<br>", tr("media.chart.hover.news"), "%{y:,.0f}<br>", tr("media.chart.hover.outlet"), medios[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        xaxis = list(type = "date", title = list(text = if (diario) tr("trends.chart.axis.date") else tr("media.chart.axis.month"), standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = -45, tickfont = list(size = 10),
          zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("media.chart.axis.news_published"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 30, l = 60, r = 30),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
      )
    }
    p %>% config(displayModeBar = TRUE, locale = current_lang())
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
    patron <- paste0(gsub("\\\\", "\\\\\\\\", gsub("_", "\\\\_", gsub("%", "\\\\%", busq, fixed = TRUE), fixed = TRUE), fixed = TRUE), "%")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " ILIKE $3", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY
        CASE WHEN LOWER(", cfg$col_t, ") = LOWER($4) THEN 0 ELSE 1 END,
        total DESC
      LIMIT 20
    ")
    out <- dbGetQuery(pool, q, params = list(f$start, f$end, patron, busq))
    if (nrow(out) == 0) return(out)
    out$total <- as.integer(as.numeric(out$total))
    if (requireNamespace("SnowballC", quietly = TRUE) && nrow(out) > 1) {
      stem_t <- function(t) {
        words <- strsplit(tolower(t), " ", fixed = TRUE)[[1]]
        paste(SnowballC::wordStem(words, language = "es"), collapse = " ")
      }
      out$stem_key <- vapply(out$termino, stem_t, character(1L))
      out <- out[!duplicated(out$stem_key), , drop = FALSE]
      out$stem_key <- NULL
    }
    head(out, 6L)
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

    # VSS no se usa aquí: ragnar_retrieve_vss devuelve los K más similares
    # semánticamente, no todos los artículos que contienen el término; el conteo
    # por medio queda sesgado a las fuentes mejor representadas en el top-K (mismo
    # problema que ya se corrigió en datos_evolucion). La tabla precalculada da
    # conteos exactos por keyword para todos los medios.
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
      r$n <- as.integer(as.numeric(r$n))
      r$termino <- rep(terms[i], nrow(r))
      grid <- expand.grid(medio = medios, termino = terms[i], stringsAsFactors = FALSE)
      r <- left_join(grid, r, by = c("medio", "termino")) %>% mutate(n = coalesce(n, 0L))
      out_list[[i]] <- r
    }
    bind_rows(out_list)
  })

  output$mas_informacion_contenido <- renderUI({
    # Total de noticias calculado de la BD (redondeado hacia abajo a 0.1M) para que la
    # cifra mostrada nunca quede obsoleta; fallback estático si la consulta falla.
    corpus_txt <- tryCatch({
      r <- DBI::dbGetQuery(get_pool(),
        "SELECT COUNT(*)::float8 AS n, EXTRACT(YEAR FROM MIN(fecha))::int AS y FROM noticias")
      sprintf(tr("info.corpus.text"), format(floor(r$n / 1e5) / 10, decimal.mark = ","), r$y)
    }, error = function(e) tr("info.corpus.fallback"))
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
      win(paste0("\U0001f4f0  ", tr("info.title.main")), icon = "",
        tags$div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px;",
          tags$div(
            tags$p(style="font-size:22px; margin:0 0 6px 0;",
              tr("info.subtitle")),
            tags$div(class = "mi-w95-badges",
              tags$span(class = "mi-w95-badge", tr("info.badge.years")),
              tags$span(class = "mi-w95-badge", tr("info.badge.media_count")),
              tags$span(class = "mi-w95-badge mi-w95-blink", tr("info.badge.online"))
            )
          )
        )
      ),

      # ── CRÉDITOS ────────────────────────────────────────────────────────────
      win_credit(tr("info.credits.title"), icon = "\u2605",
        tags$p(style="font-size:21px; margin-bottom:8px;",
          tags$strong(tr("info.credits.data_label")),
          tags$a(href = repo_prensa, target = "_blank", rel = "noopener",
            "prensa_chile"),
          sprintf(tr("info.credits.data_suffix"), corpus_txt)
        ),
        tags$p(style="font-size:21px; margin:0;",
          tags$strong(tr("info.credits.pipeline_label")),
          tags$a(href = repo_analisis, target = "_blank", rel = "noopener",
            "analisis-noticias"),
          tr("info.credits.pipeline_suffix")
        )
      ),

      # ── Dos columnas: ¿Qué es? + ¿Qué puedo hacer? ─────────────────────────
      tags$div(class = "mi-w95-grid",

        win(tr("info.what_is.title"), icon = "\u2139",
          tags$p(tr("info.what_is.p1")),
          tags$p(tr("info.what_is.p2")),
          tags$p(tr("info.what_is.p3"))
        ),

        win(tr("info.what_can.title"), icon = "\u2756",
          feat(tr("info.feat.trends.title"), tr("info.feat.trends.desc")),
          feat(tr("info.feat.compare.title"), tr("info.feat.compare.desc")),
          feat(tr("info.feat.search.title"), tr("info.feat.search.desc")),
          feat(tr("info.feat.explore.title"), tr("info.feat.explore.desc")),
          feat(tr("info.feat.volume.title"), tr("info.feat.volume.desc"))
        )
      ),

      # ── Fuentes cubiertas (ancho completo) ──────────────────────────────────
      win(tr("info.sources.title"), icon = "\U0001f4f0",
        tags$div(class = "mi-w95-group-label", paste0("\u25ba  ", tr("info.sources.tv"))),
        tags$div(class = "mi-w95-chips",
          chip("24horas", "mi-w95-chip-tv"), chip("CHV Noticias", "mi-w95-chip-tv"),
          chip("CNN Chile", "mi-w95-chip-tv"), chip("Meganoticias", "mi-w95-chip-tv"),
          chip("T13", "mi-w95-chip-tv")
        ),
        tags$div(class = "mi-w95-group-label", paste0("\u25ba  ", tr("info.sources.radio"))),
        tags$div(class = "mi-w95-chips",
          chip("ADN Radio", "mi-w95-chip-radio"), chip("Agricultura", "mi-w95-chip-radio"),
          chip("Cooperativa", "mi-w95-chip-radio"), chip("Radio Uchile", "mi-w95-chip-radio")
        ),
        tags$div(class = "mi-w95-group-label", paste0("\u25ba  ", tr("info.sources.print"))),
        tags$div(class = "mi-w95-chips",
          chip("Diario Financiero", "mi-w95-chip-print"), chip("El Siglo", "mi-w95-chip-print"),
          chip("Emol", "mi-w95-chip-print"), chip("La Cuarta", "mi-w95-chip-print"),
          chip("La Nación", "mi-w95-chip-print"), chip("La Tercera", "mi-w95-chip-print"),
          chip("Publimetro", "mi-w95-chip-print")
        ),
        tags$div(class = "mi-w95-group-label", paste0("\u25ba  ", tr("info.sources.digital"))),
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
      win(tr("info.how.title"), icon = "\u2699",
        tags$div(class = "mi-w95-grid",
          tags$div(
            step("1", tr("info.how.step1.title"), tr("info.how.step1.desc")),
            step("2", tr("info.how.step2.title"), tr("info.how.step2.desc"))
          ),
          tags$div(
            step("3", tr("info.how.step3.title"), tr("info.how.step3.desc")),
            step("4", tr("info.how.step4.title"), tr("info.how.step4.desc"))
          )
        )
      ),

      # ── Barra de estado footer ───────────────────────────────────────────────
      tags$div(class = "mi-w95-statusbar",
        tags$a(href = repo_prensa,   target = "_blank", rel = "noopener", tr("info.footer.data")),
        tags$span("|"),
        tags$a(href = repo_analisis, target = "_blank", rel = "noopener", tr("info.footer.pipeline")),
        tags$span(tr("info.footer.license"))
      )
    )
  })

  # ---- Outputs ----
  output$card_terminos_distintos <- renderUI({
    n <- n_terminos()
    if (is.na(n)) n <- 0
    div(class = "card-box",
        h5(tr("trends.card.unique_words")),
        div(class = "valor", format(as.integer(n), big.mark = ".", decimal.mark = ","))
    )
  })

  output$card_termino_top <- renderUI({
    tt <- termino_top()
    if (nrow(tt) == 0) {
      val <- "—"
      sub <- tr("trends.card.top_word.no_data")
    } else {
      val <- tt$termino
      sub <- paste(format(as.integer(tt$total), big.mark = ".", decimal.mark = ","), tr("trends.card.top_word.appearances"))
    }
    div(class = "card-box",
        h5(tr("trends.card.top_word")),
        div(class = "valor", val),
        p(style = "margin-bottom:0; font-size:0.9em;", sub)
    )
  })

  output$texto_volumen <- renderUI({
    v <- volumen()
    tags$span(
      tr("trends.volume.label"),
      tags$strong(format(round(v$total), big.mark = ".", decimal.mark = ",")), tr("trends.volume.suffix"),
      tr("trends.volume.avg_prefix"), tags$strong(v$promedio), tr("trends.volume.avg_suffix")
    )
  })

  output$selector_terminos_evol <- renderUI({
    disponibles <- terminos_evolucion_disponibles()
    anadidos    <- terminos_anadidos_por_clic()
    if (length(disponibles) == 0) return(NULL)
    choices <- setNames(disponibles, disponibles)
    top <- top_10_evol()
    current_input <- isolate(input$terminos_evolucion)
    top_sel <- if (!is.null(current_input) && length(current_input) > 0) {
      intersect(current_input, top$termino)
    } else {
      if (nrow(top) >= 2) head(top$termino, 2) else if (nrow(top) == 1) top$termino else character(0)
    }
    # Siempre incluir todos los términos añadidos por clic (evita la race condition)
    selected <- unique(c(top_sel, intersect(anadidos, disponibles)))
    if (length(selected) == 0)
      selected <- head(disponibles, min(2L, length(disponibles)))
    div(class = "term-chips-box",
      tags$label(class = "control-label", style = "display: block; margin-bottom: 6px;",
        tr("trends.terms_compare.label")
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

  # Si el usuario desmarca un término añadido, quitarlo de terminos_anadidos_por_clic
  observeEvent(input$terminos_evolucion, {
    anadidos <- terminos_anadidos_por_clic()
    desmarcados <- setdiff(anadidos, input$terminos_evolucion)
    if (length(desmarcados) > 0)
      terminos_anadidos_por_clic(setdiff(anadidos, desmarcados))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Búsqueda parcial: prefix match, exact match primero; agrupar variantes morfológicas por stemming
  resultados_busqueda_termino <- reactive({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0) return(NULL)
    f <- fechas()
    cfg <- tabla_config()
    patron <- paste0(gsub("\\\\", "\\\\\\\\", gsub("_", "\\\\_", gsub("%", "\\\\%", busq, fixed = TRUE), fixed = TRUE), fixed = TRUE), "%")
    q <- paste0("
      SELECT ", cfg$col_t, " AS termino, SUM(", cfg$col_f, ") AS total
      FROM ", cfg$tabla, "
      WHERE fecha >= $1 AND fecha <= $2 AND ", cfg$col_t, " ILIKE $3", cfg$filtro_tipo, "
      GROUP BY termino
      ORDER BY
        CASE WHEN LOWER(", cfg$col_t, ") = LOWER($4) THEN 0 ELSE 1 END,
        total DESC
      LIMIT 20
    ")
    out <- dbGetQuery(pool, q, params = list(f$start, f$end, patron, busq))
    if (nrow(out) == 0) return(out)
    out$total <- as.integer(as.numeric(out$total))
    # Agrupar variantes morfológicas (plural, conjugaciones) usando stemming español
    if (requireNamespace("SnowballC", quietly = TRUE) && nrow(out) > 1) {
      stem_t <- function(t) {
        words <- strsplit(tolower(t), " ", fixed = TRUE)[[1]]
        paste(SnowballC::wordStem(words, language = "es"), collapse = " ")
      }
      out$stem_key <- vapply(out$termino, stem_t, character(1L))
      out <- out[!duplicated(out$stem_key), , drop = FALSE]
      out$stem_key <- NULL
    }
    head(out, 6L)
  })

  output$frecuencia_termino <- renderUI({
    busq <- trimws(if (is.null(input$busqueda_termino)) "" else input$busqueda_termino)
    if (nchar(busq) == 0)
      return(tags$p(class = "frecuencia-termino-msg", tr("trends.term_search.hint")))
    res <- resultados_busqueda_termino()
    if (is.null(res) || nrow(res) == 0)
      return(tags$p(class = "frecuencia-termino-msg", paste0(tr("trends.term_search.no_match"), " \"", busq, "\".")))
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
      tags$p(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 4px;", paste0(tr("trends.term_search.variants_found"), " '", busq, "':")),
      tags$div(class = "frecuencia-termino-lista", items)
    )
  })

  # Al hacer clic en una casilla de resultado de búsqueda, se añade solo ese término a la lista y al gráfico de evolución
  observeEvent(input$termo_añadir_evol, {
    term <- input$termo_añadir_evol
    if (is.null(term) || !nzchar(trimws(term))) return()
    terminos_anadidos_por_clic(unique(c(terminos_anadidos_por_clic(), term)))
    # selector_terminos_evol se re-renderiza solo con el nuevo término ya seleccionado
  })

  output$grafico_evolucion <- renderPlotly({
    d <- datos_evolucion()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
        layout(
          title = list(text = tr("trends.chart.no_terms_selected"), font = list(size = 14)),
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
          hovertemplate = paste0(tr("trends.chart.hover.year"), "%{x}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos[i], "<extra></extra>")
        )
      }
      anos <- sort(unique(d$ano))
      p <- p %>% layout(
        xaxis = list(title = tr("trends.chart.axis.year"), tickvals = anos, zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      title = list(text = tr("trends.chart.legend.term")), font = list(size = 10, family = "Arial, sans-serif"),
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
          hovertemplate = paste0(tr("trends.chart.hover.date"), "%{x|%d/%m/%Y}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos[i], "<extra></extra>")
        )
      }
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      p <- p %>% layout(
        xaxis = list(
          type = "date",
          title = list(text = tr("trends.chart.axis.date"), standoff = 12),
          tickmode = "array",
          tickvals = tickvals_ms,
          ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45,
          tickfont = list(size = 11),
          zeroline = FALSE,
          showgrid = TRUE,
          range = as.numeric(as.POSIXct(rango, tz = "UTC")) * 1000
        ),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      title = list(text = tr("trends.chart.legend.term")), font = list(size = 10, family = "Arial, sans-serif"),
                      tracegroupgap = 0),
        margin = list(b = 60, t = 30, l = 60, r = 30)
      )
    }
    p %>% config(displayModeBar = TRUE, locale = current_lang())
  })

  output$grafico_top_terminos <- renderPlotly({
    top <- top_30_df()
    if (nrow(top) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("trends.top_terms_chart.no_data"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }

    if (store_disponible() && requireNamespace("proxy", quietly = TRUE)) {
      top_df <- tryCatch({
        emb_fn <- ragnar::embed_ollama(model = "nomic-embed-text")
        emb_mat <- emb_fn(top$termino)
        sim_mat <- as.matrix(proxy::simil(emb_mat, method = "cosine"))
        dist_mat <- as.dist(1 - sim_mat)
        hc <- hclust(dist_mat, method = "complete")
        clusters <- cutree(hc, h = 0.4)
        top$cluster <- clusters
        top %>%
          dplyr::group_by(cluster) %>%
          dplyr::mutate(cluster_label = termino[which.max(total)]) %>%
          dplyr::group_by(cluster_label) %>%
          dplyr::summarise(
            total = sum(total),
            terminos = paste(termino, collapse = ", "),
            .groups = "drop"
          ) %>%
          dplyr::arrange(total) %>%
          dplyr::rename(termino = cluster_label)
      }, error = function(e) NULL)
      if (!is.null(top_df)) {
        return(
          plot_ly(
            x = top_df$total,
            y = factor(top_df$termino, levels = top_df$termino),
            type = "bar", orientation = "h",
            customdata = top_df$terminos,
            marker = list(
              color = colorRampPalette(c("#6c9bd1", "#0d6efd"))(nrow(top_df)),
              line = list(color = "rgba(255,255,255,0)", width = 0)
            ),
            hovertemplate = paste0("<b>%{y}</b><br>", tr("trends.top_terms_chart.hover.frequency"), "%{x:,.0f}<br>", tr("trends.top_terms_chart.hover.terms"), "%{customdata}<extra></extra>")
          ) %>%
          layout(
            xaxis = list(title = tr("trends.top_terms_chart.axis.total_frequency"), zeroline = FALSE, showgrid = TRUE, gridcolor = "#eee"),
            yaxis = list(title = NULL, tickfont = list(size = 11)),
            margin = list(l = 140, r = 30, t = 20, b = 50),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff"
          ) %>%
          config(displayModeBar = TRUE, locale = current_lang())
        )
      }
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
      hovertemplate = paste0("<b>%{y}</b><br>", tr("trends.top_terms_chart.hover.frequency"), "%{x:,.0f}<extra></extra>")
    ) %>%
    layout(
      xaxis = list(title = tr("trends.top_terms_chart.axis.total_frequency"), zeroline = FALSE, showgrid = TRUE, gridcolor = "#eee"),
      yaxis = list(title = NULL, tickfont = list(size = 11)),
      margin = list(l = 140, r = 30, t = 20, b = 50),
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff"
    ) %>%
    config(displayModeBar = TRUE, locale = current_lang())
  })

  output$paginacion_noticias <- renderUI({
    total <- total_noticias_rango()
    dir <- orden_fecha()
    flecha <- if (dir == "DESC") "\u2193" else "\u2191"
    sort_link <- actionLink("toggle_fecha_orden",
      label = paste0(tr("trends.pagination.date_sort"), " ", flecha),
      style = "font-size: 0.85em; cursor: pointer;"
    )
    if (total == 0) return(tagList(sort_link, tags$p(tr("trends.pagination.no_news"))))
    pg <- page_noticias()
    desde <- (pg - 1L) * 5L + 1L
    hasta <- as.integer(min(pg * 5L, total))
    tagList(
      tags$div(style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 4px;",
        sort_link,
        tags$span(class = "small-metric", paste0(tr("trends.pagination.showing"), " ", desde, "-", hasta, " ", tr("trends.pagination.of"), " ", format(as.integer(total), big.mark = ".", decimal.mark = ","), " ", tr("trends.pagination.per_page")))
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
      actionButton("pag_prev", tr("trends.pagination.prev"), class = "btn-sm", disabled = pg <= 1L),
      tags$span(style = "margin: 0 10px;", paste(tr("trends.pagination.page"), pg, tr("trends.pagination.of"), npag)),
      actionButton("pag_next", tr("trends.pagination.next"), class = "btn-sm", disabled = pg >= npag)
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
    if (nrow(t) == 0) return(setNames(data.frame(tr("trends.table.no_news_page")), " "))
    t$url <- paste0('<a href="', t$url, '" target="_blank" rel="noopener">', tr("trends.table.col.link"), '</a>')
    t$titulo <- substr(t$titulo, 1, 70)
    t$fecha <- as.character(t$fecha)
    names(t)[names(t) == "titulo"] <- tr("trends.table.col.title")
    names(t)[names(t) == "fecha"] <- tr("trends.table.col.date")
    names(t)[names(t) == "medio"] <- tr("trends.table.col.media")
    names(t)[names(t) == "url"] <- ""
    t
  }, striped = TRUE, hover = TRUE, sanitize.text.function = function(x) x)

  # ── Chat agent ──────────────────────────────────────────────────────────────
  chat_historial  <- reactiveVal(list())   # list of list(role, content, sql)
  chat_procesando <- reactiveVal(FALSE)

  # Placeholder to avoid breaking any leftover references
  resumen_texto    <- reactiveVal(NULL)
  resumen_generando <- reactiveVal(FALSE)


  output$resumen_estado <- renderUI({ NULL })

  # ── Modal: Agente de datos ───────────────────────────────────────────────────
  observeEvent(input$btn_abrir_ia, {
    chat_historial(list())
    chat_procesando(FALSE)
    f <- fechas()
    fecha_i <- format(as.Date(f$start), "%d %b %Y")
    fecha_f <- format(as.Date(f$end), "%d %b %Y")
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center; gap: 10px;",
        div(style = "width: 34px; height: 34px; background: linear-gradient(135deg,#6610f2,#0d6efd); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: #fff; flex-shrink: 0; font-size: 0.95rem;",
          icon("robot")
        ),
        div(
          tags$strong("Agente de datos", style = "display: block; line-height: 1.2;"),
          tags$span(paste(fecha_i, "\u2013", fecha_f),
                    style = "font-size: 0.8rem; color: #6c757d; font-weight: 400;")
        )
      ),
      div(
        uiOutput("chat_mensajes_ui"),
        div(class = "chat-input-area",
          textInput("chat_pregunta", NULL,
                    placeholder = "Pregunta algo sobre las noticias del per\u00edodo\u2026"),
          tags$button(
            id = "btn_chat_enviar", class = "btn chat-send-btn action-button",
            icon("paper-plane")
          )
        ),
        # Submit on Enter
        tags$script(HTML("
          $(document).on('keydown', '#chat_pregunta', function(e) {
            if (e.key === 'Enter' && !e.shiftKey) {
              e.preventDefault();
              $('#btn_chat_enviar').click();
            }
          });
        "))
      ),
      footer = modalButton("Cerrar"),
      size = "l",
      easyClose = TRUE
    ))
  })

  # ── Chip de sugerencia seleccionado ─────────────────────────────────────────
  observeEvent(input$chat_chip, {
    updateTextInput(session, "chat_pregunta", value = input$chat_chip)
  })

  # ── Enviar pregunta al agente ────────────────────────────────────────────────
  observeEvent(input$btn_chat_enviar, {
    pregunta <- trimws(if (is.null(input$chat_pregunta)) "" else input$chat_pregunta)
    if (!nzchar(pregunta) || chat_procesando()) return()

    hist <- chat_historial()
    hist <- c(hist, list(list(role = "user", content = pregunta, sql = NULL)))
    chat_historial(hist)
    chat_procesando(TRUE)
    updateTextInput(session, "chat_pregunta", value = "")

    f <- fechas()
    fecha_inicio <- f$start
    fecha_fin    <- f$end
    # Capturar historial ANTES de onFlushed (fuera del contexto reactivo lo pierde)
    hist_snapshot <- chat_historial()

    session$onFlushed(once = TRUE, function() {

      # ── Paso 1: Generar SQL ───────────────────────────────────────────────────
      schema_sys <- paste0(
        "Eres un agente SQL para un dashboard de noticias de prensa chilena.\n",
        "Per\u00edodo activo del dashboard: ", fecha_inicio, " al ", fecha_fin, ".\n\n",
        "TABLAS DISPONIBLES (PostgreSQL):\n",
        "  noticias(id TEXT, titulo TEXT, fuente TEXT, fecha DATE, url TEXT)  -- ~853,000 art\u00edculos\n",
        "  titulos_terminos_diarios(fecha DATE, termino TEXT, frecuencia INTEGER)  -- frecuencia diaria de t\u00e9rminos en titulares\n",
        "  titulos_terminos_por_medio(fecha DATE, fuente TEXT, termino TEXT, frecuencia INTEGER)\n",
        "  mv_terminos_mensuales(fecha DATE, termino TEXT, frecuencia INTEGER)  -- agregado mensual\n",
        "  mv_terminos_por_medio_mensuales(fecha DATE, fuente TEXT, termino TEXT, frecuencia INTEGER)\n\n",
        "REGLAS:\n",
        "  1. Escribe UNA sola consulta SELECT. Nada m\u00e1s.\n",
        "  2. Usa el per\u00edodo del dashboard como filtro por defecto si el usuario no especifica fechas.\n",
        "  3. Para buscar t\u00e9rminos usa: LOWER(termino) ILIKE '%palabra%'\n",
        "  4. A\u00f1ade siempre LIMIT (m\u00e1x 30 para listas, 1 para totales).\n",
        "  5. Responde SOLO con el SQL puro, sin explicaciones ni bloques markdown."
      )

      # Incluir último intercambio para soporte de preguntas de seguimiento
      msgs_sql  <- list(list(role = "system", content = schema_sys))
      user_msgs <- Filter(function(m) m$role == "user", hist_snapshot)
      if (length(user_msgs) >= 2L) {
        prev_q <- user_msgs[[length(user_msgs) - 1L]]$content
        msgs_sql <- c(msgs_sql,
          list(list(role = "user",      content = prev_q)),
          list(list(role = "assistant", content = "[SQL generado]"))
        )
      }
      msgs_sql <- c(msgs_sql, list(list(role = "user", content = pregunta)))

      sql_raw <- tryCatch({
        resp <- httr2::request("http://localhost:11434/api/chat") |>
          httr2::req_body_json(list(
            model    = "qwen2.5:3b",
            stream   = FALSE,
            messages = msgs_sql,
            options  = list(temperature = 0.05, num_predict = 250L)
          )) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()
        httr2::resp_body_json(resp)$message$content
      }, error = function(e) NULL)

      # Limpiar y validar SQL
      sql_clean <- if (!is.null(sql_raw)) {
        s <- gsub("```sql|```", "", sql_raw)
        s <- trimws(s)
        m <- regmatches(s, regexpr("(?i)SELECT[\\s\\S]+", s, perl = TRUE))
        if (length(m) > 0L &&
            !grepl("(?i)(DROP|DELETE|INSERT|UPDATE|ALTER|CREATE|TRUNCATE|GRANT|REVOKE)", m[1L])) {
          sq <- m[1L]
          if (!grepl("(?i)LIMIT", sq)) paste(sq, "LIMIT 30") else sq
        } else NULL
      } else NULL

      # ── Paso 2: Ejecutar SQL ──────────────────────────────────────────────────
      resultado_sql <- if (!is.null(sql_clean)) {
        con <- poolCheckout(pool)
        res <- tryCatch(
          DBI::dbGetQuery(con, sql_clean),
          error = function(e) list(error = conditionMessage(e))
        )
        poolReturn(con)
        res
      } else NULL

      # ── Paso 3: Sintetizar respuesta ──────────────────────────────────────────
      respuesta <- if (is.null(sql_clean) || is.null(resultado_sql)) {
        "No pude formular una consulta para esa pregunta. Intenta reformularla con m\u00e1s detalle."
      } else if (is.list(resultado_sql) && !is.data.frame(resultado_sql) && !is.null(resultado_sql$error)) {
        paste0("Error al consultar la base de datos: ", resultado_sql$error)
      } else if (is.data.frame(resultado_sql) && nrow(resultado_sql) == 0L) {
        "La consulta no devuelve resultados para ese per\u00edodo o t\u00e9rmino."
      } else {
        data_txt <- paste(capture.output(print(resultado_sql, row.names = FALSE)), collapse = "\n")
        synth_sys <- paste(
          "Eres un analista de prensa chilena. Responde la pregunta en espa\u00f1ol,",
          "de forma directa y concisa (2-4 oraciones). Basa la respuesta exclusivamente",
          "en los datos proporcionados. Menciona cifras concretas cuando estén disponibles.",
          "No expliques el SQL ni menciones tablas de base de datos."
        )
        tryCatch({
          resp2 <- httr2::request("http://localhost:11434/api/chat") |>
            httr2::req_body_json(list(
              model    = "qwen2.5:3b",
              stream   = FALSE,
              messages = list(
                list(role = "system", content = synth_sys),
                list(role = "user",   content = paste0(
                  "Pregunta: ", pregunta, "\n\nDatos:\n", data_txt
                ))
              ),
              options  = list(temperature = 0.3, num_predict = 300L)
            )) |>
            httr2::req_timeout(60) |>
            httr2::req_perform()
          httr2::resp_body_json(resp2)$message$content
        }, error = function(e) paste0("Error al conectar con Ollama: ", conditionMessage(e)))
      }

      hist2 <- isolate(chat_historial())
      hist2 <- c(hist2, list(list(role = "assistant", content = respuesta, sql = sql_clean)))
      chat_historial(hist2)
      chat_procesando(FALSE)
    })
  })

  # ── Renderizar el chat ───────────────────────────────────────────────────────
  output$chat_mensajes_ui <- renderUI({
    hist       <- chat_historial()
    procesando <- chat_procesando()

    # Estado inicial: chips de sugerencia
    if (length(hist) == 0L && !procesando) {
      sugerencias <- c(
        "\u00bfCu\u00e1les son los t\u00e9rminos m\u00e1s frecuentes del per\u00edodo?",
        "\u00bfQu\u00e9 medio public\u00f3 m\u00e1s noticias?",
        "\u00bfCu\u00e1ntas noticias hay en total?",
        "\u00bfCu\u00e1l fue el t\u00e9rmino m\u00e1s mencionado cada mes?"
      )
      return(div(class = "chat-messages", style = "justify-content: center;",
        tags$p(class = "chat-welcome",
               "Puedes preguntarme sobre las noticias del per\u00edodo seleccionado:"),
        div(class = "chat-chips",
          lapply(sugerencias, function(q) {
            tags$button(class = "chip-btn",
              onclick = paste0('Shiny.setInputValue("chat_chip","',
                               gsub('"', '\\\\"', q, fixed = TRUE),
                               '",{priority:"event"})'),
              q)
          })
        )
      ))
    }

    # Mensajes
    bubbles <- lapply(hist, function(m) {
      if (m$role == "user") {
        div(class = "chat-msg user",
          div(class = "chat-bubble", m$content)
        )
      } else {
        div(class = "chat-msg assistant",
          div(class = "chat-avatar", icon("robot")),
          div(
            div(class = "chat-bubble", m$content),
            if (!is.null(m$sql) && nzchar(m$sql))
              tags$details(
                tags$summary(class = "chat-sql-toggle", "Ver SQL generado"),
                div(class = "chat-sql-code", m$sql)
              )
            else NULL
          )
        )
      }
    })

    if (procesando) {
      bubbles <- c(bubbles, list(
        div(class = "chat-msg assistant",
          div(class = "chat-avatar", icon("robot")),
          div(class = "chat-bubble chat-typing",
            tags$span(), tags$span(), tags$span()
          )
        )
      ))
    }

    tagList(
      div(class = "chat-messages", id = "chat-msgs-box", bubbles),
      tags$script("(function(){ var b=document.getElementById('chat-msgs-box'); if(b) b.scrollTop=b.scrollHeight; })();")
    )
  })

  output$resumen_periodo_ui <- renderUI({ NULL })

  # ---- UI y gráfico pestaña Medios: panel de términos ----
  output$selector_terminos_medios <- renderUI({
    disponibles <- terminos_medios_disponibles()
    anadidos    <- terminos_anadidos_por_clic_medios()
    if (length(disponibles) == 0) return(NULL)
    choices <- setNames(disponibles, disponibles)
    top <- top_10_medios()
    current <- isolate(input$terminos_medios)
    top_sel <- if (!is.null(current) && length(current) > 0) {
      intersect(current, top$termino)
    } else {
      if (nrow(top) >= 2) head(top$termino, 2) else if (nrow(top) == 1) top$termino else character(0)
    }
    selected <- unique(c(top_sel, intersect(anadidos, disponibles)))
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

  observeEvent(input$terminos_medios, {
    anadidos <- terminos_anadidos_por_clic_medios()
    desmarcados <- setdiff(anadidos, input$terminos_medios)
    if (length(desmarcados) > 0)
      terminos_anadidos_por_clic_medios(setdiff(anadidos, desmarcados))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  output$frecuencia_termino_medios <- renderUI({
    busq <- trimws(if (is.null(input$busqueda_termino_medios)) "" else input$busqueda_termino_medios)
    if (nchar(busq) == 0)
      return(tags$p(class = "frecuencia-termino-msg", tr("trends.term_search.hint")))
    res <- resultados_busqueda_termino_medios()
    if (is.null(res) || nrow(res) == 0)
      return(tags$p(class = "frecuencia-termino-msg", paste0(tr("trends.term_search.no_match"), " \"", busq, "\".")))
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
      tags$p(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 4px;", paste0(tr("trends.term_search.variants_found"), " '", busq, "':")),
      tags$div(class = "frecuencia-termino-lista", items)
    )
  })

  observeEvent(input$termo_añadir_medios, {
    term <- input$termo_añadir_medios
    if (is.null(term) || !nzchar(trimws(term))) return()
    terminos_anadidos_por_clic_medios(unique(c(terminos_anadidos_por_clic_medios(), term)))
  })

  # Gráfico: repetición de conceptos por medio (barras agrupadas)
  output$grafico_conceptos_por_medio <- renderPlotly({
    d <- datos_conceptos_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
        layout(
          title = list(text = tr("trends.chart.no_terms_selected"), font = list(size = 14)),
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
    metrica_label <- tr("media.chart.metric_label")
    p <- p %>% layout(
      barmode = "group",
      xaxis = list(title = tr("media.chart.axis.outlet"), tickangle = -45, categoryorder = "array", categoryarray = medios_orden),
      yaxis = list(title = metrica_label, zeroline = FALSE),
      legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                    title = list(text = tr("media.chart.legend.concept")), font = list(size = 10, family = "Arial, sans-serif"),
                    tracegroupgap = 0),
      margin = list(b = 80, t = 30, l = 60, r = 30)
    )
    p %>% config(displayModeBar = TRUE, locale = current_lang())
  })
  outputOptions(output, "grafico_conceptos_por_medio", suspendWhenHidden = FALSE)

  # ---- Diagnóstico de captura por medio (para colorear el gráfico de volumen) ----
  # Problemas conocidos que no se pueden inferir de los datos (curado a mano).
  problemas_medios_fijos <- function() c(
    izquierdadiario = tr("media.diag.scraper_broken"),
    redgol = tr("media.diag.discontinued")
  )
  DIAG_UMBRAL_GAP_DIAS <- 30L

  diagnostico_medios <- reactive({
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    q <- "
      WITH fechas_medio AS (
        SELECT DISTINCT fuente, fecha FROM noticias WHERE fecha BETWEEN $1 AND $2
      ), gaps AS (
        SELECT fuente, fecha, LAG(fecha) OVER (PARTITION BY fuente ORDER BY fecha) AS prev
        FROM fechas_medio
      ), max_gaps AS (
        SELECT DISTINCT ON (fuente) fuente,
               (fecha - prev - 1)::int AS gap_dias,
               (prev + 1)::text  AS gap_desde,
               (fecha - 1)::text AS gap_hasta
        FROM gaps WHERE prev IS NOT NULL
        ORDER BY fuente, (fecha - prev) DESC
      ), ultimos AS (
        SELECT fuente, MAX(fecha) AS ultima FROM fechas_medio GROUP BY fuente
      )
      SELECT u.fuente, COALESCE(m.gap_dias, 0) AS gap_dias,
             m.gap_desde, m.gap_hasta, u.ultima::text AS ultima
      FROM ultimos u LEFT JOIN max_gaps m USING (fuente)
    "
    d <- tryCatch(dbGetQuery(pool, q, params = list(start, f$end)),
                  error = function(e) data.frame())
    if (nrow(d) == 0) return(d)
    d$dias_inactivo <- as.integer(f$end - as.Date(d$ultima))
    d$estado <- "ok"
    d$problema <- ""
    con_gap <- d$gap_dias >= DIAG_UMBRAL_GAP_DIAS
    d$estado[con_gap] <- "gap"
    d$problema[con_gap] <- sprintf(
      tr("media.diag.gap"),
      format(as.Date(d$gap_desde[con_gap]), "%d-%m-%Y"),
      format(as.Date(d$gap_hasta[con_gap]), "%d-%m-%Y"),
      d$gap_dias[con_gap])
    inactivo <- d$dias_inactivo >= DIAG_UMBRAL_GAP_DIAS
    d$estado[inactivo] <- "malo"
    d$problema[inactivo] <- sprintf(
      tr("media.diag.inactive"),
      format(as.Date(d$ultima[inactivo]), "%d-%m-%Y"))
    problemas_fijos <- problemas_medios_fijos()
    fijo <- d$fuente %in% names(problemas_fijos)
    d$estado[fijo] <- "malo"
    d$problema[fijo] <- paste0("⛔ ", problemas_fijos[d$fuente[fijo]])
    d
  })

  # Gráfico: distribución por medio (pestaña Medios)
  output$grafico_por_medio <- renderPlotly({
    d <- noticias_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly() %>%
        add_trace(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(opacity = 0, size = 0.1), hoverinfo = "skip", showlegend = FALSE) %>%
        layout(
          title = list(text = tr("trends.top_terms_chart.no_data"), font = list(size = 14)),
          margin = list(t = 60),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        ) %>% config(displayModeBar = FALSE))
    }
    d <- d[order(d$total), ]
    diag <- diagnostico_medios()
    d$estado <- "ok"
    d$problema <- ""
    if (nrow(diag) > 0) {
      idx <- match(d$medio, diag$fuente)
      hay <- !is.na(idx)
      d$estado[hay] <- diag$estado[idx[hay]]
      d$problema[hay] <- diag$problema[idx[hay]]
    }
    colores <- colorRampPalette(c("#6c9bd1", "#0d6efd"))(nrow(d))
    colores[d$estado == "gap"]  <- "#f59e0b"
    colores[d$estado == "malo"] <- "#dc2626"
    plot_ly(
      x = d$total,
      y = factor(d$medio, levels = d$medio),
      customdata = ifelse(nzchar(d$problema), paste0("<br>", d$problema), ""),
      type = "bar", orientation = "h",
      marker = list(
        color = colores,
        line = list(color = "rgba(255,255,255,0)", width = 0)
      ),
      hovertemplate = paste0("<b>%{y}</b><br>", tr("media.chart.hover.news"), "%{x:,.0f}%{customdata}<extra></extra>")
    ) %>%
    layout(
      xaxis = list(title = tr("media.chart.axis.news_count"), zeroline = FALSE, showgrid = TRUE, gridcolor = "#eee"),
      yaxis = list(title = NULL, tickfont = list(size = 11)),
      margin = list(l = 130, r = 30, t = 20, b = 50),
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff"
    ) %>%
    config(displayModeBar = TRUE, locale = current_lang())
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
    if (length(medios) == 0) return(tags$p(class = "small-metric", tr("media.selector.no_outlets")))
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
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("media.network.no_data"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    if (!requireNamespace("igraph", quietly = TRUE)) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("media.network.missing_igraph"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    PALETA_COMUNIDADES <- c(
      "#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6",
      "#1abc9c", "#e67e22", "#e91e63", "#00bcd4", "#8bc34a",
      "#d35400", "#795548", "#607d8b", "#673ab7", "#ff9800"
    )
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
    comunidades <- tryCatch(
      igraph::cluster_louvain(g),
      error = function(e) igraph::cluster_walktrap(g)
    )
    membresia <- igraph::membership(comunidades)
    n_com <- max(membresia, 1L)
    edge_x <- numeric(0)
    edge_y <- numeric(0)
    for (k in seq_len(igraph::ecount(g))) {
      ends <- igraph::ends(g, k)
      i1 <- match(ends[1], nodo_ids)
      i2 <- match(ends[2], nodo_ids)
      edge_x <- c(edge_x, coords[i1, 1], coords[i2, 1], NA)
      edge_y <- c(edge_y, coords[i1, 2], coords[i2, 2], NA)
    }
    p <- plot_ly() %>%
      add_trace(
        x = edge_x, y = edge_y,
        type = "scatter", mode = "lines",
        line = list(color = "rgba(160,160,190,0.4)", width = 1),
        hoverinfo = "skip", showlegend = FALSE
      )
    for (com in seq_len(n_com)) {
      idx <- which(membresia == com)
      if (length(idx) == 0L) next
      sz <- 7 + 14 * (grado[idx] / max(grado, 1L))
      color_com <- PALETA_COMUNIDADES[(com - 1L) %% length(PALETA_COMUNIDADES) + 1L]
      p <- p %>% add_trace(
        x = coords[idx, 1], y = coords[idx, 2],
        type = "scatter", mode = "markers+text",
        name = paste0(tr("media.network.legend.group"), " ", com),
        text = nodo_ids[idx],
        textposition = "top center",
        textfont = list(size = 11, color = "#333"),
        marker = list(
          size = sz,
          color = color_com,
          opacity = 0.85,
          line = list(color = "#ffffff", width = 1.5)
        ),
        customdata = grado[idx],
        hovertemplate = paste0("<b>%{text}</b><br>", tr("media.network.hover.connections"), "%{customdata}<extra></extra>"),
        showlegend = TRUE
      )
    }
    if (store_disponible() && !is.null(input$umbral_semantico)) {
      emb_fn <- tryCatch(ragnar::embed_ollama(model = "nomic-embed-text"), error = function(e) NULL)
      if (!is.null(emb_fn)) {
        emb_matrix <- tryCatch(emb_fn(nodo_ids), error = function(e) NULL)
        if (!is.null(emb_matrix) && requireNamespace("proxy", quietly = TRUE)) {
          sim_mat <- as.matrix(proxy::simil(emb_matrix, method = "cosine"))
          umbral <- input$umbral_semantico
          pares_existentes <- paste(datos$termino_a, datos$termino_b)
          sem_x <- numeric(0)
          sem_y <- numeric(0)
          for (ia in seq_len(nrow(sim_mat) - 1L)) {
            for (ib in seq(ia + 1L, ncol(sim_mat))) {
              if (sim_mat[ia, ib] >= umbral) {
                par_ab <- paste(nodo_ids[ia], nodo_ids[ib])
                par_ba <- paste(nodo_ids[ib], nodo_ids[ia])
                if (!par_ab %in% pares_existentes && !par_ba %in% pares_existentes) {
                  sem_x <- c(sem_x, coords[ia, 1], coords[ib, 1], NA)
                  sem_y <- c(sem_y, coords[ia, 2], coords[ib, 2], NA)
                }
              }
            }
          }
          if (length(sem_x) > 0L) {
            p <- p %>% add_trace(
              x = sem_x, y = sem_y,
              type = "scatter", mode = "lines",
              name = tr("media.network.legend.semantic"),
              line = list(color = "rgba(13,110,253,0.3)", width = 1.5, dash = "dot"),
              hoverinfo = "skip", showlegend = TRUE
            )
          }
        }
      }
    }
    p %>% layout(
      autosize = TRUE,
      legend = list(
        orientation = "v", x = 0.01, xanchor = "left", y = 0.99, yanchor = "top",
        title = list(text = tr("media.network.legend.communities")),
        bgcolor = "rgba(255,255,255,0.82)", bordercolor = "#e2e8f0", borderwidth = 1,
        font = list(size = 10)
      ),
      xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      margin = list(l = 10, r = 10, t = 20, b = 10),
      plot_bgcolor = "#fafafa",
      paper_bgcolor = "#fafafa"
    ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # Reactivo: sentimiento por fuente
  sentimiento_por_fuente <- reactive({
    f <- fechas()
    start <- sent_inicio(f$start)
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

  # ---- Sentimiento: helpers y paleta ----
  COL_SENT <- c(positivo = "#16a34a", neutral = "#94a3b8", negativo = "#dc2626")
  NOMBRES_MEDIOS <- c(
    "24horas"="24 Horas","adnradio"="ADN Radio","agricultura"="Agricultura","biobio"="Radio BíoBío",
    "chvnoticias"="CHV Noticias","ciper"="Ciper","cnnchile"="CNN Chile","cooperativa"="Cooperativa",
    "diariofinanciero"="D. Financiero","elciudadano"="El Ciudadano","eldinamo"="El Dínamo",
    "elmostrador"="El Mostrador","elsiglo"="El Siglo","emol"="Emol","exante"="Ex-Ante",
    "lacuarta"="La Cuarta","lahora"="La Hora","lanacion"="La Nación","latercera"="La Tercera",
    "meganoticias"="Meganoticias","publimetro"="Publimetro","radiouchile"="Radio U. de Ch.",
    "t13"="T13","theclinic"="The Clinic","redgol"="RedGol","lasegunda"="La Segunda",
    "eldesconcierto"="El Desconcierto","quintopoder"="El Quinto Poder","izquierdadiario"="La Izquierda Diario")
  nombres_medios <- function(x) { y <- NOMBRES_MEDIOS[x]; y[is.na(y)] <- x[is.na(y)]; unname(y) }
  sent_sin_datos <- function(msg = tr("sent.no_data")) {
    plot_ly(type = "scatter", mode = "markers") %>%
      add_annotations(text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                      showarrow = FALSE, font = list(size = 15, color = "#94a3b8", family = "Inter, sans-serif")) %>%
      layout(xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
             yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)) %>%
      config(displayModeBar = FALSE)
  }

  # ---- Estado de filtros del tab de sentimiento ----
  medio_sel <- reactive({ m <- input$sent_filtro_medio; if (is.null(m) || !nzchar(m)) "todos" else m })
  tono_sel  <- reactiveVal("todos")
  # clic en la barra: alterna el tono (clic en el activo, o "× limpiar", vuelve a todos)
  observeEvent(input$sent_bar_click, {
    clic <- input$sent_bar_click
    if (is.null(clic)) return()
    if (identical(clic, "__clear__")) { tono_sel("todos"); return() }
    tono_sel(if (identical(tono_sel(), clic)) "todos" else clic)
  })
  # al cambiar de medio, resetear el filtro de tono
  observeEvent(medio_sel(), { tono_sel("todos") }, ignoreInit = TRUE)
  # subtítulo dinámico
  output$sent_subtitulo <- renderUI({
    m <- medio_sel()
    tags$p(class = "sent-hint", if (m == "todos")
      tr("sent.subtitle.all_media")
      else paste0(tr("sent.subtitle.showing_prefix"), " ", nombres_medios(m)))
  })

  # ---- Cobertura real del análisis de sentimiento ----
  # No sirve el MIN/MAX global: existe una isla dic-2022/ene-2023 clasificada con
  # un modelo antiguo de menor calidad (qwen2.5:3b) y luego nada hasta 2026.
  # La cobertura vigente es el bloque contiguo MÁS RECIENTE de meses con
  # clasificación real; el tab se limita a ese bloque para ser coherente.
  sent_cobertura_fechas <- reactive({
    r <- tryCatch(dbGetQuery(pool, "
      SELECT date_trunc('month', n.fecha)::date::text AS mes,
             COUNT(*)::int AS clasif,
             MIN(n.fecha)::text AS desde_mes,
             MAX(n.fecha)::text AS hasta_mes
      FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id
      GROUP BY 1 ORDER BY 1 DESC"),
      error = function(e) NULL)
    if (is.null(r) || nrow(r) == 0L) return(NULL)
    r <- r[r$clasif >= 200L, , drop = FALSE]  # meses con clasificación real, no residuos
    if (nrow(r) == 0L) return(NULL)
    meses <- as.Date(r$mes)
    fin <- 1L
    while (fin < nrow(r) && seq(meses[fin], by = "-1 month", length.out = 2L)[2L] == meses[fin + 1L])
      fin <- fin + 1L
    list(desde = as.Date(min(r$desde_mes[seq_len(fin)])),
         hasta = as.Date(max(r$hasta_mes[seq_len(fin)])))
  })

  # Inicio efectivo de las consultas del tab: nunca antes de la cobertura vigente
  sent_inicio <- function(f_start) {
    cob <- sent_cobertura_fechas()
    s <- max(f_start, FECHA_DESDE_DASHBOARD)
    if (!is.null(cob)) s <- max(s, cob$desde)
    s
  }

  # Aviso cuando el rango elegido va más allá de lo clasificado (p. ej. desde 2018):
  # sin esto, el usuario asume que el sentimiento cubre todo el rango de fechas.
  output$sent_aviso_rango <- renderUI({
    cob <- sent_cobertura_fechas()
    if (is.null(cob)) return(NULL)
    f <- fechas()
    start <- max(f$start, FECHA_DESDE_DASHBOARD)
    if (start >= cob$desde && f$end <= cob$hasta) return(NULL)
    div(class = "sent-aviso-rango",
      tags$span("⚠"),
      tags$span(HTML(sprintf(
        tr("sent.coverage_notice"),
        format(cob$desde, "%d-%m-%Y"), format(cob$hasta, "%d-%m-%Y")))))
  })

  # ---- Reactivo: distribución del período (+ período anterior para tendencia) ----
  sent_counts_rango <- function(start, end, medio = "todos") {
    cond <- ""; params <- list(start, end)
    if (medio != "todos") { params <- c(params, list(medio)); cond <- " AND n.fuente = $3" }
    r <- tryCatch(dbGetQuery(pool, paste0("
      SELECT ns.sentimiento, COUNT(*)::int n
      FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id
      WHERE n.fecha >= $1 AND n.fecha <= $2", cond, " GROUP BY ns.sentimiento"),
      params = params), error = function(e) NULL)
    v <- c(positivo = 0L, neutral = 0L, negativo = 0L)
    if (!is.null(r) && nrow(r) > 0L) v[r$sentimiento] <- as.integer(r$n)
    v
  }
  sentimiento_periodo <- reactive({
    f <- fechas(); start <- sent_inicio(f$start); end <- f$end; m <- medio_sel()
    len <- as.integer(end - start) + 1L
    cur <- sent_counts_rango(start, end, m)
    prev <- sent_counts_rango(start - len, start - 1L, m)
    tot <- sum(cur); totp <- sum(prev)
    list(
      cur = cur, tot = tot,
      pct = if (tot > 0) round(100 * cur / tot) else c(positivo = 0, neutral = 0, negativo = 0),
      net = if (tot > 0) (cur["positivo"] - cur["negativo"]) / tot * 100 else 0,
      net_prev = if (totp > 0) (prev["positivo"] - prev["negativo"]) / totp * 100 else NA_real_
    )
  })

  # ---- Card de tono neto (acento de color según el ánimo dominante) ----
  output$sent_hero_net <- renderUI({
    s <- sentimiento_periodo()
    if (s$tot == 0L) return(div(class = "sent-net-card neu",
      div(class = "lbl", tr("sent.hero.title")), div(class = "big", "—"),
      div(class = "desc", tr("sent.hero.no_data"))))
    net <- round(s$net)
    cls  <- if (net <= -8) "neg" else if (net >= 8) "pos" else "neu"
    desc <- if (net <= -8) tr("sent.hero.dominant_negative")
            else if (net >= 8) tr("sent.hero.dominant_positive")
            else tr("sent.hero.balanced")
    delta <- if (is.na(s$net_prev)) NA_real_ else net - round(s$net_prev)
    vs <- if (is.na(delta)) NULL else div(class = "vs",
      sprintf("%s %d %s", if (delta > 0) "▲" else if (delta < 0) "▼" else "→", abs(delta), tr("sent.hero.vs_previous")))
    div(class = paste("sent-net-card", cls),
      div(class = "lbl", tr("sent.hero.title")),
      div(class = "big", sprintf("%+d", net)),
      div(class = "desc", desc),
      vs)
  })

  # ---- Barra 100% interactiva (clic = filtro de tono de la tabla) ----
  output$sent_barra_periodo <- renderUI({
    s <- sentimiento_periodo()
    if (s$tot == 0L) return(div(class = "small-metric", tr("sent.bar.no_data")))
    fr <- s$cur / s$tot * 100
    activo <- tono_sel()
    seg <- function(tono, cls, val) {
      if (val <= 0) return(NULL)
      div(class = trimws(paste("seg", cls, if (identical(activo, tono)) "active" else "")),
          onclick = sprintf("Shiny.setInputValue('sent_bar_click','%s',{priority:'event'})", tono),
          style = sprintf("flex-basis:%.4f%%;", val),
          if (val >= 8) paste0(round(val), "%") else "")
    }
    chip_cls <- c(negativo = "neg", neutral = "neu", positivo = "pos")
    chip_lab <- c(negativo = tr("sent.chip.negative"), neutral = tr("sent.chip.neutral"), positivo = tr("sent.chip.positive"))
    filtro_info <- if (activo != "todos")
      div(class = "sent-filtro-activo",
        tags$span(paste0(tr("sent.bar.filtered_to"), " ")),
        tags$span(class = paste("sent-chip", chip_cls[[activo]]), chip_lab[[activo]]),
        tags$button(class = "limpiar", type = "button",
          onclick = "Shiny.setInputValue('sent_bar_click','__clear__',{priority:'event'})", tr("sent.bar.clear")))
    else
      div(class = "sent-filtro-activo",
        tags$span(style = "color:#94a3b8;", tr("sent.bar.click_hint")))
    tagList(
      div(class = trimws(paste("sent-bar", if (activo != "todos") "has-sel" else "")),
        seg("negativo", "neg", fr["negativo"]),
        seg("neutral",  "neu", fr["neutral"]),
        seg("positivo", "pos", fr["positivo"])),
      div(class = "sent-legend vals",
        span(tags$i(style = "background:#dc2626"), paste0(tr("sent.legend.negative"), " ", round(fr["negativo"]), "%")),
        span(tags$i(style = "background:#94a3b8"), paste0(tr("sent.legend.neutral"), " ", round(fr["neutral"]), "%")),
        span(tags$i(style = "background:#16a34a"), paste0(tr("sent.legend.positive"), " ", round(fr["positivo"]), "%"))),
      filtro_info
    )
  })

  # ---- Evolución del tono (área 100% + línea de tono neto) ----
  sentimiento_evolucion <- reactive({
    f <- fechas(); start <- sent_inicio(f$start); end <- f$end; m <- medio_sel()
    cond <- ""; params <- list(start, end)
    if (m != "todos") { params <- c(params, list(m)); cond <- " AND n.fuente = $3" }
    tryCatch(dbGetQuery(pool, paste0("
      SELECT date_trunc('", gran_tiempo(f), "', n.fecha)::date AS periodo, ns.sentimiento, COUNT(*)::int n
      FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id
      WHERE n.fecha >= $1 AND n.fecha <= $2", cond, " GROUP BY 1, 2 ORDER BY 1"),
      params = params), error = function(e) data.frame())
  })
  output$grafico_sent_evolucion <- renderPlotly({
    d <- sentimiento_evolucion()
    if (is.null(d) || nrow(d) == 0L) return(sent_sin_datos())
    periodos <- sort(unique(d$periodo))
    w <- function(s) { v <- setNames(rep(0L, length(periodos)), as.character(periodos));
      sub <- d[d$sentimiento == s, ]; v[as.character(sub$periodo)] <- as.integer(sub$n); as.integer(v) }
    neg <- w("negativo"); neu <- w("neutral"); pos <- w("positivo")
    tot <- neg + neu + pos; tot[tot == 0] <- 1L
    net <- round((pos - neg) / tot * 100)
    periodoD <- as.Date(periodos)
    plot_ly() %>%
      add_trace(x = periodoD, y = neg, name = tr("sent.legend.negative"), type = "scatter", mode = "none",
                stackgroup = "one", groupnorm = "percent", fillcolor = COL_SENT["negativo"],
                hovertemplate = paste0("%{y:.0f}%<extra>", tr("sent.legend.negative"), "</extra>")) %>%
      add_trace(x = periodoD, y = neu, name = tr("sent.legend.neutral"), type = "scatter", mode = "none",
                stackgroup = "one", fillcolor = COL_SENT["neutral"],
                hovertemplate = paste0("%{y:.0f}%<extra>", tr("sent.legend.neutral"), "</extra>")) %>%
      add_trace(x = periodoD, y = pos, name = tr("sent.legend.positive"), type = "scatter", mode = "none",
                stackgroup = "one", fillcolor = COL_SENT["positivo"],
                hovertemplate = paste0("%{y:.0f}%<extra>", tr("sent.legend.positive"), "</extra>")) %>%
      add_trace(x = periodoD, y = net, name = tr("sent.legend.net_tone"), type = "scatter", mode = "lines+markers",
                yaxis = "y2", line = list(color = "#0f172a", width = 2),
                marker = list(size = 4, color = "#0f172a"),
                hovertemplate = paste0(tr("sent.chart.hover.net_tone"), "%{y}<extra></extra>")) %>%
      layout(font = list(family = "Inter, sans-serif"),
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(title = tr("sent.chart.axis.composition"), ticksuffix = "%", range = c(0, 100), showgrid = FALSE),
        yaxis2 = list(title = tr("sent.legend.net_tone"), overlaying = "y", side = "right", range = c(-100, 100),
                      zeroline = TRUE, zerolinecolor = "#cbd5e1", showgrid = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 11)),
        hovermode = "x unified", margin = list(t = 34, b = 28, l = 52, r = 56)) %>%
      config(displayModeBar = FALSE, locale = current_lang())
  })

  # ---- Por medio (100% apilado horizontal, ordenado por tono neto) ----
  output$grafico_sentimiento_por_fuente <- renderPlotly({
    d <- sentimiento_por_fuente()
    if (nrow(d) == 0L) return(sent_sin_datos())
    medios <- unique(d$medio)
    mat <- function(s) { v <- setNames(rep(0L, length(medios)), medios);
      sub <- d[d$sentimiento == s, ]; v[sub$medio] <- as.integer(sub$n); v }
    neg <- mat("negativo"); neu <- mat("neutral"); pos <- mat("positivo")
    tot <- neg + neu + pos
    keep <- tot >= 10L                      # ocultar medios con muy pocos clasificados (ruido)
    if (!any(keep)) keep <- rep(TRUE, length(tot))
    medios <- medios[keep]; neg <- neg[keep]; neu <- neu[keep]; pos <- pos[keep]; tot <- tot[keep]
    totp <- ifelse(tot == 0, 1, tot)
    pneg <- neg / totp * 100; pneu <- neu / totp * 100; ppos <- pos / totp * 100
    ord <- order(ppos - pneg)               # ascendente → más positivo arriba (barras h se apilan de abajo)
    nm <- nombres_medios(medios[ord]); yf <- factor(nm, levels = nm)
    plot_ly() %>%
      add_trace(y = yf, x = pneg[ord], name = tr("sent.legend.negative"), type = "bar", orientation = "h",
                marker = list(color = COL_SENT["negativo"]), customdata = neg[ord],
                hovertemplate = paste0("%{x:.0f}%  (%{customdata})<extra>", tr("sent.legend.negative"), "</extra>")) %>%
      add_trace(y = yf, x = pneu[ord], name = tr("sent.legend.neutral"), type = "bar", orientation = "h",
                marker = list(color = COL_SENT["neutral"]), customdata = neu[ord],
                hovertemplate = paste0("%{x:.0f}%  (%{customdata})<extra>", tr("sent.legend.neutral"), "</extra>")) %>%
      add_trace(y = yf, x = ppos[ord], name = tr("sent.legend.positive"), type = "bar", orientation = "h",
                marker = list(color = COL_SENT["positivo"]), customdata = pos[ord],
                hovertemplate = paste0("%{x:.0f}%  (%{customdata})<extra>", tr("sent.legend.positive"), "</extra>")) %>%
      layout(barmode = "stack", font = list(family = "Inter, sans-serif"),
        xaxis = list(title = "", ticksuffix = "%", range = c(0, 100), showgrid = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.04, font = list(size = 11)),
        margin = list(t = 26, b = 20, l = 10, r = 12)) %>%
      config(displayModeBar = FALSE, locale = current_lang())
  })

  # ---- Evolución en el tiempo, y debajo el ranking de medios (cuando es "Todos") ----
  output$sent_bento <- renderUI({
    evo <- div(class = "chart-card",
      h4(tr("sent.bento.evolution.title")),
      tags$p(class = "small-metric", tr("sent.bento.evolution.hint")),
      plotlyOutput("grafico_sent_evolucion", height = "400px"))
    if (medio_sel() == "todos") {
      tagList(evo,
        div(class = "chart-card",
          h4(tr("sent.bento.by_outlet.title")),
          tags$p(class = "small-metric", tr("sent.bento.by_outlet.hint")),
          plotlyOutput("grafico_sentimiento_por_fuente", height = "640px")))
    } else {
      evo
    }
  })

  # ---- Poblar selector de medios (los que tienen sentimiento) ----
  observe({
    meds <- tryCatch(dbGetQuery(pool,
      "SELECT DISTINCT n.fuente AS f FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id ORDER BY 1")$f,
      error = function(e) character(0))
    if (length(meds) == 0L) return()
    ch <- setNames(meds, nombres_medios(meds))
    ch <- ch[order(names(ch))]
    sel <- isolate(input$sent_filtro_medio); if (is.null(sel)) sel <- "todos"
    updateSelectInput(session, "sent_filtro_medio",
      choices = c(setNames("todos", tr("sent.distribution.all_media")), ch), selected = sel)
  })

  # ---- Tabla de titulares: filtros compartidos + paginación (10 por página) ----
  sent_pagina <- reactiveVal(1L)
  sent_filtros_sql <- reactive({
    f <- fechas(); start <- sent_inicio(f$start); end <- f$end
    filtro <- tono_sel(); medio <- medio_sel()
    conds  <- "WHERE n.fecha >= $1 AND n.fecha <= $2"
    params <- list(start, end)
    if (filtro %in% c("negativo", "neutral", "positivo")) {
      params <- c(params, list(filtro)); conds <- paste0(conds, sprintf(" AND ns.sentimiento = $%d", length(params)))
    }
    if (nzchar(medio) && medio != "todos") {
      params <- c(params, list(medio)); conds <- paste0(conds, sprintf(" AND n.fuente = $%d", length(params)))
    }
    list(conds = conds, params = params)
  })
  observeEvent(list(tono_sel(), medio_sel(), fechas()), { sent_pagina(1L) }, ignoreInit = TRUE)
  sent_titulares_total <- reactive({
    fp <- sent_filtros_sql()
    tryCatch(dbGetQuery(pool, paste0(
      "SELECT COUNT(*)::int n FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id ", fp$conds),
      params = fp$params)$n, error = function(e) 0L)
  })
  sentimiento_titulares <- reactive({
    fp <- sent_filtros_sql(); off <- (sent_pagina() - 1L) * 10L
    tryCatch(dbGetQuery(pool, paste0(
      "SELECT n.fecha, n.fuente, n.titulo, ns.sentimiento, ns.confianza
       FROM noticias n JOIN noticias_sentimiento ns ON ns.id = n.id ", fp$conds,
      " ORDER BY n.fecha DESC, ns.procesado_en DESC LIMIT 10 OFFSET ", off),
      params = fp$params), error = function(e) data.frame())
  })
  observeEvent(input$sent_pag_prev, { if (sent_pagina() > 1L) sent_pagina(sent_pagina() - 1L) })
  observeEvent(input$sent_pag_next, {
    np <- max(1L, ceiling(sent_titulares_total() / 10L))
    if (sent_pagina() < np) sent_pagina(sent_pagina() + 1L)
  })
  output$sent_paginacion <- renderUI({
    total <- sent_titulares_total()
    if (total == 0L) return(NULL)
    np <- max(1L, ceiling(total / 10L)); pg <- min(sent_pagina(), np)
    desde <- (pg - 1L) * 10L + 1L; hasta <- as.integer(min(pg * 10L, total))
    div(style = "display:flex; align-items:center; justify-content:space-between; gap:12px; margin-top:14px; flex-wrap:wrap;",
      tags$span(class = "small-metric", style = "margin:0;",
        sprintf("%s %d–%d %s %s", tr("sent.pagination.showing"), desde, hasta, tr("trends.pagination.of"), format(as.integer(total), big.mark = ".", decimal.mark = ","))),
      div(style = "display:flex; align-items:center; gap:10px;",
        actionButton("sent_pag_prev", tr("trends.pagination.prev"), class = "btn-sm", disabled = pg <= 1L),
        tags$span(class = "small-metric", style = "margin:0;", sprintf("%s %d %s %d", tr("trends.pagination.page"), pg, tr("trends.pagination.of"), np)),
        actionButton("sent_pag_next", tr("trends.pagination.next"), class = "btn-sm", disabled = pg >= np))
    )
  })
  output$tabla_sent_titulares <- renderUI({
    d <- sentimiento_titulares()
    if (is.null(d) || nrow(d) == 0L)
      return(div(class = "small-metric", tr("sent.table.no_headlines")))
    cls_map <- c(positivo = "pos", neutral = "neu", negativo = "neg")
    lab_map <- c(positivo = tr("sent.legend.positive"), neutral = tr("sent.legend.neutral"), negativo = tr("sent.legend.negative"))
    filas <- lapply(seq_len(nrow(d)), function(i) {
      s <- d$sentimiento[i]; conf <- d$confianza[i]
      tags$tr(
        tags$td(class = "meta-celda",
          tags$span(class = "fecha", format(as.Date(d$fecha[i]), "%d-%m-%Y")),
          tags$span(class = "medio", nombres_medios(d$fuente[i]))),
        tags$td(d$titulo[i]),
        tags$td(style = "width:1%; text-align:center;",
          tags$span(class = paste("sent-chip", cls_map[[s]]), lab_map[[s]])),
        tags$td(class = "conf", if (is.na(conf)) "" else paste0(conf, "%"))
      )
    })
    tags$table(class = "sent-tabla", tags$tbody(filas))
  })

  # ---- Nota de transparencia / cobertura ----
  output$sent_meta <- renderUI({
    f <- fechas(); start <- sent_inicio(f$start); end <- f$end
    m <- tryCatch(dbGetQuery(pool, "
      SELECT COUNT(ns.id)::int clasif, COUNT(*)::int total,
             ROUND(AVG(ns.confianza))::int conf, MAX(ns.modelo) modelo
      FROM noticias n LEFT JOIN noticias_sentimiento ns ON ns.id = n.id
      WHERE n.fecha >= $1 AND n.fecha <= $2 AND n.titulo IS NOT NULL",
      params = list(start, end)), error = function(e) NULL)
    if (is.null(m) || is.na(m$total) || m$total == 0L) return(NULL)
    cob <- round(100 * m$clasif / m$total)
    modelo <- if (is.na(m$modelo)) "—" else m$modelo
    conf <- if (is.na(m$conf)) "s/d" else paste0(m$conf, "%")
    div(class = "sent-meta", HTML(sprintf(
      tr("sent.meta.coverage"),
      format(m$clasif, big.mark = ".", decimal.mark = ","), format(m$total, big.mark = ".", decimal.mark = ","), cob, modelo, conf)))
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
    start <- sent_inicio(f$start)
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
      plot_ly(type = "scatter", mode = "markers") %>%
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
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(text = tr("media.concept_evol.select_hint"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 13, color = "#6c757d")) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    d <- evolucion_concepto_por_medio()
    if (nrow(d) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(text = paste0(tr("media.concept_evol.no_data_prefix"), " \"", medio_sel, "\" ", tr("media.concept_evol.no_data_suffix")),
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
          hovertemplate = paste0(tr("trends.chart.hover.year"), "%{x}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos_grafico[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        title = list(text = paste0(tr("media.concept_evol.title_prefix"), " \"", medio_sel, "\""), font = list(size = 13), x = 0.5, xanchor = "center"),
        xaxis = list(title = tr("trends.chart.axis.year"), tickvals = sort(unique(d$ano)), zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 50, t = 50, l = 60, r = 30),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
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
          hovertemplate = paste0(tr("trends.chart.hover.date"), "%{x|%d/%m/%Y}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos_grafico[i], "<extra></extra>")
        )
      }
      p <- p %>% layout(
        title = list(text = paste0(tr("media.concept_evol.title_prefix"), " \"", medio_sel, "\""), font = list(size = 13), x = 0.5, xanchor = "center"),
        xaxis = list(type = "date", title = list(text = tr("trends.chart.axis.date"), standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45, tickfont = list(size = 11),
          zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 60, t = 50, l = 60, r = 30),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
      )
    }
    p %>% config(displayModeBar = TRUE, locale = current_lang())
  })

  output$grafico_terminos_por_medio <- renderPlotly({
    d <- terminos_por_medio_df()
    if (nrow(d) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("media.terms_by_outlet.no_data"),
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
      hovertemplate = paste0("<b>%{label}</b><br>", tr("trends.chart.hover.frequency"), "%{value:,.0f}<br>%{percent}<extra></extra>"),
      marker = list(
        colors = colores_pie[seq_len(nrow(d))],
        line = list(color = "#ffffff", width = 1.5)
      ),
      showlegend = FALSE
    ) %>%
    layout(
      title = list(
        text = paste0(tr("media.terms_by_outlet.pie_title_prefix"), " ", selected_medio_terminos()),
        font = list(size = 13), x = 0.5, xanchor = "center"
      ),
      margin = list(l = 20, r = 20, t = 50, b = 20),
      paper_bgcolor = "#ffffff"
    ) %>%
    config(displayModeBar = TRUE, locale = current_lang())
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
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("media.terms_by_outlet.no_data"),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
        config(displayModeBar = FALSE))
    }
    sel_terms <- input$terminos_evol_medio_sel
    if (!is.null(sel_terms) && length(sel_terms) > 0) d <- d[d$termino %in% sel_terms, ]
    if (nrow(d) == 0) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_annotations(
          text = tr("media.terms_by_outlet.select_hint"),
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
          hovertemplate = paste0(tr("trends.chart.hover.year"), "%{x}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos[i], "<extra></extra>")
        )
      }
      anos <- sort(unique(d$ano))
      p <- p %>% layout(
        xaxis = list(title = tr("trends.chart.axis.year"), tickvals = anos, zeroline = FALSE, showgrid = TRUE),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
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
          hovertemplate = paste0(tr("trends.chart.hover.date"), "%{x|%d/%m/%Y}<br>", tr("trends.chart.hover.frequency"), "%{y:.0f}<br>", tr("trends.chart.hover.term"), terminos[i], "<extra></extra>")
        )
      }
      tickvals_ms <- as.numeric(as.POSIXct(breaks_x, tz = "UTC")) * 1000
      p <- p %>% layout(
        xaxis = list(
          type = "date", title = list(text = tr("trends.chart.axis.date"), standoff = 12),
          tickmode = "array", tickvals = tickvals_ms, ticktext = format(breaks_x, tick_fmt),
          tickangle = if (dias_rango <= 31) -25 else -45, tickfont = list(size = 11),
          zeroline = FALSE, showgrid = TRUE
        ),
        yaxis = list(title = tr("trends.chart.axis.frequency"), zeroline = FALSE, showgrid = TRUE),
        legend = list(orientation = "v", x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
                      font = list(size = 10, family = "Arial, sans-serif"), tracegroupgap = 0),
        margin = list(b = 60, t = 30, l = 60, r = 30)
      )
    }
    p %>% config(displayModeBar = TRUE, locale = current_lang())
  })

  outputOptions(output, "grafico_evolucion_concepto_por_medio", suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_evolucion_volumen_por_medio",  suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_terminos_por_medio",           suspendWhenHidden = FALSE)
  outputOptions(output, "grafico_evolucion_terminos_por_medio", suspendWhenHidden = FALSE)
  outputOptions(output, "resumen_periodo_ui",                   suspendWhenHidden = FALSE)
}

# ------------------------------------------------------------------------------
# Lanzar app (desde R o RStudio, o: Rscript -e "shiny::runApp('dashboard', port=3838)")
# Desde la carpeta noticias: runApp("dashboard", port = 3838)
# Si no se actualizan gráficos o selectores: reiniciar la app (Ctrl+C y volver a runApp)
# y/o vaciar caché del navegador (Ctrl+Shift+R o Cmd+Shift+R para recarga forzada).
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
