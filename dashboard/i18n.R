# Traducción ES/EN del dashboard de noticias.
#
# El idioma se decide en el cliente (BabelOS, sitio padre) y se sincroniza
# vía postMessage/localStorage con el shim JS en app.R, que llama a
# Shiny.setInputValue('babel_lang', ...). El servidor lee ese input con
# current_lang() (reactive, definido en app.R) y tr(key) resuelve contra
# este catálogo. No se traduce contenido de datos (titulares reales,
# nombres de medios) — solo texto de interfaz.

i18n_strings <- list(
  "page.title" = list(
    es = "Monitor de Noticias Chile — Seguimiento de tendencias en prensa (2018-2026)",
    en = "Chile News Monitor — Tracking Press Trends (2018-2026)"
  ),
  "page.subtitle" = list(
    es = "Análisis de los temas que dominan los titulares de los principales medios chilenos",
    en = "Analysis of the topics dominating headlines across major Chilean media outlets"
  ),
  "nav.desktop_link" = list(es = "Escritorio", en = "Desktop"),

  "tab.tendencias" = list(es = "Tendencias", en = "Trends"),
  "tab.medios" = list(es = "Medios", en = "Media"),
  "tab.sentimiento" = list(es = "Sentimiento", en = "Sentiment"),
  "tab.mas_info" = list(es = "Más información", en = "More information"),
  "tab.conceptos_por_medio" = list(es = "Conceptos por medio", en = "Concepts by outlet"),
  "tab.terminos_destacados" = list(es = "Términos destacados", en = "Top terms"),
  "tab.volumen_datos" = list(es = "Volumen de datos", en = "Data volume"),
  "tab.red_palabras" = list(es = "Red de palabras", en = "Word network"),

  "sidebar.date_range" = list(es = "Rango de fechas", en = "Date range"),
  "search.placeholder_titles" = list(es = "Buscar en titulares…", en = "Search headlines…"),
  "filter.all_media" = list(es = "Todos", en = "All"),

  "language.label" = list(es = "Idioma", en = "Language"),

  # ── Pestaña Tendencias ──────────────────────────────────────────────────
  "trends.card.unique_words" = list(es = "Palabras únicas en el período", en = "Unique words in the period"),
  "trends.card.top_word" = list(es = "Palabra más mencionada", en = "Most mentioned word"),
  "trends.card.top_word.no_data" = list(es = "Sin datos", en = "No data"),
  "trends.card.top_word.appearances" = list(es = "apariciones", en = "appearances"),
  "trends.volume.label" = list(es = "Volumen: ", en = "Volume: "),
  "trends.volume.suffix" = list(es = " noticias en el período · ", en = " headlines in the period · "),
  "trends.volume.avg_prefix" = list(es = "promedio ", en = "average "),
  "trends.volume.avg_suffix" = list(es = " por día", en = " per day"),
  "trends.evolution.title" = list(es = "¿Cómo han cambiado los temas con el tiempo?", en = "How have topics changed over time?"),
  "trends.evolution.hint" = list(
    es = "Selecciona palabras en el panel izquierdo para ver cómo evolucionó su presencia en los titulares.",
    en = "Select words in the left panel to see how their presence in headlines evolved."
  ),
  "trends.top_terms.title" = list(es = "Las 30 palabras más mencionadas en el período", en = "The 30 most mentioned words in the period"),
  "trends.recent_news.title" = list(es = "Noticias recientes", en = "Recent headlines"),
  "trends.terms_compare.label" = list(es = "Términos para comparar", en = "Terms to compare"),
  "trends.term_search.hint" = list(
    es = "Escribe una palabra arriba para ver cuántas veces aparece en los titulares y sus variantes. Haz clic para añadirla al gráfico.",
    en = "Type a word above to see how many times it appears in headlines and its variants. Click to add it to the chart."
  ),
  "trends.term_search.no_match" = list(es = "No se encontraron términos que coincidan con", en = "No terms found matching"),
  "trends.term_search.variants_found" = list(es = "Variantes encontradas para", en = "Variants found for"),
  "trends.chart.no_terms_selected" = list(es = "Selecciona al menos un término en el panel izquierdo.", en = "Select at least one term in the left panel."),
  "trends.chart.axis.year" = list(es = "Año", en = "Year"),
  "trends.chart.axis.frequency" = list(es = "Frecuencia", en = "Frequency"),
  "trends.chart.axis.date" = list(es = "Fecha", en = "Date"),
  "trends.chart.legend.term" = list(es = "Término", en = "Term"),
  "trends.chart.hover.year" = list(es = "Año: ", en = "Year: "),
  "trends.chart.hover.frequency" = list(es = "Frecuencia: ", en = "Frequency: "),
  "trends.chart.hover.term" = list(es = "Término: ", en = "Term: "),
  "trends.chart.hover.date" = list(es = "Fecha: ", en = "Date: "),
  "trends.top_terms_chart.no_data" = list(es = "No hay datos para el rango elegido.", en = "No data for the selected range."),
  "trends.top_terms_chart.axis.total_frequency" = list(es = "Frecuencia total", en = "Total frequency"),
  "trends.top_terms_chart.hover.frequency" = list(es = "Frecuencia: ", en = "Frequency: "),
  "trends.top_terms_chart.hover.terms" = list(es = "Términos: ", en = "Terms: "),
  "trends.pagination.no_news" = list(es = "No hay noticias en el rango elegido.", en = "No headlines in the selected range."),
  "trends.pagination.date_sort" = list(es = "Fecha", en = "Date"),
  "trends.pagination.showing" = list(es = "Mostrando", en = "Showing"),
  "trends.pagination.of" = list(es = "de", en = "of"),
  "trends.pagination.per_page" = list(es = "(5 por página)", en = "(5 per page)"),
  "trends.pagination.prev" = list(es = "← Anterior", en = "← Previous"),
  "trends.pagination.next" = list(es = "Siguiente →", en = "Next →"),
  "trends.pagination.page" = list(es = "Página", en = "Page"),
  "trends.table.no_news_page" = list(es = "No hay noticias en esta página.", en = "No headlines on this page."),
  "trends.table.col.link" = list(es = "Enlace", en = "Link"),
  "trends.table.col.title" = list(es = "Título", en = "Title"),
  "trends.table.col.date" = list(es = "Fecha", en = "Date"),
  "trends.table.col.media" = list(es = "Medio", en = "Media")
)

#' Traduce una clave al idioma indicado (por defecto "es").
i18n_tr <- function(key, lang = "es") {
  entry <- i18n_strings[[key]]
  if (is.null(entry)) return(key)
  value <- entry[[lang]]
  if (is.null(value)) entry[["es"]] else value
}

#' Idioma para el primer render: lee la cookie babelos_lang de la request
#' (mismo mecanismo que usa el sitio Flask padre — el dashboard vive en el
#' mismo origen vía /shiny/, asi que el navegador ya la envia). Evita el
#' parpadeo ES->EN que tendria depender solo del input reactivo, que llega
#' recien despues de shiny:connected.
i18n_lang_from_request <- function(request) {
  header <- request$HTTP_COOKIE
  if (is.null(header) || !nzchar(header)) return("es")
  pares <- strsplit(header, ";\\s*")[[1]]
  for (par in pares) {
    kv <- strsplit(par, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2 && trimws(kv[1]) == "babelos_lang") {
      valor <- trimws(kv[2])
      if (valor %in% c("es", "en")) return(valor)
    }
  }
  "es"
}
