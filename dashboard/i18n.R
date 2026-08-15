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

  "language.label" = list(es = "Idioma", en = "Language")
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
