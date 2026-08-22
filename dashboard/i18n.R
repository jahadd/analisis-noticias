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
  "trends.table.col.media" = list(es = "Medio", en = "Media"),

  # ── Pestaña Medios ──────────────────────────────────────────────────────
  "media.tab.concepts.chart1.title" = list(es = "¿Qué palabras usa cada medio?", en = "What words does each outlet use?"),
  "media.tab.concepts.chart1.hint" = list(
    es = "Cuántas noticias de cada medio mencionan el término buscado en el titular, dentro del período seleccionado.",
    en = "How many headlines from each outlet mention the searched term, within the selected period."
  ),
  "media.tab.concepts.chart2.title" = list(es = "¿Cómo evolucionó este concepto en cada medio?", en = "How did this concept evolve at each outlet?"),
  "media.tab.concepts.chart2.hint" = list(
    es = "Evolución temporal del término buscado por medio de comunicación (top 8 medios por frecuencia total).",
    en = "Time evolution of the searched term by media outlet (top 8 outlets by total frequency)."
  ),
  "media.tab.top_terms.chart1.title" = list(es = "Los temas favoritos de cada medio", en = "Each outlet's favorite topics"),
  "media.tab.top_terms.chart1.hint" = list(es = "Los términos que más aparecen en los titulares del medio seleccionado.", en = "The terms that appear most in the selected outlet's headlines."),
  "media.tab.top_terms.chart2.title" = list(es = "¿Cuándo fueron más mencionados estos temas?", en = "When were these topics mentioned most?"),
  "media.tab.top_terms.chart2.hint" = list(
    es = "Evolución temporal de los términos más frecuentes del medio seleccionado. Selecciona los términos en el panel izquierdo.",
    en = "Time evolution of the selected outlet's most frequent terms. Select the terms in the left panel."
  ),
  "media.tab.volume.chart1.title" = list(es = "Volumen de noticias por medio", en = "Headline volume by outlet"),
  "media.tab.volume.chart1.hint.prefix" = list(
    es = "Top de medios ordenados por volumen total de noticias publicadas en el período seleccionado. ",
    en = "Top outlets ranked by total volume of headlines published in the selected period. "
  ),
  "media.tab.volume.chart1.hint.yellow" = list(es = "Amarillo", en = "Yellow"),
  "media.tab.volume.chart1.hint.yellow_desc" = list(es = "medio con períodos sin datos (gaps)", en = "outlet with periods without data (gaps)"),
  "media.tab.volume.chart1.hint.red" = list(es = "Rojo", en = "Red"),
  "media.tab.volume.chart1.hint.red_desc" = list(
    es = "medio con problemas de captura — pasa el cursor sobre la barra para ver el detalle.",
    en = "outlet with capture problems — hover over the bar to see the detail."
  ),
  "media.tab.volume.chart2.title" = list(es = "Evolución del volumen en el tiempo", en = "Volume evolution over time"),
  "media.tab.volume.chart2.hint" = list(
    es = "Noticias publicadas por mes y medio. Selecciona los medios en el panel izquierdo. Vista anual cuando el rango supera los 2 años.",
    en = "Headlines published by month and outlet. Select the outlets in the left panel. Yearly view when the range exceeds 2 years."
  ),
  "media.tab.network.title" = list(es = "Palabras que aparecen juntas en los titulares", en = "Words that appear together in headlines"),
  "media.tab.network.hint" = list(
    es = "Términos que aparecen en el mismo titular con frecuencia. Cuanto más grueso el nodo, más conexiones tiene.",
    en = "Terms that frequently appear in the same headline. The thicker the node, the more connections it has."
  ),
  "media.tab.network.loading" = list(es = "Calculando red de palabras…", en = "Calculating word network…"),
  "media.chart.metric_label" = list(es = "Noticias con el término en el titular", en = "Headlines with the term in the title"),
  "media.chart.axis.outlet" = list(es = "Medio", en = "Outlet"),
  "media.chart.legend.concept" = list(es = "Concepto", en = "Concept"),
  "media.chart.axis.news_count" = list(es = "Número de noticias", en = "Number of headlines"),
  "media.chart.hover.news" = list(es = "Noticias: ", en = "Headlines: "),
  "media.diag.scraper_broken" = list(
    es = "Scraper roto: el sitio bloquea la captura automática (error 403).",
    en = "Broken scraper: the site blocks automatic capture (error 403)."
  ),
  "media.diag.discontinued" = list(
    es = "Fuente descontinuada: el sitio cambió su estructura y el scraper fue retirado. Cobertura hasta noviembre 2024.",
    en = "Discontinued source: the site changed its structure and the scraper was retired. Coverage through November 2024."
  ),
  "media.diag.gap" = list(es = "⚠ Gap de datos: sin noticias del %s al %s (%d días).", en = "⚠ Data gap: no headlines from %s to %s (%d days)."),
  "media.diag.inactive" = list(es = "⛔ Inactivo: sin noticias desde el %s.", en = "⛔ Inactive: no headlines since %s."),
  "media.selector.no_outlets" = list(es = "No hay medios en el período.", en = "No outlets in this period."),
  "media.network.no_data" = list(es = "Sin datos para este período o umbral seleccionado.", en = "No data for this period or the selected threshold."),
  "media.network.missing_igraph" = list(es = "Instalar igraph: install.packages('igraph')", en = "Install igraph: install.packages('igraph')"),
  "media.network.hover.connections" = list(es = "Conexiones: ", en = "Connections: "),
  "media.network.legend.group" = list(es = "Grupo", en = "Group"),
  "media.network.legend.communities" = list(es = "Comunidades", en = "Communities"),
  "media.network.legend.semantic" = list(es = "Relación semántica", en = "Semantic relationship"),
  "media.network.slider.semantic_similarity" = list(es = "Similitud semántica mínima", en = "Minimum semantic similarity"),
  "media.volume_evol.select_outlet" = list(es = "Selecciona al menos un medio arriba.", en = "Select at least one outlet above."),
  "media.chart.hover.outlet" = list(es = "Medio: ", en = "Outlet: "),
  "media.chart.hover.month" = list(es = "Mes: ", en = "Month: "),
  "media.chart.axis.news_published" = list(es = "Noticias publicadas", en = "Headlines published"),
  "media.chart.axis.month" = list(es = "Mes", en = "Month"),
  "media.concept_evol.select_hint" = list(
    es = "Selecciona un término y un medio para ver la evolución temporal.",
    en = "Select a term and an outlet to see the time evolution."
  ),
  "media.concept_evol.no_data_prefix" = list(es = "Sin datos para los términos seleccionados en", en = "No data for the selected terms in"),
  "media.concept_evol.no_data_suffix" = list(es = "en este período.", en = "in this period."),
  "media.concept_evol.title_prefix" = list(es = "Evolución en", en = "Evolution in"),
  "media.terms_by_outlet.no_data" = list(es = "Sin datos para el medio elegido en este período.", en = "No data for the selected outlet in this period."),
  "media.terms_by_outlet.select_hint" = list(es = "Selecciona al menos un término arriba.", en = "Select at least one term above."),
  "media.terms_by_outlet.pie_title_prefix" = list(es = "Top 15 términos —", en = "Top 15 terms —"),

  # ── Sidebar de filtros (Tendencias / Medios / Más información) ──────────
  "sidebar.exclude_words.label" = list(es = "Excluir palabras de los gráficos", en = "Exclude words from charts"),
  "sidebar.exclude_words.placeholder" = list(es = "Ej: chilena, video, esto", en = "E.g.: chilean, video, this"),
  "sidebar.exclude_words.apply" = list(es = "Aplicar", en = "Apply"),
  "sidebar.exclude_words.clear" = list(es = "Limpiar", en = "Clear"),
  "mas_info.sidebar.hint" = list(es = "Detalles técnicos del proyecto.", en = "Technical details of the project."),
  "trends.sidebar.search_word_label" = list(es = "Buscar una palabra", en = "Search for a word"),
  "trends.sidebar.search_placeholder" = list(es = "Ej: presidente, economía…", en = "E.g.: president, economy…"),
  "media.sidebar.outlet_for_evolution" = list(es = "Medio para evolución temporal", en = "Outlet for time evolution"),
  "media.sidebar.choose_outlet" = list(es = "Elegir medio", en = "Choose outlet"),
  "media.sidebar.terms_in_chart" = list(es = "Términos en el gráfico", en = "Terms in the chart"),
  "media.sidebar.outlets_in_chart" = list(es = "Medios en el gráfico", en = "Outlets in the chart"),
  "media.sidebar.min_cooccurrence" = list(es = "Co-ocurrencia mínima", en = "Minimum co-occurrence"),
  "media.sidebar.max_nodes" = list(es = "Máximo de nodos", en = "Maximum nodes"),

  # ── Pestaña Sentimiento ─────────────────────────────────────────────────
  "sent.distribution.title" = list(es = "Distribución del tono", en = "Tone distribution"),
  "sent.distribution.all_media" = list(es = "Todos los medios", en = "All media"),
  "sent.table.title" = list(es = "Titulares clasificados", en = "Classified headlines"),
  "sent.table.hint" = list(
    es = "Titulares con su tono y confianza. El filtro de tono se controla con un clic en la barra de distribución de arriba.",
    en = "Headlines with their tone and confidence. The tone filter is controlled by clicking the distribution bar above."
  ),
  "sent.subtitle.all_media" = list(es = "Mostrando todos los medios — elige uno para ver su detalle", en = "Showing all outlets — choose one to see its detail"),
  "sent.subtitle.showing_prefix" = list(es = "Mostrando:", en = "Showing:"),
  "sent.no_data" = list(es = "Sin datos de sentimiento para este período todavía.", en = "No sentiment data for this period yet."),
  "sent.coverage_notice" = list(
    es = "El análisis de sentimiento cubre titulares desde el <b>%s</b> hasta el <b>%s</b>. Lo que ves abajo corresponde solo a ese período; el resto del rango elegido no tiene titulares clasificados.",
    en = "The sentiment analysis covers headlines from <b>%s</b> to <b>%s</b>. What you see below corresponds only to that period; the rest of the selected range has no classified headlines."
  ),
  "sent.hero.title" = list(es = "Tono neto del período", en = "Net tone for the period"),
  "sent.hero.no_data" = list(es = "Sin titulares clasificados aún", en = "No headlines classified yet"),
  "sent.hero.dominant_negative" = list(es = "Predomina el tono negativo", en = "Negative tone predominates"),
  "sent.hero.dominant_positive" = list(es = "Predomina el tono positivo", en = "Positive tone predominates"),
  "sent.hero.balanced" = list(es = "Tono mayormente equilibrado", en = "Mostly balanced tone"),
  "sent.hero.vs_previous" = list(es = "vs. período anterior", en = "vs. previous period"),
  "sent.bar.no_data" = list(es = "Sin datos en el período.", en = "No data in this period."),
  "sent.chip.negative" = list(es = "Negativos", en = "Negative"),
  "sent.chip.neutral" = list(es = "Neutrales", en = "Neutral"),
  "sent.chip.positive" = list(es = "Positivos", en = "Positive"),
  "sent.bar.filtered_to" = list(es = "Tabla filtrada a", en = "Table filtered to"),
  "sent.bar.clear" = list(es = "× limpiar", en = "× clear"),
  "sent.bar.click_hint" = list(es = "Haz clic en un color para filtrar ese tono en la tabla de abajo.", en = "Click a color to filter that tone in the table below."),
  "sent.legend.negative" = list(es = "Negativo", en = "Negative"),
  "sent.legend.neutral" = list(es = "Neutral", en = "Neutral"),
  "sent.legend.positive" = list(es = "Positivo", en = "Positive"),
  "sent.legend.net_tone" = list(es = "Tono neto", en = "Net tone"),
  "sent.chart.hover.net_tone" = list(es = "Tono neto: ", en = "Net tone: "),
  "sent.chart.axis.composition" = list(es = "Composición", en = "Composition"),
  "sent.bento.evolution.title" = list(es = "¿Cómo cambió el tono en el tiempo?", en = "How did the tone change over time?"),
  "sent.bento.evolution.hint" = list(
    es = "Composición mensual del tono (100% apilado) y línea de tono neto: porcentaje de positivos menos negativos.",
    en = "Monthly tone composition (100% stacked) and net tone line: percentage of positive minus negative."
  ),
  "sent.bento.by_outlet.title" = list(es = "¿Qué medios son más positivos o negativos?", en = "Which outlets are more positive or negative?"),
  "sent.bento.by_outlet.hint" = list(
    es = "Contraste del tono entre todos los medios, de más positivo (arriba) a más negativo (abajo). Elige un medio arriba para ver su detalle.",
    en = "Tone contrast across all outlets, from most positive (top) to most negative (bottom). Choose an outlet above to see its detail."
  ),
  "sent.pagination.showing" = list(es = "Mostrando", en = "Showing"),
  "sent.table.no_headlines" = list(es = "No hay titulares clasificados para este filtro/período.", en = "No headlines classified for this filter/period."),
  "sent.meta.coverage" = list(
    es = "Cobertura: <b>%s de %s</b> titulares clasificados (<b>%s%%</b>) · modelo <b>%s</b> · confianza media <b>%s</b>. El tono se clasifica con un modelo de lenguaje local sobre el titular.",
    en = "Coverage: <b>%s of %s</b> headlines classified (<b>%s%%</b>) · model <b>%s</b> · average confidence <b>%s</b>. Tone is classified using a local language model on the headline."
  ),

  # ── Pestaña Más información ──────────────────────────────────────────────
  "info.corpus.text" = list(es = "más de %s millones de noticias recopiladas desde %s", en = "over %s million headlines collected since %s"),
  "info.corpus.fallback" = list(es = "más de 1,2 millones de noticias", en = "over 1.2 million headlines"),
  "info.title.main" = list(es = "MONITOR DE NOTICIAS CHILE  —  Seguimiento de titulares 2018-2026", en = "CHILE NEWS MONITOR  —  Tracking headlines 2018-2026"),
  "info.subtitle" = list(es = "Un vistazo en tiempo real a los temas que dominan la prensa chilena.", en = "A real-time look at the topics dominating the Chilean press."),
  "info.badge.years" = list(es = "2018 — 2026", en = "2018 — 2026"),
  "info.badge.media_count" = list(es = "28 MEDIOS", en = "28 OUTLETS"),
  "info.badge.online" = list(es = "EN LINEA", en = "ONLINE"),
  "info.credits.title" = list(es = "!!  CREDITOS  !!", en = "!!  CREDITS  !!"),
  "info.credits.data_label" = list(es = "Datos y scraping: ", en = "Data and scraping: "),
  "info.credits.data_suffix" = list(es = " de Bastian Olea Herrera. Base de datos con %s.", en = " by Bastian Olea Herrera. Database with %s."),
  "info.credits.pipeline_label" = list(es = "Pipeline, análisis y dashboard: ", en = "Pipeline, analysis and dashboard: "),
  "info.credits.pipeline_suffix" = list(
    es = ". Tokenización, frecuencias, co-ocurrencias, sentimiento y visualización interactiva.",
    en = ". Tokenization, frequencies, co-occurrences, sentiment and interactive visualization."
  ),
  "info.what_is.title" = list(es = "¿Qué es esto?", en = "What is this?"),
  "info.what_is.p1" = list(es = "Este dashboard rastrea qué palabras y temas aparecen en los titulares de los principales medios chilenos.", en = "This dashboard tracks which words and topics appear in the headlines of major Chilean media outlets."),
  "info.what_is.p2" = list(
    es = "Permite ver cuándo un tema se volvió tendencia, cómo distintos medios cubren los mismos eventos y qué conceptos aparecen juntos con más frecuencia.",
    en = "It lets you see when a topic became trending, how different outlets cover the same events, and which concepts appear together most often."
  ),
  "info.what_is.p3" = list(es = "Los datos se recopilan automáticamente y se procesan para explorar sin leer miles de artículos.", en = "The data is collected automatically and processed so you can explore it without reading thousands of articles."),
  "info.what_can.title" = list(es = "¿Qué puedo hacer aquí?", en = "What can I do here?"),
  "info.feat.trends.title" = list(es = "Ver tendencias", en = "See trends"),
  "info.feat.trends.desc" = list(es = "Palabras más mencionadas y cómo cambiaron en el tiempo.", en = "Most mentioned words and how they changed over time."),
  "info.feat.compare.title" = list(es = "Comparar medios", en = "Compare outlets"),
  "info.feat.compare.desc" = list(es = "Si distintos medios priorizan los mismos temas.", en = "Whether different outlets prioritize the same topics."),
  "info.feat.search.title" = list(es = "Buscar conceptos", en = "Search concepts"),
  "info.feat.search.desc" = list(es = "Cuántas veces apareció una palabra, en qué período y medios.", en = "How many times a word appeared, in what period and outlets."),
  "info.feat.explore.title" = list(es = "Explorar conexiones", en = "Explore connections"),
  "info.feat.explore.desc" = list(es = "Términos que aparecen juntos en titulares.", en = "Terms that appear together in headlines."),
  "info.feat.volume.title" = list(es = "Seguir el volumen", en = "Track volume"),
  "info.feat.volume.desc" = list(es = "Cuántas noticias publica cada medio y cómo ha evolucionado.", en = "How many headlines each outlet publishes and how it has evolved."),
  "info.sources.title" = list(es = "Fuentes cubiertas — 28 medios chilenos", en = "Sources covered — 28 Chilean outlets"),
  "info.sources.tv" = list(es = "TELEVISION", en = "TELEVISION"),
  "info.sources.radio" = list(es = "RADIO", en = "RADIO"),
  "info.sources.print" = list(es = "PRENSA ESCRITA", en = "PRINT PRESS"),
  "info.sources.digital" = list(es = "MEDIOS DIGITALES", en = "DIGITAL MEDIA"),
  "info.how.title" = list(es = "¿Cómo funciona?", en = "How does it work?"),
  "info.how.step1.title" = list(es = "Recolección", en = "Collection"),
  "info.how.step1.desc" = list(es = "Un sistema visita periódicamente los 28 medios y guarda sus titulares.", en = "A system periodically visits the 28 outlets and saves their headlines."),
  "info.how.step2.title" = list(es = "Identificación", en = "Identification"),
  "info.how.step2.desc" = list(
    es = "Cada titular se descompone en palabras clave. Se fusionan nombres de personas, instituciones y partidos.",
    en = "Each headline is broken down into keywords. Names of people, institutions and parties are merged."
  ),
  "info.how.step3.title" = list(es = "Análisis", en = "Analysis"),
  "info.how.step3.desc" = list(es = "Se cuenta cuántas veces aparece cada término por día y por medio para detectar tendencias.", en = "Counts how many times each term appears per day and per outlet to detect trends."),
  "info.how.step4.title" = list(es = "Visualización", en = "Visualization"),
  "info.how.step4.desc" = list(es = "Resultados presentados en este dashboard interactivo. Filtra, busca y explora libremente.", en = "Results presented in this interactive dashboard. Filter, search and explore freely."),
  "info.footer.data" = list(es = "Datos: prensa_chile", en = "Data: prensa_chile"),
  "info.footer.pipeline" = list(es = "Pipeline: analisis-noticias", en = "Pipeline: analisis-noticias"),
  "info.footer.license" = list(es = "Licencia MIT · Datos sujetos a TOS de cada medio.", en = "MIT License · Data subject to each outlet's TOS."),

  # ── Sidebar: rango de fechas y accesos rápidos ───────────────────────────
  "sidebar.preset_7days" = list(es = "Últimos 7 días", en = "Last 7 days"),
  "sidebar.preset_month" = list(es = "Último mes", en = "Last month"),
  "sidebar.preset_year" = list(es = "Último año", en = "Last year"),
  "sidebar.date_separator" = list(es = " a ", en = " to "),

  # ── Medios: sidebar de "Conceptos por medio" ─────────────────────────────
  "media.sidebar.terms_for_chart" = list(es = "Términos para el gráfico por medio", en = "Terms for the outlet chart")
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
