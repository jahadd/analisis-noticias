# Plan de Mejora: Pipeline RAG para Noticias

> Basado en el tutorial [Text Analytics in R: Dense embeddings and RAG pipeline](https://rtichoke.netlify.app/posts/text-analytics-rag-pipeline-in-r.html),
> adaptado al pipeline de noticias de prensa chilena.
>
> Fecha de elaboración: 2026-04-07

---

## Contexto del proyecto actual

El pipeline actual funciona así:

```
Scraping (fuentes R)
       ↓
PostgreSQL: tabla noticias (id, titulo, bajada, cuerpo_limpio, fecha, fuente, año)
       ↓
Análisis de texto:
  run_analisis_titulos.R     → titulos_terminos_diarios, titulos_terminos_por_medio
  run_analisis_ngramas.R     → titulos_ngramas_diarios, titulos_ngramas_por_medio
  run_analisis_coocurrencia.R → titulos_coocurrencia
  run_sentimiento.R          → noticias_sentimiento (Ollama: qwen2.5:3b)
  run_analisis_tfidf.R       → titulos_tfidf_por_medio
       ↓
Dashboard Shiny (dashboard/app.R)
```

**Lo que tenemos a favor antes de empezar:**
- Ollama ya corre localmente (qwen2.5:3b para sentimiento)
- El proyecto ya es 100% R
- La tabla `noticias` es exactamente el corpus que necesita el tutorial
- `cuerpo_limpio` ya aplica limpieza de texto (HTML, basura, signos)

---

## Paso 1 — Instalación de paquetes nuevos

El tutorial requiere dos paquetes R que no están en el pipeline actual:

```r
# ragnar: vector store, chunking, recuperación semántica
install.packages("ragnar")

# ellmer: interfaz con LLMs (Ollama, OpenAI, etc.)
install.packages("ellmer")
```

Y un modelo de embeddings en Ollama (distinto al que ya se usa para sentimiento):

```bash
# El modelo de generación ya existe: qwen2.5:3b
# Solo falta el modelo de embeddings:
ollama pull nomic-embed-text
```

**Por qué nomic-embed-text:** es el que usa el tutorial, soporta español, es ligero (274 MB)
y corre en CPU sin problema. No requiere GPU.

**Verificar que ambos modelos estén disponibles:**

```bash
ollama list
# Debe aparecer: qwen2.5:3b (ya existe) + nomic-embed-text (nuevo)
```

---

## Paso 2 — El problema con el TF-IDF actual

El pipeline ya genera TF-IDF en `titulos_tfidf_por_medio` y frecuencias en
`titulos_terminos_diarios`. El límite de ese enfoque:

```r
# Ejemplo del problema (equivalente al tutorial pero con noticias chilenas)
library(quanteda)
library(quanteda.textstats)

titulos_prueba <- c(
  "Gobierno anuncia reforma al sistema de pensiones",
  "Ejecutivo presenta cambios al modelo previsional",
  "El presidente habla sobre política de salud"
)

dfm_tfidf <- titulos_prueba |>
  tokens() |>
  dfm() |>
  dfm_tfidf()

textstat_simil(dfm_tfidf, method = "cosine")
# Resultado esperado:
#   titulos_prueba1 vs titulos_prueba2 → ~0.00  ← mismo tema, vocabulario distinto
#   titulos_prueba1 vs titulos_prueba3 → ~0.00
```

"Reforma al sistema de pensiones" y "cambios al modelo previsional" hablan de lo mismo
pero TF-IDF los ve como documentos sin relación. Esto afecta la búsqueda y el clustering
del dashboard.

---

## Paso 3 — Embeddings densos como solución

Con `nomic-embed-text` vía `ragnar`:

```r
library(ragnar)

embed_fn <- embed_ollama(model = "nomic-embed-text")

titulos_prueba <- c(
  "Gobierno anuncia reforma al sistema de pensiones",
  "Ejecutivo presenta cambios al modelo previsional",
  "El presidente habla sobre política de salud"
)

embeddings <- embed_fn(titulos_prueba)

# Calcular similitud coseno manual (ragnar no tiene textstat_simil)
cos_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

cos_sim(embeddings[1,], embeddings[2,])  # → ~0.85  (mismo tema)
cos_sim(embeddings[1,], embeddings[3,])  # → ~0.45  (distinto tema)
```

Ahora "pensiones" y "previsional" tendrán alta similitud aunque no compartan palabras.

---

## Paso 4 — El corpus: tabla `noticias` como fuente

El tutorial construye un corpus sintético de 15 documentos. Aquí el corpus ya existe
en PostgreSQL. El equivalente es cargar noticias recientes:

```r
library(DBI)
library(RPostgres)
source("funciones.R")  # conectar_db()

con <- conectar_db()

# Cargar noticias de los últimos N días como corpus
corpus <- DBI::dbGetQuery(con, "
  SELECT id, titulo, cuerpo_limpio, fecha, fuente
  FROM noticias
  WHERE fecha >= CURRENT_DATE - INTERVAL '30 days'
    AND cuerpo_limpio IS NOT NULL
    AND length(cuerpo_limpio) > 100
  ORDER BY fecha DESC
")

DBI::dbDisconnect(con)

# El corpus tiene exactamente las columnas que ragnar necesita:
# id (identificador), texto (titulo + cuerpo), fuente (metadato para filtrar)
```

**Tamaño estimado del corpus:** ~30 días × ~200 artículos/día × 25 fuentes ≈ 6.000 artículos.
Para el vector store inicial se puede empezar con solo títulos (más rápido de embeber)
y luego extender a cuerpo completo.

---

## Paso 5 — Estrategia de chunking

El tutorial aplica `markdown_chunk(level = 2)` para dividir documentos largos. En noticias
los artículos ya son fragmentos razonables por sí solos. Dos opciones:

**Opción A — Chunk = artículo completo (título + cuerpo)**

```r
corpus <- corpus |>
  dplyr::mutate(
    texto_chunk = dplyr::coalesce(
      paste(titulo, cuerpo_limpio, sep = ". "),
      titulo
    ),
    chunk_id    = id,
    char_count  = nchar(texto_chunk)
  )
```

Pros: mantiene contexto completo. Contras: artículos muy largos pueden degradar la calidad
del embedding (nomic-embed-text tiene límite de ~8.192 tokens).

**Opción B — Chunk = solo título (recomendada para empezar)**

```r
corpus <- corpus |>
  dplyr::mutate(
    texto_chunk = titulo,
    chunk_id    = id,
    char_count  = nchar(titulo)
  )
```

Pros: más rápido, embeddings más limpios y semánticamente concentrados. Contras: el LLM
al generar respuestas tendrá menos contexto (se puede resolver guardando el cuerpo como
metadato y recuperándolo al responder).

**Recomendación:** Opción B para el vector store de búsqueda; Opción A para el RAG
generativo, cargando solo los artículos recuperados.

---

## Paso 6 — Construcción del vector store

El tutorial usa un store en memoria (`:memory:`). Para noticias conviene persistirlo en
disco para no re-embeber todo en cada sesión:

```r
library(ragnar)

STORE_PATH <- "datos/noticias_rag.duckdb"  # carpeta datos/ ya existe en el proyecto

embed_fn <- embed_ollama(model = "nomic-embed-text")

# Crear (o abrir si ya existe)
store <- ragnar_store_create(
  location = STORE_PATH,
  embed    = embed_fn,
  version  = 1
)

# Insertar documentos
# ragnar espera columnas: text (obligatoria) + cualquier metadato extra
docs_para_insertar <- corpus |>
  dplyr::select(
    text   = texto_chunk,   # columna requerida por ragnar
    id,
    fuente,
    fecha,
    titulo
  )

ragnar_store_insert(store, docs_para_insertar)

# Construir índice VSS (solo una vez, o después de insertar nuevos documentos)
ragnar_store_build_index(store)
```

**Actualización incremental:** en el pipeline diario, después de `run_analisis_titulos.R`,
agregar un paso que inserte solo los artículos nuevos:

```r
# Pseudo-código para actualización diaria
nuevos_ids <- setdiff(corpus$id, ragnar_store_ids(store))
if (length(nuevos_ids) > 0) {
  docs_nuevos <- corpus |> dplyr::filter(id %in% nuevos_ids) |> ...
  ragnar_store_insert(store, docs_nuevos)
  ragnar_store_build_index(store)  # re-indexar
}
```

---

## Paso 7 — Visualización del espacio de embeddings

El tutorial proyecta los embeddings a 2D con PCA y grafica por fuente/tipo de documento.
Aquí el equivalente sería colorear por `fuente` o por tema inferido:

```r
library(ggplot2)

# Extraer embeddings almacenados
embeddings_df <- DBI::dbGetQuery(
  ragnar:::store_con(store),
  "SELECT id, fuente, fecha, embedding FROM documents"
)

# PCA a 2D
emb_matrix <- do.call(rbind, embeddings_df$embedding)
pca         <- prcomp(emb_matrix, scale. = TRUE)
coords      <- as.data.frame(pca$x[, 1:2])
coords$fuente <- embeddings_df$fuente
coords$fecha  <- embeddings_df$fecha

ggplot(coords, aes(PC1, PC2, color = fuente)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(
    title    = "Espacio semántico de noticias (PCA sobre embeddings)",
    subtitle = "Cada punto es un artículo; colores por medio de comunicación"
  ) +
  theme_minimal()
```

**Qué esperar ver:** artículos del mismo tema (elecciones, economía, deportes) deberían
agruparse independientemente de la fuente. Si Emol y El Mostrador tienen clusters
separados para el mismo tema, indica sesgos de vocabulario entre medios.

---

## Paso 8 — Comparación de métodos de recuperación

El tutorial compara VSS, BM25 y Hybrid en la misma consulta. Adaptado a noticias:

```r
consulta <- "reforma al sistema de pensiones"

# Método 1: VSS (semántico — encuentra artículos sobre "sistema previsional" aunque
#            no digan "pensiones")
ragnar_retrieve_vss(store, query = consulta, top_k = 5)

# Método 2: BM25 (léxico — encuentra artículos que usan exactamente "pensiones")
ragnar_retrieve_bm25(store, query = consulta, top_k = 5)

# Método 3: Híbrido (Reciprocal Rank Fusion — combina ambas señales)
ragnar_retrieve(store, query = consulta, top_k = 5)
```

**Casos donde cada método gana en prensa chilena:**

| Consulta | Mejor método | Razón |
|---|---|---|
| "AFP pensiones" | BM25 | Término técnico específico, vocabulario consistente |
| "costo vida inflación" | Hybrid | Mezcla técnica + semántica |
| "inseguridad delincuencia" | VSS | Múltiples sinónimos según el medio (crimen, violencia, seguridad ciudadana) |
| "Boric aprobación" | Hybrid | Nombre propio (BM25) + contexto político (VSS) |

---

## Paso 9 — Integración con LLM vía `ellmer`

El tutorial registra el vector store como herramienta del chat. El proyecto ya usa
`qwen2.5:3b` en `run_sentimiento.R`; se puede reutilizar el mismo modelo:

```r
library(ellmer)

chat <- chat_ollama(
  model  = "qwen2.5:3b",   # mismo modelo que ya corre para sentimiento
  system = paste(
    "Eres un analista de prensa chilena.",
    "Responde siempre en español.",
    "Cita los medios de comunicación fuente cuando sea relevante.",
    "Si no tienes información suficiente en los artículos recuperados, dilo."
  )
)

# Registrar el vector store como herramienta de recuperación
ragnar_register_tool_retrieve(chat, store)

# El LLM ahora puede buscar en las noticias antes de responder
chat$chat("¿Qué está pasando con la reforma de pensiones en Chile?")
```

Al llamar `chat$chat()`, el LLM primero invoca la herramienta de recuperación
automáticamente, obtiene los artículos más relevantes y los usa como contexto
para generar la respuesta. No alucina fuentes porque está anclado a los artículos reales.

---

## Paso 10 — Comparación RAG vs. sin RAG

El tutorial demuestra la diferencia entre respuestas con y sin contexto recuperado.
El equivalente para validar la implementación:

```r
# Sin RAG: respuesta de memoria general del modelo
chat_sin_rag <- chat_ollama(model = "qwen2.5:3b")
respuesta_sin_rag <- chat_sin_rag$chat(
  "¿Qué cobertura ha tenido la reforma de pensiones en los medios chilenos esta semana?"
)
# → Respuesta genérica, posiblemente desactualizada o inventada

# Con RAG: respuesta anclada a artículos reales de los últimos días
chat_con_rag <- chat_ollama(model = "qwen2.5:3b", system = "...")
ragnar_register_tool_retrieve(chat_con_rag, store)
respuesta_con_rag <- chat_con_rag$chat(
  "¿Qué cobertura ha tenido la reforma de pensiones en los medios chilenos esta semana?"
)
# → "Según artículos de La Tercera (2026-04-05) y El Mostrador (2026-04-06)..."
```

---

## Paso 11 — Aplicaciones concretas para el dashboard

Basadas en el patrón `ask_rag()` del tutorial:

### A. Función `buscar_noticias_semantico()` para el dashboard Shiny

```r
# analisis/run_embeddings.R (nuevo script del pipeline)

buscar_noticias_semantico <- function(consulta, top_k = 10, fuente_filtro = NULL) {
  resultados <- ragnar_retrieve(store, query = consulta, top_k = top_k)
  if (!is.null(fuente_filtro)) {
    resultados <- resultados |> dplyr::filter(fuente == fuente_filtro)
  }
  resultados |> dplyr::select(titulo, fuente, fecha, score)
}

# En dashboard/app.R:
# textInput("busqueda_semantica", "Buscar noticias...") →
# buscar_noticias_semantico(input$busqueda_semantica)
```

### B. Clustering semántico de historias (detección de duplicados/variantes)

```r
# Detectar artículos sobre el mismo evento en distintos medios
detectar_cobertura_comun <- function(titulo_referencia, umbral_similitud = 0.80) {
  ragnar_retrieve_vss(store, query = titulo_referencia, top_k = 20) |>
    dplyr::filter(score >= umbral_similitud) |>
    dplyr::arrange(dplyr::desc(score))
}
```

### C. Resumen diario generativo

```r
generar_resumen_diario <- function(tema = "política chilena", n_articulos = 10) {
  chat <- chat_ollama(model = "qwen2.5:3b",
                      system = "Eres un editor de noticias. Resume brevemente en español.")
  ragnar_register_tool_retrieve(chat, store)

  chat$chat(glue::glue(
    "Resume los {n_articulos} artículos más relevantes sobre '{tema}' de hoy.",
    "Menciona los medios y agrupa por subtema si hay varios."
  ))
}
```

---

## Nuevas tablas en schema.sql

Para persistir los resultados del RAG en PostgreSQL (consistente con el resto del pipeline):

```sql
-- Embeddings de artículos (referencia al store DuckDB)
CREATE TABLE IF NOT EXISTS noticias_embeddings_meta (
    id              TEXT PRIMARY KEY REFERENCES noticias(id),
    embebido_en     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    modelo          VARCHAR(100) DEFAULT 'nomic-embed-text',
    store_version   INTEGER DEFAULT 1
);

-- Clusters semánticos detectados
CREATE TABLE IF NOT EXISTS noticias_clusters (
    fecha           DATE NOT NULL,
    cluster_id      INTEGER NOT NULL,
    id_noticia      TEXT NOT NULL REFERENCES noticias(id),
    score_centroide FLOAT,
    CONSTRAINT pk_noticias_clusters PRIMARY KEY (fecha, id_noticia)
);
CREATE INDEX IF NOT EXISTS idx_noticias_clusters_fecha ON noticias_clusters(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_noticias_clusters_cluster ON noticias_clusters(fecha, cluster_id);
```

---

---

## Mejoras a los gráficos existentes del dashboard

El dashboard actual tiene `dateRangeInput("fechas", ...)` como control temporal global
(con presets de 7 días, 1 mes, 1 año). Todos los gráficos ya reaccionan a ese input.
Con embeddings, varios de los gráficos existentes se pueden mejorar sin cambiar su forma
visual — solo cambia la lógica de qué datos muestran.

---

### Gráfico: Evolución temporal de términos (`grafico_evolucion`)

**Estado actual:** cuenta artículos que contienen exactamente el término buscado por día.

**Problema:** buscar "pensiones" no encuentra artículos que dicen "AFP", "previsional"
o "jubilación". El usuario tiene que buscar cada sinónimo por separado.

**Mejora con embeddings:** agregar un toggle "búsqueda semántica" junto al `busqueda_termino`
del sidebar. En modo semántico, en vez de hacer `WHERE titulo ILIKE '%pensiones%'`,
se recuperan todos los artículos del período cuyo embedding está cerca del término.
La curva mostraría el volumen real de cobertura sobre el concepto, no sobre la palabra.

```r
# Lógica actual (SQL ILIKE):
"SELECT fecha, COUNT(*) FROM noticias WHERE titulo ILIKE '%pensiones%' GROUP BY fecha"

# Lógica mejorada (ragnar VSS con filtro temporal):
ids_semanticos <- ragnar_retrieve_vss(store, query = input$busqueda_termino, top_k = 500) |>
  dplyr::filter(fecha >= input$fechas[1], fecha <= input$fechas[2]) |>
  dplyr::pull(id)

# Luego agrupar esos ids por fecha para la curva
```

---

### Gráfico: Top 30 palabras más mencionadas (`grafico_top_terminos`)

**Estado actual:** 30 tokens individuales ordenados por frecuencia. El mismo tema puede
aparecer fragmentado: "pensiones", "AFP", "previsional" ocupan 3 barras separadas.

**Mejora con embeddings:** clustering semántico antes de rankear. Agrupar tokens con
alta similitud coseno en un solo cluster etiquetado. Las 30 barras pasarían a mostrar
30 *temas* en lugar de 30 *palabras*.

```r
# Obtener top-N tokens del período (como ahora)
top_tokens <- ... # desde titulos_terminos_diarios

# Embeber todos los tokens
embeddings_tokens <- embed_fn(top_tokens$termino)

# Clustering (k-means o hclust sobre embeddings)
clusters <- kmeans(embeddings_tokens, centers = 30)

# Etiquetar cada cluster con el token más frecuente del grupo
top_tokens$cluster <- clusters$cluster
resumen_clusters <- top_tokens |>
  group_by(cluster) |>
  summarise(
    etiqueta   = termino[which.max(frecuencia)],  # término representativo
    frecuencia = sum(frecuencia)
  ) |>
  arrange(desc(frecuencia))
```

El resultado visual es idéntico (barras horizontales) pero cada barra representa un
tema real, no una palabra aislada.

---

### Gráfico: Red de palabras (`red_coocurrencia_plotly`)

**Estado actual:** todos los nodos son del mismo color (`#0d6efd`). El grafo usa
`igraph::layout_with_fr` para posicionamiento y `igraph::degree` para el tamaño de
los nodos, pero no aplica ningún análisis de comunidades.

**Mejora A — Detección de comunidades (sin embeddings, implementable ahora):**

El grafo de co-ocurrencia ya está construido con `igraph`. Solo hay que aplicar el
algoritmo de Louvain (`cluster_louvain`) sobre ese mismo grafo y colorear cada nodo
según su comunidad. No requiere ningún paquete nuevo.

```r
# Después de construir g e igraph::degree(g), antes de construir edge_x/edge_y:

PALETA_COMUNIDADES <- c(
  "#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6",
  "#1abc9c", "#e67e22", "#e91e63", "#00bcd4", "#8bc34a",
  "#d35400", "#795548", "#607d8b", "#673ab7", "#ff9800"
)

# Detección de comunidades (Louvain; fallback a walktrap si falla)
comunidades <- tryCatch(
  igraph::cluster_louvain(g),
  error = function(e) igraph::cluster_walktrap(g)
)
membresia  <- igraph::membership(comunidades)
n_com      <- max(membresia, 1L)

# Color de cada nodo según comunidad
colores_nodos <- PALETA_COMUNIDADES[(membresia - 1L) %% length(PALETA_COMUNIDADES) + 1L]
```

En el gráfico, en lugar de un solo `add_trace` con todos los nodos en azul uniforme,
se agrega **un trace por comunidad** para que la leyenda muestre cada grupo:

```r
p <- plot_ly()

# 1. Aristas (igual que ahora)
p <- p |> add_trace(x = edge_x, y = edge_y, type = "scatter", mode = "lines",
  line = list(color = "rgba(160,160,190,0.4)", width = 1),
  hoverinfo = "skip", showlegend = FALSE)

# 2. Nodos: un trace por comunidad
for (com in seq_len(n_com)) {
  idx <- which(membresia == com)
  if (length(idx) == 0L) next
  sz <- 7 + 14 * (grado[idx] / max(grado, 1L))
  color_com <- PALETA_COMUNIDADES[(com - 1L) %% length(PALETA_COMUNIDADES) + 1L]

  p <- p |> add_trace(
    x = coords[idx, 1], y = coords[idx, 2],
    type = "scatter", mode = "markers+text",
    name = paste0("Grupo ", com),
    text = nodo_ids[idx],
    textposition = "top center",
    textfont = list(size = 9, color = "#333"),
    marker = list(
      size    = sz,
      color   = color_com,
      opacity = 0.85,
      line    = list(color = "#ffffff", width = 1.5)
    ),
    customdata = grado[idx],
    hovertemplate = "<b>%{text}</b><br>Conexiones: %{customdata}<br>Grupo: NOMBRE<extra></extra>" |>
      gsub("NOMBRE", paste0("Grupo ", com), x = _),
    showlegend = TRUE
  )
}
```

El layout final añade una leyenda interactiva que permite mostrar/ocultar cada comunidad:

```r
p |> layout(
  legend = list(
    orientation = "v", x = 1.01, xanchor = "left", y = 0.99, yanchor = "top",
    title = list(text = "Comunidades"),
    bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", borderwidth = 1,
    font = list(size = 10)
  ),
  xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
  yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
  margin = list(l = 10, r = 140, t = 10, b = 10),
  plot_bgcolor = "#fafafa", paper_bgcolor = "#fafafa"
)
```

**Por qué Louvain:** es el algoritmo estándar para detección de comunidades en grafos
de lenguaje natural. Agrupa palabras que co-ocurren densamente entre sí, lo que
en práctica corresponde a temas: un grupo puede ser {gobierno, presidente, ministro},
otro {economía, dólar, inflación}, otro {fútbol, gol, partido}. Es determinístico
con `set.seed(42L)` y no requiere especificar el número de grupos.

---

**Mejora B — Aristas semánticas (requiere embeddings, fase posterior):**

Una vez que `run_embeddings.R` haya indexado los artículos, se puede agregar un segundo
tipo de arista: *similitud semántica*. Palabras que no co-ocurren en titulares pero
pertenecen al mismo tema tendrían una arista punteada azul.

```r
# Aristas semánticas (nuevas): similitud coseno entre embeddings de los nodos
nodos_red <- unique(c(datos$termino_a, datos$termino_b))
emb_nodos <- embed_fn(nodos_red)
sim_matrix <- proxy::simil(emb_nodos, method = "cosine")

# Mantener solo pares con similitud alta que NO tienen ya arista léxica
pares_existentes <- paste(datos$termino_a, datos$termino_b)
aristas_semanticas <- as.data.frame(as.matrix(sim_matrix)) |>
  tibble::rownames_to_column("termino_a") |>
  tidyr::pivot_longer(-termino_a, names_to = "termino_b", values_to = "similitud") |>
  dplyr::filter(
    similitud > 0.75,
    termino_a < termino_b,
    !paste(termino_a, termino_b) %in% pares_existentes
  )
# Aristas léxicas en gris claro; aristas semánticas en azul punteado
```

Un slider `umbral_semantico` controlaría qué tan estricto es el corte (patrón idéntico
al `umbral_coocurrencia` que ya existe en el sidebar).

---

### Gráfico: Conceptos por medio (`grafico_conceptos_por_medio`)

**Estado actual:** cuenta artículos de cada fuente que contienen el término exacto.

**Problema:** al comparar cómo Emol vs. El Mostrador cubren "seguridad", el resultado
puede ser engañoso porque uno puede usar "delincuencia" y el otro "crimen organizado".

**Mejora:** el selector de concepto del sidebar haría búsqueda semántica en lugar de
ILIKE. El gráfico mostraría cobertura real del tema, no del término.

Esto reutiliza exactamente la misma lógica que la mejora en evolución temporal —
misma función `ragnar_retrieve_vss()`, diferente aggregación (por fuente en vez de
por fecha).

---

### Tabla de noticias recientes (`tabla_noticias`)

**Estado actual:** `busqueda_titulo` hace `WHERE titulo ILIKE '%término%'`.

**Mejora:** reemplazar por búsqueda semántica. Visualmente idéntica, pero "reforma
pensional" encuentra artículos sobre "AFP" y "jubilación". Opción: mantener ambos modos
con un toggle "Exacta / Semántica".

---

### Resumen generativo vinculado a la ventana temporal

**¿Es posible?** Sí, directamente. El `dateRangeInput("fechas")` ya existe y todos
los gráficos lo usan. El resumen puede leer exactamente el mismo `input$fechas`.

**Dependencias:** ninguna nueva. `httr2` ya es dependencia del proyecto (lo usa
`run_sentimiento.R`). No necesita `ellmer` ni ragnar para funcionar — puede operar
desde el primer día con solo Ollama corriendo.

---

**UI — dónde va en el dashboard:**

Se agrega al final de la pestaña **Tendencias**, después de la tabla de noticias
recientes. Usa el mismo `input$fechas` que todos los demás gráficos.

```r
# En UI > tabPanel("Tendencias", ...) — después de uiOutput("controles_pagina"):
hr(),
h4("Resumen del período con IA"),
p(class = "small-metric",
  "Genera un resumen de los principales temas noticiosos en el rango de fechas",
  "seleccionado. Requiere Ollama en ejecución. Puede tardar 15-30 segundos."),
div(style = "display: flex; gap: 8px; align-items: center; margin-bottom: 1rem;",
  actionButton("btn_generar_resumen", "Generar resumen",
               class = "btn-primary btn-sm", icon = icon("robot")),
  uiOutput("resumen_estado")   # spinner / mensaje "Generando..."
),
uiOutput("resumen_periodo_ui")  # resultado formateado
```

---

**Server — lógica completa:**

```r
# Valor reactivo para almacenar el texto generado
resumen_texto <- reactiveVal(NULL)
resumen_generando <- reactiveVal(FALSE)

observeEvent(input$btn_generar_resumen, {
  req(!resumen_generando())           # evitar doble clic
  resumen_generando(TRUE)
  resumen_texto(NULL)

  fecha_inicio <- input$fechas[1]
  fecha_fin    <- input$fechas[2]
  n_dias       <- as.integer(fecha_fin - fecha_inicio)

  # 1. Cargar titulares del período desde PostgreSQL
  con <- poolCheckout(pool)
  articulos <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT titulo, fuente, fecha
      FROM noticias
      WHERE fecha BETWEEN $1 AND $2
        AND titulo IS NOT NULL
      ORDER BY fecha DESC
    ", params = list(fecha_inicio, fecha_fin)),
    error = function(e) NULL
  )
  poolReturn(con)

  if (is.null(articulos) || nrow(articulos) == 0) {
    resumen_texto("Sin noticias en el período seleccionado.")
    resumen_generando(FALSE)
    return()
  }

  # 2. Limitar contexto según largo del período
  #    ≤ 14 días  → hasta 300 titulares (todos si hay pocos)
  #    15–90 días → muestreo estratificado: top-N por fuente (diversidad de medios)
  #    > 90 días  → top 5 por fuente por mes (panorama temporal)
  if (n_dias <= 14L) {
    contexto_df <- head(articulos, 300L)
  } else if (n_dias <= 90L) {
    contexto_df <- articulos |>
      dplyr::group_by(fuente) |>
      dplyr::slice_head(n = 8L) |>
      dplyr::ungroup() |>
      dplyr::arrange(fecha)
  } else {
    contexto_df <- articulos |>
      dplyr::mutate(mes = format(as.Date(fecha), "%Y-%m")) |>
      dplyr::group_by(fuente, mes) |>
      dplyr::slice_head(n = 3L) |>
      dplyr::ungroup() |>
      dplyr::arrange(fecha)
  }

  # 3. Construir texto de contexto
  contexto_txt <- paste(
    paste0("[", contexto_df$fecha, "] ", contexto_df$fuente, ": ", contexto_df$titulo),
    collapse = "\n"
  )

  prompt_usuario <- paste0(
    "Resume los principales temas noticiosos entre el ", fecha_inicio,
    " y el ", fecha_fin, ". Agrupa por tema. ",
    "Usa los titulares a continuación como única fuente:\n\n", contexto_txt
  )

  # 4. Llamar a Ollama con httr2 (mismo patrón que run_sentimiento.R)
  resultado <- tryCatch({
    resp <- httr2::request("http://localhost:11434/api/chat") |>
      httr2::req_body_json(list(
        model    = "qwen2.5:3b",
        stream   = FALSE,
        messages = list(
          list(role = "system", content = paste(
            "Eres un editor de prensa chilena.",
            "Basándote SOLO en los titulares proporcionados,",
            "resume los temas noticiosos del período.",
            "Sé conciso, agrupa por tema y responde siempre en español.",
            "No inventes información que no esté en los titulares."
          )),
          list(role = "user", content = prompt_usuario)
        )
      )) |>
      httr2::req_timeout(120) |>
      httr2::req_perform()
    parsed <- httr2::resp_body_json(resp)
    parsed$message$content
  }, error = function(e) {
    paste0("Error al conectar con Ollama: ", conditionMessage(e),
           "\nVerifica que Ollama esté corriendo (ollama serve).")
  })

  resumen_texto(resultado)
  resumen_generando(FALSE)
})

output$resumen_estado <- renderUI({
  if (resumen_generando()) {
    tags$span(style = "color: #6c757d; font-size: 0.9em;", "Generando resumen…")
  }
})

output$resumen_periodo_ui <- renderUI({
  txt <- resumen_texto()
  if (is.null(txt)) return(NULL)
  div(
    style = paste(
      "background: #f8f9fa; border-left: 4px solid #0d6efd;",
      "padding: 1rem; border-radius: 4px; font-size: 0.95em;",
      "white-space: pre-wrap; margin-top: 0.5rem;"
    ),
    txt
  )
})
```

---

**Por qué el botón es obligatorio (no auto-reactivo):**

Ollama tarda entre 15 y 30 segundos en generar el resumen. Si se regenerara
automáticamente con cada cambio de fecha (drag del slider, presets "7 días / mes /
año"), el dashboard quedaría bloqueado. El botón hace que el usuario decida
explícitamente cuándo ejecutarlo.

---

**Comportamiento esperado según el rango seleccionado:**

| Rango | Estrategia de contexto | Titulares al LLM |
|---|---|---|
| ≤ 14 días | Todos (hasta 300) | ~100–400 |
| 15–90 días | Top 8 por fuente | ~200 (diversidad de medios) |
| > 90 días | Top 3 por fuente por mes | ~200 (distribuidos en el tiempo) |

Para rangos cortos el resumen es específico ("esta semana hubo X y Y"). Para rangos
largos es temático ("los temas dominantes del trimestre fueron…").

---

**Versión mejorada con embeddings (fase posterior):**

Una vez que el vector store esté disponible, el paso 2 puede reemplazarse por
recuperación semántica de los artículos más representativos, eliminando la necesidad
del muestreo manual por fuente:

```r
# Con ragnar (versión futura):
articulos_contexto <- ragnar_retrieve(
  store,
  query  = "principales temas noticias prensa chilena",
  top_k  = 60
) |> dplyr::filter(fecha >= fecha_inicio, fecha <= fecha_fin)
```

---

## Nuevo script en el pipeline: `analisis/run_embeddings.R`

Se insertaría como **paso 2 del pipeline**, después de `run_analisis_titulos.R`:

```
run_pipeline.sh
  └─ run_analisis_titulos.R   (paso 1 — ya existe)
  └─ run_embeddings.R         (paso 2 — nuevo: embeber + indexar artículos nuevos)
  └─ run_analisis_coocurrencia.R
  └─ run_analisis_ngramas.R
  └─ run_sentimiento.R
```

Responsabilidades de `run_embeddings.R`:
1. Conectar a PostgreSQL y al store DuckDB
2. Obtener IDs de artículos sin entrada en `noticias_embeddings_meta`
3. Embeber en batches de 50 (para no saturar Ollama)
4. Insertar en el store y actualizar `noticias_embeddings_meta`
5. Re-construir el índice VSS si hubo nuevos documentos

---

## Resumen de cambios respecto al tutorial original

| Tutorial | Adaptación al proyecto |
|---|---|
| Corpus sintético (15 docs) | Tabla `noticias` en PostgreSQL (miles de artículos) |
| Store en memoria (`:memory:`) | Store persistido en `datos/noticias_rag.duckdb` |
| Un solo tipo de documento | 25+ fuentes como metadato de filtrado |
| Dominio financiero (IFRS9) | Prensa chilena (política, economía, social) |
| Modelo generativo: llama3.1 | qwen2.5:3b (ya instalado para sentimiento) |
| Corpus estático | Actualización incremental diaria vía `run_embeddings.R` |
| Solo Q&A | Q&A + clustering + búsqueda semántica en dashboard |

---

## TODO

### FASE 1 — Sin embeddings (implementable de inmediato)

#### 1.1 Red de palabras: community detection

- [x] En `output$red_coocurrencia_plotly`, después de `grado <- igraph::degree(g)`, agregar detección de comunidades con `igraph::cluster_louvain(g)` (fallback a `cluster_walktrap` en `tryCatch`)
- [x] Calcular `membresia <- igraph::membership(comunidades)` y `n_com <- max(membresia, 1L)`
- [x] Definir paleta `PALETA_COMUNIDADES` de 15 colores en el bloque del render (o como constante global al inicio del server)
- [x] Reemplazar el único `add_trace` de nodos por un bucle `for (com in seq_len(n_com))` que agrega un trace por comunidad
- [x] En cada trace de comunidad: usar `customdata = grado[idx]` y `hovertemplate = "<b>%{text}</b><br>Conexiones: %{customdata}<extra></extra>"`
- [x] Actualizar `layout()`: ampliar margen derecho (`r = 140`) para dar espacio a la leyenda de comunidades
- [x] Añadir `legend = list(title = list(text = "Comunidades"), ...)` al layout
- [x] Verificar que `set.seed(42L)` sigue presente antes de `layout_with_fr` para reproducibilidad
- [x] Probar con umbral de co-ocurrencia bajo (2–3) y alto (10+) para validar que los grupos tienen sentido temático

#### 1.2 Resumen generativo

- [x] En `ui`: dentro de `tabPanel("Tendencias", ...)`, después de `uiOutput("controles_pagina")`, agregar `hr()`, título `h4`, descripción `p`, `actionButton("btn_generar_resumen", ...)`, `uiOutput("resumen_estado")` y `uiOutput("resumen_periodo_ui")`
- [x] En `server`: crear `reactiveVal` `resumen_texto <- reactiveVal(NULL)` y `resumen_generando <- reactiveVal(FALSE)`
- [x] Implementar `observeEvent(input$btn_generar_resumen, {...})` con:
  - [x] Guard `req(!resumen_generando())`
  - [x] Leer `input$fechas[1]` y `input$fechas[2]`
  - [x] Query a PostgreSQL con `poolCheckout(pool)` / `poolReturn(con)` para traer `titulo, fuente, fecha`
  - [x] Lógica de muestreo según `n_dias`: ≤14 → todos (cap 300), ≤90 → top 8 por fuente, >90 → top 3 por fuente por mes
  - [x] Construir `contexto_txt` con formato `[fecha] fuente: titulo`
  - [x] Llamada a Ollama via `httr2::request("http://localhost:11434/api/chat")` con `model = "qwen2.5:3b"`, `stream = FALSE`, mensajes system + user
  - [x] `httr2::req_timeout(120)` para evitar que cuelgue el dashboard
  - [x] Guardar resultado en `resumen_texto(...)` y `resumen_generando(FALSE)` en ambas ramas (éxito y error)
- [x] Implementar `output$resumen_estado` que muestra "Generando…" mientras `resumen_generando()` es TRUE
- [x] Implementar `output$resumen_periodo_ui` que renderiza el texto con estilo (borde azul izquierdo, fondo gris claro, `white-space: pre-wrap`)
- [x] Verificar que al cambiar el rango de fechas el resumen anterior no desaparece (solo se limpia al hacer clic de nuevo)
- [x] Probar con rango de 7 días, 1 mes y 1 año para validar que el muestreo funciona

#### 1.3 Schema

- [x] Agregar tabla `noticias_embeddings_meta` a `schema.sql` (columnas: `id TEXT PK`, `embebido_en TIMESTAMP`, `modelo VARCHAR`, `store_version INT`)
- [x] Agregar tabla `noticias_clusters` a `schema.sql` (columnas: `fecha DATE`, `cluster_id INT`, `id_noticia TEXT FK`, `score_centroide FLOAT`, PK compuesta)
- [x] Crear índices correspondientes con `IF NOT EXISTS`
- [x] Agregar `COMMENT ON TABLE` para ambas tablas

---

### FASE 2 — Setup de embeddings

#### 2.1 Infraestructura Ollama

<!-- Requiere ejecución manual en terminal -->
- [x] Ejecutar `ollama pull nomic-embed-text` en terminal y verificar con `ollama list` que aparece junto a `qwen2.5:3b`
- [x] Verificar que el modelo responde: `curl http://localhost:11434/api/embeddings -d '{"model":"nomic-embed-text","prompt":"prueba"}'`

#### 2.2 Paquetes R

<!-- Requiere ejecución manual en R -->
- [x] `install.packages("ragnar")` y verificar con `library(ragnar)`
- [x] `install.packages("ellmer")` y verificar con `library(ellmer)`
- [x] `install.packages("proxy")` (para similitud coseno en aristas semánticas) y verificar
- [x] Agregar los tres paquetes a `requirements.txt` o documentarlos en `README.md` del proyecto

#### 2.3 Script `analisis/run_embeddings.R`

- [x] Crear archivo `analisis/run_embeddings.R` con bloque de carga de `.env` (mismo patrón que `run_analisis_titulos.R`)
- [x] Implementar conexión a PostgreSQL via `conectar_db()` de `funciones.R`
- [x] Implementar apertura/creación del store: `ragnar_store_create(location = "datos/noticias_rag.duckdb", embed = embed_ollama("nomic-embed-text"), version = 1)`
- [x] Implementar detección de IDs ya indexados: query a `noticias_embeddings_meta` para obtener IDs existentes
- [x] Implementar query de artículos nuevos: `SELECT id, titulo, fuente, fecha FROM noticias WHERE id NOT IN (...)` — usar chunk de 500 IDs para no hacer un IN enorme
- [x] Implementar loop de inserción en batches de 50 artículos (para no saturar Ollama): `ragnar_store_insert()` + actualizar `noticias_embeddings_meta`
- [x] Llamar `ragnar_store_build_index(store)` solo si se insertaron artículos nuevos
- [x] Agregar logging con `message()` del mismo estilo que los otros scripts del pipeline
- [x] Manejar errores de Ollama (timeout, conexión rechazada) sin detener todo el batch — skip el artículo, loggear y continuar
- [x] Añadir argumento CLI `--backfill-dias N` para controlar cuántos días hacia atrás indexar en la primera ejecución (default: 30)
- [x] Probar con `--backfill-dias 7` antes de hacer el backfill completo

#### 2.4 Pipeline diario

- [x] Editar `run_pipeline.sh` para agregar llamada a `Rscript analisis/run_embeddings.R` después de `run_analisis_titulos.R`
- [x] Verificar que un fallo en `run_embeddings.R` no detiene el resto del pipeline (usar `|| true` o `set +e` localizado)

---

### FASE 3 — Mejoras al dashboard que requieren embeddings

#### 3.1 Cargar el store en el dashboard

- [x] Al inicio de `server()`, intentar abrir el store con `tryCatch(ragnar_store_create(location = "datos/noticias_rag.duckdb", ...), error = function(e) NULL)`
- [x] Guardar en variable `rag_store` (NULL si no existe o falla)
- [x] Crear helper reactivo `store_disponible <- reactive({ !is.null(rag_store) })` para condicionar las features semánticas
- [x] En los toggles semánticos de la UI, mostrar el control solo si `store_disponible()` es TRUE; si no, mostrar una nota "Búsqueda semántica no disponible (ejecutar run_embeddings.R)"

#### 3.2 Evolución temporal: toggle semántico

- [x] En `ui` sidebar (bloque `conditionalPanel` de Tendencias), agregar `checkboxInput("busqueda_semantica_evol", "Búsqueda semántica", value = FALSE)` debajo del `busqueda_termino`
- [x] En el reactivo `datos_evolucion`, bifurcar según `input$busqueda_semantica_evol`:
  - Rama FALSE (actual): SQL `ILIKE` sobre `titulos_terminos_diarios` sin cambios
  - Rama TRUE: `ragnar_retrieve_vss(rag_store, query = input$busqueda_termino, top_k = 500)` → filtrar por fecha → agregar por día
- [x] Añadir etiqueta visual en el gráfico cuando está en modo semántico (texto en anotación o título del eje)

#### 3.3 Tabla de noticias: toggle semántico

- [x] En `ui`, junto al `busqueda_titulo`, agregar `checkboxInput("busqueda_semantica_tabla", "Semántica", value = FALSE)` o un toggle visual
- [x] En los reactivos `total_noticias_rango` y `tabla_noticias`, bifurcar según el toggle:
  - Rama FALSE: SQL `~*` actual sin cambios
  - Rama TRUE: `ragnar_retrieve_vss(rag_store, query = busq, top_k = 100)` → filtrar por fecha y fuente → traer los IDs → `WHERE id IN (...)` en PostgreSQL para obtener los campos completos
- [x] Resetear `page_noticias(1L)` al cambiar de modo

#### 3.4 Top 30: clustering semántico

- [x] En `output$grafico_top_terminos`, después de obtener `top_30_df()`, verificar `store_disponible()`
- [x] Si TRUE: embeber los 30 términos con `embed_ollama("nomic-embed-text")(top$termino)`, calcular matriz de similitud coseno con `proxy::simil(..., method = "cosine")`, aplicar `hclust` con corte en altura 0.3 para agrupar sinónimos
- [x] Agregar columna `cluster_label` al dataframe: nombre del término más frecuente del cluster
- [x] Colapsar frecuencias por cluster y graficar barras con el label del cluster
- [x] Añadir tooltip que liste los términos agrupados en cada barra

#### 3.5 Conceptos por medio: búsqueda semántica

- [x] En `datos_conceptos_por_medio`, bifurcar según `store_disponible()`:
  - Rama sin store: SQL `lower(...) = lower($3)` actual sin cambios
  - Rama con store: `ragnar_retrieve_vss(rag_store, query = terms[i], top_k = 500)` → filtrar por fecha → contar por fuente
- [x] El gráfico `grafico_conceptos_por_medio` no necesita cambios (recibe el mismo dataframe)

#### 3.6 Red de palabras: aristas semánticas (Mejora B)

- [x] En `ui` sidebar de la pestaña "Red de palabras", agregar `sliderInput("umbral_semantico", "Similitud semántica mínima", min = 0.5, max = 0.95, value = 0.80, step = 0.05)` condicional a `store_disponible()`
- [x] En `output$red_coocurrencia_plotly`, si `store_disponible()`, embeber todos los nodos de la red con `embed_ollama("nomic-embed-text")`
- [x] Calcular matriz de similitud coseno, filtrar pares con `similitud >= input$umbral_semantico` que no tengan ya arista léxica
- [x] Agregar segundo trace de aristas semánticas: línea azul punteada (`dash = "dot"`, `color = "rgba(13,110,253,0.3)"`)
- [x] Agregar leyenda que distinga aristas léxicas (gris) vs semánticas (azul)

#### 3.7 Resumen generativo: mejorar con embeddings

- [x] En `observeEvent(input$btn_generar_resumen, {...})`, si `store_disponible()`, reemplazar el muestreo manual por `ragnar_retrieve(rag_store, query = "principales temas noticias prensa chilena", top_k = 60)` filtrado por fecha
- [x] Si no hay store, mantener la lógica de muestreo por fuente de Fase 1 como fallback

---

### Verificación final

- [x] Confirmar que todas las features semánticas degradan gracefully cuando el store no existe (no rompen el dashboard, muestran mensaje informativo)
- [x] Confirmar que `run_embeddings.R` es idempotente (se puede correr dos veces sin duplicar registros)
- [x] Confirmar que el pipeline diario en `run_pipeline.sh` sigue completando en tiempo razonable con el nuevo paso de embeddings
- [x] Actualizar `README.md` de noticias con instrucciones de setup para Fase 2

---

## Orden de implementación sugerido

Las mejoras se dividen en dos fases: las que funcionan **ahora mismo** (sin embeddings)
y las que requieren el vector store.

### Fase 1 — Sin embeddings (implementable de inmediato)

| Paso | Cambio | Archivo | Dependencias nuevas |
|---|---|---|---|
| 1 | Community detection en red de palabras | `dashboard/app.R` | ninguna (`igraph` ya instalado) |
| 2 | Resumen generativo con botón | `dashboard/app.R` | ninguna (`httr2` ya instalado) |
| 3 | Agregar tablas de metadatos a schema | `schema.sql` | ninguna |

### Fase 2 — Con embeddings (requiere setup previo)

| Paso | Cambio | Archivo | Dependencias nuevas |
|---|---|---|---|
| 4 | `ollama pull nomic-embed-text` | terminal | Ollama ya corriendo |
| 5 | `install.packages(c("ragnar", "ellmer"))` | R | — |
| 6 | Crear `analisis/run_embeddings.R` (indexado inicial, últimos 30 días) | nuevo archivo | ragnar |
| 7 | Agregar `run_embeddings.R` al pipeline diario en `run_pipeline.sh` | `run_pipeline.sh` | — |
| 8 | Toggle semántico en evolución temporal de términos | `dashboard/app.R` | ragnar |
| 9 | Toggle semántico en búsqueda de titulares (tabla noticias) | `dashboard/app.R` | ragnar |
| 10 | Top 30 con clustering semántico de términos | `dashboard/app.R` | ragnar |
| 11 | Búsqueda semántica en "Conceptos por medio" | `dashboard/app.R` | ragnar |
| 12 | Aristas semánticas en la red de palabras (Mejora B) | `dashboard/app.R` | ragnar, proxy |
| 13 | Mejorar resumen generativo con recuperación semántica | `dashboard/app.R` | ragnar |
