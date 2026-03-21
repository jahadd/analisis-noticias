# Análisis de Noticias Chile

Pipeline de recolección y análisis de titulares de **28 medios de comunicación chilenos**, con visualización interactiva en un dashboard web.

El sistema recolecta noticias diariamente, procesa los titulares con técnicas de procesamiento de lenguaje natural (NLP), y almacena los resultados en una base de datos PostgreSQL que alimenta un dashboard de solo lectura.

> **Base de datos y scrapers:** los scripts de scraping y la base de datos de noticias son de autoría de [Bastián Olea](https://github.com/bastianolea/prensa_chile). Este repositorio extiende ese trabajo con una capa de análisis y visualización propia.

---

## ¿Qué hace este proyecto?

1. **Recolecta** titulares de 28 medios chilenos de forma automática y diaria mediante scrapers web
2. **Procesa** los titulares con tokenización, detección de entidades y análisis de co-ocurrencias
3. **Clasifica** el tono de cada noticia (positivo / neutral / negativo) mediante un modelo de lenguaje
4. **Visualiza** los resultados en un dashboard interactivo con gráficos de evolución temporal, redes de conceptos y comparación entre medios

---

## Fuentes (28 medios)

24 Horas · ADN Radio · Agricultura · Radio BíoBío · CHV Noticias · Ciper · CNN Chile · Cooperativa · Diario Financiero · El Ciudadano · El Desconcierto · El Dínamo · El Mostrador · El Siglo · Emol · Ex-Ante · Izquierda Diario · La Cuarta · La Hora · La Nación · La Tercera · Meganoticias · Publimetro · El Quinto Poder · Radio U. de Chile · RedGol · T13 · The Clinic · Para mayor información acerca de los scripts de scrapping consultar con el autor original.

---

## Metodología de análisis

El análisis opera exclusivamente sobre **titulares** de noticias, que constituyen la unidad de observación. Los cuerpos de los artículos se usan únicamente como insumo para el análisis de sentimiento. Todos los resultados se pre-calculan y almacenan en PostgreSQL; el dashboard no realiza cálculos al vuelo.

Los scripts viven en `analisis/` y se ejecutan en el siguiente orden:

---

### 1. Extracción de n-gramas — `run_analisis_ngramas.R`

Se extraen **bigramas** (pares de palabras) y **trigramas** (tríos de palabras) de cada titular mediante tokenización secuencial. Se filtran las combinaciones que contengan stopwords o tokens de menos de 3 caracteres. El resultado es un conteo de frecuencia de cada n-grama por día y por medio, almacenado en la base de datos.

Estos resultados cumplen una función metodológica doble: son consultables directamente en el dashboard, y alimentan el paso siguiente para la detección automática de entidades compuestas.

---

### 2. Frecuencia de términos y detección de entidades — `run_analisis_titulos.R`

Se tokeniza cada titular usando `tidytext` y se aplica un proceso de **normalización y unificación de entidades** antes del conteo:

- **Entidades curadas manualmente:** un diccionario de ~200 expresiones compuestas conocidas (nombres de políticos, países, instituciones, partidos, ministerios) se fusionan en un único token antes de contar. Por ejemplo, "Gabriel Boric", "Estados Unidos" o "Banco Central" se tratan como una sola unidad léxica.
- **Entidades detectadas automáticamente:** se identifican los n-gramas con 50 o más apariciones históricas totales en la base de datos. Aquellos que no sean sub-expresiones de entidades ya conocidas se incorporan también como tokens unificados. Se aplica normalización de plurales para evitar duplicados ("incendios forestales" → "incendio forestal").
- **Canonicalización de nombres:** apellidos frecuentes se mapean a su nombre completo canónico ("boric" → "Gabriel Boric", "kast" → "José Antonio Kast").

Finalmente, se eliminan stopwords y se cuenta la frecuencia de cada término por día y por medio. El procesamiento se paraleliza en 4 workers con chunks de 90 días para manejar el volumen de ~882.000 artículos.

---

### 3. Análisis de co-ocurrencia — `run_analisis_coocurrencia.R`

Para cada titular se generan todos los **pares posibles de términos** que aparecen en él (`combn(tokens, 2)`), aplicando previamente el mismo proceso de normalización y detección de entidades que en el paso anterior. Se agrega el número de co-apariciones por par, fecha y medio.

El orden canónico de cada par (término A, término B) se impone mediante `LEAST()/GREATEST()` en SQL —no en R— para evitar inconsistencias de ordenamiento entre el entorno R y PostgreSQL con caracteres acentuados.

Los resultados permiten construir una red de conceptos donde los nodos son términos y las aristas representan la frecuencia con que dos términos aparecen juntos en titulares.

---


## Dashboard

El dashboard (`dashboard/app.R`) consulta los resultados pre-calculados directamente desde PostgreSQL. Incluye tres secciones:

- **Tendencias** — evolución temporal de términos seleccionados, ranking de los 30 términos más frecuentes en el período y buscador de noticias por palabra clave
- **Medios** — comparación de frecuencias entre fuentes, red de co-ocurrencias interactiva (visNetwork) y distribución del tono editorial por medio
- **Más información** — descripción del proyecto y listado de fuentes

---

## Estructura del repositorio

```
├── run_pipeline.sh       # Orquestador principal: scraping + análisis
├── schema.sql            # Estructura de la base de datos (15 tablas)
├── funciones.R           # Funciones compartidas entre todos los scripts
├── stopwords.R           # Palabras excluidas del análisis
│
├── analisis/             # Scripts de análisis (ver metodología arriba)
├── scraping/             # Scrapers por fuente, basados en prensa_chile
├── dashboard/            # Aplicación Shiny de visualización
├── scripts/              # Utilidades auxiliares
└── docs/                 # Documentación técnica detallada
```

---

## Requisitos técnicos

- **R ≥ 4.2** con los paquetes: `tidytext`, `DBI`, `RPostgres`, `furrr`, `shiny`, `ggplot2`, `plotly`, `visNetwork`
- **PostgreSQL** como base de datos
- **Ollama** (opcional) con el modelo `qwen2.5:3b` para clasificación de sentimiento; si no está disponible se usa el léxico NRC de forma automática

---

## Uso básico

Configurar las credenciales de base de datos en un archivo `.env` (ver `.env.example`) y ejecutar:

```bash
# Pipeline completo: scraping + análisis
./run_pipeline.sh

# Solo análisis (sin scrapear)
./run_pipeline.sh --solo-analisis

# Lanzar el dashboard
Rscript -e "shiny::runApp('dashboard', port = 3838)"
```

---

## Créditos

- **Scraping y base de datos:** [Bastián Olea](https://github.com/bastianolea/prensa_chile) — [`bastianolea/prensa_chile`](https://github.com/bastianolea/prensa_chile)
- **Análisis y dashboard:** este repositorio

---

## Licencia

MIT. Los datos de noticias están sujetos a los términos de uso de cada medio.
