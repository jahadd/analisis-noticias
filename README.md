# Análisis de Noticias Chile

Pipeline de recolección y análisis de titulares de **28 medios de comunicación chilenos**, con visualización interactiva en un dashboard web.

El sistema recolecta noticias diariamente, identifica los términos y entidades más mencionados, detecta relaciones entre conceptos y clasifica el tono editorial — todo visible en un dashboard Shiny de solo lectura.

> **Base de datos y scrapers:** los scripts de scraping y la base de datos de noticias son de autoría de [Bastián Olea](https://github.com/bastianolea/prensa_chile). Este repositorio extiende ese trabajo con una capa de análisis y visualización propia.

---

## ¿Qué hace este proyecto?

1. **Recolecta** titulares de 28 medios chilenos de forma automática y diaria
2. **Analiza** los términos más mencionados, detecta entidades como políticos, países e instituciones, y encuentra qué palabras aparecen juntas en los mismos titulares
3. **Clasifica** el tono de cada noticia (positivo / neutral / negativo) usando un modelo de lenguaje local
4. **Visualiza** todo en un dashboard interactivo con gráficos de evolución, redes de conceptos y comparación entre medios

---

## Fuentes (28 medios)

24 Horas · ADN Radio · Agricultura · Radio BíoBío · CHV Noticias · Ciper · CNN Chile · Cooperativa · Diario Financiero · El Ciudadano · El Desconcierto · El Dínamo · El Mostrador · El Siglo · Emol · Ex-Ante · Izquierda Diario · La Cuarta · La Hora · La Nación · La Tercera · Meganoticias · Publimetro · El Quinto Poder · Radio U. de Chile · RedGol · T13 · The Clinic

---

## Scripts de análisis

Los scripts viven en la carpeta `analisis/` y se ejecutan en este orden dentro del pipeline:

### `run_analisis_ngramas.R`
Extrae bigramas y trigramas de los titulares (pares y tríos de palabras que aparecen juntas). Estos resultados alimentan el siguiente paso para detectar expresiones compuestas frecuentes como "alerta roja" o "derechos humanos".

### `run_analisis_titulos.R`
Tokeniza todos los titulares y cuenta la frecuencia de cada término por día y por medio. Antes de contar, detecta y unifica entidades compuestas (nombres de políticos, países, instituciones, partidos) para que "Gabriel Boric" cuente como una sola entidad y no como dos palabras sueltas. Usa procesamiento paralelo para manejar el volumen de ~882.000 artículos.

### `run_analisis_coocurrencia.R`
Para cada titular, calcula qué pares de términos aparecen juntos. Esto permite construir una red de conceptos que muestra qué palabras tienden a aparecer en el mismo contexto.

### `run_sentimiento.R`
Clasifica cada artículo como positivo, neutral o negativo. Usa un modelo de lenguaje local vía Ollama (`qwen2.5:3b`) como método principal, con el léxico NRC como alternativa automática si Ollama no está disponible.

---

## Dashboard

El dashboard (`dashboard/app.R`) consulta los resultados pre-calculados en PostgreSQL — no realiza cálculos al vuelo. Incluye tres secciones:

- **Tendencias** — evolución de términos en el tiempo, top palabras del período y buscador de noticias
- **Medios** — frecuencia de términos por fuente, red de co-ocurrencias interactiva y tono editorial por medio
- **Más información** — descripción del proyecto y fuentes de datos

---

## Estructura del repositorio

```
├── run_pipeline.sh       # Orquestador principal: scraping + análisis
├── schema.sql            # Estructura de la base de datos (15 tablas)
├── funciones.R           # Funciones compartidas entre todos los scripts
├── stopwords.R           # Palabras excluidas del análisis (artículos, preposiciones, etc.)
│
├── analisis/             # Scripts de análisis (ver detalle arriba)
├── scraping/             # Scrapers por fuente, basados en prensa_chile
├── dashboard/            # Aplicación Shiny de visualización
├── scripts/              # Utilidades auxiliares (carga histórica)
└── docs/                 # Documentación técnica detallada
```

---

## Requisitos técnicos

- **R ≥ 4.2** con los paquetes: `tidytext`, `DBI`, `RPostgres`, `furrr`, `shiny`, `ggplot2`, `plotly`, `visNetwork`
- **PostgreSQL** como base de datos
- **Ollama** (opcional) con el modelo `qwen2.5:3b` para clasificación de sentimiento con LLM; si no está disponible, se usa el léxico NRC de forma automática

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
