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

## Dashboard

El dashboard incluye tres secciones principales:

- **Tendencias** — evolución de términos en el tiempo, top palabras del período y buscador de noticias
- **Medios** — frecuencia de términos por fuente, red de co-ocurrencias interactiva y tono editorial por medio
- **Más información** — descripción del proyecto y fuentes de datos

---

## Estructura del repositorio

```
├── run_pipeline.sh       # Script principal que corre todo el sistema
├── schema.sql            # Estructura de la base de datos
├── funciones.R           # Funciones compartidas entre scripts
├── stopwords.R           # Palabras excluidas del análisis
│
├── analisis/             # Scripts de análisis de texto
├── scraping/             # Scrapers por fuente (basados en prensa_chile)
├── dashboard/            # Aplicación Shiny de visualización
├── scripts/              # Utilidades auxiliares
└── docs/                 # Documentación técnica
```

---

## Requisitos técnicos

- **R ≥ 4.2** con los paquetes: `tidytext`, `DBI`, `RPostgres`, `furrr`, `shiny`, `ggplot2`, `plotly`, `visNetwork`
- **PostgreSQL** como base de datos
- **Ollama** (opcional) con el modelo `qwen2.5:3b` para clasificación de sentimiento con LLM; si no está disponible, se usa el léxico NRC como alternativa automática

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
