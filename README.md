# Análisis de Noticias Chile

Pipeline en R y PostgreSQL para **recolectar**, **analizar** y **visualizar** titulares de 28 medios de comunicación chilenos. Incluye tokenización, frecuencias de términos, detección de entidades, co-ocurrencias, n-gramas y análisis de sentimiento, todo expuesto en un dashboard Shiny de solo lectura.

**~882.000 artículos** cargados desde 2007 a la fecha (marzo 2026).

---

## Estructura del repositorio

```
noticias/
│
├── run_pipeline.sh              # Orquestador principal — punto de entrada
├── schema.sql                   # Definición completa de la BD (15 tablas)
├── vaciar_db.sql                # Vaciar tablas derivadas para re-análisis total
├── funciones.R                  # Librería compartida: DB, scraping, texto
├── stopwords.R                  # Stopwords centralizadas (fuente única)
│
├── analisis/                    # Scripts del pipeline de análisis
│   ├── run_analisis_ngramas.R       # Bigramas y trigramas de titulares
│   ├── run_analisis_titulos.R       # Tokenización + frecuencias + entidades
│   ├── run_analisis_coocurrencia.R  # Red de co-ocurrencias entre términos
│   └── run_sentimiento.R            # Clasificación de sentimiento (LLM / NRC)
│
├── scraping/
│   └── fuentes/                 # 33 scrapers — uno por fuente/variante
│
├── dashboard/
│   ├── app.R                    # Dashboard Shiny (~1530 líneas, solo lectura)
│   ├── README.md
│   └── www/                     # Imágenes estáticas
│
├── scripts/
│   └── cargar_parquet_a_postgres.py  # Carga histórica inicial (ya ejecutada)
│
└── docs/
    ├── ARCHITECTURE.md          # Arquitectura detallada, tablas, decisiones
    └── plan.md                  # Reporte técnico completo del sistema
```

---

## Arquitectura general

```
28 fuentes web
     │
     ▼  [33 scrapers R — rvest / chromote / httr2]
     │
┌─────────────────────────────┐
│   noticias  (PostgreSQL)    │  ← fuente única de verdad
│   ~882k artículos           │
└─────────────────────────────┘
     │
     ├──▶ analisis/run_analisis_ngramas.R       → titulos_ngramas_*
     ├──▶ analisis/run_analisis_titulos.R       → titulos_terminos_*
     ├──▶ analisis/run_analisis_coocurrencia.R  → titulos_coocurrencia
     └──▶ analisis/run_sentimiento.R            → noticias_sentimiento
                                                        │
                              dashboard/app.R ◀─────────┘
                              (solo lectura)
```

**Principios de diseño:**
- Todo pre-computado en PostgreSQL — el dashboard no calcula nada
- Incremental por defecto — cada script procesa solo las fechas nuevas
- PostgreSQL como fuente única de verdad — sin archivos intermedios
- Titulares como unidad de análisis; los cuerpos se usan solo para sentimiento

---

## Requisitos

**R ≥ 4.2:**
```r
install.packages(c(
  # análisis
  "DBI", "RPostgres", "tidytext", "dplyr", "furrr", "future",
  "tidyr", "purrr", "stringr", "stringi", "digest",
  # sentimiento
  "syuzhet",
  # dashboard
  "shiny", "pool", "ggplot2", "plotly", "visNetwork"
))
```

**PostgreSQL 18+** — base `noticias_chile`, usuario `noticias`. Crear con `schema.sql`.

**Ollama** (opcional) — modelo `qwen2.5:3b` para sentimiento LLM. Si no está disponible, `run_sentimiento.R` usa el léxico NRC de `syuzhet` como fallback.

**Python 3** (opcional) — solo necesario para la carga histórica inicial con `scripts/cargar_parquet_a_postgres.py`.

---

## Configuración

Las credenciales se leen desde un archivo `.env` ubicado en la carpeta padre del proyecto (`Paginaweb/.env`). **Este archivo nunca debe subirse al repositorio.**

```env
PGHOST=localhost
PGPORT=5432
PGUSER_NOTICIAS=noticias
PGPASSWORD_NOTICIAS=tu_password
PGDATABASE_NOTICIAS=noticias_chile
```

---

## Uso

Todos los comandos se ejecutan desde la **raíz del repositorio** (`noticias/`).

### 1. Crear la base de datos (una vez)

```bash
psql -U noticias -d noticias_chile -h localhost -f schema.sql
```

### 2. Pipeline completo

`run_pipeline.sh` orquesta todo: detecta automáticamente la fecha del último artículo en la BD, calcula cuántos días han pasado, y scrapea solo ese período. Luego corre el análisis incremental.

```bash
./run_pipeline.sh                   # scraping + análisis
./run_pipeline.sh --solo-analisis   # solo análisis (sin scraping)
./run_pipeline.sh --con-historicos  # incluir scrapers históricos
```

Los logs se guardan en `logs/pipeline_YYYY-MM-DD_HH-MM.log`.

### 3. Scripts de análisis individuales

El orden importa: ngramas debe correr antes que titulos (sus resultados alimentan la detección de compuestos frecuentes).

```bash
Rscript analisis/run_analisis_ngramas.R
Rscript analisis/run_analisis_titulos.R
Rscript analisis/run_analisis_coocurrencia.R
Rscript analisis/run_sentimiento.R
```

### 4. Dashboard Shiny

```bash
Rscript -e "shiny::runApp('dashboard', port = 3838, host = '0.0.0.0')"
```

Abrir en el navegador: **http://localhost:3838**

El dashboard incluye:
- **Tendencias** — top términos, evolución temporal, buscador de noticias
- **Medios** — frecuencias por fuente, red de co-ocurrencias (visNetwork), sentimiento editorial
- **Más información** — descripción del proyecto y fuentes

### 5. Ejecución automática (cron)

```
0 6 * * * cd /ruta/a/noticias && /bin/bash run_pipeline.sh >> logs/cron.log 2>&1
```

---

## Fuentes (28 medios)

24 Horas, ADN Radio, Agricultura, Radio BíoBío, CHV Noticias, Ciper, CNN Chile, Cooperativa, Diario Financiero, El Ciudadano, El Desconcierto, El Dínamo, El Mostrador, El Siglo, Emol, Ex-Ante, Izquierda Diario, La Cuarta, La Hora, La Nación, La Tercera, Meganoticias, Publimetro, El Quinto Poder, Radio U. de Chile, RedGol, T13, The Clinic.

---

## Documentación

- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — arquitectura detallada, esquema de BD, patrones de diseño
- [`docs/plan.md`](docs/plan.md) — reporte técnico completo: todos los scripts, patrones críticos, estado actual

---

## Licencia

MIT. Ver [LICENSE](LICENSE). Los datos de noticias dependen de los términos de uso de cada fuente.
