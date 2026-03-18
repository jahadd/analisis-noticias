# Arquitectura: pipeline de análisis de noticias Chile

---

## 1. Principios de diseño

- **Fuente única de verdad:** tabla `noticias` en PostgreSQL. Todos los análisis derivan de ella.
- **Pre-cómputo total:** todos los outputs se calculan offline y se almacenan en tablas. El dashboard es exclusivamente de lectura — no realiza cálculos al vuelo.
- **Titulares como unidad primaria:** todo el análisis textual opera sobre `titulo`. El campo `cuerpo_limpio` se usa solo como input al modelo de sentimiento LLM.
- **Incrementalidad:** todos los scripts detectan `MAX(fecha)` en sus tablas de salida y procesan solo las fechas nuevas (borrando desde MAX-1 día como buffer de seguridad).
- **Dependencia ordenada:** `run_analisis_ngramas.R` debe correr antes que `run_analisis_titulos.R` porque este último lee los n-gramas para auto-detectar compuestos frecuentes.

---

## 2. Arquitectura general

```
28 fuentes web
     │
     ▼  [33 scrapers — scraping/fuentes/]
     │
┌──────────────────────────────┐
│   noticias  (PostgreSQL)     │  ← fuente única de verdad, ~882k artículos
│   UNIQUE(url, fuente)        │
└──────────────────────────────┘
     │
     ├──▶ analisis/run_analisis_ngramas.R       → titulos_ngramas_diarios
     │                                          → titulos_ngramas_por_medio
     │
     ├──▶ analisis/run_analisis_titulos.R       → titulos_terminos_diarios
     │    (lee ngramas para COMPUESTOS_FRECUENTES) titulos_terminos_por_medio
     │                                          → metricas_titulos_diarias
     │
     ├──▶ analisis/run_analisis_coocurrencia.R  → titulos_coocurrencia
     │
     └──▶ analisis/run_sentimiento.R            → noticias_sentimiento

Vistas SQL (creadas automáticamente):
  titulos_terminos_diarios  ──┐
  titulos_ngramas_diarios   ──┴──▶ terminos_unificados_diarios
  titulos_terminos_por_medio──┐
  titulos_ngramas_por_medio ──┴──▶ terminos_unificados_por_medio

[dashboard/app.R] ← solo lectura de todas las tablas anteriores
```

**Orden de ejecución en el pipeline:**
1. `run_analisis_ngramas.R` (genera la tabla que alimenta el paso 2)
2. `run_analisis_titulos.R`
3. `run_analisis_coocurrencia.R`

---

## 3. Tablas PostgreSQL (15 tablas)

### `noticias` — fuente, nunca modificada por el pipeline

| Columna | Tipo | Descripción |
|---------|------|-------------|
| `id` | VARCHAR(32) PK | MD5(url + fuente) |
| `titulo` | TEXT | Titular original |
| `bajada` | TEXT | Bajada/subtítulo (nullable) |
| `cuerpo` | TEXT | Cuerpo completo (nullable) |
| `cuerpo_limpio` | TEXT | Cuerpo limpiado (input LLM) |
| `fecha` | DATE | Fecha de publicación |
| `fecha_scraping` | DATE | Fecha de captura |
| `url` | TEXT | URL original |
| `fuente` | VARCHAR(100) | Código del medio |
| `año` | INTEGER | Extraído de fecha |

UNIQUE(url, fuente).

### Tablas derivadas

| Tabla | Filas aprox. | Descripción |
|-------|-------------|-------------|
| `titulos_terminos_diarios` | ~6.7M | Frecuencia global por (fecha, termino) |
| `titulos_terminos_por_medio` | grande | Frecuencia por (fecha, fuente, termino) |
| `metricas_titulos_diarias` | ~2,500 | total_noticias + terminos_unicos por día |
| `titulos_coocurrencia` | ~20.6M | Pares de términos co-aparecidos (fecha, fuente, termino_a, termino_b, n) |
| `titulos_ngramas_diarios` | — | Bigramas/trigramas por (fecha, ngrama, tipo) |
| `titulos_ngramas_por_medio` | — | Ídem por fuente |
| `terminos_semanales` | ~2.16M | Frecuencia normalizada semanal |
| `terminos_semanales_fuente` | ~4.71M | Ídem por fuente |
| `terminos_semanales_sentimiento` | 6 | Distribución semanal de sentimiento |
| `noticias_sentimiento` | ~4k | Sentimiento por artículo (id, sentimiento, modelo) |
| `actores_sentimiento` | — | Actor político × fuente × sentimiento × fecha |
| `titulos_tfidf_por_medio` | — | TF-IDF por fuente y semana |
| `cuerpo_terminos_diarios` | — | Términos del cuerpo completo por fecha |
| `cuerpo_ngramas_diarios` | — | N-gramas del cuerpo por fecha |

### Vistas SQL

```sql
CREATE OR REPLACE VIEW terminos_unificados_diarios AS
  SELECT termino, fecha, frecuencia FROM titulos_terminos_diarios
  UNION ALL
  SELECT ngrama AS termino, fecha, n AS frecuencia FROM titulos_ngramas_diarios;

CREATE OR REPLACE VIEW terminos_unificados_por_medio AS
  SELECT termino, fecha, fuente, frecuencia FROM titulos_terminos_por_medio
  UNION ALL
  SELECT ngrama AS termino, fecha, fuente, n AS frecuencia FROM titulos_ngramas_por_medio;
```

Creadas idempotentemente al inicio de `run_analisis_ngramas.R` y al arrancar el dashboard.

---

## 4. Scripts de análisis (`analisis/`)

### `run_analisis_ngramas.R`

- Calcula bigramas (tipo=2) y trigramas (tipo=3) desde titulares.
- Normalización: minúsculas, NFC, puntuación/dígitos → espacio.
- Filtros: ninguna parte es stopword, min 3 chars, max 300 chars por n-grama.
- Sin udpipe: se removió para preservar concordancia de género ("alerta roja", no "alerta rojo").
- Incremental: borra desde MAX(fecha)-1, reprocesa solo desde ahí.
- Crea/actualiza las vistas `terminos_unificados_*` al finalizar.

### `run_analisis_titulos.R`

- Tokeniza titulares con `tidytext::unnest_tokens()`.
- Detecta y fusiona entidades compuestas (~200 hard-coded + auto-detectadas):
  - **COMPUESTOS_CONOCIDOS**: países, políticos, partidos, instituciones, ministerios, comunas San/Santa
  - **COMPUESTOS_FRECUENTES**: n-gramas con ≥50 apariciones históricas en `titulos_ngramas_diarios`, normalizados con `desplural()` y filtrados con `es_sufijo_conocido()`
  - **TOKEN_A_CANONICO**: apellidos → nombre completo ("boric" → "Gabriel Boric")
- Índice de prefijo `.idx_compuestos` para lookup O(1) en vez de O(n_compounds).
- Paralelismo: 4 workers (`furrr`), chunks de 90 días, cada worker autocontenido.
- Parámetros: `CHUNK_DAYS=90`, `MIN_TERM_LEN=3`, `MIN_FREQ_COMPUESTO=50`, `USAR_LEMMAS=FALSE`.
- Incremental: borra tablas desde MAX(fecha)-1, reprocesa desde ahí.

### `run_analisis_coocurrencia.R`

- Genera todos los pares de términos que co-aparecen en el mismo titular (`combn(tokens, 2)`).
- Orden canónico garantizado por SQL `LEAST()/GREATEST()` — no por sort de R — para evitar discrepancias de collation entre R y PostgreSQL.
- Paralelismo: 4 workers, chunks de 90 días.
- Incremental: borra desde MAX(fecha)-1, reprocesa desde ahí.
- Estado: ~20.6M filas, 0 auto-pares, 0 violaciones de orden.

### `run_sentimiento.R`

- **Modelo primario**: `qwen2.5:3b` vía Ollama (`http://localhost:11434`), batch de 30 títulos.
- **Fallback**: `syuzhet::get_nrc_sentiment(language="spanish")` si Ollama no está disponible.
- Input: título + primeros 5000 chars de `cuerpo_limpio`.
- Incremental: salta artículos ya en `noticias_sentimiento`.
- Nota: el léxico NRC tiene sesgo positivo para noticias en español (terremotos, caídas de mercado → "positivo").

---

## 5. Sistema de scraping (`scraping/fuentes/`)

**Patrón uniforme de todos los scrapers:**

```r
source("funciones.R")
con <- conectar_db()

for (url in urls) {
  if (ya_scrapeado_en_db(url, con)) next
  # ... scraping ...
  guardar_noticias_en_postgres(df, con)
}
DBI::dbDisconnect(con)
```

**Funciones clave (`funciones.R`):**
- `conectar_db()` — lee `.env` en `./`, `../`, `../../`
- `ya_scrapeado_en_db(url, con)` — deduplicación O(1) contra la BD
- `guardar_noticias_en_postgres(df, con)` — valida, genera MD5 id, limpia cuerpo, UPSERT
- `dias_a_scrapear(default=3)` — lee `DIAS_SCRAPING` exportada por el pipeline
- `fecha_desde_scraping()` — lee `FECHA_DESDE_SCRAPING` exportada por el pipeline

**Tecnologías por sitio:**
- `rvest` — mayoría de sitios (HTML estático)
- `chromote` — sitios con JS dinámico (Emol, La Tercera)
- `httr2/JSON` — APIs (BíoBío API)

**Deduplicación automática:** `run_pipeline.sh` detecta `MAX(fecha)` en la BD antes de lanzar los scrapers y exporta `DIAS_SCRAPING` + `FECHA_DESDE_SCRAPING` para que cada scraper sepa exactamente qué período cubrir.

---

## 6. Dashboard (`dashboard/app.R`)

- Pool de conexiones `pool::dbPool`, creado una vez al arrancar.
- Vistas SQL creadas idempotentemente al inicio (`CREATE OR REPLACE VIEW`).
- **Sin distinción uni/bi/trigrama:** todo aparece unificado vía las vistas.
- **Sin TF-IDF:** solo frecuencia absoluta.

**Tabs:**
1. **Tendencias** — evolución temporal, top 30 términos, buscador de noticias con paginación
2. **Medios** — conceptos por fuente, términos destacados, volumen, red de co-ocurrencia (visNetwork), tono editorial
3. **Más información** — descripción del proyecto, lista de fuentes

**Reactivos principales:**
- `all_stopwords()` — STOPWORDS base + exclusiones del usuario en la sesión
- `tabla_config()` — metadatos de qué tabla/columna usar según contexto
- `red_coocurrencia()` — filtra `titulos_coocurrencia` → nodos + aristas para visNetwork

---

## 7. Patrón UPSERT vía staging table

Todos los scripts de análisis usan este patrón para escritura masiva eficiente:

```r
dbExecute(con, "DROP TABLE IF EXISTS _stg_tabla")
dbWriteTable(con, "_stg_tabla", df, temporary = FALSE, overwrite = TRUE)
dbExecute(con, "ALTER TABLE _stg_tabla ALTER COLUMN fecha TYPE DATE USING fecha::date")
dbExecute(con, "
  INSERT INTO tabla (cols...)
  SELECT cols... FROM _stg_tabla
  ON CONFLICT (pk_cols) DO UPDATE SET col = EXCLUDED.col, ...
")
dbExecute(con, "DROP TABLE IF EXISTS _stg_tabla")
```

Evita inserts fila a fila y el límite de parámetros en queries parametrizadas grandes.

---

## 8. Despliegue

- **Desarrollo:** `shiny::runApp("dashboard", port = 3838)`
- **Producción:** proceso supervisor/systemd en el puerto 3838, Nginx como proxy
- **Cron:** `0 6 * * * cd /ruta/noticias && /bin/bash run_pipeline.sh >> logs/cron.log 2>&1`
- **PostgreSQL binario (macOS):** `/Library/PostgreSQL/18/bin/psql` (no está en PATH por defecto)
