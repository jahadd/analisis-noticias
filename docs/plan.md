# Reporte Técnico: Sistema de Análisis de Noticias Chile

_Generado el 2026-03-18. Documento de referencia — reemplaza el plan.md anterior._

---

## 1. Visión General

Sistema completo de scraping, análisis y visualización de noticias chilenas. Recolecta titulares de 28 medios, los tokeniza, calcula co-ocurrencias, n-gramas y sentimiento, y expone los resultados en un dashboard Shiny de solo lectura.

**Stack tecnológico:**
- R (tidytext, furrr, DBI, RPostgres, shiny, plotly, ggplot2, visNetwork)
- PostgreSQL 18 (almacenamiento central, ~882k artículos a marzo 2026)
- Bash (orquestación del pipeline)
- Python 3 (carga histórica desde parquet, ya ejecutada)
- Ollama / qwen2.5:3b (sentimiento LLM, opcional)

**Principios de diseño:**
1. **PostgreSQL como fuente única de verdad.** No hay archivos intermedios; todo scraper escribe directo a la BD.
2. **Todo pre-computado.** El dashboard no calcula nada; consulta tablas ya agregadas. Esto garantiza velocidad y consistencia.
3. **Incremental por defecto.** Cada script de análisis detecta `MAX(fecha)` en su tabla de salida y procesa solo lo nuevo.
4. **Titulares como unidad de análisis.** Los cuerpos de los artículos se usan solo para sentimiento (LLM), nunca para frecuencias.

---

## 2. Estructura de Archivos

```
noticias/
├── run_pipeline.sh              # Orquestador principal (bash)
├── funciones.R                  # Librería central de utilidades
├── stopwords.R                  # Stopwords centralizadas (fuente única)
├── schema.sql                   # Definición de las 15 tablas PostgreSQL
├── vaciar_db.sql                # TRUNCATE tablas derivadas (no noticias)
├── run_analisis_titulos.R       # Branch A: tokenización + frecuencias
├── run_analisis_coocurrencia.R  # Branch B: pares de co-ocurrencia
├── run_analisis_ngramas.R       # Branch E: bigramas y trigramas
├── run_analisis_cuerpo.R        # Branch F: tokenización de cuerpos
├── run_sentimiento.R            # Branch D: clasificación de sentimiento
├── run_actores_sentimiento.R    # Cross-tab actores × fuente × sentimiento
├── comparar_sentimiento.R       # Validación NRC vs LLM (script auxiliar)
├── cargar_parquet_a_postgres.py # Carga histórica (ya ejecutada)
├── prensa_datos.parquet         # Dataset histórico ~850k artículos (2.8 GB)
├── dashboard/
│   └── app.R                    # Dashboard Shiny (~1530 líneas)
├── scraping/
│   └── fuentes/                 # 33 scrapers (uno por fuente/variante)
├── docs/
│   ├── ARCHITECTURE.md
│   └── (imágenes)
└── logs/                        # Logs del pipeline (pipeline_YYYY-MM-DD.log)
```

---

## 3. Base de Datos (schema.sql)

### 3.1 Tabla principal

**`noticias`** (~882k filas)

| Columna | Tipo | Nota |
|---------|------|------|
| id | VARCHAR(32) PK | MD5(url + fuente) |
| titulo | TEXT | Titular original |
| bajada | TEXT | Bajada/subtítulo (nullable) |
| cuerpo | TEXT | Cuerpo completo (nullable) |
| cuerpo_limpio | TEXT | Cuerpo limpiado por limpiar_texto() |
| fecha | DATE | Fecha de publicación |
| fecha_scraping | DATE | Fecha de captura |
| url | TEXT | URL original |
| fuente | VARCHAR(100) | Código del medio (ej. "biobio") |
| año | INTEGER | Extraído de fecha |

Restricciones: `UNIQUE(url, fuente)` para deduplicación en UPSERT.

### 3.2 Tablas derivadas

| Tabla | Filas (mar 2026) | Descripción |
|-------|-----------------|-------------|
| `titulos_terminos_diarios` | ~6.7M | Frecuencia global por (fecha, termino) |
| `titulos_terminos_por_medio` | grande | Frecuencia por (fecha, fuente, termino) |
| `metricas_titulos_diarias` | ~2,500 | total_noticias + terminos_unicos por día |
| `titulos_coocurrencia` | ~20.6M | Pares (termino_a, termino_b) por (fecha, fuente, n) |
| `titulos_ngramas_diarios` | — | Bigramas/trigramas por (fecha, ngrama, tipo) |
| `titulos_ngramas_por_medio` | — | Ídem por fuente |
| `terminos_semanales` | ~2.16M | Frecuencia normalizada semanal por término |
| `terminos_semanales_fuente` | ~4.71M | Ídem por fuente |
| `terminos_semanales_sentimiento` | 6 | Distribución de sentimiento semanal |
| `noticias_sentimiento` | ~4k | Sentimiento por artículo (id, sentimiento, modelo) |
| `actores_sentimiento` | — | Actor político × fuente × sentimiento × fecha |
| `titulos_tfidf_por_medio` | — | TF-IDF por (fecha_semana, fuente, termino, tipo) |
| `cuerpo_terminos_diarios` | — | Términos del cuerpo por fecha |
| `cuerpo_ngramas_diarios` | — | N-gramas del cuerpo por fecha |

### 3.3 Vistas SQL

Creadas al inicio del análisis de n-gramas y al arrancar el dashboard:

```sql
-- Unigramas + n-gramas en una sola vista consultable
CREATE OR REPLACE VIEW terminos_unificados_diarios AS
  SELECT termino, fecha, frecuencia FROM titulos_terminos_diarios
  UNION ALL
  SELECT ngrama AS termino, fecha, n AS frecuencia FROM titulos_ngramas_diarios;

CREATE OR REPLACE VIEW terminos_unificados_por_medio AS
  SELECT termino, fecha, fuente, frecuencia FROM titulos_terminos_por_medio
  UNION ALL
  SELECT ngrama AS termino, fecha, fuente, n AS frecuencia FROM titulos_ngramas_por_medio;
```

### 3.4 Índices estratégicos

Cada tabla derivada tiene índices sobre `fecha DESC`, `termino/ngrama`, y `fuente` para soportar los filtros del dashboard sin full-scans.

---

## 4. Pipeline (run_pipeline.sh)

### 4.1 Flujo de ejecución

```
run_pipeline.sh
│
├── DETECCIÓN DE FECHAS
│   ├── Parsea .env → credenciales PG
│   ├── SELECT MAX(fecha) FROM noticias  →  FECHA_MAX_DB
│   ├── Calcula días de diferencia hasta hoy
│   └── Exporta: DIAS_SCRAPING, FECHA_DESDE_SCRAPING
│
├── FASE 1: SCRAPING (si no se pasa --solo-analisis)
│   ├── 30 scrapers diarios en orden (más confiables primero)
│   └── 4 scrapers históricos (si se pasa --con-historicos)
│
└── FASE 2: ANÁLISIS
    ├── run_analisis_ngramas.R       (debe ir primero: genera tabla para COMPUESTOS_FRECUENTES)
    ├── run_analisis_titulos.R
    └── run_analisis_coocurrencia.R
```

### 4.2 Modos de ejecución

```bash
./run_pipeline.sh                    # scraping + análisis
./run_pipeline.sh --solo-analisis    # solo análisis (sin scraping)
./run_pipeline.sh --con-historicos   # incluye scrapers históricos
```

### 4.3 Detección de fechas

El pipeline consulta `MAX(fecha)` en la BD antes de scrapear y exporta:
- `DIAS_SCRAPING` — número de días a cubrir (hoy - MAX(fecha))
- `FECHA_DESDE_SCRAPING` — fecha del último registro en BD

Los scrapers leen estas variables vía `dias_a_scrapear()` y `fecha_desde_scraping()` de `funciones.R`. Si psql no está disponible o falla la consulta, usa el default de 3 días.

### 4.4 Logs

Cada ejecución genera `logs/pipeline_YYYY-MM-DD_HH-MM.log` con timestamp en cada línea. Captura stdout + stderr de todos los scripts.

---

## 5. Scripts de Análisis

### 5.1 run_analisis_ngramas.R

**Propósito:** Calcular bigramas (tipo=2) y trigramas (tipo=3) de los titulares.

**Lógica:**
1. Detecta `MAX(fecha)` en `titulos_ngramas_diarios` → modo incremental (borra desde MAX-1 día) o full desde 2018
2. Por batch de 30 días: lee titulares, normaliza (minúsculas, NFC, puntuación y dígitos → espacio)
3. `tidytext::unnest_ngrams()` para n=2 y n=3
4. Filtra: ninguna parte del n-grama es stopword, min 3 chars por token, max 300 chars el n-grama
5. UPSERT en `titulos_ngramas_diarios` y `titulos_ngramas_por_medio`
6. Crea/actualiza vistas `terminos_unificados_*`

**Sin udpipe:** udpipe lematizaba adjetivos al masculino singular ("preventiva"→"preventivo", "roja"→"rojo"), rompiendo concordancia de género. Se eliminó para preservar n-gramas exactos.

**Debe correr antes que run_analisis_titulos.R** porque este último lee `titulos_ngramas_diarios` para auto-detectar COMPUESTOS_FRECUENTES.

---

### 5.2 run_analisis_titulos.R

**Propósito:** Tokenizar titulares y contar frecuencias, con detección de entidades compuestas.

**Lógica:**
1. Detecta `MAX(fecha)` en `titulos_terminos_diarios` → incremental
2. Carga COMPUESTOS_CONOCIDOS (~200 entidades hard-coded) y TOKEN_A_CANONICO (apellidos → nombre completo)
3. Lee `titulos_ngramas_diarios` → construye COMPUESTOS_FRECUENTES con:
   - `desplural()`: "incendios forestales" → "incendio forestal"
   - `es_sufijo_conocido()`: descarta "antonio kast" porque ya existe "jose antonio kast"
4. Construye índice de prefijo `.idx_compuestos` por primera palabra → lookup O(1)
5. Paraleliza con `furrr` (4 workers, chunks de 90 días)
6. Cada worker: tokeniza → fusiona compuestos → aplica TOKEN_A_CANONICO → filtra stopwords → cuenta
7. UPSERT en `titulos_terminos_diarios`, `titulos_terminos_por_medio`, `metricas_titulos_diarias`

**Parámetros clave:**
- `CHUNK_DAYS = 90` — días por chunk paralelo
- `MIN_TERM_LEN = 3` — longitud mínima de token
- `MIN_FREQ_COMPUESTO = 50` — umbral para COMPUESTOS_FRECUENTES (sobre total histórico)
- `USAR_LEMMAS = FALSE` — lematización desactivada

**Compuestos conocidos (muestra):**
- Países: "Estados Unidos", "Reino Unido", "América Latina", "Corea del Sur", ...
- Políticos: "Gabriel Boric", "José Antonio Kast", "Evelyn Matthei", "Sebastián Piñera", "Donald Trump", ...
- Partidos: "Democracia Cristiana", "Renovación Nacional", "Frente Amplio", "Partido Comunista", ...
- Instituciones: "Banco Central", "Corte Suprema", "Fuerzas Armadas", "Derechos Humanos", "Poder Judicial", ...
- Ministerios: "Ministerio del Interior", "Ministerio de Hacienda", "Ministerio de Salud", ... (16 ministerios)
- Deportes: "Colo Colo", "Copa América", "Copa Libertadores", "Universidad de Chile"
- Comunas: "San Antonio", "Santa Cruz", ... (60+ con prefijo San/Santa/Santo)

**TOKEN_A_CANONICO (apellidos → nombre completo):**
"boric"→"Gabriel Boric", "kast"→"José Antonio Kast", "matthei"→"Evelyn Matthei", "trump"→"Donald Trump", "musk"→"Elon Musk", etc. (~30 entradas)

---

### 5.3 run_analisis_coocurrencia.R

**Propósito:** Para cada titular, generar todos los pares de términos que co-aparecen.

**Lógica:**
1. Detecta `MAX(fecha)` en `titulos_coocurrencia` → incremental
2. Paraleliza con `furrr` (4 workers, chunks de 90 días)
3. Cada worker: tokeniza igual que titulos.R → `combn(tokens, 2)` genera todos los pares
4. Orden canónico con SQL `LEAST()/GREATEST()` — no R sort — para evitar discrepancias de collation
5. UPSERT en `titulos_coocurrencia` vía staging table

**Por qué LEAST/GREATEST en SQL y no sort en R:**
R y PostgreSQL pueden ordenar caracteres acentuados distinto según el locale. Si R ordena "á" < "b" pero PG ordena "á" > "b", el mismo par se almacena de ambas formas y viola la PK. Usar LEAST/GREATEST en el INSERT garantiza consistencia con el collation de PG.

**Estado:** ~20.6M filas, 0 auto-pares, 0 violaciones de orden.

---

### 5.4 run_sentimiento.R

**Propósito:** Clasificar cada artículo como positivo/neutral/negativo.

**Modelo primario:** `qwen2.5:3b` vía Ollama (`http://localhost:11434/api/chat`)
- Batch de 30 títulos por llamada, 2 workers paralelos, 3 reintentos por batch fallido

**Fallback:** `syuzhet::get_nrc_sentiment(language="spanish")`
- Activo si Ollama no está disponible
- Sesgo conocido: clasifica terremotos y caídas de mercado como "positivo"

**Input:** título + primeros 5000 chars de `cuerpo_limpio`
**Output:** `noticias_sentimiento` (id, sentimiento, modelo, procesado_en)
**Incremental:** salta artículos que ya tienen sentimiento registrado.

---

### 5.5 run_actores_sentimiento.R

Cross-tabula 30+ actores políticos × fuente × sentimiento por fecha.
Lee `noticias` (titulo) y `noticias_sentimiento`.
Output: `actores_sentimiento`.

---

### 5.6 run_analisis_cuerpo.R

Tokeniza cuerpos completos de artículos (no titulares).
Usa udpipe para lematización + POS tagging (sustantivos, nombres propios, adjetivos, verbos).
Output: `cuerpo_terminos_diarios`, `cuerpo_ngramas_diarios`.
No incluido en el pipeline diario actualmente.

---

### 5.7 comparar_sentimiento.R

Script auxiliar de validación. Compara NRC lexicon vs. LLM en 100 artículos aleatorios.
Útil para calibrar cuándo confiar en NRC vs. exigir LLM.

---

## 6. Sistema de Scraping

### 6.1 Arquitectura

Todos los scrapers siguen el mismo patrón:

```r
library(rvest)      # o httr2/chromote según el sitio
source("funciones.R")

con <- conectar_db()

for (url in urls_a_scrapear) {
  if (ya_scrapeado_en_db(url, con)) next   # deduplicación O(1)
  # ... lógica de scraping ...
  guardar_noticias_en_postgres(df, con)    # UPSERT
}

DBI::dbDisconnect(con)
```

### 6.2 Funciones centrales (funciones.R)

**`conectar_db()`** — Lee credenciales de `.env` en `./`, `../`, `../../`. Prioridad: `PGUSER_NOTICIAS` > `PGUSER`.

**`ya_scrapeado_en_db(url, con)`** — `SELECT 1 FROM noticias WHERE url = $1 LIMIT 1`. Evita re-scrapear.

**`guardar_noticias_en_postgres(df, con)`** — Pipeline completo:
1. Valida columnas requeridas (titulo, fecha, url, fuente)
2. Agrega columnas opcionales si faltan (bajada, cuerpo, fecha_scraping)
3. Genera `id = MD5(url + "_" + fuente)` vía `digest`
4. Limpia cuerpo: `limpiar_texto(cuerpo)` → `cuerpo_limpio`
5. Extrae `año` de fecha
6. Staging table + UPSERT: `INSERT ... ON CONFLICT (url, fuente) DO UPDATE SET ...`

**`dias_a_scrapear(default=3)`** — Lee `DIAS_SCRAPING` del entorno (exportada por el pipeline) o devuelve default.

**`fecha_desde_scraping()`** — Lee `FECHA_DESDE_SCRAPING` como Date o NULL.

**`limpiar_texto(x)`** — Limpieza de cuerpos: elimina código JS/CSS embebido, hashtags, entidades HTML, puntuación, dígitos, palabras basura (lista de ~150 términos técnicos de webs).

**`recodificar_fuentes(data)`** — Mapea códigos internos a nombres display: "biobio"→"Radio BíoBío", "t13"→"T13", "eldesconcierto"→"El Desconcierto", etc.

### 6.3 Inventario de scrapers

| Scraper | Fuente (código BD) | Tecnología | Notas |
|---------|-------------------|-----------|-------|
| scraping_biobio_api.r | biobio | httr2/JSON | API oficial, más confiable |
| scraping_lahora_api.r | lahora | — | **Stub vacío, sin lógica, se salta** |
| scraping_24horas.r | 24horas | rvest | — |
| scraping_adnradio.r | adnradio | rvest | — |
| scraping_agricultura.r | agricultura | rvest | — |
| scraping_biobio.r | biobio | rvest | Complementa la API |
| scraping_chvnoticias.r | chvnoticias | rvest | — |
| scraping_ciper.r | ciper | rvest | Cobertura desde 2007 |
| scraping_cnnchile.r | cnnchile | rvest | — |
| scraping_cooperativa.r | cooperativa | rvest | — |
| scraping_cooperativa_h.r | cooperativa | rvest | Histórico |
| scraping_diariofinanciero.r | diariofinanciero | rvest | — |
| scraping_elciudadano.r | elciudadano | rvest | — |
| scraping_eldesconcierto.R | eldesconcierto | rvest | — |
| scraping_eldinamo.r | eldinamo | rvest | HTML complejo |
| scraping_elmostrador.r | elmostrador | rvest | — |
| scraping_elsiglo.r | elsiglo | rvest | — |
| scraping_emol.R | emol | chromote | Requiere JS rendering |
| scraping_emol_h.R | emol | chromote | Histórico |
| scraping_exante.r | exante | rvest | — |
| scraping_izquierdadiario.R | izquierdadiario | rvest | — |
| scraping_lacuarta.r | lacuarta | rvest | — |
| scraping_lahora.r | lahora | rvest | — |
| scraping_lanacion.r | lanacion | rvest | — |
| scraping_lasegunda.r | lasegunda | rvest | Histórico |
| scraping_latercera.r | latercera | chromote | JS dinámico |
| scraping_meganoticias.r | meganoticias | rvest | — |
| scraping_publimetro.r | publimetro | rvest | — |
| scraping_quintopoder.R | quintopoder | rvest | — |
| scraping_radiouchile.r | radiouchile | rvest | — |
| scraping_redgol.R | redgol | rvest | — |
| scraping_soychile.r | soychile | rvest | Histórico |
| scraping_t13.r | t13 | rvest | — |
| scraping_theclinic.r | theclinic | rvest | — |

**28 fuentes en BD:** 24horas, adnradio, agricultura, biobio, chvnoticias, ciper, cnnchile, cooperativa, diariofinanciero, elciudadano, eldesconcierto, eldinamo, elmostrador, elsiglo, emol, exante, izquierdadiario, lacuarta, lahora, lanacion, latercera, meganoticias, publimetro, quintopoder, radiouchile, redgol, t13, theclinic.

### 6.4 Patrón de staging table para UPSERT masivo

Todos los scripts de análisis usan el mismo patrón para escritura eficiente:

```r
dbExecute(con, "DROP TABLE IF EXISTS _stg_tabla")
dbWriteTable(con, "_stg_tabla", df, temporary = FALSE, overwrite = TRUE)
dbExecute(con, "ALTER TABLE _stg_tabla ALTER COLUMN fecha TYPE DATE USING fecha::date")
dbExecute(con, "
  INSERT INTO tabla (col1, col2, ...)
  SELECT col1, col2, ... FROM _stg_tabla
  ON CONFLICT (pk_cols) DO UPDATE SET col1 = EXCLUDED.col1, ...
")
dbExecute(con, "DROP TABLE IF EXISTS _stg_tabla")
```

Esto evita inserts fila a fila y el límite de parámetros en queries parametrizadas.

---

## 7. Stopwords (stopwords.R)

Fuente única para todos los scripts y el dashboard. Incluye:
- Artículos, pronombres, preposiciones, conjunciones en español
- Auxiliares verbales (ser, estar, haber, tener conjugados)
- Palabras de alta frecuencia sin contenido informativo
- Meses del año
- Números ordinales y cardinales en texto
- Términos periodísticos chilenos sin valor semántico

Todos los scripts hacen `source("stopwords.R")` y consumen el vector `STOPWORDS`.

---

## 8. Dashboard (dashboard/app.R)

### 8.1 Arquitectura

- **Solo lectura.** No ejecuta cálculos; todas las métricas existen como filas en PG.
- Pool de conexiones (`pool::dbPool`) creado una vez al inicio del servidor.
- Vistas `terminos_unificados_*` creadas idempotentemente al arrancar (`CREATE OR REPLACE VIEW`).

### 8.2 Tabs

**Tendencias**
- Evolución temporal de términos seleccionados (line chart Plotly)
- Top 30 términos del período (bar chart)
- Buscador de noticias: filtra `noticias.titulo ILIKE` y muestra titulares con paginación

**Medios** (5 sub-tabs)
- *Conceptos por medio:* heatmap fuente × frecuencia del término elegido; evolución por fuente
- *Términos destacados:* top términos de una fuente en el período, con evolución temporal
- *Volumen de datos:* noticias por fuente, evolución de volumen total
- *Red de palabras:* red de co-ocurrencia (visNetwork): nodos=términos, aristas=co-ocurrencia, tamaño=frecuencia; umbral de co-ocurrencia y máximo de nodos ajustables
- *Tono editorial:* distribución de sentimiento por fuente (pie charts)

**Más información** — Página estática: descripción del proyecto, lista de fuentes, características técnicas.

### 8.3 Controles del sidebar

- Date range picker + presets (7d, 1m, 3m, 1a)
- Stopwords personalizadas: el usuario puede agregar/quitar palabras sin afectar los datos
- Selector de término(s) para análisis
- Selector de fuente
- Umbral mínimo de co-ocurrencia (red de palabras)
- Máximo de nodos en la red

### 8.4 Reactivos clave

- `all_stopwords()` — STOPWORDS base + exclusiones del usuario en la sesión
- `tabla_config()` — metadatos de qué tabla y columnas usar según contexto
- `datos_conceptos_diarios()` — frecuencias globales del término en el período
- `datos_conceptos_por_medio()` — frecuencias por fuente del término
- `red_coocurrencia()` — filtra `titulos_coocurrencia` → prepara nodos + aristas para visNetwork

### 8.5 Detalles y quirks

- No hay distinción unigrama/bigrama/trigrama expuesta; todo aparece unificado vía las vistas.
- La opción TF-IDF fue eliminada; solo se muestra frecuencia absoluta.
- `recodificar_fuentes()` mapea códigos a nombres display: "biobio"→"Radio BíoBío", "t13"→"T13", etc.
- El buscador de noticias usa paginación de 10 resultados por página.

---

## 9. Patrones Críticos y Decisiones de Diseño

### 9.1 Incrementalidad

Cada script de análisis:
1. Consulta `MAX(fecha)` en su tabla de salida
2. Si hay datos: borra filas desde `MAX(fecha) - 1 día` (buffer de seguridad)
3. Procesa solo noticias desde ese punto
4. Si las tablas están vacías: procesa todo desde 2018-01-01

El buffer de -1 día cubre el caso en que el scraper corra a mitad del día y la fecha tenga conteos parciales.

### 9.2 Detección de compuestos frecuentes

`run_analisis_titulos.R` combina dos enfoques:

**Hard-coded (COMPUESTOS_CONOCIDOS):** ~200 entidades curadas manualmente. Siempre activas, no dependen de la BD.

**Auto-detectados (COMPUESTOS_FRECUENTES):** lee `titulos_ngramas_diarios`, agrupa por ngrama sumando todas las fechas, filtra los que superan 50 apariciones históricas totales. Aplica:
- `desplural()`: normaliza plurales antes de comprobar el umbral ("incendios forestales" → "incendio forestal")
- `es_sufijo_conocido()`: descarta sub-frases ya cubiertas ("antonio kast" omitido porque "jose antonio kast" ya existe)

### 9.3 Índice de prefijo para fusión de compuestos

En vez de iterar ~4739 patrones por cada token (O(tokens × n_compounds)):

```r
# Construcción: O(n_compounds), una sola vez antes del paralelo
.idx_compuestos <- list()
for (.nm in names(COMPUESTOS)) {
  for (.p in COMPUESTOS[[.nm]]) {
    .w1 <- .p[1L]
    .idx_compuestos[[.w1]] <- c(.idx_compuestos[[.w1]], list(list(canon=.nm, pat=.p)))
  }
}
# Uso: O(tokens × avg_candidatos_por_primera_palabra) — drásticamente menor
```

### 9.4 LEAST/GREATEST en co-ocurrencia

El orden canónico de pares (termino_a < termino_b) se aplica en SQL, no en R:

```sql
INSERT INTO titulos_coocurrencia (termino_a, termino_b, ...)
SELECT LEAST(termino_a, termino_b), GREATEST(termino_a, termino_b), ...
FROM _staging
```

Motivo: la collation de R y PostgreSQL puede diferir para caracteres acentuados. Aplicar el orden en SQL garantiza consistencia con la PRIMARY KEY de la tabla.

### 9.5 Normalización Unicode

- NFC via `stringi::stri_trans_nfc()` (composición canónica)
- Guiones Unicode (U+2010–U+2015) → espacio en n-gramas; → `-` en titulos (preserva "bío-bío")
- HTML entities (`&amp;`, `&#160;`, `&#NNN;`) → espacio antes de tokenizar

### 9.6 Paralelismo (furrr)

```r
plan(multisession, workers = 4L)
resultados <- future_map(fechas_inicio, procesar_chunk_fecha, ...,
                         .options = furrr_options(seed = TRUE))
plan(sequential)
```

Cada worker es completamente autónomo: abre su propia conexión PG, carga sus propias librerías. No comparte estado. Los resultados se unen en el proceso principal y se escriben en un solo UPSERT.

### 9.7 integer64 desde COUNT(*)

PostgreSQL devuelve `COUNT(*)` como `int8` que R lee como `integer64` (del paquete `bit64`). Al concatenar con `cat()` o `message()` esto falla. Solución: castear en SQL con `::text` o `::bigint` antes de traer a R.

---

## 10. Variables de Entorno y Credenciales

Archivo `.env` en `/Users/matiascarmach/Desktop/Proyectos/Paginaweb/.env`:

```
PGHOST=localhost
PGPORT=5432
PGUSER_NOTICIAS=noticias
PGPASSWORD_NOTICIAS=<password>
PGDATABASE_NOTICIAS=noticias_chile
```

Todos los scripts R leen `.env` en `./`, `../`, `../../` con un parser manual (no usa `dotenv`). Precedencia: `PGUSER_NOTICIAS` > `PGUSER`.

`run_pipeline.sh` también parsea el `.env` en bash para la consulta psql de detección de fechas.

**Binario psql:** `/Library/PostgreSQL/18/bin/psql` (no está en PATH por defecto en macOS).

---

## 11. Estado Actual (marzo 2026)

| Componente | Estado |
|-----------|--------|
| Scraping (28 fuentes) | Operativo. `scraping_lahora_api.r` es stub vacío, se salta automáticamente. |
| run_analisis_ngramas.R | Incremental. Sin udpipe (género preservado en n-gramas). |
| run_analisis_titulos.R | Incremental. Paralelo (4 workers). USAR_LEMMAS=FALSE. Índice de prefijo. |
| run_analisis_coocurrencia.R | Incremental. LEAST/GREATEST. Sin filtro n_global (incompatible con incremental). |
| run_sentimiento.R | Funcional. ~4k artículos procesados. LLM en progreso. |
| Dashboard | Operativo. Sin TF-IDF. Sin distinción uni/bi/trigrama. |
| Carga histórica (parquet) | Completada. `cargar_parquet_a_postgres.py` disponible para recovery. |

**Tabla de filas clave:**
- `noticias`: ~882,000
- `titulos_terminos_diarios`: ~6.7M
- `titulos_coocurrencia`: ~20.6M (0 errores de orden, 0 auto-pares)
- `terminos_semanales`: ~2.16M
- `noticias_sentimiento`: ~4,000

---

## 12. Flujo de Datos Completo

```
Sitios web (28 fuentes)
        │
        ▼  [33 scrapers R — rvest / chromote / httr2]
        │
┌───────────────────────────────────────┐
│          noticias (PostgreSQL)        │  ← fuente única de verdad
│  ~882k filas   PK: UNIQUE(url,fuente) │
└───────────────────────────────────────┘
        │
        ├──▶ run_analisis_ngramas.R
        │         ├──▶ titulos_ngramas_diarios
        │         └──▶ titulos_ngramas_por_medio
        │
        ├──▶ run_analisis_titulos.R  (lee ngramas para COMPUESTOS_FRECUENTES)
        │         ├──▶ titulos_terminos_diarios
        │         ├──▶ titulos_terminos_por_medio
        │         └──▶ metricas_titulos_diarias
        │
        ├──▶ run_analisis_coocurrencia.R
        │         └──▶ titulos_coocurrencia
        │
        └──▶ run_sentimiento.R
                  └──▶ noticias_sentimiento
                            └──▶ run_actores_sentimiento.R
                                      └──▶ actores_sentimiento

Vistas SQL (creadas por ngramas + dashboard al arrancar):
  titulos_terminos_diarios   ──┐
  titulos_ngramas_diarios    ──┴──▶ terminos_unificados_diarios
  titulos_terminos_por_medio ──┐
  titulos_ngramas_por_medio  ──┴──▶ terminos_unificados_por_medio

Todas las tablas derivadas
        │
        ▼  [dashboard/app.R — solo lectura, pool de conexiones]
        │
   Usuario final (navegador)
```
