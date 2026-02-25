# Arquitectura: análisis y dashboard de noticias

## 1. Objetivos y criterios

- **Fuente de verdad:** solo la tabla `noticias` de PostgreSQL. No usar tablas previamente procesadas (ya eliminadas).
- **Contenido = titulares:** todo el análisis se basa en la columna `titulo`. No usar `contenido`, `temas` ni otros campos para esta capa.
- **Pre-cargado, nada en tiempo real:** las métricas y agregados se calculan por un script (R) que corre periódicamente y escribe en tablas dedicadas. El dashboard solo lee esas tablas y `noticias` con consultas acotadas.
- **Sin ML:** tokenización simple (split, minúsculas, stopwords fijos). No NER, sentimiento ni temas automáticos.
- **Dashboard profesional:** información clara, precisa, sin sobrecarga visual; pocos KPIs, uno o dos gráficos principales, tabla de detalle acotada.
- **Eficiencia:** índices adecuados, consultas con filtros de fecha y LIMIT; el dashboard no debe cargar 60k filas ni recalcular términos en cada visita.

---

## 2. Arquitectura general

```
[PostgreSQL: noticias]
        ↑
        | lectura (solo titulo, fecha; por lotes)
        |
[Script R de análisis]  ← cron / ejecución manual
        |
        | escritura
        ↓
[PostgreSQL: tablas de agregados]
        ↑
        | solo lectura (consultas con WHERE fecha, LIMIT, GROUP BY)
        |
[Dashboard R Shiny]  (puerto ej. 3838)
        ↑
        | proxy (opcional)
        |
[Nginx: /shiny/ → localhost:3838]
        ↑
        |
[Usuario / iframe /shiny/]
```

---

## 3. Base de datos: tablas a crear

Conexión: **Usuario** `noticias`, **Base de datos** `noticias_chile`. Las credenciales no deben quedar en el código; usar variables de entorno (ej. `PGUSER`, `PGPASSWORD`, `PGDATABASE`) o un archivo de configuración excluido del control de versiones.

### 3.1 Tabla: términos por fecha (desde titulares)

Almacena la frecuencia de cada término por día. Origen: columna `titulo` de `noticias`, tokenización simple.

- **Nombre sugerido:** `titulos_terminos_diarios`
- **Columnas:**
  - `fecha` DATE NOT NULL
  - `termino` VARCHAR(150) NOT NULL
  - `frecuencia` INTEGER NOT NULL
- **Restricción:** UNIQUE(fecha, termino)
- **Índices:** (fecha DESC), (termino), (fecha, termino)
- **Uso en dashboard:** top términos por período, evolución de términos clave, "términos en alza/baja" (comparando dos períodos).

### 3.2 Tabla: métricas diarias globales (opcional pero útil)

Una fila por día: total de noticias, total de títulos procesados, cantidad de términos únicos ese día. Evita que el dashboard tenga que agregar sobre 60k filas.

- **Nombre sugerido:** `metricas_titulos_diarias`
- **Columnas:**
  - `fecha` DATE NOT NULL PRIMARY KEY
  - `total_noticias` INTEGER NOT NULL
  - `terminos_unicos` INTEGER NOT NULL
  - `actualizado_en` TIMESTAMP DEFAULT CURRENT_TIMESTAMP
- **Uso en dashboard:** volumen en segundo plano (total noticias, promedio por día), validación de cobertura.

### 3.3 Índices en `noticias` (si no existen)

Asegurar índices en `noticias` para que el script lea por rango de fechas de forma eficiente:

- `noticias(fecha DESC)` o `noticias(fecha)`
- Opcional: `noticias(fecha, id)` para paginación por id en el script.

---

## 4. Script de análisis en R

### 4.1 Ubicación y entorno

- **Carpeta:** raíz del repositorio.
- **Archivo principal:** `run_analisis_titulos.R`.
- **Dependencias R:** `DBI`, `RPostgres` (o `RPostgreSQL`), `stringi` o `stringr` para normalización. No usar paquetes de ML/NLP pesados para este paso.

### 4.2 Conexión a PostgreSQL

- Leer credenciales desde variables de entorno: `PGUSER`, `PGPASSWORD`, `PGHOST` (default `localhost`), `PGPORT` (default `5432`), `PGDATABASE`.
- No dejar contraseña en el script; usar `Sys.getenv("PGPASSWORD")`. Si no está definida en el entorno, el script intenta leer `PGPASSWORD` desde un archivo `.env` en la raíz del proyecto (directorio de trabajo al ejecutar o carpeta del script).
- Documentar en README y `.env.example`.

### 4.3 Lectura de datos

- Consultar **solo** `titulo` y `fecha` de `noticias`.
- Leer por **lotes** (p. ej. por rango de fechas o por bloques de `id`) para no cargar 60k filas en memoria de una vez.
- Si el script corre incremental: guardar la última `fecha` (o último `id`) procesada y en la siguiente ejecución leer solo `WHERE fecha > última_fecha`. Para la primera ejecución, procesar todo el histórico.

### 4.4 Tokenización simple

- Por cada título: pasar a minúsculas (locale neutro o español).
- Opcional: normalizar acentos (NFC o quitar acentos).
- Split por espacios y puntuación (regex: `[^a-z0-9ñáéíóúü]+` o equivalente).
- Filtrar: longitud mínima (ej. 3 caracteres), lista fija de stopwords (vector en el script). Incluye artículos, preposiciones, pronombres, verbos auxiliares y **verbos típicos de titulares** (p. ej. *anuncia*, *confirma*, *revela*, *informa*, *asegura*, *advierte*, *destaca*, *señala*, *indica*, *reporta*, *denuncia*, *explica*, *afirma*, *sostiene*, *dice*, *declara*, *califica*, *considera*) para que no dominen los rankings.
- No stemmear ni lematizar para mantener el pipeline simple y auditable.

### 4.5 Agregación y escritura

- Para cada lote: construir un data.frame con columnas `fecha`, `termino`, `frecuencia` (conteo por (fecha, término) dentro del lote).
- Acumular por (fecha, termino) si se procesan varios lotes (sumar frecuencias).
- Al final (o por cada día procesado): **UPSERT** en `titulos_terminos_diarios`.
- Calcular y escribir `metricas_titulos_diarias`: por cada fecha, `total_noticias`, `terminos_unicos`; UPSERT por `fecha`.

### 4.6 Orden de ejecución recomendado

1. Conectar a la base.
2. Crear tablas si no existen (script SQL separado o sentencias DDL al inicio del script, con `IF NOT EXISTS`).
3. Determinar rango a procesar (todo o incremental).
4. Leer `noticias` por lotes (solo `titulo`, `fecha`).
5. Tokenizar y contar por (fecha, término).
6. Escribir/actualizar `titulos_terminos_diarios` y `metricas_titulos_diarias`.
7. Cerrar conexión.
8. Log breve a consola o archivo de log.

### 4.7 Programación (cron)

- Ejecutar el script con cron (ej. diario a las 02:00). O tras un job de scraping que inserte en `noticias`, ejecutar el script de análisis a continuación.
- Variables de entorno: configurarlas en el entorno del cron o en un script wrapper que las exporte y llame a R.

---

## 5. Dashboard R Shiny

### 5.1 Objetivo de la interfaz

- **Protagonista: etiquetas (términos).** La interfaz responde: "¿Qué términos dominan en los titulares?", "¿Cómo evoluciona cada término en el tiempo?" y "¿Cuál es el término más frecuente?". El volumen de noticias es contexto secundario.
- Sin sobrecarga: prioridad visual a términos; volumen en tarjetas pequeñas o una línea.

### 5.2 Estructura del dashboard (pestañas y sección Insights)

- **Conceptos:** tarjetas (términos distintos, término más frecuente), texto de volumen, gráfico de evolución de términos en el tiempo (selector top 10), gráfico top 30 términos, buscador de frecuencia por término (muestra todas las coincidencias con ILIKE), últimas noticias con búsqueda en titulares y paginación (5 por página).
- **Volumen de noticias:** gráfico de área apilada por medio (volumen por día/año por medio + línea total), gráfico de distribución por medio (barras).
- **Insights:** sección lateral con artículos editables (título, párrafos, imágenes). Incluye el insight "Volumen de datos" (cifras del período, distribución por medio con totales, tendencias temporales y limitaciones metodológicas de datamedios). Las imágenes estáticas se sirven desde `dashboard/www/`.

En los gráficos y tarjetas de términos se aplica el **mismo filtro de stopwords** que en `run_analisis_titulos.R` (artículos, preposiciones, “primer”, “así”, y verbos de titulares como *anuncia*, *confirma*, *revela*, *informa*, *destaca*, *señala*, *dice*, *declara*, etc.), de modo que no aparezcan en evolución ni en top 30. El buscador de términos no filtra: muestra cualquier coincidencia para consultar frecuencias.

### 5.3 Variables del dashboard (qué se muestra y de dónde sale)

| Prioridad | Variable / concepto | Origen en BD | Uso en UI |
|-----------|---------------------|--------------|-----------|
| **Alta** | Términos distintos en el período | `titulos_terminos_diarios`: COUNT(DISTINCT termino) WHERE fecha BETWEEN ? AND ? AND termino NOT IN (stopwords) | Tarjeta |
| **Alta** | Término más frecuente del período | `titulos_terminos_diarios`: SUM(frecuencia) GROUP BY termino, excl. stopwords, top 1 | Tarjeta |
| **Alta** | Evolución de términos en el tiempo | `titulos_terminos_diarios`: fecha, termino, frecuencia WHERE termino IN (seleccionados) AND fecha BETWEEN ? AND ? | Gráfico de líneas (plot_ly); eje X según rango: ≤7d día a día, ≤31d cada 2 días, ≤2a mensual, >2a anual |
| **Alta** | Top 30 términos del período | `titulos_terminos_diarios`: SUM(frecuencia) GROUP BY termino, excl. stopwords, LIMIT 30 | Gráfico de barras |
| **Alta** | Buscador de frecuencia por término | `titulos_terminos_diarios`: termino, SUM(frecuencia) WHERE termino ILIKE ? AND fecha BETWEEN ? AND ? (sin filtrar stopwords) | Lista término (N) |
| **Media** | Últimas noticias (titular, fecha, medio, enlace) | `noticias`: titulo, fecha, medio, url WHERE fecha BETWEEN ? [y titulo ILIKE ?], paginación 5 por página | Tabla |
| **Media** | Volumen por día y por medio | `noticias`: fecha, medio, COUNT(*) WHERE fecha BETWEEN ? AND ? GROUP BY fecha, medio | Gráfico de área apilada + línea total |
| **Baja** | Total y promedio noticias en el período | `metricas_titulos_diarias`: SUM(total_noticias), COUNT(*) WHERE fecha BETWEEN ? AND ? | Texto bajo tarjetas |
| **Baja** | Distribución por medio | `noticias`: medio, COUNT(*) WHERE fecha BETWEEN ? AND ? GROUP BY medio | Gráfico de barras (pestaña Volumen) |

Todas las consultas dependen del **filtro de fechas** (date range). No se usa caché (`bindCache`): cada recarga o cambio de rango vuelve a consultar la BD para reflejar las actualizaciones del pipeline.

### 5.4 Conexión a la base

- Usar `pool` + `DBI` + `RPostgres` para conexión eficiente. Crear el pool al arrancar la app (una vez por proceso).
- Credenciales desde variables de entorno (mismo esquema que el script de análisis).

### 5.5 Fuentes de datos (solo lectura)

- **Tarjetas de términos:** `titulos_terminos_diarios` con `WHERE fecha BETWEEN ? AND ? AND termino NOT IN (stopwords)` para COUNT(DISTINCT termino) y para SUM(frecuencia) GROUP BY termino (término más frecuente, top 10, top 30).
- **Evolución temporal:** `titulos_terminos_diarios` con `WHERE termino IN (seleccionados) AND fecha BETWEEN ? AND ?`, ordenado por fecha.
- **Buscador de términos:** `titulos_terminos_diarios` con `WHERE termino ILIKE ? AND fecha BETWEEN ? AND ?`, GROUP BY termino (sin filtrar stopwords).
- **Volumen (secundario):** `metricas_titulos_diarias` para total y promedio; `noticias` con GROUP BY fecha, medio para el gráfico de área apilada por medio.
- **Tabla de detalle:** `noticias` con titulo, fecha, medio, url, filtro opcional por titular (ILIKE), paginación 5 por página.

### 5.6 Filtro de fechas

- Un único control (date range input). Todas las consultas reciben ese rango. Presets ("Últimos 7 días", "Último mes", "Último trimestre") y tope máximo (ej. 365 días).

### 5.7 Elementos de la UI (orden de protagonismo)

- **Tarjetas:** términos distintos en el período; término más frecuente (nombre + número).
- **Gráfico de evolución:** líneas por término en el tiempo (selector entre top 10). Eje X adaptado: ≤7 días un tick por día, ≤31 días cada 2 días, >31 días mensual, >2 años anual.
- **Gráfico top 30:** barras horizontales de términos del período (excl. stopwords).
- **Buscador de términos:** campo de texto; lista de coincidencias con frecuencia (término (N)); sin filtrar stopwords.
- **Tabla últimas noticias:** titulo, fecha, medio, enlace; búsqueda por titular; paginación 5 por página.
- **Pestaña Volumen:** gráfico de área apilada por medio (y línea total); gráfico de distribución por medio.
- **Insights:** selector lateral; contenido por insight (títulos, párrafos, imágenes en `www/`), con soporte para varios párrafos e imágenes (incl. disposición lado a lado).

### 5.8 Estética y rendimiento

- Tema coherente, tipografía legible, colores sobrios (azul primario, tarjetas y bordes discretos).
- Gráficos con **plotly** (tooltips, leyenda, ejes adaptados al rango de fechas). Sin caché: las consultas se ejecutan en cada recarga o cambio de filtro para mostrar siempre datos actualizados tras el pipeline.

### 5.9 Despliegue del proceso Shiny

- En desarrollo: `shiny::runApp(port = 3838)`.
- En producción: Shiny Server (open source o Pro) o un proceso systemd/supervisor que ejecute la app en el puerto 3838, con variables de entorno configuradas.

---

## 6. Resumen de archivos en el repositorio

- `README.md` — entrada principal: requisitos, uso, comandos.
- `docs/ARCHITECTURE.md` — esta documentación de diseño.
- `run_analisis_titulos.R` — script de análisis que lee `noticias`, tokeniza titulares y escribe en las tablas de agregados.
- `run_scraping_datamedios.R` — scrapeo con datamedios hacia PostgreSQL (año a año 2015–2026; dedup por URL, ON CONFLICT DO UPDATE). Con argumento `hoy`: solo noticias del día (para uso desde la página/cron).
- `schema.sql` — schema completo de la BD: tabla `noticias`, tablas de agregados e índices (para replicar desde cero).
- `vaciar_db.sql` — vacía tablas para recarga completa.
- `dashboard/app.R` — aplicación Shiny.
- `dashboard/www/` — imágenes estáticas para insights (Shiny las sirve en la raíz).
- `.env.example` — plantilla de variables de entorno (copiar a `.env`, no subir `.env`).
