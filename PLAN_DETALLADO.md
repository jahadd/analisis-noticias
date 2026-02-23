# Plan detallado: capa de análisis y dashboard de noticias

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
        | proxy
        |
[Nginx: /shiny/ → localhost:3838]
        ↑
        |
[Usuario: BabelOS iframe /shiny/]
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
- **Uso en dashboard:** top términos por período, evolución de términos clave, “términos en alza/baja” (comparando dos períodos).

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

- **Carpeta:** `noticias/` (junto a este plan).
- **Archivo principal:** p. ej. `run_analisis_titulos.R` o `analisis_titulos.R`.
- **Dependencias R:** `DBI`, `RPostgres` (o `RPostgreSQL`), `stringi` o `stringr` para normalización. No usar paquetes de ML/NLP pesados para este paso.

### 4.2 Conexión a PostgreSQL

- Leer credenciales desde variables de entorno: `PGUSER`, `PGPASSWORD`, `PGHOST` (default `localhost`), `PGPORT` (default `5432`), `PGDATABASE`.
- Ejemplo de nombres: usuario `noticias`, base `noticias_chile`. No dejar contraseña en el script; usar `Sys.getenv("PGPASSWORD")` y documentar en un README o `.env.example`.

### 4.3 Lectura de datos

- Consultar **solo** `titulo` y `fecha` de `noticias`.
- Leer por **lotes** (p. ej. por rango de fechas o por bloques de `id`) para no cargar 60k filas en memoria de una vez. Por ejemplo: 10.000 filas por lote, o por año/mes.
- Si el script corre incremental: guardar la última `fecha` (o último `id`) procesada y en la siguiente ejecución leer solo `WHERE fecha > última_fecha` (o `id > último_id`). Para la primera ejecución, procesar todo el histórico.

### 4.4 Tokenización simple

- Por cada título: pasar a minúsculas (locale neutro o español).
- Opcional: normalizar acentos (NFC o quitar acentos) para no duplicar “tema” y “téma”.
- Split por espacios y puntuación (regex: `[^a-z0-9ñáéíóúü]+` o equivalente).
- Filtrar: longitud mínima (ej. 2 caracteres), lista fija de stopwords (archivo `stopwords_es.txt` o vector en el script: artículos, preposiciones, “que”, “los”, “las”, “del”, “una”, etc.).
- No stemmear ni lematizar para mantener el pipeline simple y auditable.

### 4.5 Agregación y escritura

- Para cada lote: construir un data.frame con columnas `fecha`, `termino`, `frecuencia` (conteo por (fecha, término) dentro del lote).
- Acumular por (fecha, termino) si se procesan varios lotes (sumar frecuencias).
- Al final (o por cada día procesado): **UPSERT** en `titulos_terminos_diarios` (INSERT ... ON CONFLICT (fecha, termino) DO UPDATE SET frecuencia = EXCLUDED.frecuencia o equivalente).
- Calcular y escribir `metricas_titulos_diarias`: por cada fecha, `total_noticias` = COUNT(*), `terminos_unicos` = COUNT(DISTINCT termino) en el resultado del día; UPSERT por `fecha`.

### 4.6 Orden de ejecución recomendado

1. Conectar a la base.
2. Crear tablas si no existen (script SQL separado o sentencias DDL al inicio del script, con `IF NOT EXISTS`).
3. Determinar rango a procesar (todo o incremental).
4. Leer `noticias` por lotes (solo `titulo`, `fecha`).
5. Tokenizar y contar por (fecha, término).
6. Escribir/actualizar `titulos_terminos_diarios` y `metricas_titulos_diarias`.
7. Cerrar conexión.
8. Log breve (ej. fechas procesadas, filas leídas, términos únicos escritos) a consola o archivo de log.

### 4.7 Programación (cron)

- Ejecutar el script con cron (ej. diario a las 02:00). O tras un job de scraping que inserte en `noticias`, ejecutar el script de análisis a continuación.
- Variables de entorno: configurarlas en el entorno del cron o en un script wrapper que las exporte y llame a R.

---

## 5. Dashboard R Shiny

### 5.1 Objetivo de la interfaz

- **Protagonista: etiquetas (términos).** La interfaz responde sobre todo: “¿Qué términos dominan en los titulares?”, “¿Cómo evoluciona cada término en el tiempo?” y “¿Cuál es el término más frecuente?”. El volumen de noticias es contexto secundario.
- Sin sobrecarga: prioridad visual a términos; volumen en tarjetas pequeñas o una línea.

### 5.2 Variables del dashboard (qué se muestra y de dónde sale)

| Prioridad | Variable / concepto | Origen en BD | Uso en UI |
|-----------|---------------------|--------------|-----------|
| **Alta** | Términos distintos en el período | `titulos_terminos_diarios`: COUNT(DISTINCT termino) WHERE fecha BETWEEN ? AND ? | Tarjeta principal |
| **Alta** | Término más frecuente del período (nombre + frecuencia) | `titulos_terminos_diarios`: SUM(frecuencia) GROUP BY termino, tomar top 1 | Tarjeta principal |
| **Alta** | Evolución de 2–4 términos en el tiempo | `titulos_terminos_diarios`: fecha, termino, frecuencia WHERE termino IN (...) AND fecha BETWEEN ? AND ? | Gráfico de líneas (principal) |
| **Alta** | Top 10–15 términos del período | `titulos_terminos_diarios`: SUM(frecuencia) GROUP BY termino, ORDER BY SUM DESC LIMIT 15 | Gráfico de barras horizontales |
| **Media** | Últimas noticias (título, fecha, medio, enlace) | `noticias`: titulo, fecha, medio, url WHERE fecha BETWEEN ? ORDER BY fecha DESC LIMIT 50 | Tabla de detalle |
| **Media** | Términos en alza (opcional) | `titulos_terminos_diarios`: comparar dos períodos (ej. esta semana vs anterior) | Lista o mini-gráfico |
| **Baja** | Total noticias en el período | `metricas_titulos_diarias`: SUM(total_noticias) WHERE fecha BETWEEN ? AND ? | Tarjeta pequeña o texto |
| **Baja** | Promedio noticias por día | `metricas_titulos_diarias`: suma / COUNT(días) en el rango | Tarjeta pequeña o texto |

Todas las consultas dependen del **filtro de fechas** (date range) que el usuario elija.

### 5.3 Conexión a la base

- Usar `pool` + `DBI` + `RPostgres` para conexión eficiente. Crear el pool al arrancar la app (una vez por proceso).
- Credenciales desde variables de entorno (mismo esquema que el script de análisis).

### 5.4 Fuentes de datos (solo lectura)

- **Tarjetas de términos:** `titulos_terminos_diarios` con `WHERE fecha BETWEEN ? AND ?` para COUNT(DISTINCT termino), y para SUM(frecuencia) GROUP BY termino (término más frecuente, top 15).
- **Evolución temporal:** `titulos_terminos_diarios` con `WHERE termino IN (...) AND fecha BETWEEN ? AND ?`, ordenado por fecha.
- **Volumen (secundario):** `metricas_titulos_diarias` con `WHERE fecha BETWEEN ? AND ?` para SUM(total_noticias) y promedio.
- **Tabla de detalle:** `noticias` con `SELECT id, titulo, fecha, medio, url WHERE fecha BETWEEN ? AND ? ORDER BY fecha DESC LIMIT 50`.

### 5.5 Filtro de fechas

- Un único control (date range input). Todas las consultas reciben ese rango. Presets (“Últimos 7 días”, “Último mes”, “Último trimestre”) y tope máximo (ej. 365 días).

### 5.6 Elementos de la UI (orden de protagonismo)

- **Tarjetas principales (etiquetas):** términos distintos en el período; término más frecuente (nombre + número). Opcional: “términos en alza” (comparando dos períodos).
- **Gráfico principal:** evolución de 2–4 términos en el tiempo (líneas; datos desde `titulos_terminos_diarios`). Selector de términos o lista fija.
- **Segundo gráfico:** top 10–15 términos del período (barras horizontales; mismo origen).
- **Tabla:** últimas noticias (titulo, fecha, medio, enlace). Máximo 50 filas; paginación opcional.
- **Tarjetas secundarias (volumen):** total noticias en el período y promedio por día, en tamaño reducido o una sola línea de contexto.

### 5.7 Estética y rendimiento

- Tema coherente (ej. `bslib` o `shinythemes`), tipografía legible, colores sobrios.
- Usar `bindCache()` donde aplique (p. ej. por rango de fechas) para no repetir consultas costosas.
- Mensaje de “Cargando…” en los outputs que consultan la base.

### 5.8 Despliegue del proceso Shiny

- En desarrollo: `shiny::runApp(port = 3838)`.
- En producción: Shiny Server (open source o Pro) o un proceso systemd/supervisor que ejecute la app en el puerto 3838, con variables de entorno configuradas para la conexión a PostgreSQL.

---

## 6. Integración con Nginx

### 6.1 Objetivo

- Que el dashboard sea accesible como “una app más” dentro del sitio (BabelOS), en la misma origen, sin exponer el puerto 3838 al exterior.

### 6.2 Configuración Nginx

- En el mismo servidor donde corre Nginx (y Flask para BabelOS), añadir una location que haga proxy al proceso Shiny:
  - Ruta: ej. `/shiny/`
  - Proxy pass: `http://127.0.0.1:3838/`
  - Incluir cabeceras habituales de proxy: `Host`, `X-Real-IP`, `X-Forwarded-For`, `X-Forwarded-Proto` (si hay HTTPS), y para WebSockets si Shiny los usa: `Upgrade`, `Connection`.
- Recarga de Nginx tras cambiar la config.

### 6.3 Shiny detrás del proxy

- Shiny debe estar configurado para aceptar conexiones en `127.0.0.1:3838` (o `0.0.0.0:3838` si el proxy está en la misma máquina). Si la app genera enlaces internos, puede ser necesario configurar la base URL (ej. en Shiny Server o con opciones de `runApp`) para que coincida con `/shiny/`.

### 6.4 BabelOS (Flask)

- Añadir una “app” (ventana) en el escritorio cuyo contenido sea un iframe con `src="/shiny/"`. No es necesario crear documentos nuevos en este plan; es el paso ya discutido de integración.

---

## 7. Orden de implementación sugerido

1. **Carpeta y plan** (este documento) — hecho.
2. **SQL:** script de creación de tablas `titulos_terminos_diarios` y `metricas_titulos_diarias` (y comprobación de índices en `noticias`). Ejecutar en `noticias_chile`.
3. **R – Script de análisis:** conexión, lectura por lotes, tokenización, escritura en tablas. Probar con un subconjunto pequeño de fechas.
4. **Ejecución completa** del script sobre todo el histórico (o el rango deseado) y verificación de datos en las tablas.
5. **Cron o scheduler** para ejecutar el script periódicamente.
6. **Dashboard Shiny:** estructura de la app, conexión con pool, consultas descritas arriba, UI (KPIs, gráficos, tabla, filtro de fechas). Probar en local (puerto 3838).
7. **Despliegue de Shiny** en el servidor (Shiny Server o proceso equivalente) con variables de entorno.
8. **Nginx:** añadir location `/shiny/` y recargar.
9. **BabelOS:** añadir ventana con iframe a `/shiny/`.
10. **Documentación operativa:** cómo actualizar credenciales, cómo re-ejecutar el análisis, cómo reiniciar Shiny y Nginx.

---

## 8. Resumen de archivos en `noticias/`

- `PLAN_DETALLADO.md` — este plan.
- `run_analisis_titulos.R` (o nombre acordado) — script de análisis que lee `noticias`, tokeniza titulares y escribe en las tablas de agregados.
- `schema_agregados.sql` (opcional) — DDL de `titulos_terminos_diarios` y `metricas_titulos_diarias` para ejecutar una sola vez.
- `stopwords_es.txt` (opcional) — lista de stopwords si se externaliza.
- Carpeta `dashboard/` o archivo `app.R` — aplicación Shiny.
- `.env.example` — ejemplo de variables (PGUSER, PGPASSWORD, PGDATABASE, PGHOST, PGPORT) sin valores reales; indicar que se copie a `.env` y no se suba al repo.

Con esto, la capa de análisis queda definida, pre-cargada y lista para un dashboard profesional y eficiente, con integración clara a Nginx y BabelOS.
