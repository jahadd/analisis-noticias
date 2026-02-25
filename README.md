# Análisis de noticias (Chile)

Pipeline en R y PostgreSQL para **recolectar** noticias de medios chilenos (paquete [datamedios](https://cran.r-project.org/web/packages/datamedios/)) — BioBioChile, Emol y el resto de fuentes que expone el paquete —, **analizar** titulares (tokenización y frecuencias por fecha) y **visualizar** resultados en un dashboard Shiny. Incluye schema SQL replicable y documentación de uso. Las credenciales de base de datos se configuran por variables de entorno (nunca en el código).
## Estructura

```
├── README.md
├── .env.example          # Copiar a .env y rellenar (no subir .env)
├── schema.sql            # Schema completo de la BD (replicar desde cero)
├── vaciar_db.sql         # Vaciar tablas (opcional, para recarga total)
├── run_scraping_datamedios.R
├── run_analisis_titulos.R
├── dashboard/
│   ├── app.R
│   └── www/              # Imágenes estáticas (p. ej. para insights)
└── docs/
    └── ARCHITECTURE.md    # Diseño, tablas, criterios
```

## Requisitos

- **R**: paquetes `datamedios`, `DBI`, `RPostgres`, `lubridate` (scraping y análisis); `shiny`, `pool`, `ggplot2`, `dplyr`, `plotly` (dashboard).
- **PostgreSQL**: base `noticias_chile`, usuario `noticias`. Ejecutar `schema.sql` una vez para crear todas las tablas (noticias + agregados).

Instalación R (si falta):

```r
install.packages(c("datamedios", "DBI", "RPostgres", "lubridate", "shiny", "pool", "ggplot2", "dplyr", "plotly"))
```

## Configuración

Variables de entorno (o archivo `.env` en la **raíz del proyecto**, que no se sube al repo):

| Variable    | Ejemplo          | Descripción      |
|------------|------------------|------------------|
| `PGUSER`   | noticias         | Usuario PostgreSQL |
| `PGPASSWORD` | (tu contraseña) | Obligatorio      |
| `PGHOST`   | localhost        | Servidor         |
| `PGPORT`   | 5432             | Puerto           |
| `PGDATABASE` | noticias_chile | Base de datos    |

Si usas `.env`, colócalo en la raíz del repositorio (p. ej. `Paginaweb/.env`) con líneas como `PGPASSWORD=tu_password`. El script de análisis y el dashboard leen desde ahí si la variable no está exportada en el shell.

Ejemplo en bash (alternativa a `.env`):

```bash
export PGPASSWORD='tu_password'
export PGUSER=noticias
export PGDATABASE=noticias_chile
```

## Uso

Todo desde la **raíz del repositorio**.

### 1. Crear la base de datos (una vez)

Crear la base `noticias_chile` y el usuario `noticias` en PostgreSQL (fuera de este repo). Luego aplicar el schema completo:

```bash
psql -U noticias -d noticias_chile -h localhost -f schema.sql
```

(Con contraseña en variable: `export PGPASSWORD='...'` y usar `-h localhost` si aplica.)

### 2. Pipeline de datos (scraping + análisis)

**Vaciar tablas** (solo si quieres recarga desde cero):

```bash
psql -U noticias -d noticias_chile -h localhost -f vaciar_db.sql
```

**Scraping** (recorre **año a año** 2015–2026 con datamedios; menos llamadas que mes a mes, mejor cobertura y más rápido):

```bash
Rscript run_scraping_datamedios.R
```

**Actualización diaria** (solo noticias del día; útil para cron/automatización):

```bash
Rscript run_scraping_datamedios.R hoy
```

**Análisis de titulares** (tokenización y escritura en tablas de términos y métricas):

```bash
Rscript run_analisis_titulos.R
```

### 3. Dashboard Shiny

```bash
Rscript -e "shiny::runApp('dashboard', port = 3838, host = '0.0.0.0')"
```

Abrir en el navegador: **http://localhost:3838**

El dashboard incluye: pestaña **Conceptos** (evolución de términos en el tiempo, top 30 términos, buscador de frecuencia por término, últimas noticias con búsqueda y paginación), pestaña **Volumen de noticias** (gráfico de área apilada por medio, distribución por medio) y sección **Insights** (hallazgos editables, p. ej. "Volumen de datos" con cifras, tendencias y limitaciones metodológicas del scraping con datamedios). Los gráficos de términos excluyen la misma lista de stopwords que el análisis (`run_analisis_titulos.R`): artículos, preposiciones y verbos típicos de titulares (p. ej. *anuncia*, *confirma*, *revela*, *informa*, *asegura*, *destaca*, *señala*, *indica*, *dice*, *declara*). Los datos se recargan desde la BD en cada consulta (sin caché) para reflejar las actualizaciones del pipeline.

## Documentación adicional

- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) — objetivos, esquema de BD, diseño del script de análisis y del dashboard, integración opcional con proxy.

## Licencia

MIT. Ver [LICENSE](LICENSE). Los datos de noticias dependen de los términos de uso de las fuentes (datamedios, medios de prensa).
