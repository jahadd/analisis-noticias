# Análisis de noticias (Chile)

Recolección de noticias de medios chilenos ([datamedios](https://cran.r-project.org/web/packages/datamedios/)), análisis de titulares (tokenización, frecuencias por fecha) y dashboard Shiny. Base de datos: PostgreSQL. Credenciales por variables de entorno (nunca en el código).

## Estructura

```
├── README.md
├── .env.example          # Copiar a .env y rellenar (no subir .env)
├── schema_agregados.sql  # Crear tablas de agregados (una vez)
├── vaciar_db.sql         # Vaciar tablas (opcional, para recarga total)
├── run_scraping_datamedios.R
├── run_analisis_titulos.R
├── dashboard/
│   └── app.R
└── docs/
    └── ARCHITECTURE.md    # Diseño, tablas, criterios
```

## Requisitos

- **R**: paquetes `datamedios`, `DBI`, `RPostgres`, `lubridate` (scraping y análisis); `shiny`, `pool`, `ggplot2`, `dplyr` (dashboard).
- **PostgreSQL**: base `noticias_chile`, usuario `noticias`, tabla `noticias` (creada por tu esquema o por datamedios). Tablas de agregados con `schema_agregados.sql`.

Instalación R (si falta):

```r
install.packages(c("datamedios", "DBI", "RPostgres", "lubridate", "shiny", "pool", "ggplot2", "dplyr"))
```

## Configuración

Variables de entorno (o archivo `.env` que no se sube al repo):

| Variable    | Ejemplo          | Descripción      |
|------------|------------------|------------------|
| `PGUSER`   | noticias         | Usuario PostgreSQL |
| `PGPASSWORD` | (tu contraseña) | Obligatorio      |
| `PGHOST`   | localhost        | Servidor         |
| `PGPORT`   | 5432             | Puerto           |
| `PGDATABASE` | noticias_chile | Base de datos    |

Ejemplo en bash antes de ejecutar scripts o el dashboard:

```bash
export PGPASSWORD='tu_password'
export PGUSER=noticias
export PGDATABASE=noticias_chile
```

## Uso

Todo desde la **raíz del repositorio**.

### 1. Crear tablas de agregados (una vez)

```bash
psql -U noticias -d noticias_chile -h localhost -f schema_agregados.sql
```

(Con contraseña en variable: `export PGPASSWORD='...'` y usar `-h localhost` si aplica.)

### 2. Pipeline de datos (scraping + análisis)

**Vaciar tablas** (solo si quieres recarga desde cero):

```bash
psql -U noticias -d noticias_chile -h localhost -f vaciar_db.sql
```

**Scraping** (recorre meses 2015–2026, fuentes del paquete datamedios; puede tardar mucho):

```bash
Rscript run_scraping_datamedios.R
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

## Documentación adicional

- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) — objetivos, esquema de BD, diseño del script de análisis y del dashboard, integración opcional con proxy.

## Licencia

MIT. Ver [LICENSE](LICENSE). Los datos de noticias dependen de los términos de uso de las fuentes (datamedios, medios de prensa).
