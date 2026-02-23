# Análisis de noticias (Chile)

Proyecto para recolectar noticias de medios chilenos, analizar titulares (tokenización, frecuencias por fecha) y visualizar resultados en un dashboard Shiny. La base de datos es PostgreSQL; las credenciales se configuran por **variables de entorno** (nunca en el código).

## Estructura del proyecto

```
noticias/
├── README.md                 ← estás aquí
├── README_SCRAPING.md        ← scrapeo y recarga completa (orden de pasos)
├── PLAN_DETALLADO.md         ← arquitectura, tablas, criterios del análisis
├── .env.example              ← plantilla de variables (copiar a .env, no subir .env)
├── vaciar_db.sql             ← vacía tablas para recarga
├── schema_agregados.sql      ← creación de tablas de agregados
├── run_scraping_datamedios.R ← scrapeo con paquete datamedios → PostgreSQL
├── run_analisis_titulos.R    ← análisis de titulares → tablas de términos y métricas
└── dashboard/                ← app Shiny
    ├── app.R
    └── README.md
```

## Requisitos

- **R** (con paquetes: `datamedios`, `DBI`, `RPostgres`, `lubridate` para scraping/análisis; `shiny`, `pool`, `ggplot2`, `dplyr` para el dashboard).
- **PostgreSQL**: base de datos `noticias_chile`, usuario `noticias`, tabla `noticias` y tablas de agregados (ver `schema_agregados.sql` y `PLAN_DETALLADO.md`).

Las contraseñas y datos sensibles **no** van en el repositorio. Usa variables de entorno (o un archivo `.env` que no se suba). Ver `.env.example`.

## Uso rápido

1. **Configurar conexión a la BD**  
   Exportar variables (o usar `.env`):
   - `PGUSER` (ej. `noticias`)
   - `PGPASSWORD`
   - `PGHOST` (ej. `localhost`)
   - `PGPORT` (ej. `5432`)
   - `PGDATABASE` (ej. `noticias_chile`)

2. **Scrapeo y análisis**  
   Ver [README_SCRAPING.md](README_SCRAPING.md): vaciar BD, ejecutar `run_scraping_datamedios.R`, luego `run_analisis_titulos.R`.

3. **Dashboard Shiny**  
   Ver [dashboard/README.md](dashboard/README.md): ejecutar la app desde esta carpeta (`noticias`) en el puerto 3838.

## Documentación

- **Flujo de datos y pasos concretos:** [README_SCRAPING.md](README_SCRAPING.md)
- **Dashboard (requisitos, variables, ejecución):** [dashboard/README.md](dashboard/README.md)
- **Diseño y arquitectura (tablas, script de análisis, criterios):** [PLAN_DETALLADO.md](PLAN_DETALLADO.md)

## Licencia

A definir (por ejemplo MIT). Los datos de noticias dependen de los términos de uso de las fuentes (datamedios, medios de prensa).
