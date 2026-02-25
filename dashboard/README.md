# Dashboard Shiny

Aplicación Shiny que consulta las tablas de agregados y `noticias` (solo lectura). Requiere las mismas variables de entorno que el resto del proyecto (`PGUSER`, `PGPASSWORD`, `PGHOST`, `PGPORT`, `PGDATABASE`). Dependencias R: `shiny`, `plotly`, `pool`, `DBI`, `RPostgres`, `ggplot2`, `dplyr`.

Incluye pestañas **Conceptos** (términos, evolución, top 30, buscador, últimas noticias), **Volumen de noticias** (gráficos por medio) y sección **Insights** (artículos editables con texto e imágenes; las imágenes se colocan en `www/` y se referencian por nombre en `app.R`).

**Ejecutar desde la raíz del repositorio (carpeta `noticias`):**

```bash
export PGPASSWORD='tu_password'
Rscript -e "shiny::runApp('dashboard', port = 3838, host = '0.0.0.0')"
```

Abrir http://localhost:3838

Para integración detrás de Nginx u otro proxy, ver [docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md).
