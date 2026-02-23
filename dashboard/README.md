# Dashboard de noticias (Shiny)

## Requisitos R

```r
install.packages(c("shiny", "pool", "DBI", "RPostgres", "ggplot2", "dplyr"))
```

## Variables de entorno

- `PGUSER` (default: noticias)
- `PGPASSWORD` (obligatorio)
- `PGHOST` (default: localhost)
- `PGPORT` (default: 5432)
- `PGDATABASE` (default: noticias_chile)

## Cómo ejecutar

Desde la **raíz de este repositorio** (carpeta `noticias`):

```bash
export PGPASSWORD='tu_password'
Rscript -e "shiny::runApp('dashboard', port = 3838, host = '0.0.0.0')"
```

O desde R (con el directorio de trabajo en la raíz del repo):

```r
Sys.setenv(PGPASSWORD = "tu_password")
shiny::runApp("dashboard", port = 3838)
```

Abrir en el navegador: http://localhost:3838

## Integración con Nginx y BabelOS

- **Nginx** hace proxy de `/shiny/` → `http://127.0.0.1:3838/` (ver `nginx.conf` en la raíz del proyecto). Las peticiones a `/shiny/` llegan al proceso Shiny; se incluyen cabeceras de proxy y soporte para WebSockets.
- **BabelOS**: en el escritorio hay un icono "Noticias" que abre una ventana con un iframe a `/shiny/`. Así el dashboard se ve como una app más del escritorio.

**Probar la integración:**

1. Arrancar Shiny en el puerto 3838 (comando anterior).
2. Arrancar Nginx + Waitress: desde la raíz del proyecto, `./start.sh`.
3. Abrir http://localhost (o tu dominio) y hacer doble clic en el icono "Noticias".
