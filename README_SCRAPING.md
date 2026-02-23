# Scrapeo con datamedios y recarga completa

Orden recomendado para vaciar la BD, scrapear con [datamedios](https://cran.r-project.org/web/packages/datamedios/datamedios.pdf) y regenerar los agregados.

## 1. Vaciar la base de datos

Se mantienen todas las tablas; solo se borra el contenido.

```bash
cd noticias
psql -U noticias -d noticias_chile -f vaciar_db.sql
```

O con contraseña en variable:

```bash
export PGPASSWORD='tu_password'
psql -U noticias -d noticias_chile -h localhost -f vaciar_db.sql
```

## 2. Instalar dependencias R (si faltan)

```r
install.packages(c("datamedios", "DBI", "RPostgres", "lubridate"))
```

## 3. Ejecutar el scrapeo

El script recorre **cada mes desde 2015-01-01 hasta 2026-12-31**, con **fuentes = "todas"** (BioBio + Emol y fuentes que permita el paquete). La frase de búsqueda es obligatoria en datamedios; se usa `"chile"` para maximizar cobertura de medios chilenos.

```bash
export PGPASSWORD='tu_password'
export PGUSER=noticias
export PGDATABASE=noticias_chile
Rscript run_scraping_datamedios.R
```

Puede tardar mucho (muchos meses × varias fuentes). Los resultados se insertan en la tabla `noticias`; si una URL ya existe, se actualiza la fila.

## 4. Ejecutar el análisis de titulares

Tras el scrapeo, regenerar las tablas de términos y métricas:

```bash
Rscript run_analisis_titulos.R
```

## Resumen de archivos

| Archivo | Uso |
|---------|-----|
| `vaciar_db.sql` | Vacía `noticias`, `titulos_terminos_diarios`, `metricas_titulos_diarias`. |
| `run_scraping_datamedios.R` | Scrapeo por meses 2015–2026, todas las fuentes, inserción en PostgreSQL. |
| `run_analisis_titulos.R` | Lee `noticias`, tokeniza titulares, escribe tablas de agregados. |
