-- Vaciar SOLO las tablas derivadas (análisis) de noticias_chile para forzar reprocesamiento.
-- NO toca la tabla noticias (datos scrapeados).
-- Uso (desde raíz): /Library/PostgreSQL/18/bin/psql -U noticias -d noticias_chile -h localhost -f noticias/vaciar_db.sql

-- Tablas derivadas de sentimiento
TRUNCATE TABLE actores_sentimiento;
TRUNCATE TABLE terminos_semanales_sentimiento;
-- Tablas de agregación semanal
TRUNCATE TABLE terminos_semanales_fuente;
TRUNCATE TABLE terminos_semanales;
-- Co-ocurrencias
TRUNCATE TABLE titulos_coocurrencia;
-- Sentimiento por artículo
TRUNCATE TABLE noticias_sentimiento;
-- TF-IDF
TRUNCATE TABLE titulos_tfidf_por_medio;
-- N-gramas de títulos
TRUNCATE TABLE titulos_ngramas_por_medio;
TRUNCATE TABLE titulos_ngramas_diarios;
-- Análisis de titulares (unigramas)
TRUNCATE TABLE titulos_terminos_por_medio;
TRUNCATE TABLE metricas_titulos_diarias;
TRUNCATE TABLE titulos_terminos_diarios;
