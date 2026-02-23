-- Vacía el contenido de las tablas sin eliminarlas.
-- Ejecutar en la base noticias_chile antes del scrapeo.
-- Uso: psql -U noticias -d noticias_chile -f vaciar_db.sql

TRUNCATE TABLE titulos_terminos_diarios;
TRUNCATE TABLE metricas_titulos_diarias;
TRUNCATE TABLE noticias RESTART IDENTITY CASCADE;

-- Opcional: si tienes scraping_logs y quieres vaciarla también, descomenta:
-- TRUNCATE TABLE scraping_logs;
