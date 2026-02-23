-- Schema completo de la base de datos noticias_chile.
-- Permite replicar la base desde cero. Ejecutar en una base vacía como usuario noticias.
-- Uso: psql -U noticias -d noticias_chile -h localhost -f schema.sql
--
-- Orden: 1) Crear BD y usuario (fuera de este script), 2) Ejecutar este schema, 3) Scraping y análisis.

-- ---------------------------------------------------------------------------
-- 1. Tabla principal: noticias (scraping con datamedios)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS noticias (
    id              SERIAL PRIMARY KEY,
    titulo           TEXT NOT NULL,
    contenido        TEXT,
    contenido_limpio  TEXT,
    url              TEXT NOT NULL UNIQUE,
    url_imagen       TEXT,
    autor            TEXT,
    fecha            DATE NOT NULL,
    resumen          TEXT,
    search_query     TEXT,
    medio            TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_noticias_fecha ON noticias(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_noticias_fecha_id ON noticias(fecha, id);
CREATE INDEX IF NOT EXISTS idx_noticias_url ON noticias(url);

COMMENT ON TABLE noticias IS 'Noticias recolectadas con datamedios; url es única por ON CONFLICT en el scraping.';

-- ---------------------------------------------------------------------------
-- 2. Tabla: términos por fecha (desde titulares; script run_analisis_titulos.R)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_terminos_diarios (
    fecha      DATE NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER NOT NULL,
    CONSTRAINT pk_titulos_terminos_diarios PRIMARY KEY (fecha, termino)
);

CREATE INDEX IF NOT EXISTS idx_titulos_terminos_fecha ON titulos_terminos_diarios(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_termino ON titulos_terminos_diarios(termino);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_fecha_termino ON titulos_terminos_diarios(fecha, termino);

COMMENT ON TABLE titulos_terminos_diarios IS 'Frecuencia de cada término por día; origen: columna titulo de noticias.';

-- ---------------------------------------------------------------------------
-- 3. Tabla: métricas diarias globales
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS metricas_titulos_diarias (
    fecha          DATE NOT NULL PRIMARY KEY,
    total_noticias INTEGER NOT NULL,
    terminos_unicos INTEGER NOT NULL,
    actualizado_en TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_metricas_titulos_fecha ON metricas_titulos_diarias(fecha DESC);

COMMENT ON TABLE metricas_titulos_diarias IS 'Una fila por día: total noticias procesadas y términos únicos ese día.';
