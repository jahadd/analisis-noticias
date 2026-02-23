-- Esquema de tablas de agregados para el dashboard de noticias.
-- Fuente: solo titulares de la tabla noticias. Ejecutar en la base noticias_chile.
-- Uso: script R de análisis escribe aquí; Shiny solo lee.

-- ---------------------------------------------------------------------------
-- 1. Índices en noticias (para que el script lea por fechas de forma eficiente)
-- ---------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_noticias_fecha ON noticias(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_noticias_fecha_id ON noticias(fecha, id);

-- ---------------------------------------------------------------------------
-- 2. Tabla: términos por fecha (desde titulares)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_terminos_diarios (
    fecha DATE NOT NULL,
    termino VARCHAR(150) NOT NULL,
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
    fecha DATE NOT NULL PRIMARY KEY,
    total_noticias INTEGER NOT NULL,
    terminos_unicos INTEGER NOT NULL,
    actualizado_en TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_metricas_titulos_fecha ON metricas_titulos_diarias(fecha DESC);

COMMENT ON TABLE metricas_titulos_diarias IS 'Una fila por día: total noticias procesadas y términos únicos ese día.';
