-- Schema completo de la base de datos noticias_chile.
-- Permite replicar la base desde cero. Ejecutar en una base vacía como usuario noticias.
-- Uso (desde raíz del proyecto): /Library/PostgreSQL/18/bin/psql -U noticias -d noticias_chile -h localhost -f noticias/schema.sql
-- O: cd noticias && /Library/PostgreSQL/18/bin/psql -U noticias -d noticias_chile -h localhost -f schema.sql
--
-- Orden: 1) Crear BD y usuario (fuera de este script), 2) Ejecutar este schema, 3) Scraping y análisis.

-- ---------------------------------------------------------------------------
-- 1. Tabla principal: noticias (alineada a prensa_datos.parquet)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS noticias (
    id              TEXT PRIMARY KEY,
    titulo          TEXT NOT NULL,
    bajada          TEXT,
    cuerpo          TEXT,
    cuerpo_limpio   TEXT,
    fecha           DATE NOT NULL,
    fecha_scraping  DATE,
    url             TEXT NOT NULL,
    fuente          VARCHAR(100) NOT NULL,
    año             INTEGER,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(url, fuente)
);

CREATE INDEX IF NOT EXISTS idx_noticias_fecha ON noticias(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_noticias_fecha_id ON noticias(fecha, id);
CREATE INDEX IF NOT EXISTS idx_noticias_url ON noticias(url);
CREATE INDEX IF NOT EXISTS idx_noticias_fuente ON noticias(fuente);

COMMENT ON TABLE noticias IS 'Noticias de prensa; estructura desde prensa_datos.parquet; id es hash único, UNIQUE(url, fuente).';

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

-- ---------------------------------------------------------------------------
-- 4. Tabla: términos por fecha y por medio (para pestaña Medios del dashboard)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_terminos_por_medio (
    fecha      DATE NOT NULL,
    fuente     VARCHAR(100) NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER NOT NULL,
    CONSTRAINT pk_titulos_terminos_por_medio PRIMARY KEY (fecha, fuente, termino)
);

CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha ON titulos_terminos_por_medio(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fuente ON titulos_terminos_por_medio(fuente);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha_fuente ON titulos_terminos_por_medio(fecha, fuente);

COMMENT ON TABLE titulos_terminos_por_medio IS 'Frecuencia de cada término por día y por medio (fuente); origen: run_analisis_titulos.R para el gráfico «Términos más repetidos por medio».';

-- ---------------------------------------------------------------------------
-- 5. Tabla: co-ocurrencias diarias de términos en titulares por fuente
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_coocurrencia (
    fecha      DATE NOT NULL,
    fuente     VARCHAR(100) NOT NULL,
    termino_a  VARCHAR(150) NOT NULL,
    termino_b  VARCHAR(150) NOT NULL,
    n          INTEGER NOT NULL,
    CONSTRAINT pk_titulos_coocurrencia PRIMARY KEY (fecha, fuente, termino_a, termino_b)
);

CREATE INDEX IF NOT EXISTS idx_titulos_coocurrencia_fuente_fecha ON titulos_coocurrencia(fuente, fecha DESC);
CREATE INDEX IF NOT EXISTS idx_titulos_coocurrencia_terminos ON titulos_coocurrencia(termino_a, termino_b);

COMMENT ON TABLE titulos_coocurrencia IS 'Co-ocurrencia diaria de pares de términos en titulares por fuente; termino_a < termino_b (orden alfabético); origen: run_analisis_coocurrencia.R.';

-- ---------------------------------------------------------------------------
-- 6. Tabla: sentimiento por artículo
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS noticias_sentimiento (
    id           TEXT PRIMARY KEY,
    sentimiento  VARCHAR(20) NOT NULL,
    modelo       VARCHAR(50),
    procesado_en TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE noticias_sentimiento IS 'Sentimiento por artículo (positivo/neutral/negativo); origen: run_sentimiento.R.';

-- ---------------------------------------------------------------------------
-- 7. Tabla: agregados semanales de términos (global)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS terminos_semanales (
    semana       INTEGER NOT NULL,
    fecha_inicio DATE NOT NULL,
    termino      VARCHAR(150) NOT NULL,
    n            INTEGER NOT NULL,
    p_semana     NUMERIC(10,6),
    CONSTRAINT pk_terminos_semanales PRIMARY KEY (semana, fecha_inicio, termino)
);

CREATE INDEX IF NOT EXISTS idx_terminos_semanales_fecha ON terminos_semanales(fecha_inicio DESC);
CREATE INDEX IF NOT EXISTS idx_terminos_semanales_termino ON terminos_semanales(termino);

COMMENT ON TABLE terminos_semanales IS 'Frecuencia semanal normalizada de términos en titulares (global); origen: run_analisis_semanal.R.';

-- ---------------------------------------------------------------------------
-- 8. Tabla: agregados semanales de términos por fuente
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS terminos_semanales_fuente (
    semana       INTEGER NOT NULL,
    fecha_inicio DATE NOT NULL,
    fuente       VARCHAR(100) NOT NULL,
    termino      VARCHAR(150) NOT NULL,
    n            INTEGER NOT NULL,
    p_semana     NUMERIC(10,6),
    CONSTRAINT pk_terminos_semanales_fuente PRIMARY KEY (semana, fecha_inicio, fuente, termino)
);

CREATE INDEX IF NOT EXISTS idx_terminos_semanales_fuente_fecha ON terminos_semanales_fuente(fecha_inicio DESC);
CREATE INDEX IF NOT EXISTS idx_terminos_semanales_fuente_fuente ON terminos_semanales_fuente(fuente);

COMMENT ON TABLE terminos_semanales_fuente IS 'Frecuencia semanal normalizada de términos en titulares por fuente; origen: run_analisis_semanal.R.';

-- ---------------------------------------------------------------------------
-- 9. Tabla: conteo semanal de artículos por sentimiento
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS terminos_semanales_sentimiento (
    semana       INTEGER NOT NULL,
    fecha_inicio DATE NOT NULL,
    sentimiento  VARCHAR(20) NOT NULL,
    n_noticias   INTEGER NOT NULL,
    CONSTRAINT pk_terminos_semanales_sentimiento PRIMARY KEY (semana, fecha_inicio, sentimiento)
);

COMMENT ON TABLE terminos_semanales_sentimiento IS 'Conteo semanal de artículos por sentimiento; origen: run_analisis_semanal.R.';

-- ---------------------------------------------------------------------------
-- 10. Tabla: sentimiento por actor político, fuente y fecha
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS actores_sentimiento (
    fecha       DATE          NOT NULL,
    fuente      VARCHAR(100)  NOT NULL,
    actor       VARCHAR(150)  NOT NULL,
    sentimiento VARCHAR(20)   NOT NULL,
    n           INTEGER       NOT NULL,
    CONSTRAINT pk_actores_sentimiento PRIMARY KEY (fecha, fuente, actor, sentimiento)
);
CREATE INDEX IF NOT EXISTS idx_actores_sentimiento_fecha   ON actores_sentimiento(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_actores_sentimiento_fuente  ON actores_sentimiento(fuente);
CREATE INDEX IF NOT EXISTS idx_actores_sentimiento_actor   ON actores_sentimiento(actor);

COMMENT ON TABLE actores_sentimiento IS 'Sentimiento de artículos que mencionan a un actor político conocido, por fecha, fuente y sentimiento; origen: run_actores_sentimiento.R.';

ALTER TABLE actores_sentimiento ADD COLUMN IF NOT EXISTS tipo VARCHAR(10) DEFAULT 'manual';

-- ---------------------------------------------------------------------------
-- 11. Tabla: n-gramas diarios de títulos (bigramas tipo=2, trigramas tipo=3)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_ngramas_diarios (
    fecha   DATE         NOT NULL,
    ngrama  VARCHAR(300) NOT NULL,
    n       INTEGER      NOT NULL,
    tipo    SMALLINT     NOT NULL,
    CONSTRAINT pk_titulos_ngramas_diarios PRIMARY KEY (fecha, ngrama, tipo)
);
CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_diarios_fecha  ON titulos_ngramas_diarios(fecha DESC, tipo);
CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_diarios_ngrama ON titulos_ngramas_diarios(ngrama, tipo);

COMMENT ON TABLE titulos_ngramas_diarios IS 'N-gramas diarios de títulos (tipo=2 bigramas, tipo=3 trigramas); origen: run_analisis_ngramas.R.';

-- ---------------------------------------------------------------------------
-- 12. Tabla: n-gramas por medio (bigramas tipo=2, trigramas tipo=3)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_ngramas_por_medio (
    fecha   DATE         NOT NULL,
    fuente  VARCHAR(100) NOT NULL,
    ngrama  VARCHAR(300) NOT NULL,
    n       INTEGER      NOT NULL,
    tipo    SMALLINT     NOT NULL,
    CONSTRAINT pk_titulos_ngramas_por_medio PRIMARY KEY (fecha, fuente, ngrama, tipo)
);
CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_por_medio_fecha  ON titulos_ngramas_por_medio(fecha DESC, tipo);
CREATE INDEX IF NOT EXISTS idx_titulos_ngramas_por_medio_fuente ON titulos_ngramas_por_medio(fuente, tipo);

COMMENT ON TABLE titulos_ngramas_por_medio IS 'N-gramas por medio y día (tipo=2 bigramas, tipo=3 trigramas); origen: run_analisis_ngramas.R.';

-- ---------------------------------------------------------------------------
-- 13. Tabla: TF-IDF de términos/n-gramas por medio y semana
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_tfidf_por_medio (
    fecha_semana DATE         NOT NULL,
    fuente       VARCHAR(100) NOT NULL,
    termino      VARCHAR(300) NOT NULL,
    tfidf        FLOAT        NOT NULL,
    tipo         SMALLINT     NOT NULL DEFAULT 1,
    CONSTRAINT pk_titulos_tfidf_por_medio PRIMARY KEY (fecha_semana, fuente, termino, tipo)
);
CREATE INDEX IF NOT EXISTS idx_tfidf_fecha_fuente ON titulos_tfidf_por_medio(fecha_semana DESC, fuente, tipo);

COMMENT ON TABLE titulos_tfidf_por_medio IS 'TF-IDF semanal por fuente (tipo=1 unigrama, tipo=2 bigrama, tipo=3 trigrama); origen: run_analisis_tfidf.R.';

-- ---------------------------------------------------------------------------
-- 14. Tabla: términos diarios del cuerpo de artículos
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS cuerpo_terminos_diarios (
    fecha      DATE         NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER      NOT NULL,
    CONSTRAINT pk_cuerpo_terminos_diarios PRIMARY KEY (fecha, termino)
);
CREATE INDEX IF NOT EXISTS idx_cuerpo_terminos_diarios_fecha   ON cuerpo_terminos_diarios(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_cuerpo_terminos_diarios_termino ON cuerpo_terminos_diarios(termino);

COMMENT ON TABLE cuerpo_terminos_diarios IS 'Frecuencia diaria de términos del cuerpo de artículos; origen: run_analisis_cuerpo.R.';

-- ---------------------------------------------------------------------------
-- 15. Tabla: n-gramas diarios del cuerpo de artículos
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS cuerpo_ngramas_diarios (
    fecha  DATE         NOT NULL,
    ngrama VARCHAR(300) NOT NULL,
    n      INTEGER      NOT NULL,
    tipo   SMALLINT     NOT NULL,
    CONSTRAINT pk_cuerpo_ngramas_diarios PRIMARY KEY (fecha, ngrama, tipo)
);
CREATE INDEX IF NOT EXISTS idx_cuerpo_ngramas_diarios_fecha ON cuerpo_ngramas_diarios(fecha DESC, tipo);

COMMENT ON TABLE cuerpo_ngramas_diarios IS 'N-gramas diarios del cuerpo de artículos (tipo=2 bigramas, tipo=3 trigramas); origen: run_analisis_cuerpo.R.';
