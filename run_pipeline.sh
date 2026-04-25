#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# run_pipeline.sh — Pipeline completo: scraping incremental + análisis
# Scrapea todos los medios desde la última noticia en BD hasta hoy,
# luego corre los scripts de análisis en orden.
#
# Uso:
#   ./run_pipeline.sh               # pipeline completo
#   ./run_pipeline.sh --solo-analisis  # saltar scraping, solo análisis
#
# Variables de entorno: las mismas que los scripts R (.env en Paginaweb/)
# Logs: logs/pipeline_YYYY-MM-DD_HH-MM.log
# ------------------------------------------------------------------------------

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

LOG_DIR="$SCRIPT_DIR/logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/pipeline_$(date '+%Y-%m-%d_%H-%M').log"

SOLO_ANALISIS=false
for arg in "$@"; do
  [[ "$arg" == "--solo-analisis" ]] && SOLO_ANALISIS=true
done

# Redirigir stdout y stderr al log Y a consola
exec > >(tee -a "$LOG_FILE") 2>&1

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"; }

log "======================================================"
log "  Pipeline noticias — inicio"
log "  Log: $LOG_FILE"
log "======================================================"

SCRAPERS_DIR="$SCRIPT_DIR/scraping/fuentes"
RSCRIPT="$(command -v Rscript)"
if [[ -z "$RSCRIPT" ]]; then
  log "ERROR: Rscript no encontrado en PATH"
  exit 1
fi

# ------------------------------------------------------------------------------
# Función: correr un script R desde la raíz del proyecto
# ------------------------------------------------------------------------------
run_r() {
  local script="$1"
  local label="${2:-$script}"
  log "--- Iniciando: $label ---"
  local t0=$SECONDS
  if "$RSCRIPT" "$script" 2>&1; then
    log "OK $label ($(( SECONDS - t0 ))s)"
    return 0
  else
    log "FALLO $label ($(( SECONDS - t0 ))s) — continuando pipeline"
    return 1
  fi
}

SCRAPERS_FALLIDOS=()
SCRAPERS_OK=0

# ------------------------------------------------------------------------------
# Detectar rango de fechas a scrapear desde la BD
# ------------------------------------------------------------------------------
PSQL="$(command -v psql 2>/dev/null || true)"

# Parsear .env para obtener credenciales de PostgreSQL
_parse_env() {
  local env_file
  for env_file in "$SCRIPT_DIR/.env" "$SCRIPT_DIR/../.env"; do
    [[ -f "$env_file" ]] || continue
    while IFS= read -r line; do
      line="${line%%#*}"                   # strip comments
      line="${line#"${line%%[![:space:]]*}"}"  # ltrim
      [[ -z "$line" ]] && continue
      [[ "$line" == PGHOST=*           ]] && export PGHOST="${line#PGHOST=}"
      [[ "$line" == PGPORT=*           ]] && export PGPORT="${line#PGPORT=}"
      [[ "$line" == PGUSER_NOTICIAS=*  ]] && export PGUSER_NOTICIAS="${line#PGUSER_NOTICIAS=}"
      [[ "$line" == PGPASSWORD_NOTICIAS=* ]] && export PGPASSWORD_NOTICIAS="${line#PGPASSWORD_NOTICIAS=}"
      [[ "$line" == PGDATABASE_NOTICIAS=* ]] && export PGDATABASE_NOTICIAS="${line#PGDATABASE_NOTICIAS=}"
      [[ "$line" == PGUSER=*           ]] && export PGUSER="${line#PGUSER=}"
      [[ "$line" == PGPASSWORD=*       ]] && export PGPASSWORD="${line#PGPASSWORD=}"
      [[ "$line" == PGDATABASE=*       ]] && export PGDATABASE="${line#PGDATABASE=}"
    done < "$env_file"
    break
  done
}

_parse_env

_PG_HOST="${PGHOST:-localhost}"
_PG_PORT="${PGPORT:-5432}"
_PG_USER="${PGUSER_NOTICIAS:-${PGUSER:-noticias}}"
_PG_PASS="${PGPASSWORD_NOTICIAS:-${PGPASSWORD:-}}"
_PG_DB="${PGDATABASE_NOTICIAS:-${PGDATABASE:-noticias_chile}}"

# Quitar posibles comillas de los valores parseados
_PG_USER="${_PG_USER//\'/}"; _PG_USER="${_PG_USER//\"/}"
_PG_PASS="${_PG_PASS//\'/}"; _PG_PASS="${_PG_PASS//\"/}"
_PG_DB="${_PG_DB//\'/}";   _PG_DB="${_PG_DB//\"/}"

# Consultar MAX(fecha) en la BD (default: 3 días atrás si falla)
FECHA_DESDE_SCRAPING=""
DIAS_SCRAPING=3

if [[ -n "$PSQL" && -x "$PSQL" && -n "$_PG_PASS" ]]; then
  FECHA_MAX_DB=$(PGPASSWORD="$_PG_PASS" "$PSQL" \
    -h "$_PG_HOST" -p "$_PG_PORT" -U "$_PG_USER" -d "$_PG_DB" \
    -t -c "SELECT MAX(fecha)::text FROM noticias;" 2>/dev/null | tr -d '[:space:]')

  if [[ "$FECHA_MAX_DB" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
    HOY=$(date '+%Y-%m-%d')
    # Calcular días: compatible macOS (date -j) y Linux (date -d)
    if date -j -f "%Y-%m-%d" "$HOY" "+%s" &>/dev/null; then
      EPOCH_MAX=$(date -j -f "%Y-%m-%d" "$FECHA_MAX_DB" "+%s" 2>/dev/null)
      EPOCH_HOY=$(date -j -f "%Y-%m-%d" "$HOY"          "+%s" 2>/dev/null)
    else
      EPOCH_MAX=$(date -d "$FECHA_MAX_DB" "+%s" 2>/dev/null)
      EPOCH_HOY=$(date -d "$HOY"          "+%s" 2>/dev/null)
    fi

    if [[ -n "$EPOCH_MAX" && -n "$EPOCH_HOY" ]]; then
      DIAS_SCRAPING=$(( (EPOCH_HOY - EPOCH_MAX) / 86400 ))
      [[ $DIAS_SCRAPING -lt 1 ]] && DIAS_SCRAPING=1
      FECHA_DESDE_SCRAPING="$FECHA_MAX_DB"
      log "BD actualizada hasta: $FECHA_MAX_DB  →  scrapear $DIAS_SCRAPING días (hasta $HOY)"
    fi
  else
    log "AVISO: no se pudo obtener MAX(fecha) de la BD — usando DIAS_SCRAPING=3 por defecto"
  fi
else
  log "AVISO: psql no disponible o sin contraseña — usando DIAS_SCRAPING=3 por defecto"
fi

export DIAS_SCRAPING
export FECHA_DESDE_SCRAPING

# ------------------------------------------------------------------------------
# Scrapers de actualización diaria (orden: más confiables primero)
# ------------------------------------------------------------------------------
SCRAPERS_DIARIOS=(
  scraping_biobio_api.r
  scraping_24horas.r
  scraping_adnradio.r
  scraping_agricultura.r
  scraping_biobio.r
  scraping_chvnoticias.r
  scraping_ciper.r
  scraping_cnnchile.r
  scraping_cooperativa.r
  scraping_diariofinanciero.r
  scraping_elciudadano.r
  scraping_eldesconcierto.R
  scraping_eldinamo.r
  scraping_elmostrador.r
  scraping_elsiglo.r
  scraping_emol.R
  scraping_exante.r
  scraping_izquierdadiario.R
  scraping_lacuarta.r
  scraping_lahora.r
  scraping_lanacion.r
  scraping_latercera.r
  scraping_meganoticias.r
  scraping_publimetro.r
  scraping_quintopoder.R
  scraping_radiouchile.r
  scraping_redgol.R
  scraping_t13.r
  scraping_theclinic.r
)

# Scrapers históricos (solo incluir si se pasa --con-historicos)
SCRAPERS_HISTORICOS=(
  scraping_cooperativa_h.r
  scraping_emol_h.R
  scraping_lasegunda.r
  scraping_soychile.r
)

CON_HISTORICOS=false
for arg in "$@"; do
  [[ "$arg" == "--con-historicos" ]] && CON_HISTORICOS=true
done

if [[ "$SOLO_ANALISIS" == false ]]; then
  log ""
  log "=== FASE 1: SCRAPING ==="

  for scraper in "${SCRAPERS_DIARIOS[@]}"; do
    ruta="$SCRAPERS_DIR/$scraper"
    if [[ ! -f "$ruta" ]]; then
      log "AVISO: $scraper no encontrado, saltando"
      continue
    fi
    # Cambiar al directorio del proyecto para que source("funciones.R") funcione
    if (cd "$SCRIPT_DIR" && "$RSCRIPT" "$ruta" 2>&1); then
      log "OK $scraper"
      (( SCRAPERS_OK++ )) || true
    else
      log "FALLO $scraper"
      SCRAPERS_FALLIDOS+=("$scraper")
    fi
  done

  if [[ "$CON_HISTORICOS" == true ]]; then
    log ""
    log "--- Scrapers históricos ---"
    for scraper in "${SCRAPERS_HISTORICOS[@]}"; do
      ruta="$SCRAPERS_DIR/$scraper"
      if [[ ! -f "$ruta" ]]; then
        log "AVISO: $scraper no encontrado, saltando"
        continue
      fi
      if (cd "$SCRIPT_DIR" && "$RSCRIPT" "$ruta" 2>&1); then
        log "OK $scraper"
        (( SCRAPERS_OK++ )) || true
      else
        log "FALLO $scraper"
        SCRAPERS_FALLIDOS+=("$scraper")
      fi
    done
  fi

  log ""
  log "Scraping: $SCRAPERS_OK OK, ${#SCRAPERS_FALLIDOS[@]} fallidos"
  if [[ ${#SCRAPERS_FALLIDOS[@]} -gt 0 ]]; then
    log "Fallidos: ${SCRAPERS_FALLIDOS[*]}"
  fi
fi

# ------------------------------------------------------------------------------
# Análisis
# ------------------------------------------------------------------------------
log ""
log "=== FASE 2: ANÁLISIS ==="

run_r "analisis/run_analisis_ngramas.R"       "Ngramas"       || true
run_r "analisis/run_analisis_titulos.R"       "Titulos"       || true
run_r "analisis/run_embeddings.R"             "Embeddings RAG" || true
run_r "analisis/run_analisis_coocurrencia.R"  "Coocurrencia"  || true

# Refrescar vistas materializadas mensuales (para queries rápidas en el dashboard)
log "--- Refrescando vistas materializadas mensuales ---"
if [[ -n "$_PG_PASS" ]]; then
  PGPASSWORD="$_PG_PASS" psql -h "$_PG_HOST" -p "$_PG_PORT" -U "$_PG_USER" -d "$_PG_DB" \
    -c "REFRESH MATERIALIZED VIEW mv_terminos_mensuales; REFRESH MATERIALIZED VIEW mv_terminos_por_medio_mensuales;" \
    2>&1 && log "OK vistas materializadas" || log "WARN: fallo al refrescar vistas materializadas"
fi

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
log ""
log "======================================================"
log "  Pipeline completado — $(date '+%Y-%m-%d %H:%M:%S')"
if [[ "$SOLO_ANALISIS" == false ]]; then
  log "  Scrapers OK:      $SCRAPERS_OK"
  log "  Scrapers fallidos: ${#SCRAPERS_FALLIDOS[@]}"
fi
log "  Log guardado en:  $LOG_FILE"
log "======================================================"
