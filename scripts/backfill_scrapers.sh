#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# backfill_scrapers.sh — Puesta al día completa (SIN TOPE de páginas).
# Corre los scrapers diarios SECUENCIALMENTE (obligatorio: comparten la tabla de
# staging _stg_noticias_scraping) y SIN timeout, para que cada uno termine y
# guarde. Orden: más rápidos primero, pesados y de mayor atraso al final
# (lacuarta es el último).
#
# El número de páginas lo decide n_paginas_fuente() según el atraso real de cada
# fuente (hoy - última fecha en BD), ya sin techo (max_pags = Inf).
#
# Uso:
#   scripts/backfill_scrapers.sh
# Logs: logs/backfill/<scraper>.log  + RESUMEN.txt
# ------------------------------------------------------------------------------
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SCRIPT_DIR"

SCRAPERS_DIR="$SCRIPT_DIR/scraping/fuentes"
RSCRIPT="$(command -v Rscript)"

OUT_DIR="$SCRIPT_DIR/logs/backfill"
mkdir -p "$OUT_DIR"
RESUMEN="$OUT_DIR/RESUMEN.txt"
: > "$RESUMEN"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$RESUMEN"; }

# Orden: rápidos/APIs primero → pesados (chromote, muchas páginas) → mayor atraso al final
SCRAPERS=(
  # --- rápidos (APIs / portada, segundos a ~2 min) ---
  scraping_biobio_api.r
  scraping_biobio.r
  scraping_izquierdadiario.R
  scraping_lahora.r
  scraping_cooperativa.r
  scraping_exante.r
  scraping_quintopoder.R
  # --- paginados con atraso normal (~96 págs) ---
  scraping_lanacion.r
  scraping_elsiglo.r
  scraping_radiouchile.r
  scraping_ciper.r
  scraping_eldinamo.r
  scraping_elciudadano.r
  scraping_agricultura.r
  scraping_adnradio.r
  scraping_chvnoticias.r
  scraping_24horas.r
  scraping_publimetro.r
  scraping_t13.r
  scraping_meganoticias.r
  scraping_diariofinanciero.r
  scraping_cnnchile.r
  scraping_theclinic.r
  # --- pesados (chromote / muchas páginas) ---
  scraping_emol.R
  scraping_elmostrador.r
  scraping_latercera.r
  scraping_eldesconcierto.R
  # --- mayor atraso (los más lentos, al final) ---
  scraping_lacuarta.r     # ~376 días de atraso (~564 págs)
)

log "======================================================"
log "  Backfill scrapers — inicio (SIN TOPE de páginas)"
log "  ${#SCRAPERS[@]} scrapers, secuencial, sin timeout"
log "======================================================"
printf "%-32s %-8s %-10s %s\n" "SCRAPER" "ESTADO" "FILAS" "DURACION" | tee -a "$RESUMEN"

TOTAL_FILAS=0
for scraper in "${SCRAPERS[@]}"; do
  ruta="$SCRAPERS_DIR/$scraper"
  log="$OUT_DIR/${scraper}.log"
  if [[ ! -f "$ruta" ]]; then
    printf "%-32s %-8s %-10s %s\n" "$scraper" "NOFILE" "-" "-" | tee -a "$RESUMEN"
    continue
  fi

  log ">>> $scraper ..."
  t0=$SECONDS
  if (cd "$SCRIPT_DIR" && "$RSCRIPT" "$ruta" > "$OUT_DIR/${scraper}.log" 2>&1); then
    rc=0
  else
    rc=$?
  fi
  dur=$(( SECONDS - t0 ))

  filas=$(grep -oE '[0-9]+ filas insertadas' "$OUT_DIR/${scraper}.log" | tail -1 | grep -oE '^[0-9]+')
  [[ -z "$filas" ]] && filas=0
  TOTAL_FILAS=$(( TOTAL_FILAS + filas ))

  if [[ $rc -eq 0 ]]; then estado="OK"; else estado="FALLO(rc=$rc)"; fi
  printf "%-32s %-8s %-10s %dm%ds\n" "$scraper" "$estado" "$filas" "$((dur/60))" "$((dur%60))" | tee -a "$RESUMEN"
done

log "======================================================"
log "  Backfill completado. Total filas insertadas/actualizadas: $TOTAL_FILAS"
log "  Logs por scraper: $OUT_DIR/"
log "======================================================"
