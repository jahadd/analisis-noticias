#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# verificar_scrapers.sh — Health check de los scrapers diarios.
# Corre cada scraper individualmente (secuencial, porque comparten la tabla de
# staging _stg_noticias_scraping), con timeout, capturando log por scraper y un
# resumen final: OK / FALLO / TIMEOUT + filas insertadas + snippet de error.
#
# Uso:
#   scripts/verificar_scrapers.sh                 # todos, timeout 300s
#   TIMEOUT=180 scripts/verificar_scrapers.sh     # timeout personalizado
#   scripts/verificar_scrapers.sh scraping_t13.r scraping_emol.R   # solo algunos
# ------------------------------------------------------------------------------
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SCRIPT_DIR"

SCRAPERS_DIR="$SCRIPT_DIR/scraping/fuentes"
RSCRIPT="$(command -v Rscript)"
TIMEOUT="${TIMEOUT:-300}"

OUT_DIR="$SCRIPT_DIR/logs/test_scrapers"
mkdir -p "$OUT_DIR"
RESUMEN="$OUT_DIR/RESUMEN.txt"
: > "$RESUMEN"

# Lista de scrapers diarios (misma que run_pipeline.sh)
SCRAPERS_DIARIOS=(
  scraping_biobio_api.r scraping_24horas.r scraping_adnradio.r
  scraping_agricultura.r scraping_biobio.r scraping_chvnoticias.r
  scraping_ciper.r scraping_cnnchile.r scraping_cooperativa.r
  scraping_diariofinanciero.r scraping_elciudadano.r scraping_eldesconcierto.R
  scraping_eldinamo.r scraping_elmostrador.r scraping_elsiglo.r
  scraping_emol.R scraping_exante.r scraping_izquierdadiario.R
  scraping_lacuarta.r scraping_lahora.r scraping_lanacion.r
  scraping_latercera.r scraping_meganoticias.r scraping_publimetro.r
  scraping_quintopoder.R scraping_radiouchile.r
  scraping_t13.r scraping_theclinic.r
)

# Si se pasan argumentos, usar esa lista en su lugar
if [[ $# -gt 0 ]]; then SCRAPERS_DIARIOS=("$@"); fi

# Ejecuta un comando con timeout portable (macOS no trae `timeout`).
# Retorna 124 si se agotó el tiempo.
run_with_timeout() {
  local secs="$1"; shift
  "$@" &
  local pid=$!
  ( sleep "$secs"; kill -9 "$pid" 2>/dev/null ) &
  local watcher=$!
  wait "$pid" 2>/dev/null
  local rc=$?
  kill "$watcher" 2>/dev/null; wait "$watcher" 2>/dev/null
  if [[ $rc -eq 137 || $rc -eq 9 ]]; then return 124; fi
  return $rc
}

printf "%-32s %-8s %-10s %s\n" "SCRAPER" "ESTADO" "FILAS" "DETALLE" | tee -a "$RESUMEN"
printf '%.0s-' {1..90} | tee -a "$RESUMEN"; echo | tee -a "$RESUMEN"

for scraper in "${SCRAPERS_DIARIOS[@]}"; do
  ruta="$SCRAPERS_DIR/$scraper"
  log="$OUT_DIR/${scraper}.log"
  if [[ ! -f "$ruta" ]]; then
    printf "%-32s %-8s %-10s %s\n" "$scraper" "NOFILE" "-" "no encontrado" | tee -a "$RESUMEN"
    continue
  fi

  t0=$SECONDS
  run_with_timeout "$TIMEOUT" "$RSCRIPT" "$ruta" > "$log" 2>&1
  rc=$?
  dur=$(( SECONDS - t0 ))

  # filas insertadas (última coincidencia)
  filas=$(grep -oE '[0-9]+ filas insertadas' "$log" | tail -1 | grep -oE '^[0-9]+')
  [[ -z "$filas" ]] && filas="-"

  # señales de que el scraper estaba haciendo trabajo real (encontró/scrapeó artículos)
  progreso=$(grep -cE 'scraping http|Se obtuvieron [0-9]|noticias en https|filas insertadas' "$log")

  if [[ $rc -eq 124 ]]; then
    # timeout: si había progreso, está OK pero lento; si no, sospechoso
    if [[ "$progreso" -gt 0 ]]; then
      estado="LENTO"
      detalle="${dur}s, ${progreso} señales de scraping (vivo)"
    else
      estado="TIMEOUT"
      detalle="${dur}s sin progreso (revisar)"
    fi
  elif [[ $rc -eq 0 ]]; then
    if [[ "$filas" == "-" || "$filas" == "0" ]] && [[ "$progreso" -eq 0 ]]; then
      estado="VACIO"
      detalle="${dur}s, 0 filas, sin progreso (selectores rotos?)"
    else
      estado="OK"
      detalle="${dur}s"
    fi
  else
    estado="FALLO"
    # snippet del primer error
    err=$(grep -iE 'error|cannot|no such|could not|unable' "$log" | head -1 | cut -c1-60)
    detalle="${dur}s rc=$rc | ${err}"
  fi

  printf "%-32s %-8s %-10s %s\n" "$scraper" "$estado" "$filas" "$detalle" | tee -a "$RESUMEN"
done

echo | tee -a "$RESUMEN"
echo "Logs por scraper en: $OUT_DIR/" | tee -a "$RESUMEN"
echo "Resumen guardado en: $RESUMEN" | tee -a "$RESUMEN"
