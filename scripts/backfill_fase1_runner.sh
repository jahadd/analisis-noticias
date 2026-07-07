#!/usr/bin/env bash
# Fase 1 del plan de relleno de gaps (plan_relleno_gaps.md): backfill de los
# gaps feb-jun 2026 de los medios sin cobertura. Secuencial, chicos primero.
# BF_DESDE=2026-02-15 descarta artículos viejos re-archivados que se cuelan
# (la fecha CDX es de archivo, no de publicación).
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
mkdir -p logs/backfill_fase1
RSCRIPT="$(command -v Rscript)"
# exante excluido: 403 anti-bot, requiere chromote — fase aparte
FUENTES=(biobio eldesconcierto agricultura theclinic emol cnnchile adnradio cooperativa latercera)
echo "=== BACKFILL FASE 1 (gaps 2026) — inicio $(date '+%Y-%m-%d %H:%M') ==="
for f in "${FUENTES[@]}"; do
  urls="datos/backfill_$f/urls_fase1.txt"
  if [[ ! -s "$urls" ]]; then echo "[$f] sin urls, saltando"; continue; fi
  echo "=== $f ($(wc -l < "$urls") urls) — $(date '+%H:%M') ==="
  BF_FUENTE="$f" BF_URLS="$urls" BF_DESDE="2026-02-15" BF_TIMEOUT="10" \
    "$RSCRIPT" scripts/backfill_generico.R > "logs/backfill_fase1/$f.log" 2>&1
  echo "  -> $(grep -E 'LISTO' "logs/backfill_fase1/$f.log" | tail -1)"
done
echo "=== BACKFILL FASE 1 — completo $(date '+%Y-%m-%d %H:%M') ==="
