#!/usr/bin/env bash
# Corre el backfill genérico para cada medio de bajo volumen, secuencial
# (comparten la tabla de staging). Orden: chicos primero.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
mkdir -p logs/backfill_lowvol
RSCRIPT="$(command -v Rscript)"
# exante excluido: devuelve 403 (anti-bot), requiere chromote — se hace aparte
FUENTES=(ciper quintopoder elsiglo publimetro lahora 24horas t13)
echo "=== BACKFILL BAJO VOLUMEN — inicio $(date '+%Y-%m-%d %H:%M') ==="
for f in "${FUENTES[@]}"; do
  urls="datos/backfill_$f/urls_2022.txt"        # preferir lista ya filtrada a 2022-2026
  [[ -s "$urls" ]] || urls="datos/backfill_$f/urls.txt"
  if [[ ! -s "$urls" ]]; then echo "[$f] sin urls, saltando"; continue; fi
  echo "=== $f ($(wc -l < "$urls") urls) — $(date '+%H:%M') ==="
  BF_FUENTE="$f" BF_URLS="$urls" BF_DESDE="2022-01-01" BF_TIMEOUT="10" \
    "$RSCRIPT" scripts/backfill_generico.R > "logs/backfill_lowvol/$f.log" 2>&1
  echo "  -> $(grep -E 'LISTO' "logs/backfill_lowvol/$f.log" | tail -1)"
done
echo "=== BACKFILL BAJO VOLUMEN — completo $(date '+%Y-%m-%d %H:%M') ==="
