#!/usr/bin/env bash
# Fase 1 del plan de relleno de gaps (plan_relleno_gaps.md): enumera URLs de
# artículos feb-jul 2026 vía Wayback CDX para los medios con gaps 2026 sin
# cobertura. Genera datos/backfill_<fuente>/urls_fase1.txt
# (la fecha CDX es de ARCHIVO; el filtro fino de publicación lo hace
#  backfill_generico.R con BF_DESDE)
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

enum() {
  local fuente=$1 prefix=$2 keep=$3
  mkdir -p "datos/backfill_$fuente"
  curl -s "http://web.archive.org/cdx/search/cdx?url=${prefix}&matchType=prefix&collapse=urlkey&from=20260201&to=20260715&output=text&fl=original" 2>/dev/null \
    | grep -iE "$keep" \
    | sed -E 's#\?.*$##; s#^http:#https:#' \
    | grep -viE "outputType=amp|/tag/|/autor/|/page/|/categor|/buscar|/search|\.jpg|\.png|\.pdf|/feed" \
    | sort -u > "datos/backfill_$fuente/urls_fase1.txt"
  echo "$fuente: $(wc -l < "datos/backfill_$fuente/urls_fase1.txt") urls"
}

enum agricultura    "radioagricultura.cl/noticias" "radioagricultura\.cl/noticias/[a-z0-9-]+/[a-z0-9-]+_20[0-9]{6}"
enum cnnchile       "cnnchile.com"                 "cnnchile\.com/[a-z0-9-]+/[a-z0-9-]{15,}/?$"
enum cooperativa    "cooperativa.cl/noticias"      "cooperativa\.cl/noticias/.+/20[0-9]{2}-[0-9]{2}-[0-9]{2}/"
enum adnradio       "adnradio.cl"                  "adnradio\.cl/20[0-9]{2}/[0-9]{2}/[0-9]{2}/[a-z0-9-]{10,}"
enum eldesconcierto "eldesconcierto.cl"            "eldesconcierto\.cl/[a-z-]+/[a-z0-9-]+-n[0-9]+"
enum theclinic      "theclinic.cl"                 "theclinic\.cl/20[0-9]{2}/[0-9]{2}/[0-9]{2}/[a-z0-9-]{10,}"
enum emol           "emol.com/noticias"            "emol\.com/noticias/[A-Za-z]+/20[0-9]{2}/[0-9]{2}/[0-9]{2}/"
enum latercera      "latercera.com"                "latercera\.com/[a-z0-9-]+/noticia/[a-z0-9-]{10,}"
enum biobio         "biobiochile.cl"               "biobiochile\.cl/[a-z0-9/_-]*noticias/20[0-9]{2}/[0-9]{2}/[0-9]{2}/[a-z0-9-]{10,}"
echo "=== enumeración fase 1 completa ==="
