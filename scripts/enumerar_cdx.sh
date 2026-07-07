#!/usr/bin/env bash
# Enumera URLs históricas de artículos (2022-2026) vía Wayback CDX por medio.
# Genera datos/backfill_<fuente>/urls.txt
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

enum() {
  local fuente=$1 prefix=$2 keep=$3
  mkdir -p "datos/backfill_$fuente"
  curl -s "http://web.archive.org/cdx/search/cdx?url=${prefix}&matchType=prefix&collapse=urlkey&from=20220101&to=20260701&output=text&fl=original" 2>/dev/null \
    | grep -iE "$keep" \
    | sed -E 's#\?.*$##; s#^http:#https:#' \
    | grep -viE "outputType=amp|/tag/|/autor/|/page/|/categor|/buscar|/search|\.jpg|\.png|\.pdf|/feed" \
    | sort -u > "datos/backfill_$fuente/urls.txt"
  echo "$fuente: $(wc -l < "datos/backfill_$fuente/urls.txt") urls"
}

enum exante       "ex-ante.cl"            "ex-ante\.cl/[a-z0-9-]{20,}/?$"
enum 24horas      "24horas.cl"            "24horas\.cl/[a-z0-9-]+/[a-z0-9-]+/[a-z0-9-]{15,}"
enum lahora       "lahora.cl"             "lahora\.cl/[a-z-]+/20[0-9]{2}/[0-9]{2}/[0-9]{2}/"
enum publimetro   "publimetro.cl/noticias" "publimetro\.cl/noticias/20[0-9]{2}/[0-9]{2}/[0-9]{2}/"
enum t13          "t13.cl/noticia"        "t13\.cl/noticia/[a-z0-9-]+/[a-z0-9-]{15,}"
enum elsiglo      "elsiglo.cl"            "elsiglo\.cl/[a-z0-9-]{20,}/?$"
enum quintopoder  "elquintopoder.cl"      "elquintopoder\.cl/[a-z-]+/[a-z0-9-]{15,}"
enum ciper        "ciperchile.cl"         "ciperchile\.cl/20[0-9]{2}/[0-9]{2}/[0-9]{2}/"
echo "=== enumeración completa ==="
