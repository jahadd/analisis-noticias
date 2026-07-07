# Plan de relleno de gaps de captura (julio 2026)

Objetivo: eliminar los huecos ≥30 días detectados en `noticias` (los que el
dashboard marca en amarillo/rojo en "Volumen de datos"), reutilizando la
infraestructura de backfill existente:

- `scripts/enumerar_cdx.sh` — enumera URLs archivadas en Wayback CDX → `datos/backfill_<fuente>/urls.txt`
- `scripts/backfill_generico.R` — descarga y guarda (env: `BF_FUENTE`, `BF_URLS`, `BF_DESDE`, `BF_HASTA`, `BF_TIMEOUT`)
- `scripts/backfill_lowvol_runner.sh` — corre fuentes en secuencia (chicos primero), logs en `logs/backfill_lowvol/`

El reproceso del análisis es automático: `analisis_cobertura` detecta los
conteos alterados y el pipeline diario reprocesa desde la fecha más antigua
modificada. No hay pasos manuales post-backfill.

## Inventario de gaps (consulta del 2 jul 2026, umbral 30 días)

### A. Runner lowvol (scope 2022→2026) — ✅ COMPLETADO 2026-07-05

| Fuente | Guardadas |
|---|---|
| ciper | 3.792 |
| quintopoder | 296 |
| elsiglo | 3.084 |
| publimetro | 10.752 |
| lahora | 76.889 |
| 24horas | 99.624 |
| t13 | 88.247 (82.522 corrida paralela + 5.725 barrido) — su gap 2021 queda para fase 2 |

Nota (2026-07-05): `guardar_noticias_en_postgres()` ahora usa staging con sufijo
PID, así que los backfills pueden correr en paralelo entre sí y con el pipeline
diario sin pisarse la tabla de staging.

### B. Gaps 2026 (feb–jun) sin cobertura — PRIORDAD 1
Son el período entre corridas manuales del pipeline; afecta directamente lo que
muestra el dashboard hoy. Volumen estimado bajo (~30-60 notas/día por fuente).

| Fuente | Gap(s) | Días |
|---|---|---|
| adnradio | 2026-03-07 → 2026-06-07 | 93 |
| eldesconcierto | 2026-03-07 → 2026-05-27 | 82 |
| cnnchile | 2026-03-07→04-14, 04-25→06-25 | 39 + 62 |
| cooperativa | 2026-03-07→04-22, 04-25→06-25 | 47 + 62 |
| agricultura | 2026-04-25 → 2026-06-23 | 60 |
| latercera | 2026-03-07→04-20, 04-25→06-22 | 45 + 59 |
| biobio | 2026-04-25 → 2026-06-21 | 58 |
| theclinic | 2026-04-25 → 2026-06-12 | 49 |
| emol | 2026-03-07→04-20, 04-25→06-13 | 45 + 50 |
| exante | 2026-03-06 → 2026-04-09 | 35 (⚠ 403: requiere chromote) |

### C. Gaps históricos grandes — PRIORIDAD 2

| Fuente | Gap | Días | Volumen estimado |
|---|---|---|---|
| diariofinanciero | 2020-04-08 → 2022-04-20 | 743 | ~35-40k URLs |
| biobio | 2020-11-04 → 2022-09-10 (+4 gaps de 30d en 2020) | 676+ | ~25-30k URLs |
| theclinic | 2018-03-15 → 2018-12-27 | 288 | ~8k |
| adnradio | 2023-12→2024-03 (112), 2025-04→11 (226) | 338 | ~5k |
| cooperativa | 2022-02-05 → 2022-06-11 | 127 | ~3k |
| t13 | 2021-07→10 (100), 2021-12→2022-01 (33) | 133 | ~4k |
| exante | 2025-02-09 → 2025-04-17 | 68 | ~1k (chromote) |
| theclinic | 2025-05-08 → 2025-06-21 | 45 | ~1k |
| emol | 2025-09-02 → 2025-10-04 | 33 | ~1k |
| latercera | 2025-04-07 → 2025-05-11 | 35 | ~1k |
| lacuarta | 2018 (43), 2020 (43) | 86 | ~2k |
| agricultura | 2025-06-19 → 2025-07-20 | 32 | ~1k |

### D. Fuentes rotas — requieren decisión de producto

| Fuente | Problema | Opciones |
|---|---|---|
| izquierdadiario | Anti-bot 403 (SPA); sin datos desde 2026-03-06 | (a) reescribir scraper con chromote, (b) eliminar del pipeline |
| redgol | ~~Secciones scrapeadas ya no existen; sin datos desde 2024-11-27~~ **ELIMINADO 2026-07-04** (decisión del usuario): scraper borrado y quitado de run_pipeline.sh/verificar/backfill; datos históricos hasta nov 2024 se conservan; el dashboard lo marca como fuente descontinuada | — |

## Fases de ejecución

**Fase 0 — dejar terminar el runner actual.** Nada que hacer; al finalizar
24horas, verificar `logs/backfill_lowvol/_runner.log` y correr la consulta de
gaps (abajo) para confirmar que A quedó cerrado.

**Fase 1 — gaps 2026 (grupo B). ✅ COMPLETADA 2026-07-04.**
68.099 noticias guardadas (biobio 639, eldesconcierto 1.434, agricultura 3.657,
theclinic 2.417, emol 4.281, cnnchile 3.076, adnradio 13.392, cooperativa 17.397,
latercera 21.806). Verificado: sin gaps ≥7 días en 2026 salvo 3 mini-huecos de
12-19 días (cnnchile mar-abr, eldesconcierto mar) donde Wayback no archivó.
Scripts: `scripts/enumerar_cdx_fase1.sh` + `scripts/backfill_fase1_runner.sh`.
exante quedó pendiente (chromote). Mecánica original:
1. `scripts/enumerar_cdx.sh <fuente>` acotado a la ventana del gap (CDX `from`/`to`
   por fecha de ARCHIVO + margen de ±1 mes; el filtro fino de publicación lo hace
   `backfill_generico.R` con `BF_DESDE`/`BF_HASTA`).
2. Encolar en un runner tipo `backfill_lowvol_runner.sh` (chicos primero:
   agricultura → cnnchile → cooperativa → adnradio → eldesconcierto → theclinic
   → emol → latercera → biobio).
3. exante aparte: adaptar `backfill_generico.R` para usar los helpers
   `iniciar_chrome()`/`chrome_navegar()` de funciones.R (bypass 403).
   Volumen total estimado: ~15-20k URLs → 1-2 noches.

**Fase 2 — gaps históricos (grupo C).** Igual mecánica, un runner aparte,
partiendo por los chicos (cooperativa 2022, t13 2021, lacuarta, theclinic 2018)
y dejando para el final los dos gigantes (biobio 2020-22, diariofinanciero
2020-22, ~60-70k URLs → varios días a ~28 req/min de Wayback). Correr de noche;
son gaps viejos que no afectan la vista por defecto del dashboard.

**Fase 3 — decisión sobre rotos (grupo D).** Si se decide mantenerlos,
reescritura de scrapers (izquierdadiario con chromote; redgol con secciones
actuales) + backfill de su hueco. Si no, quitarlos de `run_pipeline.sh` y
documentar el corte (el dashboard ya los marca en rojo con explicación).

## Consideraciones operativas

- **CDX devuelve fecha de archivo, no de publicación** → siempre filtrar por
  fecha de URL o `BF_DESDE`/`BF_HASTA` (lección del backfill anterior: artículos
  viejos re-archivados se cuelan).
- **Mientras un backfill esté activo, la corrida diaria del pipeline reprocesa
  desde la fecha alterada más antigua** (analisis_cobertura) — es correcto pero
  pesado. Conviene agrupar los backfills en tandas y no eternizarlos.
- El reproceso completo de coocurrencia ya usa TRUNCATE + rebuild de índices
  (rápido); los reprocesos parciales usan DELETE, aceptable para ventanas cortas.
- Sentimiento: lo backfilleado ≥2025 entra a la reclasificación llama3.1:8b en
  curso; lo anterior queda sin clasificar hasta extender el alcance (decisión aparte).

## Verificación de cierre

```sql
-- debe devolver 0 filas para las fuentes activas (o solo gaps justificados/documentados)
WITH fechas_medio AS (SELECT DISTINCT fuente, fecha FROM noticias WHERE fecha >= '2018-01-01'),
gaps AS (SELECT fuente, fecha, LAG(fecha) OVER (PARTITION BY fuente ORDER BY fecha) prev FROM fechas_medio)
SELECT fuente, (prev+1)::date desde, (fecha-1)::date hasta, (fecha-prev-1)::int dias
FROM gaps WHERE fecha - prev - 1 >= 30 ORDER BY fuente, desde;
```

Y en el dashboard: "Medios → Volumen de datos" con rango 2018→hoy no debería
mostrar barras amarillas (solo rojas si se decide no rescatar izquierdadiario/redgol).
