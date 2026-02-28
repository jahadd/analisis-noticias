# Chequear si se guardaron los archivos de scraping (cada fuente con archivos de hoy y > 10kb).
# Se invoca desde prensa_scraping.R al final del orquestador.
# Requiere que funciones.R esté cargado (revisar_resultados).

revisar_resultados("scraping/datos")
