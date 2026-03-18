# stopwords.R — Fuente única de stopwords para el pipeline de análisis y el dashboard.
# Todos los scripts de análisis y dashboard/app.R importan esta lista via source("stopwords.R").
# Incluye el union de run_analisis_titulos.R y dashboard/app.R (divergencia "así" resuelta incluyéndolo).

STOPWORDS <- c(
  # Artículos y pronombres
  "el", "la", "los", "las", "un", "una", "unos", "unas",
  # Conjunciones, preposiciones frecuentes
  "y", "o", "pero", "que", "en", "a", "de", "del", "al", "a la",
  "por", "para", "con", "sin", "sobre", "entre", "hasta", "desde",
  # Pronombres y determinantes
  "su", "sus", "se", "lo", "le", "como", "más", "menos", "muy",
  "este", "esta", "estos", "estas", "ese", "esa", "eso", "aquél", "aquella",
  # Interrogativos y relativos
  "qué", "cuál", "cómo", "cuándo", "dónde", "quién", "cuánto",
  # Verbos auxiliares y copulativos
  "ser", "es", "son", "fue", "fueron", "ha", "han", "hay", "está", "están",
  # Adverbios y conectores
  "también", "solo", "sólo", "después", "antes", "durante", "tras",
  "según", "contra", "mediante", "excepto", "hacia",
  # Negación y otras partículas
  "no", "ni", "nos", "nosotros", "ante", "bajo", "tras",
  # Unidades temporales genéricas
  "años", "año", "mes", "meses", "día", "días", "hora", "horas",
  # Números y cuantificadores
  "mil", "nuevo", "nueva", "nuevos", "nuevas", "dos", "tres", "uno",
  "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "ciento", "cientos",
  # Ordinales
  "primera", "primero", "primer", "segunda", "segundo", "tercera", "tercero", "cuarta", "quinto",
  # Indefinidos
  "otro", "otra", "otros", "otras", "mismo", "misma", "mismos", "mismas",
  "todo", "toda", "todos", "todas", "algo", "alguno", "alguna", "algunos", "algunas",
  "cada", "cual", "cuales", "cualquier", "cualesquiera",
  # Verbos modales
  "puede", "pueden", "poder", "debe", "deben", "deber",
  # Formas verbales auxiliares
  "sido", "estado", "será", "serán", "había", "habían", "habrá", "habrán",
  # Verbos de titulares de prensa (ruido frecuente en titulares noticiosos)
  "revisa", "cable", "aquí", "tiene", "pide", "así",
  "anuncia", "anuncian", "anuncio", "confirma", "confirman", "confirmó",
  "revela", "revelan", "informa", "informan", "asegura", "aseguran",
  "advierte", "advierten", "destaca", "destacan", "señala", "señalan",
  "indica", "indican", "reporta", "reportan", "denuncia", "denuncian",
  "explica", "explican", "afirma", "afirman", "sostiene", "sostienen",
  "dice", "dicen", "declara", "declaran", "califica", "considera",
  # Entidades HTML que pueden quedar como tokens
  "quot", "amp", "lt", "gt", "nbsp", "mdash", "ndash", "rsquo", "lsquo", "hellip"
)
