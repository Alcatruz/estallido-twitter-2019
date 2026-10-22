# ---------------------------------------------------------------------------
# 05_corpus_comunidades.R
#
# Para cada comunidad de la red del 18-O:
#   - cuentas fuertes (autoridades: mas retuiteadas / hubs: que mas retuitean)
#   - el corpus de texto que la comunidad produce y el que amplifica
#   - palabras y hashtags caracteristicos (no solo los mas frecuentes)
#
# Un tweet tiene DOS comunidades posibles y no son lo mismo:
#   comunidad_autor  = la del que escribio el texto  -> que dice la comunidad
#   comunidad_emisor = la del que lo retuiteo        -> que amplifica
#
# Salidas:
#   data/processed/corpus_18oct_comunidades.parquet  todo el texto etiquetado
#   output/tablas/18oct_cuentas_fuertes.csv
#   output/tablas/18oct_corpus_com_<k>.csv           top textos por comunidad
#   output/tablas/18oct_palabras_por_comunidad.csv
#   output/tablas/18oct_hashtags_por_comunidad.csv
#   output/figuras/18oct_palabras_caracteristicas.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(ggplot2); library(scales)
})

DIA      <- as.Date("2019-10-18")
N_TEXTOS <- 300L   # textos guardados por comunidad
N_TOP    <- 15L    # palabras/hashtags mostrados por comunidad

nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
top_com <- as.integer(names(head(sort(table(nodos$comunidad), decreasing = TRUE), 8)))

# --- 1. Texto del dia ------------------------------------------------------
# status_id se fuerza a character: son enteros de 19 digitos que como numeric
# pierden precision silenciosamente.
ruta_csv <- file.path(RUTA_PROC, "tw_chile_social_crisis.csv")
tx <- cronometrar("Leyendo texto del CSV completo", {
  fread(ruta_csv,
        select = c("status_id", "screen_name", "retweet_screen_name",
                   "text", "created_at", "is_retweet", "retweet_count"),
        colClasses = list(character = "status_id"),
        showProgress = FALSE)
})

# Mismo cuidado que en 01b: parsear el ISO con formato explicito.
tx[, created_at := as.POSIXct(as.character(created_at),
                              format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
tx <- tx[as.Date(format(created_at, "%Y-%m-%d", tz = "America/Santiago")) == DIA]
message(sprintf("Textos del 18-O: %s", format(nrow(tx), big.mark = ".")))

# --- 2. Etiquetar por comunidad -------------------------------------------
com_de <- setNames(nodos$comunidad, nodos$cuenta)
tx[, autor := fifelse(is_retweet == TRUE & !is.na(retweet_screen_name) &
                        retweet_screen_name != "",
                      retweet_screen_name, screen_name)]
tx[, comunidad_autor  := com_de[autor]]
tx[, comunidad_emisor := com_de[screen_name]]

message(sprintf("Con comunidad asignada al autor: %s (%.1f%%)",
                format(sum(!is.na(tx$comunidad_autor)), big.mark = "."),
                100 * mean(!is.na(tx$comunidad_autor))))

write_parquet(tx[!is.na(comunidad_autor) | !is.na(comunidad_emisor)],
              file.path(RUTA_PROC, "corpus_18oct_comunidades.parquet"))

# --- 3. Cuentas fuertes ----------------------------------------------------
# Dos roles distintos: quien es amplificado y quien amplifica.
autoridades <- nodos[comunidad %in% top_com][order(-fuerza_in),
                                             head(.SD, 20L), by = comunidad][
  , .(comunidad, rol = "autoridad", cuenta,
      retweets_recibidos = fuerza_in, retweets_emitidos = outd,
      tweets_propios = tweets_emitidos)]

hubs <- nodos[comunidad %in% top_com][order(-outd),
                                      head(.SD, 20L), by = comunidad][
  , .(comunidad, rol = "hub", cuenta,
      retweets_recibidos = fuerza_in, retweets_emitidos = outd,
      tweets_propios = tweets_emitidos)]

fuertes <- rbind(autoridades, hubs)[order(comunidad, rol, -retweets_recibidos)]
fwrite(fuertes, file.path(RUTA_TAB, "18oct_cuentas_fuertes.csv"))
message("  tabla  -> 18oct_cuentas_fuertes.csv")

# --- 4. Corpus por comunidad ----------------------------------------------
# Se deduplica por texto: un tweet viral aparece miles de veces como retweet y
# sin deduplicar el "corpus" seria una sola frase repetida.
limpiar_rt <- function(x) sub("^RT @[A-Za-z0-9_]+: ", "", x)

for (k in top_com) {
  sub <- tx[comunidad_autor == k]
  if (!nrow(sub)) next
  textos <- sub[, .(veces = .N,
                    autor = autor[1],
                    primer_registro = min(created_at)),
                by = .(texto = limpiar_rt(text))][order(-veces)][1:min(N_TEXTOS, .N)]
  fwrite(textos, file.path(RUTA_TAB, sprintf("18oct_corpus_com_%d.csv", k)))
}
message(sprintf("  tablas -> 18oct_corpus_com_{%s}.csv",
                paste(top_com, collapse = ",")))

# --- 5. Palabras caracteristicas -------------------------------------------
# No las mas frecuentes (que serian las mismas en todas las comunidades) sino
# las mas SOBRERREPRESENTADAS: frecuencia relativa en la comunidad dividida por
# la frecuencia relativa en el resto del corpus del dia.
vacias <- unique(c(stopwords::stopwords("es"), stopwords::stopwords("en"),
                   "rt", "https", "t.co", "amp", "q", "si", "hoy", "ahora"))

unicos <- tx[!is.na(comunidad_autor) & comunidad_autor %in% top_com,
             .(veces = .N), by = .(comunidad = comunidad_autor,
                                   texto = limpiar_rt(text))]

tokens <- unicos[, {
  t <- tolower(texto)
  t <- gsub("https?://\\S+|@\\S+|[^[:alpha:][:space:]áéíóúñü#]", " ", t)
  list(palabra = unlist(strsplit(t, "\\s+")), veces = veces)
}, by = .(comunidad, id = seq_len(nrow(unicos)))]

tokens <- tokens[nchar(palabra) > 3 & !palabra %in% vacias]

# Se pondera por "veces": una palabra en un tweet retuiteado 5.000 veces pesa
# lo que circulo, no lo que se escribio una vez.
cuenta <- tokens[, .(n = sum(veces)), by = .(comunidad, palabra)]
cuenta[, total_com := sum(n), by = comunidad]
cuenta[, total_pal := sum(n), by = palabra]
N <- sum(cuenta$n)
# Suavizado de Laplace: sin el +1, toda palabra exclusiva de una comunidad da
# Inf (denominador cero) y el ranking se vuelve un empate arbitrario entre
# decenas de terminos. Con el +1, entre dos palabras exclusivas gana la que
# mas circulo, que es lo que interesa.
cuenta[, especificidad := ((n + 1) / (total_com + 1)) /
                          ((total_pal - n + 1) / (N - total_com + 1))]

caracteristicas <- cuenta[n >= 200][order(-especificidad),
                                    head(.SD, N_TOP), by = comunidad]
fwrite(caracteristicas[order(comunidad, -especificidad)],
       file.path(RUTA_TAB, "18oct_palabras_por_comunidad.csv"))
message("  tabla  -> 18oct_palabras_por_comunidad.csv")

hashtags <- cuenta[startsWith(palabra, "#") & n >= 100][
  order(-n), head(.SD, N_TOP), by = comunidad]
fwrite(hashtags[order(comunidad, -n)],
       file.path(RUTA_TAB, "18oct_hashtags_por_comunidad.csv"))
message("  tabla  -> 18oct_hashtags_por_comunidad.csv")

# --- 6. Figura -------------------------------------------------------------
caracteristicas[, etiqueta := paste0("Com. ", comunidad)]
caracteristicas[, etiqueta := factor(etiqueta,
                                     levels = paste0("Com. ", top_com))]
# Orden dentro de cada faceta: la misma palabra puede aparecer en dos
# comunidades, asi que el nivel del factor lleva la comunidad pegada y se
# recorta al momento de etiquetar.
caracteristicas[, orden := paste0(palabra, "__", comunidad)]
caracteristicas[, orden := factor(orden,
                                  levels = caracteristicas[order(comunidad, especificidad)]$orden)]

p <- ggplot(caracteristicas, aes(orden, especificidad)) +
  geom_col(fill = "#c1121f") +
  coord_flip() +
  facet_wrap(~ etiqueta, scales = "free", ncol = 4) +
  scale_x_discrete(labels = function(x) sub("__.*$", "", x)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Palabras características de cada comunidad, 18-O",
       subtitle = "sobrerrepresentación frente al resto del corpus del día (ponderada por circulación)",
       x = NULL, y = "veces más frecuente que en el resto")
ggsave(file.path(RUTA_FIG, "18oct_palabras_caracteristicas.png"), p,
       width = 16, height = 9, dpi = 150, bg = "white")
message("  figura -> 18oct_palabras_caracteristicas.png")

print(caracteristicas[, .(comunidad, palabra, n, esp = round(especificidad, 1))][
  , head(.SD, 8), by = comunidad])
