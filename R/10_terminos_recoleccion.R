# ---------------------------------------------------------------------------
# 10_terminos_recoleccion.R
#
# Reconstruye los terminos con que se recolecto el corpus. No venian
# documentados: tw_chile_social_crisis.rds trae las 90 columnas estandar de
# rtweet y ninguna dice por que consulta entro cada tweet.
#
# La pista vino del taller de ARS de Calvo, cuyos archivos (evasion.rds,
# evasion_masiva_todo_eldia.rds) SI tienen una columna 91 "consulta" con el
# termino buscado: "#EvasionMasiva" y "#EvasionMasivaTodoElDia". Pero esos dos
# archivos suman 830.723 tweets y este corpus tiene 6.119.933, asi que se
# recolecto con mas terminos.
#
# Metodo: si un corpus se arma siguiendo una lista de terminos, casi todo tweet
# tiene que contener al menos uno. Se extraen los hashtags de los 6,1 millones
# de textos y se resuelve una cobertura glotona: en cada paso se elige el
# hashtag que cubre mas tweets todavia sin cubrir. Los primeros terminos de esa
# secuencia son los candidatos a consulta; cuando la ganancia marginal se
# desploma, lo que sigue es vocabulario de la conversacion, no de la consulta.
#
# Salidas: output/tablas/terminos_recoleccion.csv   secuencia de cobertura
#          output/tablas/hashtags_corpus.csv        los 300 mas frecuentes
#          output/figuras/cobertura_terminos.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(stringi); library(ggplot2); library(scales)
})
theme_set(theme_minimal(base_size = 12))

N_PASOS <- 40L   # terminos de la secuencia glotona

# --- 1. Texto del corpus ----------------------------------------------------
tx <- cronometrar("Leyendo el texto del corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("text", "is_retweet"), showProgress = FALSE)
})
n_tw <- nrow(tx)
message(sprintf("Tweets: %s", format(n_tw, big.mark = ".")))

# --- 2. Hashtags por tweet --------------------------------------------------
# Se extraen del texto y no de la columna "hashtags" de rtweet porque en los
# retweets esa columna puede venir recortada, y el filtro de recoleccion actuo
# sobre el texto tal como se entrego.
hs <- cronometrar("Extrayendo hashtags", {
  stri_extract_all_regex(tx$text, "#[\\p{L}0-9_]+")
})
largo <- data.table(id = rep.int(seq_len(n_tw), lengths(hs)),
                    ht = tolower(unlist(hs, use.names = FALSE)))
largo <- largo[!is.na(ht)]
largo <- unique(largo, by = c("id", "ht"))   # un hashtag repetido en el mismo tweet cuenta una vez

con_ht <- uniqueN(largo$id)
message(sprintf("Tweets con al menos un hashtag: %s (%.1f%%)",
                format(con_ht, big.mark = "."), 100 * con_ht / n_tw))

frec <- largo[, .(tweets = .N), by = ht][order(-tweets)]
frec[, pct := round(100 * tweets / n_tw, 2)]
fwrite(head(frec, 300), file.path(RUTA_TAB, "hashtags_corpus.csv"))
message("  tabla  -> hashtags_corpus.csv")

# --- 3. Cobertura glotona ---------------------------------------------------
# En cada paso: cual es el hashtag presente en mas tweets aun no cubiertos.
restante <- copy(largo)
cubiertos <- 0L
pasos <- vector("list", N_PASOS)

for (k in seq_len(N_PASOS)) {
  if (!nrow(restante)) break
  conteo <- restante[, .N, by = ht][order(-N)]
  elegido <- conteo$ht[1]
  nuevos  <- conteo$N[1]
  ids <- restante[ht == elegido, unique(id)]
  restante <- restante[!id %in% ids]
  cubiertos <- cubiertos + nuevos
  pasos[[k]] <- data.table(paso = k, termino = elegido, tweets_nuevos = nuevos,
                           acumulado = cubiertos,
                           pct_acumulado = round(100 * cubiertos / n_tw, 2))
}
cob <- rbindlist(pasos)
cob[, ganancia_pct := round(100 * tweets_nuevos / n_tw, 2)]
fwrite(cob, file.path(RUTA_TAB, "terminos_recoleccion.csv"))
message("  tabla  -> terminos_recoleccion.csv")

message("\n== Secuencia de cobertura ==")
print(cob[1:25, .(paso, termino, tweets_nuevos, ganancia_pct, pct_acumulado)])

# --- 4. Familias de terminos -------------------------------------------------
# No hay un corte limpio entre "terminos de consulta" y "vocabulario": la
# ganancia marginal baja de forma gradual (11,2% el primero, 0,1% el que hace
# 40). Eso en si es el hallazgo: no se rastrearon dos o tres hashtags sino una
# lista larga, y por eso el corpus llega a 6,1 millones cuando los dos archivos
# del taller suman 830 mil.
familias <- list(
  fecha      = "^#[0-9]{1,2}(oct|o)$",
  evasion    = "evasion|evasi.n",
  ddhh       = "derechoshumanos|humanrights|ddhh|asesino|milicos|represion|violat",
  consigna   = "chiledesperto|chiledespert|estopasaenchile|estonohaterminado|chilesecanso|chilesecans|chileresiste|chilesomostodos|noestamosenguerra|pazparachile",
  gobierno   = "piñera|pinera|gabinete|chadwick|congreso|acusacion|acusaci.n|renuncia",
  movilizacion = "paronacional|marchadelmill|cacerolazo|toquedequeda|chalecosamarillos|apagalatele",
  ingles     = "protests|humanrights|chileprotest"
)
cob[, familia := "otro"]
for (f in rev(names(familias)))
  cob[grepl(familias[[f]], termino, perl = TRUE), familia := f]

message("
== Terminos por familia (sobre los 40 de la secuencia) ==")
fam <- cob[, .(terminos = .N, tweets = sum(tweets_nuevos),
               pct = round(100 * sum(tweets_nuevos) / n_tw, 1)), by = familia][order(-tweets)]
print(fam)
fwrite(cob, file.path(RUTA_TAB, "terminos_recoleccion.csv"))

# Los hashtags de fecha son la puerta por la que entro contenido no chileno:
# "#18Oct" lo usa cualquiera en el mundo ese dia.
fechas <- cob[familia == "fecha"]
message(sprintf("
Hashtags de fecha: %d terminos, %s tweets (%.1f%% del corpus)",
                nrow(fechas), format(sum(fechas$tweets_nuevos), big.mark = "."),
                100 * sum(fechas$tweets_nuevos) / n_tw))
message("  ", paste(fechas$termino, collapse = ", "))

sin_cubrir <- n_tw - cob[.N, acumulado]
message(sprintf("
Tweets sin ninguno de los %d hashtags: %s (%.1f%%). Son sobre todo",
                nrow(cob), format(sin_cubrir, big.mark = "."), 100 * sin_cubrir / n_tw))
message("  retweets cuyo texto llega truncado y respuestas dentro de un hilo ya capturado.")

# --- 5. Figura --------------------------------------------------------------
p <- ggplot(cob[1:min(25, .N)], aes(reorder(termino, pct_acumulado), pct_acumulado)) +
  geom_col(aes(fill = familia), width = 0.75) +
  geom_text(aes(label = sprintf("+%.1f", ganancia_pct)), hjust = -0.15, size = 3, colour = "grey30") +
  scale_fill_manual(values = c(fecha = "#c1121f", evasion = "#003049",
                               ddhh = "#5c374c", consigna = "#2a9d8f",
                               gobierno = "#f4a261", movilizacion = "#457b9d",
                               ingles = "#6a994e", otro = "grey75"), name = NULL) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(c(0, 0.12))) +
  theme(legend.position = "top") +
  labs(title = "Que terminos explican la existencia del corpus",
       subtitle = "cobertura acumulada; la etiqueta es cuanto aporta cada termino por si solo.
Los hashtags de fecha son la via de entrada de contenido no chileno.",
       x = NULL, y = "% del corpus cubierto")
ggsave(file.path(RUTA_FIG, "cobertura_terminos.png"), p,
       width = 10, height = 8, dpi = 150, bg = "white")
message("  figura -> cobertura_terminos.png")
