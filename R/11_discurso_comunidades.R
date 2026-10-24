# ---------------------------------------------------------------------------
# 11_discurso_comunidades.R
#
# Dos preguntas sobre el 18-O:
#
#   A. Comunidades contagiadas. Los hashtags de fecha (#17oct a #22oct) son
#      15,4% de la recoleccion (ver R/10) y los usa cualquiera en el mundo ese
#      dia. Que comunidades entraron al corpus por esa puerta y no por el
#      vocabulario del estallido? Se mide con un indice de contagio: la
#      fraccion de tweets de la comunidad que traen hashtag de fecha pero
#      NINGUN termino chileno de la consulta.
#
#   B. Discurso de la clase politica frente al resto. Los cargos electos son
#      125 cuentas de 702.103, pero reciben 956 retweets cada una. De que
#      hablan, y en que se diferencia de lo que dicen las comunidades?
#
# Toma prestado del taller de ARS de Calvo la idea de pasar atributos de los
# nodos a las aristas para leer la conversacion por comunidad (Snippet 2), pero
# resuelta con un match por nombre en vez de la funcion merge.with.order.
#
# Salidas: output/tablas/18oct_contagio_comunidades.csv
#          output/tablas/18oct_discurso_por_clase.csv
#          output/figuras/18oct_contagio.png
#          output/figuras/18oct_discurso_politica.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(stringi); library(ggplot2); library(scales)
})
theme_set(theme_minimal(base_size = 12))

# --- 0. Insumos -------------------------------------------------------------
tx <- as.data.table(read_parquet(file.path(RUTA_PROC, "corpus_18oct_comunidades.parquet")))
nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
pol <- fread(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"), encoding = "UTF-8")
pol[, cl := tolower(cuenta)]

top_com <- as.integer(names(head(sort(table(nodos$comunidad), decreasing = TRUE), 8)))
tx <- tx[comunidad_autor %in% top_com]
message(sprintf("Tweets del 18-O en las 8 comunidades mayores: %s",
                format(nrow(tx), big.mark = ".")))

# Deduplicar por texto: un tweet viral aparece miles de veces como retweet y sin
# esto el "discurso" de una comunidad seria una sola frase repetida.
limpiar_rt <- function(x) sub("^RT @[A-Za-z0-9_]+: ", "", x)
tx[, texto := limpiar_rt(text)]
tx[, es_politico := tolower(autor) %in% pol$cl]

# --- A. Indice de contagio ---------------------------------------------------
PAT_FECHA <- "#(1[0-9]|2[0-9]|[0-9])\\s?oct"
PAT_CHILE <- paste0("evasi|chiledespert|estopasaenchile|estonohaterminado|",
                    "chilesecans|chileresiste|chilesomostodos|noestamosenguerra|",
                    "pazparachile|toquedequeda|paronacional|cacerolazo|",
                    "marchadelmill|chalecosamarillos|apagalatele|",
                    "piñera|pinera|chadwick|milicos|metrosantiago|",
                    "chiledesperto|chileencrisis|renunciapi")

tx[, txt_l := tolower(texto)]
tx[, tiene_fecha := stri_detect_regex(txt_l, PAT_FECHA)]
tx[, tiene_chile := stri_detect_regex(txt_l, PAT_CHILE)]
# Solo por la puerta generica: trae hashtag de fecha y ningun termino chileno
tx[, solo_fecha := tiene_fecha & !tiene_chile]

contagio <- tx[, .(
  tweets        = .N,
  pct_con_fecha = round(100 * mean(tiene_fecha), 1),
  pct_con_chile = round(100 * mean(tiene_chile), 1),
  pct_solo_fecha = round(100 * mean(solo_fecha), 1),
  pct_sin_ninguno = round(100 * mean(!tiene_fecha & !tiene_chile), 1)
), by = .(comunidad = comunidad_autor)][order(-pct_solo_fecha)]
contagio[, indice_contagio := round(pct_solo_fecha / pmax(pct_con_chile, 0.1), 2)]

fwrite(contagio, file.path(RUTA_TAB, "18oct_contagio_comunidades.csv"))
message("\n== Indice de contagio por comunidad ==")
print(contagio)

p <- ggplot(contagio, aes(reorder(factor(comunidad), pct_solo_fecha))) +
  geom_col(aes(y = pct_con_chile, fill = "vocabulario del estallido"), width = 0.7) +
  geom_col(aes(y = -pct_solo_fecha, fill = "solo hashtag de fecha"), width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("vocabulario del estallido" = "#003049",
                               "solo hashtag de fecha" = "#c1121f"), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  theme(legend.position = "top") +
  labs(title = "Por que puerta entro cada comunidad al corpus",
       subtitle = "a la derecha, tweets con terminos chilenos de la consulta; a la izquierda,\ntweets que solo traen un hashtag de fecha (#17oct a #22oct)",
       x = "comunidad", y = NULL)
ggsave(file.path(RUTA_FIG, "18oct_contagio.png"), p, width = 9, height = 6, dpi = 150, bg = "white")
message("  figura -> 18oct_contagio.png")

# --- B. Discurso por clase, sobre el corpus completo -------------------------
# No se hace sobre el 18-O: ese dia la clase politica deja solo 54 textos
# unicos, y la mitad del "perfil lexico" saldria de un tweet de Jackson
# retuiteado 3.769 veces. Con los 19 dias hay volumen suficiente.
cl <- as.data.table(read_parquet(file.path(RUTA_PROC, "clasificacion_cuentas.parquet")))
clase_de <- setNames(as.character(cl$clase), tolower(cl$cuenta))

full <- cronometrar("Leyendo el corpus completo para el perfil lexico", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("text", "screen_name", "retweet_screen_name", "is_retweet"),
        showProgress = FALSE)
})
full[, autor := fifelse(is_retweet == TRUE & !is.na(retweet_screen_name) &
                          retweet_screen_name != "", retweet_screen_name, screen_name)]
full[, grupo := clase_de[tolower(autor)]]
full <- full[!is.na(grupo)]
full[, texto := limpiar_rt(text)]

unidades <- full[, .(veces = .N), by = .(grupo, texto)]
message("
== Textos unicos por clase (corpus completo) ==")
print(unidades[, .(textos_unicos = .N, circulacion = sum(veces)), by = grupo][order(-textos_unicos)])
rm(full); invisible(gc())

vacias <- unique(c(stopwords::stopwords("es"), stopwords::stopwords("en"),
                   "rt", "https", "t.co", "amp", "q", "si", "hoy", "ahora", "va",
                   "ser", "solo", "todos", "todo", "aqui", "asi", "mas"))

tok <- unidades[, {
  t <- tolower(texto)
  t <- gsub("https?://\\S+|@\\S+", " ", t)
  t <- gsub("[^[:alpha:][:space:]#]", " ", t)
  list(palabra = unlist(strsplit(t, "\\s+")), veces = veces)
}, by = .(grupo, id = seq_len(nrow(unidades)))]
tok <- tok[nchar(palabra) > 3 & !palabra %in% vacias]

cuenta <- tok[, .(n = sum(veces)), by = .(grupo, palabra)]
cuenta[, total_grupo := sum(n), by = grupo]
cuenta[, total_pal := sum(n), by = palabra]
N <- sum(cuenta$n)
# Suavizado de Laplace: sin el +1 toda palabra exclusiva da Inf y el ranking
# queda en un empate arbitrario.
cuenta[, especificidad := ((n + 1) / (total_grupo + 1)) /
                          ((total_pal - n + 1) / (N - total_grupo + 1))]

# ATENCION. Este perfil lexico NO es interpretable para la clase politica: son
# 451 textos unicos contra 665.994 de la ciudadania, asi que la especificidad
# premia rarezas de dos o tres tweets virales ("aferremos", "nietit") en vez de
# vocabulario caracteristico. Se deja calculado por comparabilidad, pero la
# medida que si aguanta con 451 textos es el rendimiento por texto de mas abajo.
carac <- cuenta[n >= 50][order(-especificidad), head(.SD, 15), by = grupo]
fwrite(carac[order(grupo, -especificidad)], file.path(RUTA_TAB, "discurso_por_clase.csv"))
message("  tabla  -> discurso_por_clase.csv")

message("
== Palabras caracteristicas por clase ==")
for (g in c("politica", "medio", "ciudadana"))
  print(carac[grupo == g, .(grupo, palabra, n, esp = round(especificidad, 1))][1:10])

# Cuanta circulacion logra cada texto segun quien lo escribe
rend <- unidades[, .(textos = .N, circulacion = sum(veces)), by = grupo]
rend[, circulacion_por_texto := round(circulacion / textos, 1)]
message("
== Rendimiento por texto ==")
print(rend[order(-circulacion_por_texto)])
fwrite(rend, file.path(RUTA_TAB, "rendimiento_por_clase.csv"))

plot_dat <- carac[grupo %in% c("politica", "medio", "ciudadana")]
plot_dat[, orden := factor(paste0(palabra, "__", grupo),
                           levels = plot_dat[order(grupo, especificidad)][
                             , paste0(palabra, "__", grupo)])]

p <- ggplot(plot_dat, aes(orden, especificidad, fill = grupo == "politica")) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ grupo, scales = "free", ncol = 3) +
  scale_x_discrete(labels = function(x) sub("__.*$", "", x)) +
  scale_fill_manual(values = c(`TRUE` = "#c1121f", `FALSE` = "#003049")) +
  labs(title = "De que habla cada clase de cuenta",
       subtitle = "sobrerrepresentacion frente a las otras clases, corpus completo, ponderada por circulacion",
       x = NULL, y = "veces mas frecuente que en el resto")
ggsave(file.path(RUTA_FIG, "discurso_por_clase.png"), p,
       width = 13, height = 6, dpi = 150, bg = "white")
message("  figura -> discurso_por_clase.png")

# --- Cuanto pesa la clase politica dentro de cada comunidad -------------------
peso <- tx[, .(tweets = .N,
               de_politicos = sum(es_politico),
               pct = round(100 * mean(es_politico), 2)), by = .(comunidad = comunidad_autor)]
message("\n== Peso de la clase politica dentro de cada comunidad ==")
print(peso[order(-pct)])
