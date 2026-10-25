# ---------------------------------------------------------------------------
# 18_subconjunto_limpio.R
#
# Define el subconjunto recomendado para analisis sustantivo del estallido y
# aporta la evidencia de que efectivamente es chileno.
#
# REGLA DE EXCLUSION, en dos partes:
#   (a) Nivel publicacion. Se excluye la que porta hashtag de fecha (#17oct a
#       #22Oct) y ningun termino del vocabulario chileno de la consulta. Es el
#       indicador de ingreso por consulta ambigua.
#   (b) Nivel cuenta. Se excluyen todas las publicaciones de cuentas que en la
#       red del 18 de octubre pertenecen a las comunidades 1, 2 y 8, cuyo
#       indice de contaminacion va de 89,9% a 100%.
#
# La regla (b) solo alcanza a cuentas presentes en la red de ese dia; la (a)
# opera sobre el corpus entero. Se aplican ambas porque capturan cosas
# distintas: (a) publicaciones sueltas que entraron por la puerta generica,
# (b) actores cuya participacion completa es ajena al tema.
#
# VALIDACION. Un subconjunto que se declara chileno tiene que mostrarlo. Se
# comparan corpus completo y subconjunto en seis indicadores independientes de
# la regla usada para construirlo: pais geolocalizado, ubicacion declarada,
# idioma, marcadores lexicos chilenos, marcadores venezolanos y composicion de
# hashtags.
#
# Salidas: data/processed/subconjunto_limpio.parquet   status_id + marca
#          output/tablas/subconjunto_validacion.csv
#          output/tablas/subconjunto_hashtags.csv
#          output/figuras/subconjunto_validacion.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(stringi); library(ggplot2); library(scales)
})
theme_set(theme_minimal(base_size = 11))

COM_CONTAMINADAS <- c(1L, 2L, 8L)

PAT_FECHA <- "#(1[0-9]|2[0-9]|[0-9])[[:space:]]?oct"
PAT_CHILE <- paste0("evasi|chiledespert|estopasaenchile|estonohaterminado|",
                    "chilesecans|chileresiste|chilesomostodos|noestamosenguerra|",
                    "pazparachile|toquedequeda|paronacional|cacerolazo|",
                    "marchadelmill|chalecosamarillos|apagalatele|",
                    "piñera|pinera|chadwick|milicos|metrosantiago|",
                    "chileencrisis|renunciapi|lamoneda|carabineros|indh")

# Indicadores de validacion. Deliberadamente NO usan los terminos de la regla
# de exclusion, para que la validacion no sea circular.
MARCA_CL <- paste0("we[oó]n|hue[oó]n|\\bwn\\b|\\bwea\\b|cachai|\\bpo\\b|\\bpoh\\b|",
                   "altiro|al tiro|\\bfome\\b|bac[aá]n|cu[aá]tico|\\bpacos?\\b|",
                   "carrete|la raja|\\bluca\\b|\\bcachar\\b|\\bpololo|micro\\b")
MARCA_VE <- paste0("venezuela|caracas|chavism|chavista|maduro|guaid[oó]|",
                   "bolivarian|maracaibo|psuv|telesur|cne\\b|zulia|t[aá]chira")
PAT_LUGAR_CL <- paste0("chile|santiago|valpara|concepci|antofagasta|temuco|iquique|",
                       "serena|rancagua|talca|puerto montt|arica|coquimbo|chill|",
                       "osorno|valdivia|calama|copiap|punta arenas|vi.a del mar")

a_utc <- function(x) {
  if (inherits(x, "POSIXct")) { setattr(x, "tzone", "UTC"); return(x) }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# --- 1. Datos ---------------------------------------------------------------
x <- cronometrar("Leyendo el corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("status_id", "created_at", "text", "screen_name",
                   "retweet_screen_name", "is_retweet", "lang", "country", "location"),
        colClasses = list(character = "status_id"), showProgress = FALSE)
})
x[, created_at := a_utc(created_at)]
tl <- tolower(x$text)

# --- 2. Regla de exclusion ---------------------------------------------------
x[, solo_fecha := stri_detect_regex(tl, PAT_FECHA) & !stri_detect_regex(tl, PAT_CHILE)]

nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
cuentas_mal <- tolower(nodos[comunidad %in% COM_CONTAMINADAS, cuenta])
x[, autor := fifelse(is_retweet == TRUE & !is.na(retweet_screen_name) &
                       retweet_screen_name != "", retweet_screen_name, screen_name)]
x[, cuenta_contaminada := tolower(autor) %in% cuentas_mal |
                          tolower(screen_name) %in% cuentas_mal]

x[, limpio := !solo_fecha & !cuenta_contaminada]

n <- nrow(x); nl <- sum(x$limpio)
message(sprintf("\nCorpus completo: %s | subconjunto limpio: %s (%.1f%%)",
                format(n, big.mark = "."), format(nl, big.mark = "."), 100 * nl / n))
message(sprintf("Excluidas por hashtag de fecha sin termino chileno: %s",
                format(sum(x$solo_fecha), big.mark = ".")))
message(sprintf("Excluidas por pertenecer a comunidad contaminada: %s",
                format(sum(x$cuenta_contaminada), big.mark = ".")))

# Las dos marcas se guardan por separado a proposito, para que quien reutilice
# el corpus pueda aplicar una, otra o ambas. Se solapan en 681.353
# publicaciones: aplicar solo (a) deja 85,8% del corpus, solo (b) deja 84,7%,
# y ambas 81,6%.
#
# SOBRE-EXCLUSION DECLARADA. La regla (b) retira TODA la actividad de las
# cuentas de las comunidades 1, 2 y 8, incluida la que si trata de Chile. La
# comunidad 1 son medios estatales venezolanos -teleSUR entre ellos- que
# cubrieron el estallido: su indice de contaminacion es 89,9%, no 100%. Por eso
# entre lo excluido quedan 66.680 usos de #toquedequeda y 50.569 de
# #ChileDesperto. Es cobertura extranjera del evento, no conversacion chilena,
# y se la trata como tal; quien prefiera conservarla aplica solo la regla (a).
write_parquet(x[, .(status_id, limpio, solo_fecha, cuenta_contaminada)],
              file.path(RUTA_PROC, "subconjunto_limpio.parquet"))

# --- 3. Validacion -----------------------------------------------------------
x[, `:=`(ml_cl = stri_detect_regex(tl, MARCA_CL),
         ml_ve = stri_detect_regex(tl, MARCA_VE),
         loc_cl = stri_detect_regex(tolower(location), PAT_LUGAR_CL),
         loc_ve = stri_detect_regex(tolower(location), MARCA_VE))]
rm(tl); invisible(gc())

ind <- function(d, etiqueta) {
  geo <- d[!is.na(country) & country != ""]
  con_loc <- d[!is.na(location) & location != ""]
  data.table(
    conjunto = etiqueta,
    publicaciones = nrow(d),
    `pais = Chile (geolocalizadas)` = round(100 * mean(geo$country == "Chile"), 1),
    `ubicacion declara Chile` = round(100 * mean(con_loc$loc_cl, na.rm = TRUE), 1),
    `ubicacion declara Venezuela` = round(100 * mean(con_loc$loc_ve, na.rm = TRUE), 1),
    `idioma español` = round(100 * mean(d$lang == "es", na.rm = TRUE), 1),
    `marcador lexico chileno` = round(100 * mean(d$ml_cl), 2),
    `marcador venezolano` = round(100 * mean(d$ml_ve), 2))
}

val <- rbind(ind(x, "Corpus completo"), ind(x[limpio == TRUE], "Subconjunto limpio"),
             ind(x[limpio == FALSE], "Excluido"))
fwrite(val, file.path(RUTA_TAB, "subconjunto_validacion.csv"))
message("\n== Validacion ==")
print(t(val))

# --- 4. Composicion de hashtags ---------------------------------------------
hs <- function(d, etiqueta) {
  h <- stri_extract_all_regex(d$text, "#[\\p{L}0-9_]+")
  t <- data.table(ht = tolower(unlist(h, use.names = FALSE)))[!is.na(ht)]
  t[, .N, by = ht][order(-N)][1:12][, conjunto := etiqueta][]
}
comp <- rbind(hs(x, "Corpus completo"), hs(x[limpio == TRUE], "Subconjunto limpio"),
              hs(x[limpio == FALSE], "Excluido"))
fwrite(comp, file.path(RUTA_TAB, "subconjunto_hashtags.csv"))
message("\n== Hashtags del material EXCLUIDO ==")
print(comp[conjunto == "Excluido"])

# --- 5. Figura ---------------------------------------------------------------
largo <- melt(val[, .(conjunto, `Ubicación declara Chile` = `ubicacion declara Chile`,
                      `Ubicación declara Venezuela` = `ubicacion declara Venezuela`,
                      `Marcador léxico chileno` = `marcador lexico chileno`,
                      `Marcador venezolano` = `marcador venezolano`)],
              id.vars = "conjunto", variable.name = "indicador", value.name = "pct")
largo[, conjunto := factor(conjunto,
        levels = c("Excluido", "Corpus completo", "Subconjunto limpio"))]

p <- ggplot(largo, aes(conjunto, pct, fill = conjunto)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = format(pct, decimal.mark = ",")), hjust = -0.18, size = 2.9) +
  coord_flip() +
  facet_wrap(~ indicador, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c(`Excluido` = "grey72", `Corpus completo` = "grey50",
                               `Subconjunto limpio` = "grey20")) +
  scale_y_continuous(expand = expansion(c(0, 0.28))) +
  labs(x = NULL, y = "% de las publicaciones",
       title = "El subconjunto limpio contra el material excluido",
       subtitle = "indicadores independientes de la regla con que se construyó la exclusión")
ggsave(file.path(RUTA_FIG, "subconjunto_validacion.png"), p,
       width = 9, height = 5.2, dpi = 150, bg = "white")
message("  figura -> subconjunto_validacion.png")
