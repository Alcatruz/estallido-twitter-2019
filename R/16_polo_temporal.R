# ---------------------------------------------------------------------------
# 16_polo_temporal.R
#
# Cuando estuvo y cuando desaparecio el polo progobierno.
#
# R/15 mostro que en la red del 18 de octubre no hay ningun cargo de Chile
# Vamos, de modo que no se puede estimar un eje ideologico. Queda la pregunta
# de si ese polo existio antes y colapso, o si nunca estuvo en el corpus.
#
# Se mide por dos vias independientes:
#
#   (1) LEXICA. Volumen diario de hashtags de cada polo. Es la via mas robusta
#       porque no depende de identificar cuentas.
#   (2) ACTORES. Cuentas del padron de cada bloque que aparecen cada dia, sea
#       publicando o siendo retuiteadas, y los retweets que reciben.
#
# AMBIGUEDAD DECLARADA. #ChileSomosTodos (136.885 tweets) se reporta aparte y
# NO entra en el indice: fue usado por ambos lados como consigna de unidad, asi
# que contarlo como progobierno inflaria ese polo de forma arbitraria.
#
# Salidas: output/tablas/polo_por_dia.csv
#          output/tablas/polo_actores_por_dia.csv
#          output/figuras/polo_temporal.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(stringi); library(ggplot2); library(scales); library(patchwork)
})
theme_set(theme_minimal(base_size = 11))

# --- Lexico de polos (hashtags verificados en hashtags_corpus.csv) ----------
PRO  <- "#yoapoyoalpresidente|#condenamosladelincuencia|#pazparachile|#nomasviolencia|#chilerespeta"
ANTI <- paste0("#renunciapi|#pi[ñn]erarenuncia|#pi[ñn]eradictador|#chadwickasesino|",
               "#quesevayanlosmilicos|#vayansemilicos|#carabinerosasesinos|",
               "#chileviolateshumanrights|#noestamosenguerra")
AMBIG <- "#chilesomostodos"

a_utc <- function(x) {
  if (inherits(x, "POSIXct")) { setattr(x, "tzone", "UTC"); return(x) }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

x <- cronometrar("Leyendo el corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("created_at", "text", "screen_name", "retweet_screen_name",
                   "is_retweet"),
        showProgress = FALSE)
})
x[, created_at := a_utc(created_at)]
# as.Date() sobre POSIXct usa UTC: hay que formatear primero.
x[, dia := as.Date(format(created_at, "%Y-%m-%d", tz = "America/Santiago"))]
x[, hora := as.integer(format(created_at, "%H", tz = "America/Santiago"))]
x[, tl := tolower(text)]

# --- 1. Via lexica -----------------------------------------------------------
x[, `:=`(pro = stri_detect_regex(tl, PRO),
         anti = stri_detect_regex(tl, ANTI),
         ambig = stri_detect_regex(tl, AMBIG))]

dia <- x[, .(total = .N, pro = sum(pro), anti = sum(anti), ambig = sum(ambig)),
         by = dia][order(dia)]
dia[, `:=`(pro_x10mil = round(1e4 * pro / total, 1),
           anti_x10mil = round(1e4 * anti / total, 1),
           razon = round(anti / pmax(pro, 1), 1))]
fwrite(dia, file.path(RUTA_TAB, "polo_por_dia.csv"))

message("\n== Polos por dia (tasas por 10.000 tweets) ==")
print(dia[, .(dia, total, pro, anti, pro_x10mil, anti_x10mil,
              `anti:pro` = razon)])

message(sprintf("\nTotal corpus: progobierno %s (%.3f%%) | antigobierno %s (%.2f%%)",
                format(sum(dia$pro), big.mark = "."), 100 * sum(dia$pro) / sum(dia$total),
                format(sum(dia$anti), big.mark = "."), 100 * sum(dia$anti) / sum(dia$total)))
message(sprintf("#ChileSomosTodos (ambiguo, excluido del indice): %s",
                format(sum(dia$ambig), big.mark = ".")))

# --- 2. Via actores ----------------------------------------------------------
IZQ <- c("Partido Comunista de Chile", "Revolución Democrática", "Convergencia Social",
         "Comunes", "Partido Humanista", "Partido Socialista de Chile",
         "Partido por la Democracia", "Partido Demócrata Cristiano", "PDC", "PS",
         "PH", "PPD", "RD", "FRVS", "FREVS", "Partido Liberal de Chile",
         "Partido Ecologista Verde", "Partido Progresista", "Partido Poder")
DER <- c("Partido Unión Demócrata Independiente", "Partido Renovación Nacional",
         "Partido Evolución Política", "EVOP", "UDI", "RN", "IND-RN",
         "IND-Chile Vamos", "PRCh", "Partido Republicano de Chile")

pol <- fread(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"), encoding = "UTF-8")
pol[, cl := tolower(cuenta)]
pol[, bloque := fifelse(partido %in% IZQ, "Oposición",
                 fifelse(partido %in% DER, "Chile Vamos", "Otro"))]
bl <- setNames(pol$bloque, pol$cl)

x[, bl_emisor := bl[tolower(screen_name)]]
x[is_retweet == TRUE, bl_autor := bl[tolower(retweet_screen_name)]]
x[is_retweet == FALSE, bl_autor := bl_emisor]

# data.table no admite una constante dentro de `by` junto a columnas: la
# etiqueta `via` se agrega despues de agrupar.
a1 <- x[!is.na(bl_emisor), .(cuentas = uniqueN(screen_name)), by = .(dia, bloque = bl_emisor)]
a1[, via := "publica"]
a2 <- x[is_retweet == TRUE & !is.na(bl_autor),
        .(cuentas = uniqueN(retweet_screen_name)), by = .(dia, bloque = bl_autor)]
a2[, via := "es retuiteada"]
act <- rbind(a1, a2)
rt <- x[is_retweet == TRUE & !is.na(bl_autor), .(rt = .N), by = .(dia, bloque = bl_autor)]

actores <- merge(dcast(act, dia + bloque ~ via, value.var = "cuentas", fill = 0),
                 rt, by = c("dia", "bloque"), all = TRUE)
actores[is.na(actores)] <- 0L
fwrite(actores, file.path(RUTA_TAB, "polo_actores_por_dia.csv"))

message("\n== Cargos activos y retweets recibidos, por dia y bloque ==")
print(dcast(actores[bloque != "Otro"], dia ~ bloque,
            value.var = c("es retuiteada", "rt"), fill = 0))

# --- 3. Figura ---------------------------------------------------------------
largo <- melt(dia[, .(dia, `antigobierno` = anti_x10mil, `progobierno` = pro_x10mil)],
              id.vars = "dia", variable.name = "polo", value.name = "tasa")

p1 <- ggplot(largo, aes(dia, tasa, linetype = polo, shape = polo)) +
  geom_line(colour = "grey20", linewidth = 0.5) +
  geom_point(colour = "grey20", size = 1.8) +
  scale_linetype_manual(values = c(antigobierno = "solid", progobierno = "22"), name = NULL) +
  scale_shape_manual(values = c(antigobierno = 16, progobierno = 1), name = NULL) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
  scale_y_continuous(trans = "log1p", breaks = c(0, 3, 10, 30, 100, 300, 1000)) +
  labs(x = NULL, y = "tweets por 10.000 (escala log)",
       subtitle = "(a) Consignas de cada polo, corregidas por el volumen del día")

p2 <- ggplot(dia, aes(dia, razon)) +
  geom_col(fill = "grey35", width = 0.75) +
  geom_text(aes(label = razon), vjust = -0.4, size = 2.6, colour = "grey25") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
  scale_y_continuous(expand = expansion(c(0, 0.15))) +
  labs(x = NULL, y = "veces", subtitle = "(b) Cuántas consignas antigobierno por cada una progobierno")

ac <- actores[bloque != "Otro"]
p3 <- ggplot(ac, aes(dia, `es retuiteada`, fill = bloque)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_fill_manual(values = c(`Oposición` = "grey25", `Chile Vamos` = "grey72"), name = NULL) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
  labs(x = NULL, y = "cargos distintos", subtitle = "(c) Cargos públicos retuiteados cada día, por bloque")

ggsave(file.path(RUTA_FIG, "polo_temporal.png"), p1 / p2 / p3,
       width = 9.5, height = 9, dpi = 150, bg = "white")
message("  figura -> polo_temporal.png")
