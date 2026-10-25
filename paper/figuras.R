# ---------------------------------------------------------------------------
# paper/figuras.R
#
# Figuras del data descriptor, en PDF vectorial y con un estilo unico.
#
# Se construyen desde las tablas versionadas en output/tablas/, no desde el
# corpus: el paper se recompila clonando el repositorio, sin los 7 GB. La
# unica excepcion es la lamina de la red, que no se puede regenerar sin los
# datos y se reutiliza desde output/figuras/.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales); library(patchwork)
})

TAB <- "../output/tablas"
SAL <- "fig"
dir.create(SAL, showWarnings = FALSE)
leer <- function(f) fread(file.path(TAB, f), encoding = "UTF-8", na.strings = c("", "NA"))

ROJO <- "#A4161A"; AZUL <- "#1B3A4B"; ARENA <- "#D9A066"; GRIS <- "grey60"

tema <- theme_minimal(base_size = 9, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
        axis.title = element_text(size = 8.5),
        plot.title = element_text(size = 9.5, face = "bold"),
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        strip.text = element_text(size = 8.5, face = "bold"),
        legend.key.size = unit(0.4, "cm"),
        legend.position = "top", legend.title = element_blank())
theme_set(tema)

guardar <- function(p, nombre, w, h) {
  ruta <- file.path(SAL, paste0(nombre, ".pdf"))
  ggsave(ruta, p, width = w, height = h, device = cairo_pdf)
  message(sprintf("  %s  (%.0f x %.0f mm)", basename(ruta), w * 25.4, h * 25.4))
}

# --- Figura 1. Cobertura temporal y composicion ------------------------------
dia <- leer("global_por_dia.csv")[, dia := as.Date(dia)]
largo <- melt(dia[, .(dia, Original, Cita, Retweet)], id.vars = "dia",
              variable.name = "tipo", value.name = "n")
largo[, tipo := factor(tipo, levels = c("Retweet", "Cita", "Original"))]

hitos <- data.table(dia = as.Date(c("2019-10-18", "2019-10-25", "2019-10-28")),
                    lab = c("18-O", "25-O", "28-O"))

p1a <- ggplot(largo, aes(dia, n / 1000, fill = tipo)) +
  geom_col(width = 0.82) +
  geom_vline(data = hitos, aes(xintercept = dia), linetype = "22",
             colour = "grey35", linewidth = 0.3) +
  geom_text(data = hitos, aes(x = dia, y = Inf, label = lab), inherit.aes = FALSE,
            vjust = 1.5, hjust = -0.18, size = 2.5, colour = "grey25") +
  scale_fill_manual(values = c(Retweet = AZUL, Cita = ARENA, Original = ROJO)) +
  scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
  labs(x = NULL, y = "miles de tweets", subtitle = "(a) Volumen diario por tipo")

p1b <- ggplot(dia, aes(dia, pct_rt)) +
  geom_hline(yintercept = 87.5, linetype = "dotted", colour = GRIS) +
  geom_line(colour = AZUL, linewidth = 0.5) +
  geom_point(colour = AZUL, size = 1.1) +
  scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
  coord_cartesian(ylim = c(80, 100)) +
  labs(x = NULL, y = "% retweets",
       subtitle = "(b) Proporcion de retweets (punteada: media del corpus, 87,5%)")

hora <- leer("global_por_hora.csv")
p1c <- ggplot(hora, aes(hora, Total / 1000)) +
  geom_col(fill = AZUL, width = 0.85) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(x = "hora local de Chile", y = "miles de tweets",
       subtitle = "(c) Perfil horario acumulado")

guardar(p1a / (p1b | p1c) + plot_layout(heights = c(1.25, 1)), "fig1_cobertura", 6.6, 5.0)

# --- Figura 2. Reconstruccion de la consulta y validacion --------------------
val <- leer("validacion_reconstruccion.csv")
val[, acierto := fifelse(en_originales, "consulta original", "no era consulta")]

p2a <- ggplot(val, aes(paso, pct_acumulado)) +
  geom_step(colour = AZUL, linewidth = 0.5) +
  geom_point(aes(colour = acierto), size = 1.5) +
  scale_colour_manual(values = c("consulta original" = AZUL, "no era consulta" = ROJO)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(x = "terminos acumulados", y = "cobertura del corpus",
       subtitle = "(a) Cobertura glotona: ganancia decreciente, sin corte limpio")

top <- val[1:18]
p2b <- ggplot(top, aes(reorder(termino, tweets_nuevos), tweets_nuevos / 1000,
                       fill = acierto)) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = c("consulta original" = AZUL, "no era consulta" = ROJO)) +
  labs(x = NULL, y = "miles de tweets que aporta",
       subtitle = "(b) Los 18 terminos de mayor aporte")

guardar(p2a / p2b + plot_layout(heights = c(1, 1.5)), "fig2_consulta", 6.6, 5.6)

# --- Figura 3. Indice de contagio --------------------------------------------
cg <- leer("18oct_contagio_comunidades.csv")
etq <- c(`8` = "8 · Guardia Nac. Bolivariana", `2` = "2 · medios opositores VE",
         `1` = "1 · medios estatales VE", `6` = "6 · mixta", `5` = "5 · prensa chilena",
         `11` = "11 · ciudadana CL", `10` = "10 · nucleo politico CL",
         `7` = "7 · fandom K-pop CL")
cg[, lab := etq[as.character(comunidad)]]
cg[, lab := factor(lab, levels = cg[order(pct_solo_fecha)]$lab)]

p3 <- ggplot(cg) +
  geom_col(aes(lab, pct_con_chile, fill = "con vocabulario del estallido"), width = 0.68) +
  geom_col(aes(lab, -pct_solo_fecha, fill = "solo hashtag de fecha"), width = 0.68) +
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.3) +
  geom_text(aes(lab, pct_con_chile, label = ifelse(pct_con_chile > 3, sprintf("%.0f%%", pct_con_chile), "")),
            hjust = -0.25, size = 2.4, colour = AZUL) +
  geom_text(aes(lab, -pct_solo_fecha, label = ifelse(pct_solo_fecha > 3, sprintf("%.0f%%", pct_solo_fecha), "")),
            hjust = 1.25, size = 2.4, colour = ROJO) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("con vocabulario del estallido" = AZUL,
                               "solo hashtag de fecha" = ROJO)) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-118, 118)) +
  labs(x = NULL, y = NULL,
       subtitle = "Por que puerta entro cada comunidad de la red del 18 de octubre")

guardar(p3, "fig3_contagio", 6.6, 3.1)

# --- Figura 4. Clases de cuenta ----------------------------------------------
cl <- leer("clases_resumen.csv")
rend <- leer("rendimiento_por_clase.csv")
cl[, clase := factor(clase, levels = c("politica", "medio", "ciudadana"))]

m <- melt(cl[, .(clase, `cuentas` = pct_cuentas, `tweets` = pct_tweets,
                 `retweets recibidos` = pct_rt)],
          id.vars = "clase", variable.name = "medida", value.name = "pct")
m[, medida := factor(medida, levels = c("cuentas", "tweets", "retweets recibidos"))]

p4a <- ggplot(m, aes(medida, pct, fill = clase)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = ifelse(pct >= 4, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5), size = 2.5, colour = "white") +
  scale_fill_manual(values = c(politica = ROJO, medio = ARENA, ciudadana = AZUL)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(x = NULL, y = NULL, subtitle = "(a) Participacion de cada clase")

rend[, grupo := factor(grupo, levels = c("politica", "medio", "ciudadana"))]
p4b <- ggplot(rend, aes(grupo, circulacion_por_texto, fill = grupo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = format(circulacion_por_texto, decimal.mark = ",")),
            vjust = -0.45, size = 2.6) +
  scale_fill_manual(values = c(politica = ROJO, medio = ARENA, ciudadana = AZUL)) +
  scale_y_continuous(expand = expansion(c(0, 0.16))) +
  labs(x = NULL, y = "retweets por texto unico",
       subtitle = "(b) Rendimiento: circulacion que obtiene cada texto")

guardar(p4a / p4b + plot_layout(heights = c(1, 1.15)), "fig4_clases", 6.6, 3.8)

message("\nFiguras generadas en paper/fig/")
