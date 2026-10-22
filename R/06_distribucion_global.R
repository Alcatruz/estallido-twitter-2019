# ---------------------------------------------------------------------------
# 06_distribucion_global.R
#
# Distribucion de tweets y retweets en TODO el corpus (11 al 29 de octubre
# de 2019): por dia, por hora y por dia-hora.
#
# Salidas:
#   output/figuras/global_distribucion.png     lamina compuesta
#   output/figuras/global_composicion.png      % de retweets dia a dia
#   output/tablas/global_por_dia.csv
#   output/tablas/global_por_hora.csv
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(ggplot2); library(scales); library(patchwork)
})

theme_set(theme_minimal(base_size = 12))
col_tipo <- c(Original = "#c1121f", Cita = "#f4a261", Retweet = "#003049")

dt <- as.data.table(read_parquet(file.path(RUTA_PROC, "tabla_analisis.parquet")))
dt[, tipo := factor(tipo, levels = c("original", "cita", "retweet"),
                    labels = c("Original", "Cita", "Retweet"))]
n_total <- nrow(dt)

# --- Por dia ---------------------------------------------------------------
por_dia <- dt[, .N, by = .(dia = as.Date(dia_cl), tipo)]
ancho_dia <- dcast(por_dia, dia ~ tipo, value.var = "N", fill = 0)
ancho_dia[, Total := Original + Cita + Retweet]
ancho_dia[, pct_rt := 100 * Retweet / Total]
fwrite(ancho_dia, file.path(RUTA_TAB, "global_por_dia.csv"))

# --- Por hora --------------------------------------------------------------
por_hora <- dt[, .N, by = .(hora = hora_cl, tipo)]
ancho_hora <- dcast(por_hora, hora ~ tipo, value.var = "N", fill = 0)
ancho_hora[, Total := Original + Cita + Retweet]
ancho_hora[, pct_rt := 100 * Retweet / Total]
fwrite(ancho_hora, file.path(RUTA_TAB, "global_por_hora.csv"))

hitos <- data.table(
  dia = as.Date(c("2019-10-18", "2019-10-25", "2019-10-28")),
  etiqueta = c("18-O", "25-O", "28-O")
)

# --- Panel 1: volumen diario apilado --------------------------------------
p1 <- ggplot(por_dia, aes(dia, N, fill = tipo)) +
  geom_col(width = 0.85) +
  geom_vline(data = hitos, aes(xintercept = dia), linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_text(data = hitos, aes(x = dia, y = Inf, label = etiqueta),
            inherit.aes = FALSE, vjust = 1.4, hjust = -0.15,
            size = 3.2, colour = "grey25") +
  scale_fill_manual(values = col_tipo, name = NULL) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k",
                                           big.mark = ".")) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
  labs(x = NULL, y = "tweets por día",
       title = "Distribución de tweets y retweets, corpus completo",
       subtitle = sprintf("%s publicaciones entre el 11 y el 29 de octubre de 2019 (hora de Chile)",
                          format(n_total, big.mark = "."))) +
  theme(legend.position = "top")

# --- Panel 2: perfil horario ----------------------------------------------
p2 <- ggplot(por_hora, aes(hora, N, fill = tipo)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = col_tipo, guide = "none") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k",
                                           big.mark = ".")) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "hora local de Chile", y = "tweets acumulados",
       subtitle = "Perfil horario acumulado de los 19 días")

# --- Panel 3: proporcion de retweets --------------------------------------
# El apilado esconde la composicion: en volumen los retweets tapan todo. Esta
# vista pregunta otra cosa: que fraccion de lo publicado es amplificacion.
p3 <- ggplot(ancho_dia, aes(dia, pct_rt)) +
  geom_hline(yintercept = 100 * dt[tipo == "Retweet", .N] / n_total,
             linetype = "dotted", colour = "grey45") +
  geom_line(colour = "#003049", linewidth = 1) +
  geom_point(colour = "#003049", size = 2) +
  geom_vline(data = hitos, aes(xintercept = dia), linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(75, 95)) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
  labs(x = NULL, y = "% que son retweets",
       subtitle = "Cuánto se amplifica cada día (punteada: promedio del corpus)")

lamina <- p1 / (p2 | p3) + plot_layout(heights = c(1.15, 1))
ggsave(file.path(RUTA_FIG, "global_distribucion.png"), lamina,
       width = 13, height = 9.5, dpi = 150, bg = "white")
message("  figura -> global_distribucion.png")

# --- Consola ---------------------------------------------------------------
message("\n== Composicion del corpus ==")
print(dt[, .(tweets = .N), by = tipo][
  , pct := round(100 * tweets / n_total, 2)][order(-tweets)])

message("\n== Por dia ==")
print(ancho_dia[, .(dia = format(dia, "%a %d-%b"), Original, Cita, Retweet,
                    Total, `%RT` = round(pct_rt, 1))])

message("\n== Por hora ==")
print(ancho_hora[, .(hora = sprintf("%02d", hora), Original, Cita, Retweet,
                     Total, `%RT` = round(pct_rt, 1))])
