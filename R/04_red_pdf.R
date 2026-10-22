# ---------------------------------------------------------------------------
# 04_red_pdf.R
#
# Version vectorial (PDF) de la red del 18-O, con y sin etiquetas.
#
# Nota sobre el peso del archivo: el curso generaba PDFs de 290 MB porque
# dibujaba las ~400.000 aristas con curvatura. Aca se dibuja una muestra de
# aristas rectas y todos los nodos, lo que baja el archivo un orden de magnitud
# sin perder la estructura visible.
#
# Salidas:
#   output/figuras/18oct_red.pdf            red completa, sin etiquetas
#   output/figuras/18oct_red_etiquetas.pdf  con las cuentas mas retuiteadas
#   output/figuras/18oct_red_facetas.pdf    una lamina por comunidad
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(igraph); library(ggplot2); library(ggrepel); library(scales)
})

set.seed(SEMILLA)

N_ARISTAS  <- 60000L  # aristas dibujadas (muestra)
N_ETIQUETA <- 8L      # cuentas etiquetadas por comunidad

net   <- readRDS(file.path(RUTA_PROC, "red_18oct.rds"))
nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))

tam_com <- sort(table(nodos$comunidad), decreasing = TRUE)
top_com <- as.integer(names(head(tam_com, 8)))

paleta  <- c("#c1121f", "#003049", "#f4a261", "#2a9d8f", "#5c374c",
             "#e9c46a", "#457b9d", "#6a994e")
niveles <- c(as.character(top_com), "otras")
colores <- setNames(c(paleta[seq_along(top_com)], "grey80"), niveles)

nodos[, com_plot := factor(fifelse(comunidad %in% top_com,
                                   as.character(comunidad), "otras"),
                           levels = niveles)]

el <- as.data.table(as_data_frame(net, what = "edges"))
seg <- el[sample(.N, min(N_ARISTAS, .N))]
seg[, `:=`(x1 = nodos$x[match(from, nodos$cuenta)],
           y1 = nodos$y[match(from, nodos$cuenta)],
           x2 = nodos$x[match(to,   nodos$cuenta)],
           y2 = nodos$y[match(to,   nodos$cuenta)])]

base <- ggplot() +
  geom_segment(data = seg, aes(x1, y1, xend = x2, yend = y2),
               colour = "grey70", linewidth = 0.04, alpha = 0.10) +
  geom_point(data = nodos[order(ind)],
             aes(x, y, colour = com_plot, size = log1p(ind)), alpha = 0.8) +
  scale_colour_manual(values = colores, name = "comunidad") +
  scale_size_continuous(range = c(0.1, 6), guide = "none") +
  coord_equal() +
  theme_void(base_size = 14) +
  theme(legend.position = "bottom")

guardar_pdf <- function(p, nombre, w, h) {
  ruta <- file.path(RUTA_FIG, paste0(nombre, ".pdf"))
  ggsave(ruta, p, width = w, height = h, device = cairo_pdf, bg = "white")
  message(sprintf("  %s -> %s", basename(ruta), tam(file.size(ruta))))
}

# --- 1. Sin etiquetas ------------------------------------------------------
guardar_pdf(base + labs(title = "Red de retweets del 18 de octubre de 2019",
                        subtitle = sprintf("%s cuentas | %s aristas ponderadas | tamano = veces retuiteado",
                                           format(nrow(nodos), big.mark = "."),
                                           format(nrow(el), big.mark = "."))),
            "18oct_red", 16, 14)

# --- 2. Con etiquetas ------------------------------------------------------
etiquetas <- nodos[comunidad %in% top_com][order(-fuerza_in),
                                           head(.SD, N_ETIQUETA), by = comunidad]

guardar_pdf(
  base +
    geom_text_repel(data = etiquetas, aes(x, y, label = cuenta),
                    size = 2.6, max.overlaps = Inf, box.padding = 0.4,
                    segment.size = 0.2, segment.colour = "grey45",
                    min.segment.length = 0, colour = "grey10") +
    labs(title = "Red de retweets del 18-O: cuentas mas retuiteadas",
         subtitle = sprintf("las %d cuentas con mas retweets recibidos en cada una de las 8 comunidades mayores",
                            N_ETIQUETA)),
  "18oct_red_etiquetas", 20, 18)

# --- 3. Una lamina por comunidad -------------------------------------------
# Facetas: cada panel resalta una comunidad sobre el resto en gris. Deja ver
# si una comunidad ocupa una region compacta o esta repartida por toda la red.
faceta <- rbindlist(lapply(top_com, function(k) {
  copia <- copy(nodos)
  copia[, `:=`(panel = paste0("Comunidad ", k, "  (",
                              format(sum(nodos$comunidad == k), big.mark = "."), " cuentas)"),
               resalta = comunidad == k)]
  copia
}))
faceta[, panel := factor(panel, levels = unique(panel))]

p <- ggplot(faceta[order(resalta)], aes(x, y)) +
  geom_point(aes(colour = resalta, size = ifelse(resalta, log1p(ind), 0.3)),
             alpha = 0.7) +
  scale_colour_manual(values = c("TRUE" = "#c1121f", "FALSE" = "grey88"),
                      guide = "none") +
  scale_size_continuous(range = c(0.1, 4), guide = "none") +
  facet_wrap(~ panel, ncol = 4) +
  coord_equal() +
  theme_void(base_size = 12) +
  theme(strip.text = element_text(margin = margin(b = 6))) +
  labs(title = "Dónde vive cada comunidad en la red del 18-O")
guardar_pdf(p, "18oct_red_facetas", 18, 10)

message("\nPDFs listos en output/figuras/")
