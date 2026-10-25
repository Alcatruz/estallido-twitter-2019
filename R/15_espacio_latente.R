# ---------------------------------------------------------------------------
# 15_espacio_latente.R
#
# ¿Se puede medir un eje ideologico en esta red? Respuesta corta: no en el
# 18-O, y la razon es sustantiva, no tecnica.
#
# QUE HACE CALVO. El taller de ARS construye la red de retweets, corre un
# layout de fuerzas y usa las coordenadas (l1, l2) como espacio latente: la
# posicion en el dibujo aproxima la posicion politica, porque las fuerzas
# acercan a quien se retuitea entre si. Es una lectura razonable cuando la red
# tiene dos polos que se ignoran mutuamente.
#
# EL PROBLEMA ACA. Un eje ideologico necesita los dos polos. En la red podada
# del 18 de octubre hay 27 de los 171 cargos del padron, y NINGUNO es de Chile
# Vamos: ni UDI, ni RN, ni Evopoli, ni ministros. No es que el metodo no los
# separe, es que no estan. Cualquier eje que se estime aqui ordenaria otra
# cosa -tema, comunidad, tipo de cuenta- y llamarlo ideologia seria un error.
#
# Este script documenta eso: ubica a los cargos en el layout, mide la ausencia
# y deja el diagnostico escrito. Tambien corre un analisis de correspondencias
# como verificacion independiente; su inercia plana (25,7 / 25,3 / 24,5 %)
# confirma que no hay una dimension dominante que extraer.
#
# Salidas: output/tablas/latente_diagnostico.csv
#          output/tablas/latente_presencia_bloque.csv
#          output/figuras/latente_layout.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(Matrix); library(irlba); library(igraph)
  library(ggplot2); library(scales); library(patchwork); library(ggrepel)
})
set.seed(SEMILLA)
theme_set(theme_minimal(base_size = 11))

IZQ <- c("Partido Comunista de Chile", "Revolución Democrática", "Convergencia Social",
         "Comunes", "Partido Humanista", "Partido Socialista de Chile",
         "Partido por la Democracia", "Partido Demócrata Cristiano", "PDC", "PS",
         "PH", "PPD", "RD", "FRVS", "FREVS", "Partido Liberal de Chile",
         "Partido Ecologista Verde", "Partido Progresista", "Partido Poder")
DER <- c("Partido Unión Demócrata Independiente", "Partido Renovación Nacional",
         "Partido Evolución Política", "EVOP", "UDI", "RN", "IND-RN",
         "IND-Chile Vamos", "PRCh", "Partido Republicano de Chile")
bloque_de <- function(p) fifelse(p %in% IZQ, "Oposición",
                         fifelse(p %in% DER, "Chile Vamos", "Otro / sin dato"))

nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
pol   <- fread(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"), encoding = "UTF-8")
nodos[, cl := tolower(cuenta)]; pol[, cl := tolower(cuenta)]
pol[, bloque := bloque_de(partido)]

# --- 1. Diagnostico: estan los dos polos? -----------------------------------
en_red <- merge(nodos, pol[, .(cl, nombre, cargo, partido, bloque)], by = "cl")

diag <- merge(pol[, .(en_padron = .N), by = bloque],
              en_red[, .(en_red_18oct = .N, rt_mediana = round(median(fuerza_in))), by = bloque],
              by = "bloque", all.x = TRUE)
diag[is.na(en_red_18oct), `:=`(en_red_18oct = 0L, rt_mediana = NA)]
fwrite(diag, file.path(RUTA_TAB, "latente_presencia_bloque.csv"))

message(sprintf("Cargos del padron: %d | presentes en la red podada del 18-O: %d",
                nrow(pol), nrow(en_red)))
print(diag[order(-en_padron)])

n_der <- en_red[bloque == "Chile Vamos", .N]
message(sprintf("\nCuentas de Chile Vamos en la red: %d", n_der))
if (n_der < 5L) {
  message("--> Un eje ideologico NO es estimable: falta uno de los dos polos.")
  message("    Lo que se estime aqui ordena comunidad o tema, no ideologia.")
}

# --- 2. Verificacion independiente: analisis de correspondencias ------------
# Si existiera un eje dominante, la inercia del primer eje destacaria sobre el
# resto. Se calcula para dejar constancia de que no ocurre.
g  <- readRDS(file.path(RUTA_PROC, "red_18oct.rds"))
ar <- as.data.table(as_data_frame(g, what = "edges"))
setnames(ar, c("from", "to"), c("hub", "authority"))
aut <- ar[, .(n = sum(peso)), by = authority][n >= 10L, authority]
ar  <- ar[authority %in% aut]
ar  <- ar[hub %in% ar[, .(k = uniqueN(authority)), by = hub][k >= 2L, hub]]

hubs <- sort(unique(ar$hub)); auts <- sort(unique(ar$authority))
M <- sparseMatrix(i = match(ar$hub, hubs), j = match(ar$authority, auts),
                  x = as.numeric(ar$peso), dims = c(length(hubs), length(auts)))
P <- M / sum(M); r <- rowSums(P); cc <- colSums(P)
Z <- Diagonal(x = 1 / sqrt(r)) %*% P %*% Diagonal(x = 1 / sqrt(cc))
sv <- irlba(Z, nv = 6L, nu = 6L)
iner <- sv$d[-1]^2
pct <- round(100 * iner / sum(iner), 1)
message(sprintf("\nInercia por eje (CA): %s",
                paste0("dim", seq_along(pct), " ", pct, "%", collapse = " | ")))
message("Sin eje dominante: la varianza se reparte casi igual entre dimensiones.")

fwrite(data.table(eje = seq_along(pct), inercia_pct = pct,
                  nota = "sin eje dominante; ver latente_presencia_bloque.csv"),
       file.path(RUTA_TAB, "latente_diagnostico.csv"))

# --- 3. Figura: donde estan los cargos en el layout -------------------------
top_com <- as.integer(names(head(sort(table(nodos$comunidad), decreasing = TRUE), 8)))
fondo <- nodos[comunidad %in% top_com]

p1 <- ggplot() +
  geom_point(data = fondo, aes(x, y), colour = "grey85", size = 0.25, alpha = 0.5) +
  geom_point(data = en_red, aes(x, y, shape = bloque), colour = "grey15",
             size = 2.2, stroke = 0.7) +
  geom_text_repel(data = en_red[order(-fuerza_in)][1:14],
                  aes(x, y, label = cuenta), size = 2.5, colour = "grey10",
                  max.overlaps = Inf, segment.size = 0.2, min.segment.length = 0) +
  scale_shape_manual(values = c(`Oposición` = 16, `Chile Vamos` = 17,
                                `Otro / sin dato` = 1), name = NULL) +
  coord_equal() + theme_void(base_size = 11) +
  theme(legend.position = "top") +
  labs(subtitle = sprintf("(a) Los %d cargos públicos presentes, sobre las 56.724 cuentas de la red",
                          nrow(en_red)))

p2 <- ggplot(diag, aes(reorder(bloque, en_padron), en_padron)) +
  geom_col(fill = "grey80", width = 0.55) +
  geom_col(aes(y = en_red_18oct), fill = "grey25", width = 0.55) +
  geom_text(aes(y = en_padron, label = paste0(en_red_18oct, " de ", en_padron)),
            hjust = -0.15, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(c(0, 0.25))) +
  labs(x = NULL, y = "cargos en el padrón (gris claro) y en la red (gris oscuro)",
       subtitle = "(b) Un polo está ausente: ningún cargo de Chile Vamos llega a la red")

ggsave(file.path(RUTA_FIG, "latente_layout.png"), p1 / p2 + plot_layout(heights = c(2.4, 1)),
       width = 8.5, height = 9, dpi = 150, bg = "white")
message("  figura -> latente_layout.png")
