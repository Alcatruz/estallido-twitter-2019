# ---------------------------------------------------------------------------
# 03_red_18oct.R
#
# Red de retweets del 18 de octubre de 2019 (hora de Chile).
#
# Reescritura del pipeline del curso de ARS (Clases 2 y 3) con los reemplazos
# que escalan y sin estado global:
#   - graph_from_data_frame() en vez de graph.empty()+add.vertices()+add.edges()
#     con atributos por indice posicional
#   - components() + which.max(csize) en vez de asumir que el componente
#     gigante es el numero 1
#   - cluster_leiden() en vez de walktrap.community()
#   - graphlayouts::layout_with_sparse_stress() en vez de layout_with_fr()
#   - aristas agregadas y ponderadas: un par (A retuitea a B) es UNA arista con
#     peso, no N aristas paralelas
#
# El script corre de cero y no depende de ningun .RData previo.
#
# Salidas:
#   data/processed/red_18oct.rds        grafo igraph con todos los atributos
#   data/processed/nodos_18oct.parquet  tabla de nodos (comunidad, grados, xy)
#   output/tablas/18oct_*.csv
#   output/figuras/18oct_*.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(igraph); library(graphlayouts)
  library(ggplot2); library(scales)
})

set.seed(SEMILLA)
theme_set(theme_minimal(base_size = 12))

DIA     <- as.Date("2019-10-18")
K_CORE  <- 2L    # se conservan los nodos con coreness >= K_CORE
N_PIVOT <- 150L  # pivotes del layout sparse-stress

fig <- function(p, nombre, w = 10, h = 8) {
  ruta <- file.path(RUTA_FIG, paste0("18oct_", nombre, ".png"))
  ggsave(ruta, p, width = w, height = h, dpi = 150, bg = "white")
  message("  figura -> ", basename(ruta))
}
tab <- function(x, nombre) {
  ruta <- file.path(RUTA_TAB, paste0("18oct_", nombre, ".csv"))
  fwrite(x, ruta)
  message("  tabla  -> ", basename(ruta))
}

# --- 1. Tweets del dia -----------------------------------------------------
d <- as.data.table(read_parquet(file.path(RUTA_PROC, "tabla_analisis.parquet")))
d <- d[as.Date(dia_cl) == DIA]
message(sprintf("18-oct-2019: %s tweets, %s cuentas activas",
                format(nrow(d), big.mark = "."),
                format(uniqueN(d$screen_name), big.mark = ".")))

# --- 2. Lista de aristas ponderada -----------------------------------------
# Direccion: quien retuitea -> quien es retuiteado. El "hub" emite, la
# "authority" recibe. Es la convencion del curso y la que hace que el
# in-degree se lea como "cuanto me amplifican".
rt <- d[is_retweet == TRUE &
        !is.na(retweet_screen_name) & retweet_screen_name != "" &
        !is.na(screen_name) & screen_name != ""]

rt[, latencia_min := as.numeric(difftime(created_at, retweet_created_at,
                                         units = "mins"))]

aristas <- rt[, .(peso = .N,
                  latencia_mediana = median(latencia_min[latencia_min >= 0],
                                            na.rm = TRUE),
                  # as.numeric(): median() sobre enteros devuelve integer con n
                  # impar y double con n par, y data.table exige tipo estable
                  hora_mediana = as.numeric(median(hora_cl))),
              by = .(hub = screen_name, authority = retweet_screen_name)]

message(sprintf("Retweets: %s | aristas unicas: %s",
                format(nrow(rt), big.mark = "."),
                format(nrow(aristas), big.mark = ".")))

# --- 3. Grafo --------------------------------------------------------------
g <- graph_from_data_frame(aristas, directed = TRUE)
message(sprintf("Red completa: %s nodos, %s aristas",
                format(vcount(g), big.mark = "."),
                format(ecount(g), big.mark = ".")))

# Volumen propio de cada cuenta (tweets emitidos ese dia), no solo su posicion
emitidos <- d[, .N, by = .(name = screen_name)]
V(g)$tweets_emitidos <- emitidos$N[match(V(g)$name, emitidos$name)]
V(g)$tweets_emitidos[is.na(V(g)$tweets_emitidos)] <- 0L

# --- 4. Poda ---------------------------------------------------------------
# k-core en vez del filtro por grado suelto: conserva el nucleo densamente
# conectado en vez de cualquier nodo que supere un umbral aislado.
V(g)$coreness <- coreness(g, mode = "all")
nucleo <- induced_subgraph(g, which(V(g)$coreness >= K_CORE))

# Componente gigante: se identifica por tamano, NO se asume que sea el #1.
comp <- components(nucleo, mode = "weak")
net  <- induced_subgraph(nucleo, which(comp$membership == which.max(comp$csize)))

message(sprintf("Tras k-core>=%d y componente gigante: %s nodos, %s aristas (%s%% de los nodos originales)",
                K_CORE,
                format(vcount(net), big.mark = "."),
                format(ecount(net), big.mark = "."),
                format(round(100 * vcount(net) / vcount(g), 1), decimal.mark = ",")))

V(net)$ind  <- degree(net, mode = "in")    # cuanto lo retuitean
V(net)$outd <- degree(net, mode = "out")   # cuanto retuitea
V(net)$fuerza_in <- strength(net, mode = "in", weights = E(net)$peso)

# --- 5. Comunidades --------------------------------------------------------
# Leiden sobre la version no dirigida: el objetivo de modularidad no usa
# direccion, y Leiden garantiza comunidades bien conectadas (Louvain no).
net_u <- as_undirected(net, mode = "collapse", edge.attr.comb = list(peso = "sum"))
com <- cluster_leiden(net_u, objective_function = "modularity",
                      weights = E(net_u)$peso, n_iterations = 5)
V(net)$membership <- membership(com)[match(V(net)$name, names(membership(com)))]

mod <- modularity(net_u, V(net)$membership[match(V(net_u)$name, V(net)$name)],
                  weights = E(net_u)$peso)
tam_com <- sort(table(V(net)$membership), decreasing = TRUE)
message(sprintf("Comunidades: %d (modularidad %.3f). Las 6 mayores: %s",
                length(tam_com), mod,
                paste(head(tam_com, 6), collapse = ", ")))

# --- 6. Layout -------------------------------------------------------------
# Sparse stress: O(n) por iteracion contra el O(n^2) de Fruchterman-Reingold,
# que a esta escala no converge.
l <- cronometrar("Calculando layout (sparse stress)",
                 layout_with_sparse_stress(net_u, pivots = N_PIVOT))
idx <- match(V(net)$name, V(net_u)$name)
V(net)$x <- l[idx, 1]
V(net)$y <- l[idx, 2]

nodos <- data.table(
  cuenta          = V(net)$name,
  comunidad       = V(net)$membership,
  ind             = V(net)$ind,
  outd            = V(net)$outd,
  fuerza_in       = V(net)$fuerza_in,
  tweets_emitidos = V(net)$tweets_emitidos,
  coreness        = V(net)$coreness,
  x = V(net)$x, y = V(net)$y
)
write_parquet(nodos, file.path(RUTA_PROC, "nodos_18oct.parquet"))
saveRDS(net, file.path(RUTA_PROC, "red_18oct.rds"))

# --- 7. Matriz de dialogo --------------------------------------------------
# La idea del Snippet 2 del curso, resuelta con un match por nombre en vez de
# la funcion merge.with.order: cada arista hereda la comunidad de su emisor y
# la de su receptor, y se tabula el cruce.
el <- as.data.table(as_data_frame(net, what = "edges"))
el[, com_hub  := V(net)$membership[match(from, V(net)$name)]]
el[, com_auth := V(net)$membership[match(to,   V(net)$name)]]

top_com <- as.integer(names(head(tam_com, 8)))
dial <- el[com_hub %in% top_com & com_auth %in% top_com,
           .(retweets = sum(peso)), by = .(com_hub, com_auth)]
dial[, share := retweets / sum(retweets), by = com_hub]

interna <- el[, .(dentro = sum(peso[com_hub == com_auth]), total = sum(peso))]
message(sprintf("Retweets dentro de la misma comunidad: %.1f%% (cruce entre comunidades: %.1f%%)",
                100 * interna$dentro / interna$total,
                100 * (1 - interna$dentro / interna$total)))

tab(dcast(dial, com_hub ~ com_auth, value.var = "retweets", fill = 0),
    "matriz_dialogo")

# --- 8. Figuras ------------------------------------------------------------
paleta <- c("#c1121f", "#003049", "#f4a261", "#2a9d8f", "#5c374c",
            "#e9c46a", "#457b9d", "#6a994e")
nodos[, com_plot := fifelse(comunidad %in% top_com,
                            as.character(comunidad), "otras")]
niveles <- c(as.character(top_com), "otras")
nodos[, com_plot := factor(com_plot, levels = niveles)]
colores <- setNames(c(paleta[seq_along(top_com)], "grey80"), niveles)

# Muestra de aristas: dibujar 451 mil segmentos satura la imagen y el archivo.
set.seed(SEMILLA)
seg <- el[sample(.N, min(40000L, .N))]
seg[, `:=`(x1 = V(net)$x[match(from, V(net)$name)],
           y1 = V(net)$y[match(from, V(net)$name)],
           x2 = V(net)$x[match(to,   V(net)$name)],
           y2 = V(net)$y[match(to,   V(net)$name)])]

p <- ggplot() +
  geom_segment(data = seg, aes(x1, y1, xend = x2, yend = y2),
               colour = "grey70", linewidth = 0.05, alpha = 0.12) +
  geom_point(data = nodos[order(ind)],
             aes(x, y, colour = com_plot, size = log1p(ind)), alpha = 0.75) +
  scale_colour_manual(values = colores, name = "comunidad") +
  scale_size_continuous(range = c(0.15, 5), guide = "none") +
  coord_equal() +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(title = "Red de retweets del 18 de octubre de 2019",
       subtitle = sprintf("%s cuentas, %s aristas ponderadas | Leiden, modularidad %.2f | tamano = in-degree",
                          format(vcount(net), big.mark = "."),
                          format(ecount(net), big.mark = "."), mod))
fig(p, "red", w = 11, h = 10)

p <- ggplot(dial, aes(factor(com_auth), factor(com_hub), fill = share)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(share > 0.02,
                               format(round(100 * share), nsmall = 0), "")),
            size = 3, colour = "grey15") +
  scale_fill_gradient(low = "#f8f9fa", high = "#c1121f",
                      labels = label_percent(), name = "% de los RT\nque emite la fila") +
  labs(title = "Quien retuitea a quien, entre comunidades",
       subtitle = "cada fila suma 100%: a que comunidades dirige sus retweets",
       x = "comunidad retuiteada (authority)",
       y = "comunidad que retuitea (hub)")
fig(p, "matriz_dialogo", w = 8, h = 6.5)

top_auth <- nodos[order(-fuerza_in)][1:30]
p <- ggplot(top_auth, aes(reorder(cuenta, fuerza_in), fuerza_in,
                          fill = factor(comunidad))) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = setNames(colores[niveles], niveles), name = "comunidad") +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  labs(title = "Las 30 cuentas mas retuiteadas del 18-O",
       x = NULL, y = "retweets recibidos ese dia")
fig(p, "top_autoridades", w = 9, h = 8)
tab(top_auth, "top_autoridades")

# Perfil horario por comunidad: quien se activa antes.
act <- el[com_hub %in% top_com,
          .(retweets = sum(peso)), by = .(comunidad = com_hub, hora = hora_mediana)]
act[, share := retweets / sum(retweets), by = comunidad]
p <- ggplot(act, aes(hora, share, colour = factor(comunidad))) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = setNames(colores[niveles], niveles), name = "comunidad") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(title = "Curva de activacion por comunidad",
       subtitle = "% de los retweets de cada comunidad emitidos en cada hora, 18-O",
       x = "hora local de Chile", y = NULL)
fig(p, "activacion_por_comunidad", w = 9, h = 5.5)

lat_com <- el[com_hub %in% top_com & is.finite(latencia_mediana),
              .(mediana_min = median(latencia_mediana),
                aristas = .N), by = .(comunidad = com_hub)][order(mediana_min)]
tab(lat_com, "latencia_por_comunidad")
print(lat_com)

# --- 9. Quienes son las comunidades ---------------------------------------
# Sin esto la deteccion de comunidades es un dibujo bonito: hay que nombrarlas.
# Se perfila cada una por ubicacion declarada, idioma y cuentas mas retuiteadas.
perfil <- d[, .(loc = tolower(trimws(location))[1], lang = lang[1]),
            by = .(cuenta = screen_name)]
nodos <- merge(nodos, perfil, by = "cuenta", all.x = TRUE)

pat_cl <- paste0("chile|santiago|valpara|concepci|antofagasta|temuco|iquique|",
                 "serena|rancagua|talca|puerto montt|arica|coquimbo|chill|",
                 "osorno|valdivia|calama|copiap|punta arenas|vi.a del mar")
pat_vz <- "venezuela|caracas|maracaibo|barquisimeto|maracay|valencia, ven"

comunidades <- nodos[comunidad %in% top_com, .(
  cuentas      = .N,
  pct_chile    = round(100 * mean(grepl(pat_cl, loc), na.rm = TRUE), 1),
  pct_vzla     = round(100 * mean(grepl(pat_vz, loc), na.rm = TRUE), 1),
  pct_sin_loc  = round(100 * mean(is.na(loc) | loc == ""), 1),
  pct_es       = round(100 * mean(lang == "es", na.rm = TRUE), 1),
  rt_recibidos = sum(fuerza_in),
  top_cuentas  = paste(cuenta[order(-fuerza_in)][1:5], collapse = ", ")
), by = comunidad][order(-cuentas)]
comunidades <- merge(comunidades, lat_com[, .(comunidad, latencia_mediana_min = mediana_min)],
                     by = "comunidad", all.x = TRUE)[order(-cuentas)]
tab(comunidades, "perfil_comunidades")
print(comunidades[, .(comunidad, cuentas, pct_chile, pct_vzla, pct_es,
                      latencia_mediana_min)])

resumen <- data.table(
  metrica = c("tweets del dia", "retweets", "nodos red completa", "aristas unicas",
              "nodos tras poda", "aristas tras poda", "comunidades", "modularidad",
              "% RT dentro de la comunidad"),
  valor = c(nrow(d), nrow(rt), vcount(g), nrow(aristas), vcount(net), ecount(net),
            length(tam_com), round(mod, 3),
            round(100 * interna$dentro / interna$total, 1))
)
tab(resumen, "resumen")
print(resumen)

message("\nRed guardada en data/processed/red_18oct.rds")
