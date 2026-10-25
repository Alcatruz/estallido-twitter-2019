# ---------------------------------------------------------------------------
# 17_red_por_dia.R
#
# La red de retweets dia a dia, y que pasa con el polo progobierno dentro de
# ella.
#
# R/15 concluyo que en la red podada del 18 de octubre no hay ningun cargo de
# Chile Vamos. R/16 mostro, por via lexica y de actores, que ese polo si
# aparece desde el 19 de octubre. Las dos cosas son compatibles: el polo existe
# en el corpus pero no sobrevive la poda de un solo dia.
#
# Aca se reconstruye la red de CADA dia y se mide, por comunidad, la
# composicion de polo del contenido que produce. La pregunta operativa es si
# existe en algun momento una comunidad progobierno reconocible, cuan grande
# es, y cuando desaparece.
#
# Una comunidad se clasifica por la via lexica de R/16: la fraccion de los
# tweets escritos por sus miembros que porta consignas de cada polo. No se usa
# el padron de cargos, que es demasiado escaso para clasificar comunidades.
#
# Salidas: output/tablas/red_por_dia.csv          una fila por dia
#          output/tablas/comunidades_por_dia.csv  una fila por comunidad y dia
#          output/figuras/red_por_dia.png         laminas de dias clave
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(igraph); library(graphlayouts); library(stringi)
  library(ggplot2); library(scales); library(patchwork)
})
set.seed(SEMILLA)
theme_set(theme_minimal(base_size = 10))

MIN_TWEETS <- 20000L   # dias con menos no dan red estable
DIAS_FIG   <- as.Date(c("2019-10-17", "2019-10-19", "2019-10-21",
                        "2019-10-23", "2019-10-26", "2019-10-28"))

PRO  <- "#yoapoyoalpresidente|#condenamosladelincuencia|#pazparachile|#nomasviolencia|#chilerespeta"
ANTI <- paste0("#renunciapi|#pi[nñ]erarenuncia|#pi[nñ]eradictador|#chadwickasesino|",
               "#quesevayanlosmilicos|#vayansemilicos|#carabinerosasesinos|",
               "#chileviolateshumanrights|#noestamosenguerra")

a_utc <- function(x) {
  if (inherits(x, "POSIXct")) { setattr(x, "tzone", "UTC"); return(x) }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

x <- cronometrar("Leyendo el corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("created_at", "text", "screen_name", "retweet_screen_name", "is_retweet"),
        showProgress = FALSE)
})
x[, created_at := a_utc(created_at)]
x[, dia := as.Date(format(created_at, "%Y-%m-%d", tz = "America/Santiago"))]
tl <- tolower(x$text)
x[, `:=`(pro = stri_detect_regex(tl, PRO), anti = stri_detect_regex(tl, ANTI))]
rm(tl); invisible(gc())

# El polo del AUTOR del contenido, no del que retuitea: asi la consigna se
# atribuye a quien la escribio.
x[, autor := fifelse(is_retweet == TRUE & !is.na(retweet_screen_name) &
                       retweet_screen_name != "", retweet_screen_name, screen_name)]

dias <- x[, .N, by = dia][N >= MIN_TWEETS][order(dia)]$dia
message(sprintf("Dias con al menos %s tweets: %d", format(MIN_TWEETS, big.mark = "."), length(dias)))

# --- Red de un dia -----------------------------------------------------------
red_del_dia <- function(d, con_layout = FALSE) {
  s <- x[dia == d & is_retweet == TRUE &
           !is.na(retweet_screen_name) & retweet_screen_name != "" &
           !is.na(screen_name) & screen_name != ""]
  if (nrow(s) < 5000L) return(NULL)

  ar <- s[, .(peso = .N), by = .(hub = screen_name, authority = retweet_screen_name)]
  g <- graph_from_data_frame(ar, directed = TRUE)
  V(g)$coreness <- coreness(g, mode = "all")
  g <- induced_subgraph(g, which(V(g)$coreness >= 2L))
  if (vcount(g) < 500L) return(NULL)
  comp <- components(g, mode = "weak")
  g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))

  gu <- as_undirected(g, mode = "collapse", edge.attr.comb = list(peso = "sum"))
  com <- cluster_leiden(gu, objective_function = "modularity",
                        weights = E(gu)$peso, n_iterations = 5)
  V(g)$com <- membership(com)[match(V(g)$name, names(membership(com)))]
  mod <- modularity(gu, V(g)$com[match(V(gu)$name, V(g)$name)], weights = E(gu)$peso)

  # Polo de cada nodo: consignas en lo que ESCRIBIO ese dia
  pol_nodo <- x[dia == d & autor %in% V(g)$name,
                .(pro = sum(pro), anti = sum(anti), n = .N), by = .(name = autor)]
  idx <- match(V(g)$name, pol_nodo$name)
  V(g)$pro  <- fifelse(is.na(idx), 0L, pol_nodo$pro[idx])
  V(g)$anti <- fifelse(is.na(idx), 0L, pol_nodo$anti[idx])

  if (con_layout) {
    l <- layout_with_sparse_stress(gu, pivots = 120L)
    i2 <- match(V(g)$name, V(gu)$name)
    V(g)$x <- l[i2, 1]; V(g)$y <- l[i2, 2]
  }
  list(g = g, mod = mod, tweets = nrow(s))
}

# --- 1. Serie diaria ---------------------------------------------------------
res <- list(); coms <- list()
for (d in as.list(dias)) {
  r <- cronometrar(sprintf("Red del %s", format(d, "%d-%b")),
                   red_del_dia(d, con_layout = d %in% DIAS_FIG))
  if (is.null(r)) next
  g <- r$g
  cm <- data.table(com = V(g)$com, pro = V(g)$pro, anti = V(g)$anti,
                   ind = degree(g, mode = "in"))[
    , .(nodos = .N, pro = sum(pro), anti = sum(anti), ind = sum(ind)), by = com]
  cm[, `:=`(dia = d, pct_nodos = round(100 * nodos / sum(nodos), 1),
            indice_pro = round(pro / pmax(pro + anti, 1), 3))]
  coms[[length(coms) + 1L]] <- cm

  # Una comunidad cuenta como progobierno si mas de la mitad de sus consignas
  # lo son y emite al menos 30, para no clasificar con dos tweets.
  pg <- cm[pro + anti >= 30L & indice_pro > 0.5]
  res[[length(res) + 1L]] <- data.table(
    dia = d, tweets = r$tweets, nodos = vcount(g), aristas = ecount(g),
    comunidades = uniqueN(V(g)$com), modularidad = round(r$mod, 3),
    com_progobierno = nrow(pg),
    nodos_progobierno = sum(pg$nodos),
    pct_progobierno = round(100 * sum(pg$nodos) / vcount(g), 2))
  if (d %in% DIAS_FIG) saveRDS(g, file.path(RUTA_PROC, sprintf("red_%s.rds", format(d, "%Y%m%d"))))
}
serie <- rbindlist(res); comdt <- rbindlist(coms)
fwrite(serie, file.path(RUTA_TAB, "red_por_dia.csv"))
fwrite(comdt, file.path(RUTA_TAB, "comunidades_por_dia.csv"))

message("\n== La red dia a dia ==")
print(serie)

# --- 2. Laminas de dias clave ------------------------------------------------
laminas <- lapply(DIAS_FIG, function(d) {
  f <- file.path(RUTA_PROC, sprintf("red_%s.rds", format(d, "%Y%m%d")))
  if (!file.exists(f)) return(NULL)
  g <- readRDS(f)
  nd <- data.table(x = V(g)$x, y = V(g)$y, ind = degree(g, mode = "in"),
                   pro = V(g)$pro, anti = V(g)$anti)
  nd[, polo := fifelse(pro > anti, "progobierno",
               fifelse(anti > pro, "antigobierno", "sin consigna"))]
  s <- serie[dia == d]
  ggplot(nd[order(polo == "progobierno")], aes(x, y)) +
    geom_point(aes(colour = polo, size = log1p(ind), alpha = polo)) +
    scale_colour_manual(values = c(`progobierno` = "grey10",
                                   `antigobierno` = "grey55",
                                   `sin consigna` = "grey88"), name = NULL) +
    scale_alpha_manual(values = c(`progobierno` = 1, `antigobierno` = .8,
                                  `sin consigna` = .35), guide = "none") +
    scale_size_continuous(range = c(0.15, 2.4), guide = "none") +
    coord_equal() + theme_void(base_size = 9) +
    theme(legend.position = "none", plot.subtitle = element_text(size = 8.5)) +
    labs(subtitle = sprintf("%s · %s nodos · %s%% progob.",
                            format(d, "%d-%b"), format(s$nodos, big.mark = "."),
                            format(s$pct_progobierno, decimal.mark = ",")))
})
laminas <- Filter(Negate(is.null), laminas)
if (length(laminas)) {
  ggsave(file.path(RUTA_FIG, "red_por_dia.png"),
         wrap_plots(laminas, nrow = 2) +
           plot_annotation(title = "La red de retweets, día a día",
                           subtitle = "en negro las cuentas que portan consignas progobierno; en gris medio las antigobierno"),
         width = 13, height = 8.5, dpi = 150, bg = "white")
  message("  figura -> red_por_dia.png")
}
