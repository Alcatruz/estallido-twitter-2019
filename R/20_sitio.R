# ---------------------------------------------------------------------------
# 20_sitio.R
#
# Exporta los datos que lee el sitio de GitHub Pages (docs/).
#
# El sitio es HTML escrito a mano con Plotly, igual que los otros posts del
# autor. Este script produce los dos archivos de datos que las paginas cargan
# como script, leyendo solo las tablas versionadas de output/tablas y data/.
# Es decir: el sitio se reconstruye sin el corpus.
#
# Salidas:
#   docs/datos/datos.js  -> window.ES
#   docs/datos/red.js    -> window.ESRED  (nucleo con layout de la proyeccion)
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(jsonlite); library(igraph); library(graphlayouts)
})

RUTA_SITIO <- file.path(RUTA_PROY, "docs")
dir.create(file.path(RUTA_SITIO, "datos"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RUTA_SITIO, "figuras"), recursive = TRUE, showWarnings = FALSE)

t_ <- function(f, dir = RUTA_TAB) fread(file.path(dir, f), encoding = "UTF-8",
                                        na.strings = c("", "NA"))
r2 <- function(x, d = 1) round(as.numeric(x), d)

ES <- list()

# --- Series globales ---------------------------------------------------------
d <- t_("global_por_dia.csv")
ES$dia <- lapply(seq_len(nrow(d)), function(i) list(
  d = as.character(d$dia[i]), o = d$Original[i], c = d$Cita[i],
  r = d$Retweet[i], t = d$Total[i], pct = r2(d$pct_rt[i])))

h <- t_("global_por_hora.csv")
ES$hora <- lapply(seq_len(nrow(h)), function(i) list(
  h = h$hora[i], o = h$Original[i], c = h$Cita[i], r = h$Retweet[i],
  t = h$Total[i], pct = r2(h$pct_rt[i])))

# --- Recoleccion -------------------------------------------------------------
tr <- t_("validacion_reconstruccion.csv")
ES$terminos <- lapply(seq_len(nrow(tr)), function(i) list(
  paso = tr$paso[i], term = tr$termino[i], nuevos = tr$tweets_nuevos[i],
  acum = r2(tr$pct_acumulado[i], 2), fam = tr$familia[i],
  ok = isTRUE(tr$en_originales[i])))

cs <- t_("consultas_originales.csv", file.path(RUTA_PROY, "data", "externos"))
setorder(cs, consulta)
ES$consultas <- lapply(seq_len(nrow(cs)), function(i) list(
  q = cs$consulta[i], src = cs$origen[i]))

# --- Contagio y subconjunto --------------------------------------------------
cg <- t_("18oct_contagio_comunidades.csv")
setorder(cg, -pct_solo_fecha)
ES$contagio <- lapply(seq_len(nrow(cg)), function(i) list(
  com = cg$comunidad[i], tweets = cg$tweets[i],
  fecha = r2(cg$pct_con_fecha[i]), chile = r2(cg$pct_con_chile[i]),
  solo = r2(cg$pct_solo_fecha[i])))

sv <- t_("subconjunto_validacion.csv")
ES$validacion <- lapply(seq_len(nrow(sv)), function(i) list(
  conj = sv$conjunto[i], n = sv$publicaciones[i],
  geo = r2(sv[[3]][i]), ubcl = r2(sv[[4]][i]), ubve = r2(sv[[5]][i]),
  es = r2(sv[[6]][i]), lexcl = r2(sv[[7]][i]), lexve = r2(sv[[8]][i])))

# --- Polos -------------------------------------------------------------------
p <- t_("polo_por_dia.csv")
ES$polo <- lapply(seq_len(nrow(p)), function(i) list(
  d = as.character(p$dia[i]), t = p$total[i], pro = p$pro[i], anti = p$anti[i],
  pro10k = r2(p$pro_x10mil[i]), anti10k = r2(p$anti_x10mil[i]),
  razon = r2(p$razon[i], 0)))

rd <- t_("red_por_dia.csv")
ES$redDia <- lapply(seq_len(nrow(rd)), function(i) list(
  d = as.character(rd$dia[i]), tweets = rd$tweets[i], nodos = rd$nodos[i],
  aristas = rd$aristas[i], coms = rd$comunidades[i],
  mod = r2(rd$modularidad[i], 3), compro = rd$com_progobierno[i],
  nodospro = rd$nodos_progobierno[i], pctpro = r2(rd$pct_progobierno[i])))

# --- Clases y politicos ------------------------------------------------------
cl <- t_("clases_resumen.csv")
setorder(cl, -rt_recibidos)
ES$clases <- lapply(seq_len(nrow(cl)), function(i) list(
  clase = cl$clase[i], cuentas = cl$cuentas[i], tweets = cl$tweets_emitidos[i],
  rt = cl$rt_recibidos[i], pctc = r2(cl$pct_cuentas[i], 2),
  pctt = r2(cl$pct_tweets[i], 2), pctrt = r2(cl$pct_rt[i], 2),
  rtxc = r2(cl$rt_por_cuenta[i])))

rn <- t_("rendimiento_por_clase.csv")
setorder(rn, -circulacion_por_texto)
ES$rendimiento <- lapply(seq_len(nrow(rn)), function(i) list(
  grupo = rn$grupo[i], textos = rn$textos[i], circ = rn$circulacion[i],
  cxt = r2(rn$circulacion_por_texto[i])))

pol <- t_("cuentas_politicas.csv", file.path(RUTA_PROY, "data"))
cr <- pol[, .(n = .N), by = .(cargo)][order(-n)]
ES$cargos <- lapply(seq_len(nrow(cr)), function(i) list(
  cargo = cr$cargo[i], n = cr$n[i]))

dip <- t_("diputados_cuentas.csv")[en_ejercicio_oct2019 == TRUE]
ab <- c("Partido Comunista de Chile" = "PC",
        "Revolución Democrática" = "RD",
        "Convergencia Social" = "CS",
        "Partido Humanista" = "PH",
        "Partido Renovación Nacional" = "RN",
        "Partido Unión Demócrata Independiente" = "UDI",
        "Partido Evolución Política" = "Evópoli",
        "Partido Socialista de Chile" = "PS",
        "Partido por la Democracia" = "PPD",
        "Partido Demócrata Cristiano" = "DC")
dip[, P := fifelse(partido %in% names(ab), ab[partido], "otros / sin dato")]
pt <- dip[, .(dips = .N, corpus = sum(presente_corpus),
              rt = sum(rt_recibidos, na.rm = TRUE)), by = P][dips >= 4]
pt[, pctp := round(100 * corpus / dips)]
pt[, rtxd := round(rt / dips)]
setorder(pt, -rtxd)
ES$partidos <- lapply(seq_len(nrow(pt)), function(i) list(
  p = pt$P[i], dips = pt$dips[i], corpus = pt$corpus[i], rt = pt$rt[i],
  pctp = pt$pctp[i], rtxd = pt$rtxd[i]))

# --- Comunidades del 18-O ----------------------------------------------------
pf <- t_("18oct_perfil_comunidades.csv")
lt <- t_("18oct_latencia_por_comunidad.csv")
pf <- merge(pf, lt[, .(comunidad, lat = mediana_min)], by = "comunidad", all.x = TRUE)
setorder(pf, -cuentas)
ES$comunidades <- lapply(seq_len(nrow(pf)), function(i) list(
  com = pf$comunidad[i], cuentas = pf$cuentas[i], chile = r2(pf$pct_chile[i]),
  vzla = r2(pf$pct_vzla[i]), rt = pf$rt_recibidos[i],
  lat = r2(pf$lat[i], 0), top = pf$top_cuentas[i]))

md <- t_("18oct_matriz_dialogo.csv")
m <- as.matrix(md[, -1])
ES$dialogo <- list(coms = as.integer(names(md)[-1]), hubs = md$com_hub,
                   m = lapply(seq_len(nrow(m)), function(i)
                     as.numeric(r2(100 * m[i, ] / sum(m[i, ]), 1))))

au <- t_("18oct_top_autoridades.csv")
setorder(au, -fuerza_in)
ES$autoridades <- lapply(seq_len(min(20L, nrow(au))), function(i) list(
  cuenta = au$cuenta[i], com = au$comunidad[i], rt = au$fuerza_in[i],
  hubs = au$ind[i]))

# --- Vocabulario caracteristico ---------------------------------------------
vb <- t_("18oct_palabras_por_comunidad.csv")
setorder(vb, comunidad, -especificidad)
vb <- vb[, head(.SD, 8L), by = comunidad]
ES$vocab <- lapply(seq_len(nrow(vb)), function(i) list(
  com = vb$comunidad[i], w = vb$palabra[i], n = vb$n[i],
  esp = r2(vb$especificidad[i], 0)))

write(paste0("// Generado por R/20_sitio.R. No editar a mano.\nwindow.ES = ",
             toJSON(ES, auto_unbox = TRUE, digits = 4, na = "null"), ";"),
      file.path(RUTA_SITIO, "datos", "datos.js"))
message("docs/datos/datos.js escrito")

# --- El nucleo de la red, con layout de la proyeccion -----------------------
# Las coordenadas de nucleo_nodos.csv son del layout de la red de retweets.
# Para dibujar la proyeccion por audiencia hace falta su propio layout.
nd <- t_("nucleo_nodos.csv")
ar <- t_("nucleo_aristas.csv")

g <- graph_from_data_frame(ar[, .(from, to, weight = jaccard)],
                           directed = FALSE, vertices = nd$cuenta)
# El layout usa distancias de grafo, no el peso: en stress majorization
# los pesos se leen como distancias deseadas y el Jaccard es similitud, de
# modo que pasarlo separaria justamente a las cuentas mas parecidas.
xy <- layout_with_stress(g, weights = NA)
nd[, px := r2(xy[, 1], 3)][, py := r2(xy[, 2], 3)]

ESRED <- list(
  nodos = lapply(seq_len(nrow(nd)), function(i) list(
    c = nd$cuenta[i], com = nd$com[i], cl = substr(nd$clase[i], 1, 1),
    cargo = if (is.na(nd$cargo[i])) "" else nd$cargo[i],
    pa = if (is.na(nd$partido[i])) "" else nd$partido[i],
    rt = nd$rt_recibidos[i], aud = nd$audiencia[i],
    x = nd$px[i], y = nd$py[i])),
  aristas = lapply(seq_len(nrow(ar)), function(i) c(
    match(ar$from[i], nd$cuenta) - 1L, match(ar$to[i], nd$cuenta) - 1L,
    ar$jaccard[i])))

write(paste0("// Generado por R/20_sitio.R. No editar a mano.\nwindow.ESRED = ",
             toJSON(ESRED, auto_unbox = TRUE, digits = 4, na = "null"), ";"),
      file.path(RUTA_SITIO, "datos", "red.js"))
message(sprintf("docs/datos/red.js escrito (%d nodos, %d aristas)",
                nrow(nd), nrow(ar)))

# --- Figuras que el sitio usa como imagen -----------------------------------
# Solo las que no tienen sentido como grafico interactivo: la red completa del
# 18-O (56.724 nodos) y la lamina de las 16 redes diarias.
for (f in c("18oct_red.png", "red_por_dia.png",
            "18oct_palabras_caracteristicas.png")) {
  file.copy(file.path(RUTA_FIG, f), file.path(RUTA_SITIO, "figuras", f),
            overwrite = TRUE)
}
message("figuras copiadas a docs/figuras")
