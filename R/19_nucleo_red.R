# ---------------------------------------------------------------------------
# 19_nucleo_red.R
#
# Exporta un nucleo legible de la red del 18-O para visualizacion interactiva.
#
# POR QUE UN NUCLEO. La red podada tiene 56.724 nodos. Un widget interactivo
# con esa cantidad no se renderiza en un navegador y, aunque lo hiciera, no se
# puede leer. Se conserva el subgrafo de las cuentas mas amplificadas, que es
# donde ocurre la estructura que interesa mostrar.
#
# LIMPIEZA. Se aplica el subconjunto recomendado de R/18: quedan fuera las
# comunidades 1, 2 y 8, que entraron por hashtag de fecha y no contienen
# vocabulario del estallido.
#
# UMBRAL Y NOMBRES. El corte en retweets recibidos no es solo tecnico. Las
# cuentas que superan el umbral son actores publicos de esa conversacion -se
# las retuiteo cientos de veces ese dia- y se las nombra, igual que en el resto
# del sitio. Las cuentas por debajo no aparecen.
#
# Salidas (versionadas, para que el sitio se reconstruya sin el corpus):
#   output/tablas/nucleo_nodos.csv
#   output/tablas/nucleo_aristas.csv
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({ library(arrow); library(igraph) })

MIN_RT   <- 100L   # retweets recibidos ese dia para entrar al nucleo
MIN_PESO <- 2L     # retweets entre dos cuentas para dibujar la arista
COM_FUERA <- c(1L, 2L, 8L)

g     <- readRDS(file.path(RUTA_PROC, "red_18oct.rds"))
nodos <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
cl    <- as.data.table(read_parquet(file.path(RUTA_PROC, "clasificacion_cuentas.parquet")))
pol   <- fread(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"), encoding = "UTF-8")

message(sprintf("Red completa: %s nodos", format(vcount(g), big.mark = ".")))

# --- 1. Limpieza y umbral ----------------------------------------------------
nodos <- nodos[!comunidad %in% COM_FUERA]
message(sprintf("Tras excluir las comunidades contaminadas: %s nodos",
                format(nrow(nodos), big.mark = ".")))

nucleo <- nodos[fuerza_in >= MIN_RT]
message(sprintf("Nucleo (>= %d retweets recibidos): %s cuentas",
                MIN_RT, format(nrow(nucleo), big.mark = ".")))

# --- 2. Atributos ------------------------------------------------------------
nucleo[, cl_lower := tolower(cuenta)]
cl[, cl_lower := tolower(cuenta)]
pol[, cl_lower := tolower(cuenta)]
nucleo[, clase := as.character(cl$clase[match(cl_lower, cl$cl_lower)])]
nucleo[is.na(clase), clase := "ciudadana"]
nucleo <- merge(nucleo, pol[, .(cl_lower, nombre_cargo = nombre, cargo, partido)],
                by = "cl_lower", all.x = TRUE)
nucleo[, es_cargo := !is.na(cargo)]

# Las comunidades se renumeran por tamano, para que "1" sea la mayor y los
# numeros del sitio no dependan del orden interno del algoritmo.
orden <- nucleo[, .N, by = comunidad][order(-N)]
orden[, com := .I]
nucleo[, com := orden$com[match(comunidad, orden$comunidad)]]

# --- 3. Proyeccion por audiencia compartida ---------------------------------
# Las autoridades casi no se retuitean entre si: la red de retweets es radial,
# miles de cuentas chicas apuntando a unas pocas grandes. Un subgrafo de solo
# autoridades queda practicamente vacio -151 de 431 nodos conectados-, asi que
# no sirve para mostrar estructura.
#
# La proyeccion por audiencia si la muestra: dos cuentas se conectan cuando las
# retuitea la misma gente, con peso igual al numero de cuentas que retuitearon
# a ambas. Es la lectura estandar para el espacio de elites en redes de
# retweets, y es lo que revela agrupamientos de publico.
suppressPackageStartupMessages(library(Matrix))

el <- as.data.table(as_data_frame(g, what = "edges"))
el <- el[to %in% nucleo$cuenta]

hubs <- unique(el$from); auts <- nucleo$cuenta
A <- sparseMatrix(i = match(el$from, hubs), j = match(el$to, auts),
                  x = 1, dims = c(length(hubs), length(auts)))
co <- as.matrix(Matrix::crossprod(A))      # autoridades x autoridades
diag(co) <- 0

# Umbral relativo: se conserva el vinculo si la audiencia compartida alcanza al
# menos el 8% de la audiencia de la cuenta menor del par. Un umbral absoluto
# favoreceria a las cuentas grandes solo por tamano.
aud <- colSums(A)
umbral <- outer(aud, aud, pmin) * 0.08
idx <- which(co >= umbral & co >= 20, arr.ind = TRUE)
idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]

el2 <- data.table(from = auts[idx[, 1]], to = auts[idx[, 2]],
                  peso = co[idx], 
                  jaccard = round(co[idx] / (aud[idx[, 1]] + aud[idx[, 2]] - co[idx]), 3))
message(sprintf("Vinculos por audiencia compartida: %s", format(nrow(el2), big.mark = ".")))

# Backbone: 16.857 vinculos sobre 429 nodos dan grado medio 78 y el dibujo se
# vuelve una mancha. Se conservan los K vinculos mas fuertes de cada cuenta
# -medidos por Jaccard, que corrige por tamano de audiencia- y se toma la
# union. Es la poda habitual para hacer legible una proyeccion densa.
K <- 6L
fuertes <- rbind(el2[, .(a = from, b = to, jaccard)],
                 el2[, .(a = to, b = from, jaccard)])
setorder(fuertes, a, -jaccard)
mantener <- unique(fuertes[, head(.SD, K), by = a][, .(par = paste(pmin(a, b), pmax(a, b)))])
el2[, par := paste(pmin(from, to), pmax(from, to))]
el2 <- el2[par %in% mantener$par][, par := NULL]
message(sprintf("Tras el backbone (K=%d por cuenta): %s vinculos",
                K, format(nrow(el2), big.mark = ".")))

conectadas <- unique(c(el2$from, el2$to))
nucleo <- nucleo[cuenta %in% conectadas]
message(sprintf("Nucleo final: %s cuentas, %s vinculos",
                format(nrow(nucleo), big.mark = "."), format(nrow(el2), big.mark = ".")))

fwrite(nucleo[order(-fuerza_in),
              .(cuenta, com, comunidad_original = comunidad, clase, es_cargo,
                nombre_cargo, cargo, partido, rt_recibidos = fuerza_in,
                rt_emitidos = outd, audiencia = aud[match(cuenta, auts)], x, y)],
       file.path(RUTA_TAB, "nucleo_nodos.csv"))
fwrite(el2[order(-peso)], file.path(RUTA_TAB, "nucleo_aristas.csv"))
message("  tablas -> nucleo_nodos.csv, nucleo_aristas.csv")

message("
== Composicion del nucleo ==")
print(nucleo[, .(cuentas = .N, rt = sum(fuerza_in)), by = .(com)][order(com)][1:8])
print(nucleo[, .N, by = clase][order(-N)])
message(sprintf("Cargos publicos en el nucleo: %d", nucleo[es_cargo == TRUE, .N]))
