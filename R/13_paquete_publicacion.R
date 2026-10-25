# ---------------------------------------------------------------------------
# 13_paquete_publicacion.R
#
# Arma el paquete de datos redistribuibles para deposito con DOI (Zenodo u OSF)
# y para un data descriptor.
#
# POR QUE NO SE PUBLICA EL CORPUS. Los terminos de servicio de X/Twitter
# prohiben redistribuir el contenido de los tweets y los campos de perfil. La
# practica aceptada es publicar los identificadores para que quien reutilice
# los "rehidrate" por la API. Desde que la API dejo de tener acceso gratuito
# eso es casi inservible en la practica, asi que se publican ademas DERIVADOS
# que no son contenido: conteos, estructura de red, frecuencias lexicas y
# registros de figuras publicas. El precedente chileno mas cercano
# (twConstitution, González-Bustamante 2023, DOI 10.17605/OSF.IO/73NDB)
# resolvio lo mismo con acceso bajo solicitud.
#
# SEUDONIMIZACION. Las figuras publicas que ejercian un cargo (el registro de
# R/09) conservan su identificador: son cargos publicos actuando en publico y
# la trazabilidad es parte del valor del dato. El resto de las cuentas se
# reemplaza por un hash estable, que preserva la estructura de la red sin
# exponer a personas particulares.
#
# Salidas (data/processed/publicacion/, fuera del repositorio por peso):
#   tweet_ids.txt.gz            los 6.119.933 identificadores
#   serie_temporal.csv          volumen por dia, hora y tipo
#   hashtags.csv                frecuencia de hashtags
#   red_retweets_18oct.csv      lista de aristas seudonimizada
#   cuentas.csv                 tabla de cuentas seudonimizada
#   codebook.md                 documentacion de variables
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({ library(arrow); library(digest); library(igraph) })

RUTA_PUB <- file.path(RUTA_PROC, "publicacion")
dir.create(RUTA_PUB, showWarnings = FALSE, recursive = TRUE)

SAL <- "estallido2019"   # sal del hash: sin ella el hash de un handle corto es reversible por fuerza bruta
seudo <- function(x) {
  u <- unique(tolower(x))
  mapa <- setNames(paste0("u", substr(vapply(paste0(SAL, u), digest, character(1),
                                             algo = "xxhash64"), 1, 12)), u)
  unname(mapa[tolower(x)])
}

a_utc <- function(x) {
  if (inherits(x, "POSIXct")) { setattr(x, "tzone", "UTC"); return(x) }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

pol <- fread(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"), encoding = "UTF-8")
publicas <- tolower(pol$cuenta)

# --- 1. Identificadores ------------------------------------------------------
x <- cronometrar("Leyendo el corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("status_id", "created_at", "screen_name", "retweet_screen_name",
                   "is_retweet", "is_quote", "lang", "source", "followers_count",
                   "account_created_at"),
        colClasses = list(character = c("status_id")), showProgress = FALSE)
})
x[, created_at := a_utc(created_at)]
x[, account_created_at := a_utc(account_created_at)]
x[, h := as.POSIXct(format(created_at, tz = "America/Santiago"), tz = "America/Santiago")]
# OJO: as.Date() sobre un POSIXct convierte usando UTC, NO el tzone del objeto.
# Usarlo aqui correria la ventana tres horas y se comeria las horas peak de la
# noche chilena. La fecha local se saca formateando.
x[, fecha_cl := as.Date(format(h, "%Y-%m-%d"))]

fwrite(x[, .(status_id)], file.path(RUTA_PUB, "tweet_ids.txt.gz"),
       col.names = FALSE, compress = "gzip")
message(sprintf("  tweet_ids.txt.gz -> %s identificadores (%s)",
                format(nrow(x), big.mark = "."),
                tam(file.size(file.path(RUTA_PUB, "tweet_ids.txt.gz")))))

# --- 2. Serie temporal -------------------------------------------------------
x[, tipo := fifelse(is_retweet, "retweet", fifelse(is_quote, "cita", "original"))]
serie <- x[, .(tweets = .N, cuentas = uniqueN(screen_name)),
           by = .(fecha = fecha_cl, hora = as.integer(format(h, "%H")), tipo)]
setorder(serie, fecha, hora, tipo)
fwrite(serie, file.path(RUTA_PUB, "serie_temporal.csv"))
message("  serie_temporal.csv -> ", nrow(serie), " filas")

# --- 3. Tabla de cuentas -----------------------------------------------------
# La antiguedad va en anio y no en fecha exacta: la fecha de creacion al segundo
# mas el conteo de seguidores identifica a una cuenta aunque se oculte el nombre.
cl <- as.data.table(read_parquet(file.path(RUTA_PROC, "clasificacion_cuentas.parquet")))

# El universo son TODAS las cuentas del corpus, no solo las que emitieron: una
# cuenta puede aparecer unicamente como retuiteada. Si el mapa de seudonimos se
# arma solo con emisoras, las aristas cuyo destino nunca tuiteo quedan en NA y
# se pierde un tercio de la red.
perfiles <- readRDS(file.path(RUTA_PROC, "perfiles.rds"))
emitidas <- x[, .(tweets = .N,
                  anio_cuenta = as.integer(format(min(account_created_at), "%Y")),
                  seguidores_orden = 10^floor(log10(pmax(median(followers_count, na.rm = TRUE), 1)))),
              by = .(cuenta = screen_name)]
emitidos <- merge(perfiles[, .(cuenta)], emitidas, by = "cuenta", all.x = TRUE)
emitidos[is.na(tweets), tweets := 0L]
emitidos[, clase := cl$clase[match(tolower(cuenta), tolower(cl$cuenta))]]
emitidos[, es_publica := tolower(cuenta) %in% publicas]
emitidos[, id := fifelse(es_publica, cuenta, seudo(cuenta))]
fwrite(emitidos[, .(id, es_publica, clase, tweets, anio_cuenta, seguidores_orden)],
       file.path(RUTA_PUB, "cuentas.csv"))
message(sprintf("  cuentas.csv -> %s cuentas (%d identificables por ser cargo publico)",
                format(nrow(emitidos), big.mark = "."), sum(emitidos$es_publica)))

# --- 4. Red de retweets del 18-O ---------------------------------------------
mapa <- setNames(emitidos$id, tolower(emitidos$cuenta))
rt <- x[fecha_cl == as.Date("2019-10-18") & is_retweet == TRUE &
          !is.na(retweet_screen_name) & retweet_screen_name != ""]
aristas <- rt[, .(peso = .N), by = .(hub = mapa[tolower(screen_name)],
                                     authority = mapa[tolower(retweet_screen_name)])]
aristas <- aristas[!is.na(hub) & !is.na(authority)]
nodos18 <- as.data.table(read_parquet(file.path(RUTA_PROC, "nodos_18oct.parquet")))
nodos18[, id := mapa[tolower(cuenta)]]
fwrite(aristas, file.path(RUTA_PUB, "red_retweets_18oct.csv"))
fwrite(nodos18[!is.na(id), .(id, comunidad, ind, outd, fuerza_in, x, y)],
       file.path(RUTA_PUB, "nodos_18oct.csv"))
message(sprintf("  red_retweets_18oct.csv -> %s aristas, %s nodos",
                format(nrow(aristas), big.mark = "."),
                format(nodos18[!is.na(id), .N], big.mark = ".")))

# --- 4b. Subconjunto limpio recomendado -------------------------------------
# Las marcas de R/18, para que quien reutilice el corpus pueda filtrar sin
# tener que replicar el criterio ni decidirlo.
f_sub <- file.path(RUTA_PROC, "subconjunto_limpio.parquet")
if (file.exists(f_sub)) {
  sub <- as.data.table(read_parquet(f_sub))
  fwrite(sub, file.path(RUTA_PUB, "subconjunto_limpio.csv.gz"), compress = "gzip")
  message(sprintf("  subconjunto_limpio.csv.gz -> %s publicaciones, %s en el subconjunto (%.1f%%)",
                  format(nrow(sub), big.mark = "."),
                  format(sum(sub$limpio), big.mark = "."),
                  100 * mean(sub$limpio)))
} else {
  message("  (falta subconjunto_limpio.parquet: correr R/18 antes)")
}

# --- 5. Copias de lo ya calculado -------------------------------------------
for (f in c("hashtags_corpus.csv", "terminos_recoleccion.csv",
            "18oct_contagio_comunidades.csv", "global_por_dia.csv",
            "global_por_hora.csv", "subconjunto_validacion.csv",
            "polo_por_dia.csv", "red_por_dia.csv"))
  file.copy(file.path(RUTA_TAB, f), file.path(RUTA_PUB, f), overwrite = TRUE)
file.copy(file.path(RUTA_PROY, "data", "cuentas_politicas.csv"),
          file.path(RUTA_PUB, "cuentas_politicas.csv"), overwrite = TRUE)
file.copy(file.path(RUTA_PROY, "data", "padron_diputados_2018_2022.csv"),
          file.path(RUTA_PUB, "padron_diputados_2018_2022.csv"), overwrite = TRUE)

message("\n== Paquete ==")
inv <- data.table(archivo = list.files(RUTA_PUB))
inv[, bytes := file.size(file.path(RUTA_PUB, archivo))]
inv[, peso := vapply(bytes, tam, character(1))]
print(inv[order(-bytes), .(archivo, peso)])
message(sprintf("\nTotal: %s", tam(sum(inv$bytes))))
