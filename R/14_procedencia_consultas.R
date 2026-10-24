# ---------------------------------------------------------------------------
# 14_procedencia_consultas.R
#
# Procedencia del corpus y validacion de la reconstruccion de R/10.
#
# El corpus lo recolecto Daniel Alcatruz en tiempo real entre el 17 y el 29 de
# octubre de 2019, con rtweet::search_tweets(), una consulta por archivo. Los
# archivos originales sobreviven en el proyecto tweets_estallido_social, junto
# con tt.R, que muestra el consolidado: a cada data.frame se le asignaba una
# columna "consulta" y despues un do.call(rbind, mget(...)) sobre el ambiente.
#
# Este script lee esos 53 archivos UNO A UNO -juntos son varios GB y no caben
# en memoria a la vez- y extrae de cada uno: filas, columnas, los valores de
# "consulta" y el rango de fechas. Con eso se arma la lista literal de
# consultas, que es el dato que faltaba para la seccion de metodos.
#
# Despues compara esa lista contra los 40 terminos que R/10 reconstruyo a
# ciegas, solo desde los hashtags del corpus. Esa comparacion es la validacion
# del metodo: si la reconstruccion acierta, sirve para cualquier corpus cuya
# consulta se haya perdido.
#
# Salidas: data/externos/recoleccion_original.csv
#          output/tablas/validacion_reconstruccion.csv
# ---------------------------------------------------------------------------

source("R/00_setup.R")

RUTA_ORIG <- "C:/Users/Daniel/Documents/safe/tweets/input"
if (!dir.exists(RUTA_ORIG)) stop("No encuentro los archivos originales en ", RUTA_ORIG)

archivos <- list.files(RUTA_ORIG, pattern = "\\.rds$", full.names = TRUE)
message(sprintf("Archivos originales: %d", length(archivos)))

# --- 1. Metadatos archivo por archivo ---------------------------------------
# Se acumulan tambien los status_id para saber si el corpus consolidado es la
# union deduplicada de estos archivos.
ids_vistos <- character(0)
RUTA_META <- file.path(RUTA_PROY, "data", "externos", "recoleccion_original.csv")
RUTA_IDS  <- file.path(RUTA_PROC, "ids_originales.rds")

# Leer los 53 originales toma varios minutos y son varios GB en disco. Si el
# resultado ya existe se reusa, para poder iterar sobre la validacion sin
# repetir la lectura.
if (file.exists(RUTA_META) && file.exists(RUTA_IDS)) {
  meta <- fread(RUTA_META, encoding = "UTF-8", na.strings = c("", "NA"))
  ids_vistos <- readRDS(RUTA_IDS)
  message("Metadatos cacheados: no se releen los archivos originales.")
} else {

meta <- rbindlist(lapply(seq_along(archivos), function(i) {
  f <- archivos[i]
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) {
    message(sprintf("  [%2d/%d] %-34s vacio o ilegible", i, length(archivos), basename(f)))
    return(data.table(archivo = basename(f), filas = 0L, columnas = NA_integer_,
                      consulta = NA_character_, desde = as.POSIXct(NA), hasta = as.POSIXct(NA)))
  }
  setDT(d)
  cons <- if ("consulta" %in% names(d)) paste(sort(unique(as.character(d$consulta))), collapse = " | ")
          else NA_character_
  fechas <- if ("created_at" %in% names(d)) range(as.POSIXct(d$created_at), na.rm = TRUE)
            else c(as.POSIXct(NA), as.POSIXct(NA))
  if ("status_id" %in% names(d))
    ids_vistos <<- unique(c(ids_vistos, as.character(d$status_id)))

  message(sprintf("  [%2d/%d] %-34s %9s filas  consulta: %s", i, length(archivos),
                  basename(f), format(nrow(d), big.mark = "."),
                  if (is.na(cons)) "(sin columna)" else substr(cons, 1, 40)))
  out <- data.table(archivo = basename(f), filas = nrow(d), columnas = ncol(d),
                    consulta = cons, desde = fechas[1], hasta = fechas[2])
  rm(d); invisible(gc())
  out
}))

meta[, termino := fifelse(!is.na(consulta), consulta, sub("\\.rds$", "", archivo))]
fwrite(meta, RUTA_META)
saveRDS(ids_vistos, RUTA_IDS)
}
message(sprintf("\nFilas sumadas en los originales: %s | status_id unicos: %s",
                format(sum(meta$filas), big.mark = "."),
                format(length(ids_vistos), big.mark = ".")))

# --- 2. El corpus consolidado, es la union de estos archivos? ---------------
ids_corpus <- fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
                    select = "status_id", colClasses = "character",
                    showProgress = FALSE)$status_id
en_ambos <- sum(ids_corpus %in% ids_vistos)
message(sprintf("\nDel corpus (%s tweets), estan en los originales: %s (%.1f%%)",
                format(length(ids_corpus), big.mark = "."),
                format(en_ambos, big.mark = "."), 100 * en_ambos / length(ids_corpus)))
# OJO: esto NO son duplicados. La deduplicacion ya ocurrio antes (15.185.311
# filas en los originales -> 6.714.876 status_id unicos). Lo que sigue es un
# filtro ADICIONAL aplicado al consolidar. La explicacion mas probable es la
# ventana temporal: varios originales empiezan el 10 de octubre y el corpus
# arranca el 11.
message(sprintf("Unicos en los originales que NO estan en el corpus: %s (%.1f%%) -> filtro adicional al consolidar",
                format(length(ids_vistos) - en_ambos, big.mark = "."),
                100 * (length(ids_vistos) - en_ambos) / length(ids_vistos)))

# --- 3. Validacion de la reconstruccion -------------------------------------
# La lista autorizada son los valores de la columna "consulta", NO los nombres
# de archivo: el autor los abrevio a mano y no coinciden. "cuartoOCT.rds"
# contiene #21Oct y "quintoocutbre.rds" contiene #22Oct. Tres archivos son a su
# vez consolidados y traen varias consultas separadas por " | ".
norm <- function(x) {
  x <- stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
  gsub("[^a-z0-9]", "", x)
}

consultas <- unique(trimws(unlist(strsplit(meta[!is.na(consulta), consulta], "|", fixed = TRUE))))
consultas <- consultas[nzchar(consultas)]
# Los archivos sin columna "consulta" quedaron sin etiquetar en tt.R: se usa el
# nombre de archivo, que para esos casos es lo unico que hay.
sin_etiqueta <- meta[is.na(consulta) & filas > 0, sub("\\.rds$", "", archivo)]

lista <- data.table(consulta = c(consultas, sin_etiqueta),
                    origen = c(rep("columna consulta", length(consultas)),
                               rep("nombre de archivo", length(sin_etiqueta))))
lista[, clave := norm(consulta)]
lista <- unique(lista, by = "clave")
fwrite(lista[order(origen, consulta)],
       file.path(RUTA_PROY, "data", "externos", "consultas_originales.csv"))
message(sprintf("
Consultas originales distintas: %d (%d etiquetadas, %d por nombre de archivo)",
                nrow(lista), sum(lista$origen == "columna consulta"),
                sum(lista$origen == "nombre de archivo")))
print(lista[origen == "columna consulta"][order(consulta), consulta])

rec <- fread(file.path(RUTA_TAB, "terminos_recoleccion.csv"))
rec[, clave := norm(termino)]
rec[, en_originales := clave %in% lista$clave]

val <- rec[, .(paso, termino, tweets_nuevos, pct_acumulado, familia, en_originales)]
fwrite(val, file.path(RUTA_TAB, "validacion_reconstruccion.csv"))
message(sprintf("
== Validacion: %d de %d terminos reconstruidos son consulta original (%.0f%%) ==",
                sum(val$en_originales), nrow(val), 100 * mean(val$en_originales)))
message("Cobertura del corpus que explican los terminos acertados: ",
        sprintf("%.1f%%", rec[en_originales == TRUE, sum(tweets_nuevos)] / length(ids_corpus) * 100))
message("
Reconstruidos que NO son consulta original:")
print(val[en_originales == FALSE, .(termino, tweets_nuevos, familia)])

message("
Consultas originales que la reconstruccion no recupero:")
lista[, recuperada := clave %in% rec$clave]
faltan <- merge(lista[recuperada == FALSE], meta[, .(clave = norm(termino), filas)],
                by = "clave", all.x = TRUE)
print(faltan[order(-filas), .(consulta, origen, filas)])
