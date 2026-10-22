# ---------------------------------------------------------------------------
# 01_rds_a_csv.R
#
# Lee tw_chile_social_crisis.rds (~1.4 GB comprimido), lo describe, lo exporta
# a CSV y saca una muestra aleatoria manejable.
#
# Todo ocurre en UNA sola lectura del .rds: descomprimirlo toma varios minutos
# y ocupa varios GB de RAM, asi que no conviene volver a leerlo para muestrear.
#
# Salidas:
#   data/processed/tw_chile_social_crisis.csv      (tabla completa)
#   data/processed/muestra_200k.csv                (muestra aleatoria)
#   data/processed/muestra_200k.rds               (misma muestra, carga rapida)
#   output/tablas/estructura_datos.txt            (reporte de estructura)
# ---------------------------------------------------------------------------

source("R/00_setup.R")

message("Archivo fuente: ", ARCHIVO_RDS)
message("Tamano en disco: ", tam(file.size(ARCHIVO_RDS)))

# --- 1. Leer el .rds -------------------------------------------------------
datos <- cronometrar("Leyendo el .rds (paciencia, son varios GB)", {
  readRDS(ARCHIVO_RDS)
})

message("Clase del objeto: ", paste(class(datos), collapse = ", "))
message("Memoria ocupada : ", tam(as.numeric(object.size(datos))))

# Si no es una tabla rectangular, describimos y paramos: hay que decidir que
# elemento exportar antes de seguir.
if (!is.data.frame(datos)) {
  message("\nEl objeto NO es un data.frame. Estructura de primer nivel:")
  print(utils::str(datos, max.level = 2, list.len = 30))
  stop("Revisa la estructura de arriba y elige que componente exportar.")
}

setDT(datos)   # convertir a data.table por referencia (sin copiar)

message(sprintf("Dimensiones: %s filas x %d columnas",
                format(nrow(datos), big.mark = "."), ncol(datos)))

# --- 2. Reporte de estructura ---------------------------------------------
estructura <- data.table(
  columna   = names(datos),
  tipo      = vapply(datos, function(x) paste(class(x), collapse = "/"), character(1)),
  n_na      = vapply(datos, function(x) sum(is.na(x)), numeric(1)),
  n_unicos  = vapply(datos, function(x) if (is.list(x)) NA_real_ else uniqueN(x), numeric(1)),
  ejemplo   = vapply(datos, function(x) {
    v <- x[!is.na(x)]
    if (!length(v)) return(NA_character_)
    v <- if (is.list(v)) v[[1]] else v[1]
    substr(paste(as.character(v), collapse = "|"), 1, 60)
  }, character(1))
)
estructura[, pct_na := round(100 * n_na / nrow(datos), 2)]
setcolorder(estructura, c("columna", "tipo", "n_na", "pct_na", "n_unicos", "ejemplo"))

print(estructura)

ruta_estructura <- file.path(RUTA_TAB, "estructura_datos.txt")
capture.output({
  cat("Archivo :", ARCHIVO_RDS, "\n")
  cat("Filas   :", format(nrow(datos), big.mark = "."), "\n")
  cat("Columnas:", ncol(datos), "\n")
  cat("Memoria :", tam(as.numeric(object.size(datos))), "\n\n")
  print(estructura, nrows = 500)
}, file = ruta_estructura)
fwrite(estructura, file.path(RUTA_TAB, "estructura_datos.csv"))
message("Reporte de estructura -> ", ruta_estructura)

# --- 3. Aplanar columnas-lista --------------------------------------------
# rtweet guarda hashtags/menciones/urls como listas; el CSV necesita texto.
cronometrar("Aplanando columnas-lista", aplanar_columnas_lista(datos))

# --- 4. Muestra aleatoria (va primero: es barata y permite explorar ya) ----
set.seed(SEMILLA)
n <- min(N_MUESTRA, nrow(datos))
idx <- sort(sample.int(nrow(datos), n))
muestra <- datos[idx]

ruta_m_csv <- file.path(RUTA_PROC, sprintf("muestra_%dk.csv", n %/% 1000L))
ruta_m_rds <- file.path(RUTA_PROC, sprintf("muestra_%dk.rds", n %/% 1000L))

fwrite(muestra, ruta_m_csv)
saveRDS(muestra, ruta_m_rds, compress = "gzip")

message(sprintf("Muestra de %s filas (semilla %d)", format(n, big.mark = "."), SEMILLA))
message("  ", ruta_m_csv, "  (", tam(file.size(ruta_m_csv)), ")")
message("  ", ruta_m_rds, "  (", tam(file.size(ruta_m_rds)), ")")

# --- 5. Exportar la tabla completa a CSV ----------------------------------
# Va al final: es el paso mas lento y el archivo mas pesado.
if (EXPORTAR_CSV) {
  ruta_csv <- file.path(RUTA_PROC,
                        if (COMPRIMIR_CSV) "tw_chile_social_crisis.csv.gz"
                        else               "tw_chile_social_crisis.csv")
  cronometrar(paste("Escribiendo CSV completo ->", basename(ruta_csv)), {
    fwrite(datos, ruta_csv, compress = if (COMPRIMIR_CSV) "gzip" else "none")
  })
  message("CSV completo: ", tam(file.size(ruta_csv)))
} else {
  message("EXPORTAR_CSV = FALSE: me salto el CSV completo.")
}

message("\nListo. Sigue con: source('R/02_explorar_muestra.R')")
