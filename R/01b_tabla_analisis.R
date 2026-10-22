# ---------------------------------------------------------------------------
# 01b_tabla_analisis.R
#
# Construye una tabla liviana con SOLO las columnas que necesita el analisis
# del corpus completo, para no tener que levantar los 13,6 GB del .rds cada
# vez que se corre el reporte.
#
# Estrategia: leer esas columnas desde el CSV completo con fread(select=),
# que es multihilo y no carga las 90 columnas. Si el conteo de filas no
# cuadra con el original, cae de vuelta al .rds (lento pero seguro).
#
# Salida: data/processed/tabla_analisis.parquet
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages(library(arrow))

N_ESPERADO <- 6119933L

COLS <- c("status_id", "created_at", "is_retweet", "is_quote", "lang",
          "screen_name", "retweet_screen_name", "retweet_created_at",
          "favorite_count", "retweet_count", "source", "country", "location")

ruta_csv     <- file.path(RUTA_PROC, "tw_chile_social_crisis.csv")
ruta_parquet <- file.path(RUTA_PROC, "tabla_analisis.parquet")

leer_desde_csv <- function() {
  if (!file.exists(ruta_csv)) return(NULL)
  dt <- cronometrar("Leyendo columnas clave desde el CSV completo", {
    fread(ruta_csv, select = COLS, showProgress = TRUE)
  })
  if (nrow(dt) != N_ESPERADO) {
    warning(sprintf("El CSV entrego %s filas y se esperaban %s: uso el .rds",
                    format(nrow(dt), big.mark = "."),
                    format(N_ESPERADO, big.mark = ".")))
    return(NULL)
  }
  dt
}

leer_desde_rds <- function() {
  cronometrar("Leyendo el .rds completo (plan B)", {
    d <- readRDS(ARCHIVO_RDS)
    setDT(d)
    d[, ..COLS]
  })
}

dt <- leer_desde_csv()
if (is.null(dt)) dt <- leer_desde_rds()

stopifnot(nrow(dt) == N_ESPERADO)

# --- Tiempos ---------------------------------------------------------------
# Cuidado: fread puede devolver created_at como texto ISO ("2019-10-22T17:49:53Z").
# as.POSIXct() sin formato explicito NO reconoce la "T", cae al patron "%Y-%m-%d"
# y trunca todo a medianoche en silencio. Hay que parsear con formato explicito.
a_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    setattr(x, "tzone", "UTC")
    return(x)
  }
  x <- as.character(x)
  y <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (all(is.na(y))) y <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  y
}

dt[, created_at         := a_utc(created_at)]
dt[, retweet_created_at := a_utc(retweet_created_at)]

# Guarda contra el truncamiento a medianoche: si todas las horas fueran iguales
# el parseo fallo y no tiene sentido seguir.
horas_utc <- unique(as.integer(format(head(dt$created_at, 1e5L), "%H")))
if (length(horas_utc) < 12L) {
  stop("created_at quedo con solo ", length(horas_utc),
       " horas distintas: el parseo de fechas fallo.")
}

# Hora local de Chile. Un POSIXct es un instante absoluto: basta cambiar el
# atributo tzone para leerlo en America/Santiago (R resuelve el horario de
# verano solo; en octubre de 2019 Chile continental estaba en -03).
dt[, fecha_cl := created_at]
setattr(dt$fecha_cl, "tzone", "America/Santiago")

dt[, `:=`(
  dia_cl   = as.IDate(format(created_at, "%Y-%m-%d", tz = "America/Santiago")),
  hora_cl  = as.integer(format(created_at, "%H", tz = "America/Santiago")),
  hora_utc = as.integer(format(created_at, "%H", tz = "UTC")),
  tipo     = fifelse(is_retweet, "retweet", fifelse(is_quote, "cita", "original"))
)]

write_parquet(dt, ruta_parquet, compression = "zstd")
message("Tabla de analisis -> ", ruta_parquet, " (", tam(file.size(ruta_parquet)), ")")
message("Filas: ", format(nrow(dt), big.mark = "."), " | Columnas: ", ncol(dt))
print(dt[, .N, by = tipo][order(-N)])
print(range(dt$fecha_cl, na.rm = TRUE))
print(dt[, .N, by = hora_cl][order(hora_cl)])
