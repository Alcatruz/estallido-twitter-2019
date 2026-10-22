# ---------------------------------------------------------------------------
# 00_setup.R -- Configuracion comun del proyecto
# Se carga con source() al inicio de cada script.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
})

# Usar todos los nucleos disponibles menos uno (fread/fwrite son multihilo)
data.table::setDTthreads(max(1L, parallel::detectCores() - 1L))

# --- Rutas -----------------------------------------------------------------
# Raiz del proyecto: si se usa el .Rproj el working dir ya es la raiz.
RUTA_PROY  <- normalizePath(".", winslash = "/", mustWork = TRUE)
RUTA_RAW   <- file.path(RUTA_PROY, "data", "raw")
RUTA_PROC  <- file.path(RUTA_PROY, "data", "processed")
RUTA_FIG   <- file.path(RUTA_PROY, "output", "figuras")
RUTA_TAB   <- file.path(RUTA_PROY, "output", "tablas")

for (d in c(RUTA_RAW, RUTA_PROC, RUTA_FIG, RUTA_TAB)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# El .rds original puede estar en la raiz del proyecto o en data/raw/
ARCHIVO_RDS <- local({
  candidatos <- c(
    file.path(RUTA_PROY, "tw_chile_social_crisis.rds"),
    file.path(RUTA_RAW,  "tw_chile_social_crisis.rds")
  )
  hit <- candidatos[file.exists(candidatos)]
  if (!length(hit)) stop("No encuentro tw_chile_social_crisis.rds en la raiz ni en data/raw/")
  hit[1]
})

# --- Parametros ------------------------------------------------------------
SEMILLA        <- 2019L      # reproducibilidad de la muestra
N_MUESTRA      <- 200000L    # filas de la muestra aleatoria
EXPORTAR_CSV   <- TRUE       # FALSE para saltarse el CSV completo (es enorme)
COMPRIMIR_CSV  <- FALSE      # TRUE -> escribe .csv.gz en vez de .csv

# --- Utilidades ------------------------------------------------------------

# Cronometro con mensaje
cronometrar <- function(etiqueta, expr) {
  t0 <- Sys.time()
  message(sprintf("[%s] %s ...", format(t0, "%H:%M:%S"), etiqueta))
  res <- force(expr)
  message(sprintf("    -> listo en %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(res)
}

# Tamano legible
tam <- function(bytes) {
  u <- c("B", "KB", "MB", "GB", "TB")
  i <- max(1, min(length(u), floor(log(max(bytes, 1), base = 1024)) + 1))
  sprintf("%.2f %s", bytes / 1024^(i - 1), u[i])
}

# Aplana columnas-lista (hashtags, menciones, urls de rtweet) a texto "a|b|c"
# para que el CSV sea legible por cualquier lector.
aplanar_columnas_lista <- function(dt, sep = "|") {
  stopifnot(data.table::is.data.table(dt))
  cols <- names(dt)[vapply(dt, is.list, logical(1))]
  for (cl in cols) {
    data.table::set(dt, j = cl, value = vapply(
      dt[[cl]],
      function(x) if (length(x) == 0L || all(is.na(x))) NA_character_
                  else paste(as.character(x), collapse = sep),
      character(1)
    ))
  }
  if (length(cols)) message("    columnas-lista aplanadas: ", paste(cols, collapse = ", "))
  invisible(cols)
}
