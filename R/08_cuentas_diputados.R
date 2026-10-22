# ---------------------------------------------------------------------------
# 08_cuentas_diputados.R
#
# Cuenta de X/Twitter de cada persona que ejercio como diputada o diputado en
# 2018-2022, y su presencia en el corpus del estallido.
#
# Padron: data/padron_diputados_2018_2022.csv (ver R/08a_padron_diputados.R),
# 163 personas reconciliadas entre la BCN y Wikidata.
#
# Tres fuentes de cuenta, en orden de autoridad:
#   1. Resena biografica oficial de la BCN (enlace a la cuenta)
#   2. Wikidata, propiedad P2002
#   3. Busqueda por nombre en las 702.103 cuentas del propio corpus
#
# La 1 y la 2 pueden traer la cuenta ACTUAL, no la de 2019: Fidel Espinoza
# figura como @fidelsenador pero en el corpus tuiteaba como @fideldiputado. La
# 3 encuentra la cuenta tal como existia en 2019, pero sin control produce
# homonimos: para diputados de bajo perfil devolvia cuentas como "Quake32" o
# "ElPirataErrante". Por eso una cuenta hallada por nombre solo se acepta si su
# handle, display o bio tiene marcas de actividad politica chilena.
#
# Salida: output/tablas/diputados_cuentas.csv (una fila por persona)
# ---------------------------------------------------------------------------

source("R/00_setup.R")

# stri_trans_general y no iconv(..., "ASCII//TRANSLIT"): en Windows iconv
# devuelve NA para TODO el texto si encuentra un caracter que no sabe
# transliterar (un emoji, una bandera), y la bio o el nombre quedan vacios.
norm <- function(x) {
  x <- stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}
PARTICULAS <- c("de", "del", "la", "las", "los", "van", "von", "da", "di", "y")
separa <- function(x) if (is.na(x) || !nzchar(x)) character(0) else
  strsplit(x, "|", fixed = TRUE)[[1]]

padron <- fread(file.path(RUTA_PROY, "data", "padron_diputados_2018_2022.csv"),
                encoding = "UTF-8", na.strings = c("", "NA"))
message(sprintf("Padron: %d personas (%d en ejercicio durante el corpus)",
                nrow(padron), padron[en_ejercicio_oct2019 == TRUE, .N]))

perfiles <- readRDS(file.path(RUTA_PROC, "perfiles.rds"))
perfiles[, `:=`(cuenta_l = tolower(cuenta), nombre_n = norm(nombre))]
perfiles[, nombre_sin := gsub(" ", "", nombre_n, fixed = TRUE)]
perfiles[, texto_n := norm(paste(cuenta, nombre, bio))]

# --- Validacion anti-homonimos ---------------------------------------------
# Marcas ESPECIFICAS de diputado. El vocabulario politico general no sirve:
# "JosManuelPrezZ4" pasaba por Jose Perez por decir "ANTICOMUNISTA RADICAL", y
# "urrutiaconcejal" es un concejal de La Cisterna, politico pero no el diputado.
MARCA_DIP <- "diputad|distrito|bancada|camara de diputad|camara cl|congreso|parlamentari"
# Y fuera las cuentas extranjeras: "pedrochapulin" pasaba por Pedro Velasquez y
# es un militante chavista venezolano.
MARCA_EXT <- paste0("venezuela|caracas|chavista|bolivarian|colombia|argentin|",
                    "peronista|mexico|peru|bolivia|ecuador|espana|miami")
perfiles[, es_pol := grepl(MARCA_DIP, texto_n, perl = TRUE) &
                     !grepl(MARCA_EXT, texto_n, perl = TRUE)]

# --- Fuente 3: busqueda por nombre en el corpus ------------------------------
# Primer nombre + paterno, como palabras en el display o pegados (display en
# CamelCase: "#PamelaJilesDiputada"). Solo el PRIMER nombre de pila: la BCN trae
# segundos nombres y con ellos "Jorge Andres Duran" calzaba con un "Andres
# Duran" cualquiera. Se pierden quienes usan su segundo nombre (Giorgio
# Jackson), pero esos tienen cuenta oficial y no dependen de esta fuente.
buscar_nombre <- function(nom) {
  t <- strsplit(norm(gsub("-", " ", nom)), " ")[[1]]
  t <- t[nzchar(t) & !t %in% PARTICULAS]
  if (length(t) < 2L) return(NA_character_)
  pat  <- if (length(t) >= 3L) t[length(t) - 1L] else t[length(t)]
  pila <- t[1L]
  en_pat  <- grepl(sprintf("(^| )%s( |$)", pat), perfiles$nombre_n) |
             grepl(pat, perfiles$nombre_sin, fixed = TRUE)
  en_pila <- grepl(sprintf("(^| )%s( |$)", pila), perfiles$nombre_n) |
             grepl(paste0(pila, pat), perfiles$nombre_sin, fixed = TRUE)
  cand <- perfiles[en_pat & en_pila & es_pol][order(-rt_recibidos, -seguidores)]
  if (nrow(cand)) cand$cuenta[1] else NA_character_
}
padron[, handle_corpus_nombre := vapply(nombre, buscar_nombre, character(1))]

# --- Consolidacion ---------------------------------------------------------
fila_perfil <- function(h) if (is.na(h)) NULL else perfiles[cuenta_l == tolower(h)][1]

res <- rbindlist(lapply(seq_len(nrow(padron)), function(i) {
  r <- padron[i]
  oficiales <- unique(tolower(c(separa(r$handle_bcn), separa(r$handle_wikidata))))
  por_nombre <- r$handle_corpus_nombre

  # 1. oficial presente en el corpus; 2. hallada por nombre y validada (si hay
  # oficial distinta, es un handle de 2019 que despues cambio); 3. oficial pero
  # ausente del corpus; 4. nada.
  hit <- perfiles[cuenta_l %in% oficiales][order(-rt_recibidos)]
  if (nrow(hit)) {
    cuenta <- hit$cuenta[1]; fuente <- "oficial, en el corpus"
  } else if (!is.na(por_nombre)) {
    cuenta <- por_nombre
    fuente <- if (length(oficiales)) "nombre en corpus (handle 2019 distinto al oficial)"
              else "nombre en corpus, validada por bio"
  } else if (length(oficiales)) {
    cuenta <- c(separa(r$handle_bcn), separa(r$handle_wikidata))[1]
    fuente <- "oficial, ausente del corpus"
  } else {
    cuenta <- NA_character_; fuente <- "sin cuenta conocida"
  }

  confirmada <- length(oficiales) && !is.na(por_nombre) && tolower(por_nombre) %in% oficiales
  p <- fila_perfil(cuenta)
  en_corpus <- !is.null(p) && nrow(p) > 0L && !is.na(p$cuenta)

  data.table(
    nombre = r$nombre, partido = r$partido, distrito = r$distrito,
    en_ejercicio_oct2019 = r$en_ejercicio_oct2019,
    cuenta, fuente,
    confirmacion = fifelse(confirmada, "dos fuentes coinciden",
                   fifelse(grepl("distinto", fuente), "revisar: handle cambio", "una fuente")),
    handle_bcn = r$handle_bcn, handle_wikidata = r$handle_wikidata,
    handle_corpus_nombre = por_nombre,
    presente_corpus = en_corpus,
    seguidores   = if (en_corpus) p$seguidores else NA_integer_,
    rt_recibidos = if (en_corpus) p$rt_recibidos else NA_integer_,
    bio          = if (en_corpus) substr(gsub("[\r\n]+", " ", p$bio), 1, 120) else NA_character_,
    fuente_padron = r$fuente_padron
  )
}))
setorder(res, -rt_recibidos, nombre, na.last = TRUE)

# Control: una misma cuenta no puede ser de dos personas.
dup <- res[!is.na(cuenta), .(n = .N, quienes = paste(nombre, collapse = " / ")),
           by = .(c = tolower(cuenta))][n > 1]
if (nrow(dup)) {
  warning("Cuentas asignadas a mas de una persona:\n",
          paste(sprintf("  @%s -> %s", dup$c, dup$quienes), collapse = "\n"))
} else message("Control: ninguna cuenta asignada a dos personas.")

fwrite(res, file.path(RUTA_TAB, "diputados_cuentas.csv"))
message("  tabla  -> diputados_cuentas.csv")

# --- Resumen (solo quienes ejercian en octubre de 2019) ---------------------
e <- res[en_ejercicio_oct2019 == TRUE]
message(sprintf("\nEn ejercicio en oct-2019: %d | con cuenta identificada: %d (%.0f%%)",
                nrow(e), e[!is.na(cuenta), .N], 100 * e[!is.na(cuenta), .N] / nrow(e)))
message("\n== Por fuente ==");       print(e[, .N, by = fuente][order(-N)])
message("\n== Confirmacion ==");     print(e[, .N, by = confirmacion][order(-N)])
message(sprintf("\nPresentes en el corpus: %d | con 0 RT recibidos: %d | con cuenta pero ausentes: %d | sin cuenta conocida: %d",
                e[presente_corpus == TRUE, .N], e[presente_corpus == TRUE & rt_recibidos == 0, .N],
                e[fuente == "oficial, ausente del corpus", .N], e[fuente == "sin cuenta conocida", .N]))

message("\n== Handle de 2019 distinto al oficial (revisar) ==")
print(e[confirmacion == "revisar: handle cambio",
        .(nombre = substr(nombre, 1, 30), oficial = handle_bcn, cuenta_2019 = cuenta,
          bio = substr(bio, 1, 50))])

message("\n== Sin cuenta conocida ==")
print(e[fuente == "sin cuenta conocida", nombre])
