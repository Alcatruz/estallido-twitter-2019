# ---------------------------------------------------------------------------
# 07_politicos.R
#
# Identifica en el corpus las cuentas de politicos en ejercicio en octubre de
# 2019: diputados, senadores, ministros y el Presidente.
#
# Metodo: NO se adivinan handles. Se parte del padron real
# (data/roster_politicos_2019.csv) y se cruza contra el nombre para mostrar
# ("name") y el handle de las 702.103 cuentas del corpus. El nombre para
# mostrar es el campo util: "PamJiles" no se parece a "Pamela Jiles", pero su
# display name es "#PamelaJilesDiputada".
#
# La salida es una tabla PARA REVISAR, no un resultado cerrado: trae la
# evidencia de cada match (bio, seguidores, retweets) para validar a ojo.
#
# Salidas:
#   output/tablas/politicos_match.csv        cuentas identificadas
#   output/tablas/politicos_sin_match.csv    padron sin cuenta encontrada
#   output/tablas/politicos_ambiguos.csv     mas de una cuenta candidata
# ---------------------------------------------------------------------------

source("R/00_setup.R")

perfiles <- readRDS(file.path(RUTA_PROC, "perfiles.rds"))
roster   <- fread(file.path(RUTA_PROY, "data", "roster_politicos_2019.csv"),
                  encoding = "UTF-8")
message(sprintf("Padron: %d personas | Corpus: %s cuentas",
                nrow(roster), format(nrow(perfiles), big.mark = ".")))

# --- Normalizacion ---------------------------------------------------------
# stri_trans_general y no iconv(..., "ASCII//TRANSLIT"): en Windows iconv
# devuelve NA para TODO el texto si encuentra un caracter que no sabe
# transliterar (un emoji, una bandera), y la bio o el nombre quedan vacios.
norm <- function(x) {
  x <- stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

perfiles[, nombre_n := norm(nombre)]
perfiles[, cuenta_n := gsub("[^a-z0-9]", "", tolower(cuenta))]
perfiles[, nombre_sin := gsub(" ", "", nombre_n, fixed = TRUE)]

# En castellano el orden es nombre(s) + apellido paterno + materno. El apellido
# paterno es el penultimo token cuando hay 3 o mas ("Jose Miguel Insulza
# Salinas" -> insulza), no el segundo.
roster[, tokens := lapply(strsplit(norm(nombre), " "), function(t) t[nchar(t) > 1])]
roster[, primer_nombre := vapply(tokens, `[`, character(1), 1L)]
# Ojo con las particulas: en "Guillermo Teillier del Valle" el penultimo token
# es "del", no un apellido. Si cae una particula hay que retroceder uno mas.
PARTICULAS <- c("de", "del", "la", "las", "los", "van", "von", "da", "di",
                "du", "dos", "y", "san", "santa", "mac", "mc")
roster[, apellido := vapply(tokens, function(t) {
  n <- length(t)
  if (n < 3L) return(t[n])
  ap <- t[n - 1L]
  if (ap %in% PARTICULAS && n >= 4L) ap <- t[n - 2L]
  ap
}, character(1))]

# --- Emparejamiento --------------------------------------------------------
# Dos vias independientes:
#   (a) el display name contiene nombre Y apellido como palabras
#   (b) el handle contiene la concatenacion nombre+apellido o inicial+apellido
buscar <- function(pn, ap) {
  patron_nombre <- sprintf("(^| )%s( |$)", pn)
  patron_apell  <- sprintf("(^| )%s( |$)", ap)
  via_a <- perfiles[grepl(patron_nombre, nombre_n) & grepl(patron_apell, nombre_n), which = TRUE]

  claves <- c(paste0(pn, ap), paste0(substr(pn, 1, 1), ap))
  via_b <- perfiles[cuenta_n %in% claves, which = TRUE]

  # (c) display names en CamelCase, sin espacios que separen las palabras:
  # "#PamelaJilesDiputada" no matchea por palabra pero si por concatenacion.
  via_c <- perfiles[grepl(paste0(pn, ap), nombre_sin, fixed = TRUE), which = TRUE]

  sort(unique(c(via_a, via_b, via_c)))
}

res <- rbindlist(lapply(seq_len(nrow(roster)), function(i) {
  idx <- buscar(roster$primer_nombre[i], roster$apellido[i])
  if (!length(idx)) return(NULL)
  cbind(roster[i, .(nombre, cargo, partido, territorio)],
        perfiles[idx, .(cuenta, display = nombre, seguidores, rt_recibidos,
                        verificada, bio = substr(gsub("[\r\n]+", " ", bio), 1, 90))])
}))

# Cuando hay varias cuentas candidatas para una misma persona, la correcta casi
# siempre es la de mas alcance: parodias y homonimos tienen poco publico.
setorder(res, nombre, -rt_recibidos, -seguidores)
res[, n_candidatas := .N, by = nombre]

elegidas  <- res[, head(.SD, 1L), by = nombre]
ambiguos  <- res[n_candidatas > 1]
sin_match <- roster[!nombre %in% res$nombre, .(nombre, cargo, partido, territorio)]

# Un no-match puede ser falla del emparejamiento o ausencia real del corpus, y
# la diferencia importa: lo segundo es un dato, no un error. Se resuelve
# buscando el apellido solo, que es mucho mas laxo que nombre+apellido.
sin_match[, apellido := roster$apellido[match(nombre, roster$nombre)]]
# Buscar el apellido a secas es demasiado laxo: "munoz" o "gonzalez" devuelven
# miles de desconocidos. Solo cuentan las cuentas con algo de circulacion
# (>=20 RT recibidos), que es donde estaria un parlamentario si estuviera.
sin_match[, rastro := vapply(apellido, function(a)
  perfiles[rt_recibidos >= 20L &
             (grepl(a, nombre_sin, fixed = TRUE) | grepl(a, cuenta_n, fixed = TRUE)), .N],
  integer(1))]
sin_match[, diagnostico := fifelse(rastro == 0L, "ausente del corpus",
                                   "revisar: hay cuentas con ese apellido y alcance")]

fwrite(elegidas,  file.path(RUTA_TAB, "politicos_match.csv"))
fwrite(ambiguos,  file.path(RUTA_TAB, "politicos_ambiguos.csv"))
fwrite(sin_match, file.path(RUTA_TAB, "politicos_sin_match.csv"))

# --- Cobertura -------------------------------------------------------------
message(sprintf("\nIdentificados: %d de %d (%.1f%%)",
                nrow(elegidas), nrow(roster), 100 * nrow(elegidas) / nrow(roster)))
message(sprintf("Sin cuenta encontrada: %d | Con mas de una candidata: %d",
                nrow(sin_match), uniqueN(ambiguos$nombre)))

message("\n== Cobertura por cargo ==")
cob <- merge(roster[, .(padron = .N), by = cargo],
             elegidas[, .(hallados = .N), by = cargo], by = "cargo", all.x = TRUE)
cob[is.na(hallados), hallados := 0L][, pct := round(100 * hallados / padron, 1)]
print(cob[order(-padron)])

message("\n== Los 25 politicos mas retuiteados del corpus ==")
print(elegidas[order(-rt_recibidos)][1:25,
  .(nombre = substr(nombre, 1, 26), cargo, partido,
    cuenta, seguidores, rt = rt_recibidos)])

message("\n== Retweets recibidos por partido ==")
print(elegidas[, .(politicos = .N, rt = sum(rt_recibidos),
                   seguidores = sum(as.numeric(seguidores))),
               by = partido][order(-rt)][1:12])

message("\n== Diagnostico de los no encontrados ==")
print(sin_match[, .N, by = diagnostico])

message("\n== Presencia por partido: padron vs corpus ==")
pres <- merge(roster[, .(en_padron = .N), by = partido],
              elegidas[, .(en_corpus = .N, rt = sum(rt_recibidos)), by = partido],
              by = "partido", all.x = TRUE)
pres[is.na(en_corpus), `:=`(en_corpus = 0L, rt = 0L)]
pres[, `:=`(pct_presente = round(100 * en_corpus / en_padron, 0),
            rt_por_politico = round(rt / pmax(en_corpus, 1)))]
print(pres[order(-rt_por_politico)])
