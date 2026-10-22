# ---------------------------------------------------------------------------
# 08a_padron_diputados.R
#
# Padron de TODAS las personas que ejercieron como diputadas o diputados en el
# periodo 2018-2022 (LV periodo legislativo), incluidos reemplazos, con su
# cuenta de X segun fuentes oficiales.
#
# Por que no basta con una lista:
#   - El padron de 155 que se armo antes (data/roster_politicos_2019.csv) salio
#     de un resumen automatico de Wikipedia y tiene errores: omite a Bellolio,
#     Hugo Gutierrez, Garin, Melero y Kort, y confunde apellidos maternos.
#   - La nomina de la BCN trae 147 personas: tampoco esta completa.
#   - Wikidata trae 158, pero con etiquetas cortas y sin partido en muchos casos.
# Se unen BCN y Wikidata emparejando por apellidos, que en Chile identifican
# mucho mejor que el nombre de pila (la BCN usa segundos nombres: "Kenneth
# Giorgio Jackson Drago").
#
# Entradas: data/externos/bcn_nomina_2018_2022.csv
#           data/externos/bcn_resenas_twitter.csv
#           data/externos/wikidata_diputados_raw.csv
# Salida:   data/padron_diputados_2018_2022.csv
# ---------------------------------------------------------------------------

source("R/00_setup.R")

norm <- function(x) {
  x <- stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}
PARTICULAS <- c("de", "del", "la", "las", "los", "van", "von", "da", "di", "y")

# Separa nombres de pila y apellidos. Los apellidos compuestos con guion se
# pegan antes de normalizar ("Alvarez-Salamanca" -> "alvarezsalamanca") para que
# cuenten como uno solo.
partir <- function(nom) {
  t <- strsplit(norm(gsub("-", "", nom)), " ")[[1]]
  t <- t[nzchar(t) & !t %in% PARTICULAS]
  n <- length(t)
  if (n >= 3L) list(nombres = t[seq_len(n - 2L)], pat = t[n - 1L], mat = t[n])
  else if (n == 2L) list(nombres = t[1L], pat = t[2L], mat = NA_character_)
  else list(nombres = t, pat = NA_character_, mat = NA_character_)
}

tokens <- function(nom) {
  t <- strsplit(norm(gsub("-", " ", nom)), " ")[[1]]
  t[nzchar(t) & !t %in% PARTICULAS]
}

# Tres reglas, basta una:
#   1. mismo apellido paterno y materno
#   2. el nombre corto esta contenido en el largo: "Juan Luis Castro" dentro de
#      "Juan Luis Castro Gonzalez", o "Luciano Cruz-Coke" dentro de "Luciano
#      Cruz Coke Carvallo" (la BCN escribe el apellido con espacio, Wikidata con
#      guion; separando el guion ambos quedan como "cruz coke")
#   3. la misma cuenta de X en ambas fuentes: resuelve los nombres artisticos
#      ("Florcita Motuda" = "Raul Florcita Alarcon Rojas")
misma_persona <- function(na, nb, ha = NA, hb = NA) {
  a <- partir(na); b <- partir(nb)
  if (!is.na(a$pat) && !is.na(b$pat) && !is.na(a$mat) && !is.na(b$mat) &&
      a$pat == b$pat && a$mat == b$mat) return(TRUE)
  ta <- tokens(na); tb <- tokens(nb)
  corto <- if (length(ta) <= length(tb)) ta else tb
  largo <- if (length(ta) <= length(tb)) tb else ta
  if (length(corto) >= 2L && all(corto %in% largo)) return(TRUE)
  if (!is.na(ha) && !is.na(hb) && nzchar(ha) && nzchar(hb)) {
    if (length(intersect(tolower(strsplit(ha, "|", fixed = TRUE)[[1]]),
                         tolower(strsplit(hb, "|", fixed = TRUE)[[1]])))) return(TRUE)
  }
  FALSE
}

# --- BCN: nomina + cuenta desde la resena -----------------------------------
bcn <- fread(file.path(RUTA_PROY, "data", "externos", "bcn_nomina_2018_2022.csv"),
             encoding = "UTF-8")
tw_bcn <- fread(file.path(RUTA_PROY, "data", "externos", "bcn_resenas_twitter.csv"),
                encoding = "UTF-8")[estado == "ok"]
bcn[, handle_bcn := tw_bcn$handles[match(page_id, tw_bcn$slug)]]

# La pagina de la BCN enlaza resenas validas que NO estan en los bloques de la
# nomina (Catalina Perez, Alessandri, Leonidas Romero, Claudia Mix, Rodrigo
# Gonzalez). Se suman con el nombre tomado del propio enlace; partido y
# distrito quedan vacios.
extra <- tw_bcn[!slug %in% bcn$page_id]

# Una resena extra puede ser de alguien que YA esta en la nomina con un enlace
# roto: la nomina trae "Claudia_Nathalie_Mix_Jimenez" (sin tilde, 404) y la
# resena valida es "..._Jiménez". En ese caso completa la fila existente en vez
# de sumar una persona nueva.
if (nrow(extra)) {
  nuevas <- 0L
  for (i in seq_len(nrow(extra))) {
    nom_x <- gsub("_", " ", extra$slug[i])
    j <- which(vapply(bcn$nombre, function(n) misma_persona(n, nom_x), logical(1)))
    if (length(j)) {
      j <- j[1]
      if (is.na(bcn$handle_bcn[j]) || !nzchar(bcn$handle_bcn[j])) bcn$handle_bcn[j] <- extra$handles[i]
    } else {
      bcn <- rbind(bcn, data.table(page_id = extra$slug[i], nombre = nom_x,
                                   cargo = "Diputado", distrito = NA_character_,
                                   partido = NA_character_, handle_bcn = extra$handles[i]),
                   fill = TRUE)
      nuevas <- nuevas + 1L
    }
  }
  message(sprintf("Resenas BCN fuera de la nomina: %d (%d personas nuevas, %d completan una fila existente)",
                  nrow(extra), nuevas, nrow(extra) - nuevas))
}

# --- Wikidata: una fila por persona, con fechas del cargo --------------------
wd_raw <- fread(file.path(RUTA_PROY, "data", "externos", "wikidata_diputados_raw.csv"),
                encoding = "UTF-8")[startsWith(termLabel, "LV periodo")]
wd <- wd_raw[, .(nombre_wd = pLabel[1],
                 inicio = suppressWarnings(min(as.IDate(substr(inicio, 1, 10)), na.rm = TRUE)),
                 fin    = suppressWarnings(max(as.IDate(substr(fin, 1, 10)), na.rm = TRUE)),
                 handle_wikidata = paste(unique(tw[!is.na(tw) & tw != ""][
                   !duplicated(tolower(tw[!is.na(tw) & tw != ""]))]), collapse = "|")),
             by = .(qid = sub(".*/", "", p))]
wd[!is.finite(inicio), inicio := NA][!is.finite(fin), fin := NA]
wd[handle_wikidata == "", handle_wikidata := NA]

# --- Union ------------------------------------------------------------------
# Una persona de la BCN puede calzar con VARIOS items de Wikidata: Wikidata
# tiene duplicados (Cristhian Moreira aparece como Q59185111 y Q55842420). Se
# funden todos: fechas extremas y cualquier cuenta que traigan.
enlaces <- rbindlist(lapply(seq_len(nrow(bcn)), function(i) {
  hit <- which(vapply(seq_len(nrow(wd)), function(j)
    misma_persona(bcn$nombre[i], wd$nombre_wd[j], bcn$handle_bcn[i], wd$handle_wikidata[j]),
    logical(1)))
  if (!length(hit)) return(NULL)
  data.table(page_id_bcn = bcn$page_id[i], qid = wd$qid[hit])
}))

junta <- function(x) { x <- unique(unlist(strsplit(na.omit(x), "|", fixed = TRUE)))
                       x <- x[nzchar(x) & !duplicated(tolower(x))]
                       if (length(x)) paste(x, collapse = "|") else NA_character_ }
wd_por_bcn <- merge(enlaces, wd, by = "qid")[, .(
  qid = paste(qid, collapse = "|"),
  inicio = suppressWarnings(min(inicio, na.rm = TRUE)),
  fin    = suppressWarnings(max(fin, na.rm = TRUE)),
  handle_wikidata = junta(handle_wikidata),
  nombre_wd = nombre_wd[1]), by = page_id_bcn]
wd_por_bcn[!is.finite(inicio), inicio := NA][!is.finite(fin), fin := NA]

con_bcn <- merge(bcn[, .(nombre, partido, distrito, page_id_bcn = page_id, handle_bcn)],
                 wd_por_bcn, by = "page_id_bcn", all.x = TRUE)

# Items de Wikidata que no calzaron con nadie de la BCN: personas que faltan en
# la nomina de la BCN. Tambien pueden venir duplicadas entre si.
solo_wd <- wd[!qid %in% enlaces$qid]
grupo <- integer(nrow(solo_wd)); g <- 0L
for (i in seq_len(nrow(solo_wd))) {
  if (grupo[i]) next
  g <- g + 1L; grupo[i] <- g
  for (j in seq_len(nrow(solo_wd))[-seq_len(i)]) {
    if (!grupo[j] && misma_persona(solo_wd$nombre_wd[i], solo_wd$nombre_wd[j],
                                   solo_wd$handle_wikidata[i], solo_wd$handle_wikidata[j]))
      grupo[j] <- g
  }
}
solo_wd[, grupo := grupo]
solo_wd <- solo_wd[, .(
  nombre = nombre_wd[which.max(nchar(nombre_wd))],   # la etiqueta mas completa
  qid = paste(qid, collapse = "|"),
  inicio = suppressWarnings(min(inicio, na.rm = TRUE)),
  fin    = suppressWarnings(max(fin, na.rm = TRUE)),
  handle_wikidata = junta(handle_wikidata),
  nombre_wd = nombre_wd[1]), by = grupo][, grupo := NULL]
solo_wd[!is.finite(inicio), inicio := NA][!is.finite(fin), fin := NA]

padron <- rbind(con_bcn, solo_wd, fill = TRUE)
padron[, fuente_padron := fifelse(!is.na(page_id_bcn) & !is.na(nombre_wd), "BCN + Wikidata",
                          fifelse(!is.na(page_id_bcn), "solo BCN", "solo Wikidata"))]

# En ejercicio durante el corpus (11 al 29 de octubre de 2019). Sin fechas en
# Wikidata se asume que si: la mayoria de esos casos son diputados del periodo
# completo cuyo item no tiene los calificadores cargados.
ini_c <- as.IDate("2019-10-11"); fin_c <- as.IDate("2019-10-29")
padron[, en_ejercicio_oct2019 := (is.na(inicio) | inicio <= fin_c) & (is.na(fin) | fin >= ini_c)]

setorder(padron, distrito, nombre, na.last = TRUE)
padron[, c("nombre_wd") := NULL]
fwrite(padron, file.path(RUTA_PROY, "data", "padron_diputados_2018_2022.csv"))

message(sprintf("Personas en el padron: %d", nrow(padron)))
print(padron[, .N, by = fuente_padron])
message(sprintf("En ejercicio durante el corpus (oct-2019): %d", padron[en_ejercicio_oct2019 == TRUE, .N]))
message(sprintf("Con cuenta en BCN: %d | en Wikidata: %d | en alguna: %d",
                padron[!is.na(handle_bcn) & handle_bcn != "", .N],
                padron[!is.na(handle_wikidata), .N],
                padron[(!is.na(handle_bcn) & handle_bcn != "") | !is.na(handle_wikidata), .N]))
message("\n== Solo en Wikidata (revisar: pueden ser reemplazos o nombres que no calzaron) ==")
print(padron[fuente_padron == "solo Wikidata", .(nombre, qid, inicio, fin, handle_wikidata)])
