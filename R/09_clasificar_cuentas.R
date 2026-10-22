# ---------------------------------------------------------------------------
# 09_clasificar_cuentas.R
#
# Clasifica las 702.103 cuentas del corpus en tres clases y compara su peso en
# la conversacion:
#
#   politica   cargo de eleccion o designacion en octubre de 2019 (diputados,
#              senadores, ministros, Presidente, presidentes de partido)
#   medio      prensa, radio, television e instituciones (heuristica por bio,
#              con error en ambas direcciones: ver la nota del bloque 2)
#   ciudadana  el resto
#
# El tercer grupo se separa del segundo a proposito: si "normales" incluyera a
# los medios, la comparacion con la clase politica mediria otra cosa. La
# pregunta es si el estallido amplifico a los cargos electos o a la gente.
#
# Entradas: data/processed/perfiles.rds, data/processed/tabla_analisis.parquet
#           output/tablas/diputados_cuentas.csv, output/tablas/politicos_match.csv
# Salidas:  data/cuentas_politicas.csv            registro chico, versionable
#           data/processed/clasificacion_cuentas.parquet   las 702.103 cuentas
#           output/tablas/clases_resumen.csv
#           output/figuras/clases_amplificacion.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(arrow); library(ggplot2); library(scales)
})
theme_set(theme_minimal(base_size = 12))

norm <- function(x) {
  x <- stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

perfiles <- readRDS(file.path(RUTA_PROC, "perfiles.rds"))
perfiles[, cuenta_l := tolower(cuenta)]
perfiles[, texto_n := norm(paste(cuenta, nombre, bio))]

# --- 1. Registro de cuentas politicas ---------------------------------------
dip <- fread(file.path(RUTA_TAB, "diputados_cuentas.csv"), encoding = "UTF-8",
             na.strings = c("", "NA"))[en_ejercicio_oct2019 == TRUE & !is.na(cuenta)]
dip <- dip[, .(nombre, cargo = "diputado", partido, cuenta, fuente)]

# Senadores, ministros y Presidente. La fuente primaria es la misma que para
# los diputados: la resena biografica de la BCN. Solo si la BCN no tiene la
# persona se usa el emparejamiento por nombre de 07, que no controla homonimos
# (devolvia @juankcoloma para Coloma y @jduquecas para Juan Castro).
bcn_sm <- fread(file.path(RUTA_PROY, "data", "externos",
                          "bcn_senadores_ministros_twitter.csv"),
                encoding = "UTF-8", na.strings = c("", "NA"))[estado == "ok" & !is.na(handles)]

otros <- fread(file.path(RUTA_TAB, "politicos_match.csv"), encoding = "UTF-8",
               na.strings = c("", "NA"))[cargo != "diputado"]
otros[, handle_bcn := bcn_sm$handles[match(nombre, bcn_sm$nombre)]]

# Sin cuenta oficial, el nombre solo vale si el perfil tiene marcas del cargo.
MARCA_CARGO <- "senador|ministr|diputad|gobierno de chile|presidente de la republica|partido|bancada|congreso|parlamentari"
MARCA_EXT   <- paste0("venezuela|caracas|chavista|bolivarian|colombia|argentin|",
                      "peronista|mexico|peru|bolivia|ecuador|espana|miami")
otros[, txt := perfiles$texto_n[match(tolower(cuenta), perfiles$cuenta_l)]]
otros[is.na(txt), txt := ""]
otros[, nombre_ok := !is.na(cuenta) & grepl(MARCA_CARGO, txt, perl = TRUE) &
                     !grepl(MARCA_EXT, txt, perl = TRUE)]

otros[, `:=`(cuenta_final = fifelse(!is.na(handle_bcn), handle_bcn,
                                    fifelse(nombre_ok, cuenta, NA_character_)),
             fuente = fifelse(!is.na(handle_bcn), "resena BCN",
                              fifelse(nombre_ok, "nombre + control de bio", NA_character_)))]

message(sprintf("Senadores/ministros: %d en el padron | %d con cuenta BCN | %d solo por nombre | %d sin cuenta",
                nrow(otros), otros[!is.na(handle_bcn), .N],
                otros[is.na(handle_bcn) & nombre_ok == TRUE, .N],
                otros[is.na(cuenta_final), .N]))
message("  recuperados por BCN que el filtro de bio habria descartado: ",
        paste0("@", otros[!is.na(handle_bcn) & nombre_ok == FALSE, handle_bcn], collapse = ", "))

otros <- otros[!is.na(cuenta_final), .(nombre, cargo, partido, cuenta = cuenta_final, fuente)]

politicos <- unique(rbind(dip, otros), by = "cuenta")
politicos[, cuenta_l := tolower(cuenta)]
fwrite(politicos[order(cargo, nombre)], file.path(RUTA_PROY, "data", "cuentas_politicas.csv"))
message(sprintf("\nRegistro de cuentas politicas: %d", nrow(politicos)))
print(politicos[, .N, by = cargo][order(-N)])

# --- 2. Medios e instituciones ----------------------------------------------
# ATENCION: a diferencia de "politica", esta clase NO sale de un registro sino
# de buscar vocabulario en la bio, y se equivoca en las dos direcciones: deja
# fuera a @teleSURtv (su bio no dice ninguna de estas palabras) y marca como
# medio a periodistas y estudiantes a titulo personal. Sirve para que la clase
# "ciudadana" no quede contaminada de diarios, no como censo de medios.
MARCA_MEDIO <- paste0("periodis|noticias|diario|radio |radio$|^radio|prensa|",
                      "television|canal |telev|medio de comunicacion|",
                      "cobertura|informativo|redaccion|agencia|revista|",
                      "ministerio|municipalidad|gobierno|subsecretaria|",
                      "servicio nacional|cuenta oficial|institucional|",
                      "universidad|fundacion|sindicato|federacion de estudiantes")
perfiles[, es_medio := grepl(MARCA_MEDIO, texto_n, perl = TRUE)]

# --- 3. Clasificacion --------------------------------------------------------
perfiles[, clase := fifelse(cuenta_l %in% politicos$cuenta_l, "politica",
                     fifelse(es_medio, "medio", "ciudadana"))]
perfiles[, clase := factor(clase, levels = c("politica", "medio", "ciudadana"))]

write_parquet(perfiles[, .(cuenta, clase, seguidores, rt_recibidos)],
              file.path(RUTA_PROC, "clasificacion_cuentas.parquet"))

message("\n== Cuentas por clase ==")
print(perfiles[, .(cuentas = .N), by = clase][
  , pct := round(100 * cuentas / sum(cuentas), 3)][order(-cuentas)])

# --- 4. Peso en la conversacion ---------------------------------------------
dt <- as.data.table(read_parquet(file.path(RUTA_PROC, "tabla_analisis.parquet")))
clase_de <- setNames(as.character(perfiles$clase), perfiles$cuenta_l)

dt[, clase_emisor := clase_de[tolower(screen_name)]]
dt[is_retweet == TRUE & !is.na(retweet_screen_name) & retweet_screen_name != "",
   clase_autor := clase_de[tolower(retweet_screen_name)]]
dt[is_retweet == FALSE, clase_autor := clase_emisor]

resumen <- merge(
  perfiles[, .(cuentas = .N), by = clase],
  dt[!is.na(clase_emisor), .(tweets_emitidos = .N), by = .(clase = clase_emisor)],
  by = "clase", all.x = TRUE)
resumen <- merge(resumen,
  dt[is_retweet == TRUE & !is.na(clase_autor),
     .(rt_recibidos = .N), by = .(clase = clase_autor)], by = "clase", all.x = TRUE)
resumen[is.na(resumen)] <- 0L
resumen[, `:=`(pct_cuentas = round(100 * cuentas / sum(cuentas), 2),
               pct_tweets  = round(100 * tweets_emitidos / sum(tweets_emitidos), 2),
               pct_rt      = round(100 * rt_recibidos / sum(rt_recibidos), 2),
               rt_por_cuenta = round(rt_recibidos / cuentas, 1))]
setorder(resumen, -rt_recibidos)
fwrite(resumen, file.path(RUTA_TAB, "clases_resumen.csv"))
message("\n== Peso en la conversacion ==")
print(resumen)

# --- 5. Quien es amplificado -------------------------------------------------
message("\n== Top 15 cuentas mas retuiteadas, con su clase ==")
top <- dt[is_retweet == TRUE & !is.na(retweet_screen_name) & retweet_screen_name != "",
          .N, by = .(cuenta = retweet_screen_name)][order(-N)][1:15]
top[, clase := clase_de[tolower(cuenta)]]
print(top)

message("\n== Las 10 cuentas politicas mas retuiteadas ==")
pol <- dt[is_retweet == TRUE & clase_autor == "politica",
          .N, by = .(cuenta_l = tolower(retweet_screen_name))][order(-N)][1:10]
pol <- merge(pol, politicos[, .(cuenta_l, nombre, cargo, partido)], by = "cuenta_l")
print(pol[order(-N), .(nombre = substr(nombre, 1, 30), cargo, partido, N)])

# --- 6. Figura ---------------------------------------------------------------
largo <- melt(resumen[, .(clase, `% de las cuentas` = pct_cuentas,
                          `% de los tweets` = pct_tweets,
                          `% de los retweets recibidos` = pct_rt)],
              id.vars = "clase", variable.name = "medida", value.name = "pct")

p <- ggplot(largo, aes(medida, pct, fill = clase)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(pct >= 3, paste0(format(round(pct), nsmall = 0), "%"), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.6) +
  scale_fill_manual(values = c(politica = "#c1121f", medio = "#f4a261",
                               ciudadana = "#003049"), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(title = "Quien habla y a quien se amplifica en el estallido",
       subtitle = sprintf("corpus completo: %s tweets de %s cuentas",
                          format(nrow(dt), big.mark = "."),
                          format(nrow(perfiles), big.mark = ".")),
       x = NULL, y = NULL)
ggsave(file.path(RUTA_FIG, "clases_amplificacion.png"), p,
       width = 10, height = 4.5, dpi = 150, bg = "white")
message("  figura -> clases_amplificacion.png")
