# ---------------------------------------------------------------------------
# 12_eventos_gobierno.R
#
# Estudio de eventos: que le pasa a la conversacion antes, durante y despues de
# cada intervencion del gobierno.
#
# La pregunta no es si sube el volumen (siempre sube) sino DONDE sube: si la
# actividad se acumula ANTES del anuncio, el anuncio llega a una conversacion
# ya encendida y no la enciende el; si salta justo en el anuncio, la noticia es
# el motor. Es la distincion entre una racha endogena, que nace de la propia
# red, y una exogena, disparada desde afuera.
#
# ANCLAS. No son el instante en que el gobierno hablo sino el MINUTO PEAK DE
# REACCION en el corpus, localizado buscando la frase caracteristica de cada
# episodio. Van unos minutos despues de la intervencion real, y asi se declaran.
#
# TONO. Se mide sobre el subconjunto que menciona al gobierno, no sobre todo el
# corpus: asi la serie es tono hacia el gobierno y no volumen general.
#
# Salidas: output/tablas/eventos_ventanas.csv
#          output/tablas/eventos_resumen.csv
#          output/figuras/eventos_gobierno.png
# ---------------------------------------------------------------------------

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(stringi); library(ggplot2); library(scales); library(patchwork)
})
theme_set(theme_minimal(base_size = 11))

VENTANA_H <- 12L   # horas a cada lado del ancla
BIN_MIN   <- 15L   # ancho del bin

# --- Lexicos (parametros declarados, no escondidos) --------------------------
# Antagonismo: condena explicita. Se deja fuera "weon" a proposito, que en
# Chile es muletilla y no insulto dirigido.
PAT_ANTAGONISMO <- paste0(
  "asesin|genocid|criminal|dictadur|fascist|represor|represi|",
  "milicos|pacos|tirano|corrupt|ladron|ladr[oó]n|sinverguenza|",
  "culia|cul[ií]a|ctm|conchetumadre|qliao|chuchetumadre|",
  "violador(es)? de (los )?ddhh|violacion de ddhh")
PAT_DEMANDA <- "renuncia|que se vaya|fuera pi|dimit|destitu|acusacion constitucional"
PAT_GOBIERNO <- paste0("piñera|pinera|gobierno|ministr|la moneda|chadwick|",
                       "presidente|ejecutivo|intendente|cadena nacional")

# --- Eventos ----------------------------------------------------------------
eventos <- data.table(
  id = c("emergencia", "guerra", "agenda", "gabinete"),
  etiqueta = c("Estado de emergencia\n(19-oct 00:28)",
               "\"Estamos en guerra\"\n(20-oct 23:28)",
               "Agenda social\n(22-oct 22:14)",
               "Cambio de gabinete\n(28-oct 12:52)"),
  ancla = as.POSIXct(c("2019-10-19 00:28:00", "2019-10-20 23:28:00",
                       "2019-10-22 22:14:00", "2019-10-28 12:52:00"),
                     tz = "America/Santiago")
)

# --- Datos ------------------------------------------------------------------
a_utc <- function(x) {
  if (inherits(x, "POSIXct")) { setattr(x, "tzone", "UTC"); return(x) }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
x <- cronometrar("Leyendo el corpus", {
  fread(file.path(RUTA_PROC, "tw_chile_social_crisis.csv"),
        select = c("created_at", "text", "is_retweet", "screen_name"),
        showProgress = FALSE)
})
x[, created_at := a_utc(created_at)]
x[, h := as.POSIXct(format(created_at, tz = "America/Santiago"), tz = "America/Santiago")]
x[, tl := tolower(text)]
x[, `:=`(gob  = stri_detect_regex(tl, PAT_GOBIERNO),
         anta = stri_detect_regex(tl, PAT_ANTAGONISMO),
         dem  = stri_detect_regex(tl, PAT_DEMANDA))]
message(sprintf("Menciona al gobierno: %.1f%% | antagonismo: %.1f%% | demanda de salida: %.1f%%",
                100 * mean(x$gob), 100 * mean(x$anta), 100 * mean(x$dem)))

# --- Ventanas ---------------------------------------------------------------
ventanas <- rbindlist(lapply(seq_len(nrow(eventos)), function(i) {
  e <- eventos[i]
  w <- x[h >= e$ancla - VENTANA_H * 3600 & h <= e$ancla + VENTANA_H * 3600]
  w[, min_rel := as.numeric(difftime(h, e$ancla, units = "mins"))]
  w[, bin := floor(min_rel / BIN_MIN) * BIN_MIN]
  w[, .(tweets = .N,
        cuentas = uniqueN(screen_name),
        pct_original = 100 * mean(!is_retweet),
        tweets_gob = sum(gob),
        # tono: sobre los que hablan del gobierno, no sobre todo
        pct_antagonismo = 100 * sum(anta & gob) / pmax(sum(gob), 1),
        pct_demanda = 100 * sum(dem & gob) / pmax(sum(gob), 1)),
     by = bin][, `:=`(id = e$id, etiqueta = e$etiqueta)]
}))
fwrite(ventanas, file.path(RUTA_TAB, "eventos_ventanas.csv"))

# --- Antes / durante / despues ----------------------------------------------
# "Durante" es la hora del ancla; antes y despues, las 6 horas a cada lado.
ventanas[, fase := fcase(bin < -60, "antes (-6h a -1h)",
                         bin >= -60 & bin <= 60, "durante (+/-1h)",
                         default = "despues (+1h a +6h)")]
res <- ventanas[abs(bin) <= 360, .(
  tweets_por_hora = round(sum(tweets) / (uniqueN(bin) * BIN_MIN / 60)),
  pct_antagonismo = round(weighted.mean(pct_antagonismo, tweets_gob), 1),
  pct_demanda     = round(weighted.mean(pct_demanda, tweets_gob), 1),
  pct_original    = round(weighted.mean(pct_original, tweets), 1)
), by = .(etiqueta = sub("\n.*", "", etiqueta), fase)]
res[, fase := factor(fase, levels = c("antes (-6h a -1h)", "durante (+/-1h)", "despues (+1h a +6h)"))]
setorder(res, etiqueta, fase)
fwrite(res, file.path(RUTA_TAB, "eventos_resumen.csv"))
message("\n== Antes / durante / despues ==")
print(res)

# --- Endogeno o exogeno, controlando el ciclo diario -------------------------
# Sin control esto no se puede afirmar: la emergencia (00:28) y "estamos en
# guerra" (23:28) caen de noche, cuando el volumen ya baja solo, y el cambio de
# gabinete al mediodia, cuando sube. Un "pico antes del anuncio" podria ser
# nada mas que la curva circadiana.
#
# Control: para cada bin de 15 minutos se calcula el volumen ESPERADO como la
# mediana de ese mismo minuto-del-dia en los demas dias del corpus, excluyendo
# los dias de evento. La serie relevante es la razon observado/esperado.
x[, min_dia := as.integer(format(h, "%H")) * 60L + as.integer(format(h, "%M"))]
x[, bin_dia := floor(min_dia / BIN_MIN) * BIN_MIN]
# as.Date() sobre POSIXct usa UTC: formatear primero para que el dia sea el
# chileno y los dias de evento se excluyan por su frontera real.
x[, dia := as.Date(format(h, "%Y-%m-%d"))]
dias_evento <- unique(as.Date(format(c(eventos$ancla - 86400, eventos$ancla,
                                       eventos$ancla + 86400), "%Y-%m-%d")))

perfil <- x[!dia %in% dias_evento, .N, by = .(dia, bin_dia)][
  , .(esperado = median(N)), by = bin_dia]
message(sprintf("
Perfil diario base: %d bins, de %d dias no-evento",
                nrow(perfil), uniqueN(x[!dia %in% dias_evento]$dia)))

ventanas[, bin_dia := {
  t <- eventos$ancla[match(id, eventos$id)] + bin * 60
  floor((as.integer(format(t, "%H")) * 60L + as.integer(format(t, "%M"))) / BIN_MIN) * BIN_MIN
}]
ventanas[, esperado := perfil$esperado[match(bin_dia, perfil$bin_dia)]]
ventanas[, exceso := tweets / pmax(esperado, 1)]

forma <- ventanas[abs(bin) <= 360, {
  .(bin_peak_crudo = bin[which.max(tweets)],
    bin_peak_ajustado = bin[which.max(exceso)],
    exceso_pre  = round(mean(exceso[bin >= -120 & bin < 0]), 2),
    exceso_post = round(mean(exceso[bin > 0 & bin <= 120]), 2))
}, by = .(etiqueta = sub("
.*", "", etiqueta))]
forma[, cambio := round(exceso_post / exceso_pre, 2)]
forma[, lectura := fifelse(cambio >= 1.5, "el anuncio ENCIENDE (exogeno)",
                    fifelse(cambio <= 0.67, "el anuncio APAGA",
                            "el anuncio NO MUEVE el volumen (endogeno)"))]
message("
== Forma de la racha, ajustada por ciclo diario ==")
print(forma)
fwrite(forma, file.path(RUTA_TAB, "eventos_forma.csv"))

# --- Figura ------------------------------------------------------------------
ventanas[, horas := bin / 60]
p1 <- ggplot(ventanas, aes(horas, exceso)) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey45") +
  geom_vline(xintercept = 0, colour = "#c1121f", linewidth = 0.5) +
  geom_line(colour = "#003049", linewidth = 0.6) +
  facet_wrap(~ etiqueta, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-12, 12, 6)) +
  labs(x = NULL, y = "veces lo esperado",
       subtitle = "Volumen sobre lo esperado para esa hora del dia (punteada = dia normal)")

p2 <- ggplot(ventanas[tweets_gob >= 30], aes(horas, pct_antagonismo)) +
  geom_vline(xintercept = 0, colour = "#c1121f", linewidth = 0.5) +
  geom_line(colour = "#5c374c", linewidth = 0.6) +
  geom_smooth(se = FALSE, colour = "#f4a261", linewidth = 0.6, method = "loess", span = 0.3) +
  facet_wrap(~ etiqueta, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-12, 12, 6)) +
  labs(x = "horas respecto del anuncio", y = "% antagonismo",
       subtitle = "Tono hacia el gobierno (sobre los tweets que lo mencionan)")

lam <- p1 / p2 +
  plot_annotation(title = "La conversacion antes, durante y despues de cada intervencion del gobierno",
                  subtitle = "linea roja: minuto peak de reaccion en el corpus")
ggsave(file.path(RUTA_FIG, "eventos_gobierno.png"), lam,
       width = 14, height = 7.5, dpi = 150, bg = "white")
message("  figura -> eventos_gobierno.png")
