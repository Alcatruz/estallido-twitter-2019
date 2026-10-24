# ---------------------------------------------------------------------------
# 02_explorar_muestra.R
#
# Exploracion descriptiva sobre la muestra generada en 01_rds_a_csv.R.
# El script se adapta a las columnas que efectivamente existan: cada bloque
# se salta solo si le faltan variables.
#
# Salidas: output/figuras/*.png  y  output/tablas/*.csv
# ---------------------------------------------------------------------------

source("R/00_setup.R")

suppressPackageStartupMessages({
  library(ggplot2)
  library(lubridate)
})

theme_set(theme_minimal(base_size = 12))
guardar_fig <- function(p, nombre, w = 9, h = 5.5) {
  ruta <- file.path(RUTA_FIG, paste0(nombre, ".png"))
  ggsave(ruta, p, width = w, height = h, dpi = 150, bg = "white")
  message("  figura -> ", basename(ruta))
}
guardar_tab <- function(x, nombre) {
  ruta <- file.path(RUTA_TAB, paste0(nombre, ".csv"))
  fwrite(x, ruta)
  message("  tabla  -> ", basename(ruta))
}

# --- 0. Cargar la muestra --------------------------------------------------
archivo_muestra <- list.files(RUTA_PROC, pattern = "^muestra_.*\\.rds$", full.names = TRUE)
if (!length(archivo_muestra)) stop("No hay muestra. Corre primero R/01_rds_a_csv.R")
m <- as.data.table(readRDS(archivo_muestra[1]))
message("Muestra: ", basename(archivo_muestra[1]), " -- ",
        format(nrow(m), big.mark = "."), " filas x ", ncol(m), " columnas")

hay <- function(...) all(c(...) %in% names(m))

# --- 1. Vista general ------------------------------------------------------
message("\n== Estructura ==")
print(data.table(
  columna = names(m),
  tipo    = vapply(m, function(x) paste(class(x), collapse = "/"), character(1)),
  pct_na  = round(100 * vapply(m, function(x) mean(is.na(x)), numeric(1)), 1)
))

faltantes <- data.table(
  columna = names(m),
  pct_na  = round(100 * vapply(m, function(x) mean(is.na(x)), numeric(1)), 2)
)[order(-pct_na)]
guardar_tab(faltantes, "muestra_valores_faltantes")

# --- 2. Cobertura temporal -------------------------------------------------
col_fecha <- intersect(c("created_at", "fecha", "date", "timestamp"), names(m))[1]
if (!is.na(col_fecha)) {
  # El corpus viene en UTC; para leerlo como chileno basta cambiar el tzone
  # (un POSIXct es un instante absoluto, no una hora de pared).
  m[, .fecha := as.POSIXct(get(col_fecha), tz = "UTC")]
  setattr(m$.fecha, "tzone", "America/Santiago")
  message("\n== Rango temporal ==")
  print(range(m$.fecha, na.rm = TRUE))

  # as.Date() sobre POSIXct usa UTC y no el tzone del objeto: hay que formatear
  # primero o la serie diaria queda corrida tres horas.
  por_dia <- m[!is.na(.fecha), .N,
               by = .(dia = as.Date(format(.fecha, "%Y-%m-%d")))][order(dia)]
  guardar_tab(por_dia, "muestra_tweets_por_dia")

  p <- ggplot(por_dia, aes(dia, N)) +
    geom_col(fill = "#c1121f", width = 1) +
    labs(title = "Tweets por dia (muestra aleatoria)",
         subtitle = "Estallido social chileno",
         x = NULL, y = "tweets en la muestra")
  guardar_fig(p, "01_tweets_por_dia")

  por_hora <- m[!is.na(.fecha),
                .N, by = .(hora = as.integer(format(.fecha, "%H")))][order(hora)]
  p <- ggplot(por_hora, aes(hora, N)) +
    geom_col(fill = "#003049") +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(title = "Distribucion horaria (hora de Chile)",
         x = "hora local", y = "tweets")
  guardar_fig(p, "02_tweets_por_hora", w = 7, h = 4)
}

# --- 3. Retweets vs contenido original -------------------------------------
if (hay("is_retweet")) {
  message("\n== Retweets ==")
  tab <- m[, .N, by = is_retweet][, pct := round(100 * N / sum(N), 1)][]
  print(tab)
  guardar_tab(tab, "muestra_retweets")
}

# --- 4. Usuarios mas activos -----------------------------------------------
col_user <- intersect(c("screen_name", "usuario", "user"), names(m))[1]
if (!is.na(col_user)) {
  top_u <- m[, .N, by = c(col_user)][order(-N)][1:20]
  setnames(top_u, col_user, "usuario")
  message("\n== Top 20 cuentas por volumen ==")
  print(top_u)
  guardar_tab(top_u, "muestra_top_usuarios")

  p <- ggplot(top_u, aes(reorder(usuario, N), N)) +
    geom_col(fill = "#780000") + coord_flip() +
    labs(title = "Cuentas mas activas en la muestra", x = NULL, y = "tweets")
  guardar_fig(p, "03_top_usuarios", h = 6)
}

# --- 5. Hashtags -----------------------------------------------------------
if ("hashtags" %in% names(m)) {
  hs <- m[!is.na(hashtags) & hashtags != "", tolower(unlist(strsplit(hashtags, "|", fixed = TRUE)))]
  hs <- hs[nzchar(hs)]
  top_h <- as.data.table(sort(table(hs), decreasing = TRUE)[1:30])
  setnames(top_h, c("hashtag", "N"))
  top_h[, N := as.integer(N)]
  message("\n== Top 30 hashtags ==")
  print(top_h)
  guardar_tab(top_h, "muestra_top_hashtags")

  p <- ggplot(top_h[1:20], aes(reorder(hashtag, N), N)) +
    geom_col(fill = "#0f4c5c") + coord_flip() +
    labs(title = "Hashtags mas frecuentes", x = NULL, y = "menciones")
  guardar_fig(p, "04_top_hashtags", h = 6)
}

# --- 5b. Cuentas mas retuiteadas -------------------------------------------
# Distinto del top de volumen: aqui manda quien es amplificado, no quien publica.
if (hay("is_retweet", "retweet_screen_name")) {
  top_rt <- m[is_retweet == TRUE, .N, by = .(cuenta = retweet_screen_name)][order(-N)][1:20]
  message("
== Top 20 cuentas mas retuiteadas ==")
  print(top_rt)
  guardar_tab(top_rt, "muestra_top_retuiteadas")

  p <- ggplot(top_rt, aes(reorder(cuenta, N), N)) +
    geom_col(fill = "#5c374c") + coord_flip() +
    labs(title = "Cuentas mas retuiteadas en la muestra",
         subtitle = "cuenta original del RT, no quien lo republica",
         x = NULL, y = "retweets recibidos")
  guardar_fig(p, "07_top_retuiteadas", h = 6)
}

# --- 6. Idioma y cliente ---------------------------------------------------
if ("lang" %in% names(m)) {
  tab <- m[, .N, by = lang][order(-N)][1:10][, pct := round(100 * N / nrow(m), 2)][]
  message("\n== Idiomas ==");  print(tab)
  guardar_tab(tab, "muestra_idiomas")
}
if ("source" %in% names(m)) {
  tab <- m[, .N, by = source][order(-N)][1:15]
  message("\n== Clientes/apps ==");  print(tab)
  guardar_tab(tab, "muestra_fuentes")
}

# --- 7. Metricas de difusion -----------------------------------------------
metricas <- intersect(c("retweet_count", "favorite_count", "quote_count",
                        "reply_count", "followers_count"), names(m))
# quote_count y reply_count vienen 100% NA en esta descarga: se descartan solas
metricas <- metricas[vapply(metricas, function(v) !all(is.na(m[[v]])), logical(1))]
if (length(metricas)) {
  message("\n== Metricas de difusion ==")
  res <- rbindlist(lapply(metricas, function(v) {
    x <- as.numeric(m[[v]])
    data.table(metrica = v, media = mean(x, na.rm = TRUE),
               mediana = median(x, na.rm = TRUE),
               p90 = quantile(x, .90, na.rm = TRUE),
               p99 = quantile(x, .99, na.rm = TRUE),
               maximo = max(x, na.rm = TRUE))
  }))
  print(res)
  guardar_tab(res, "muestra_metricas_difusion")

  if ("retweet_count" %in% metricas) {
    p <- ggplot(m[retweet_count > 0], aes(retweet_count)) +
      geom_histogram(bins = 50, fill = "#c1121f") +
      scale_x_log10(labels = scales::comma) +
      labs(title = "Distribucion de retweets (escala log, solo RT > 0)",
           x = "retweets", y = "tweets")
    guardar_fig(p, "05_distribucion_retweets", w = 7, h = 4)
  }
}

# --- 8. Palabras mas frecuentes -------------------------------------------
col_texto <- intersect(c("text", "texto", "full_text"), names(m))[1]
if (!is.na(col_texto) && requireNamespace("tidytext", quietly = TRUE)) {
  suppressPackageStartupMessages(library(tidytext))
  vacias <- unique(c(stopwords::stopwords("es"), stopwords::stopwords("en"),
                     "rt", "q", "si", "https", "t.co", "amp"))
  palabras <- m[, .(txt = tolower(get(col_texto)))][
    , txt := gsub("https?://\\S+|@\\S+|#\\S+|[^[:alpha:][:space:]áéíóúñü]", " ", txt)]
  tokens <- unlist(strsplit(palabras$txt, "\\s+"))
  tokens <- tokens[nchar(tokens) > 3 & !tokens %in% vacias]
  top_p <- as.data.table(sort(table(tokens), decreasing = TRUE)[1:40])
  setnames(top_p, c("palabra", "N")); top_p[, N := as.integer(N)]
  message("\n== Top 40 palabras ==");  print(top_p)
  guardar_tab(top_p, "muestra_top_palabras")

  p <- ggplot(top_p[1:25], aes(reorder(palabra, N), N)) +
    geom_col(fill = "#335c67") + coord_flip() +
    labs(title = "Palabras mas frecuentes (sin stopwords)", x = NULL, y = "frecuencia")
  guardar_fig(p, "06_top_palabras", h = 6)
}

message("\nExploracion lista. Revisa output/figuras y output/tablas.")
