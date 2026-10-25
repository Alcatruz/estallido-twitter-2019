suppressPackageStartupMessages({library(data.table); library(ggplot2); library(scales); library(knitr)})
TAB <- "../output/tablas"; DAT <- "../data"
leer <- function(f, dir = TAB) fread(file.path(dir, f), encoding = "UTF-8", na.strings = c("", "NA"))
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
pc <- function(x, d = 1) paste0(format(round(x, d), decimal.mark = ","), "%")
theme_set(theme_minimal(base_size = 12))
tabla <- function(x, ...) kable(x, format.args = list(big.mark = "."), ...)
