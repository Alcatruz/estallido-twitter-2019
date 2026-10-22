# estallido_data

Procesamiento y exploracion de `tw_chile_social_crisis.rds`: corpus de tweets
del estallido social chileno (octubre 2019 en adelante).

## Estructura

```
estallido_data/
├── estallido_data.Rproj
├── informe_corpus.qmd        reporte Quarto sobre el CORPUS COMPLETO
├── R/
│   ├── 00_setup.R            rutas, parametros y utilidades comunes
│   ├── 01_rds_a_csv.R        lee el .rds -> CSV completo + muestra aleatoria
│   ├── 01b_tabla_analisis.R  tabla liviana (parquet) para el corpus completo
│   ├── 02_explorar_muestra.R exploracion descriptiva sobre la muestra
│   ├── 03_red_18oct.R        red de retweets del 18-O (Leiden, sparse stress)
│   ├── 04_red_pdf.R          red en PDF vectorial, con y sin etiquetas
│   ├── 05_corpus_comunidades.R  cuentas fuertes y corpus por comunidad
│   ├── 06_distribucion_global.R tweets y retweets en los 19 dias
│   ├── 07_politicos.R        cruce preliminar padron-corpus por nombre
│   ├── 08a_padron_diputados.R padron 2018-2022 reconciliado (BCN + Wikidata)
│   └── 08_cuentas_diputados.R cuenta de X de cada diputado y presencia en corpus
├── scripts/
│   ├── bajar_resenas_bcn.py  descarga las resenas BCN y extrae la cuenta de X
│   └── wikidata_diputados.rq consulta SPARQL de diputados con usuario de X
├── data/
│   ├── externos/             insumos descargados: nomina y resenas BCN, Wikidata
│   ├── padron_diputados_2018_2022.csv   162 personas, 156 en ejercicio oct-2019
│   ├── roster_politicos_2019.csv        padron preliminar (tiene errores, ver 08a)
│   ├── raw/                  (el .rds original vive en la raiz del proyecto)
│   └── processed/            CSV completo, muestra .csv y .rds
├── output/
│   ├── figuras/              graficos .png
│   └── tablas/               reportes y tablas .csv
└── tw_chile_social_crisis.rds   ~1.4 GB, comprimido con gzip
```

## Como correrlo

Abre `estallido_data.Rproj` (asi el working directory queda en la raiz) y:

```r
source("R/01_rds_a_csv.R")        # ~10 min: descomprime 1.4 GB y escribe el CSV
source("R/01b_tabla_analisis.R")  # ~40 s: parquet liviano con las columnas clave
source("R/02_explorar_muestra.R") # rapido: trabaja solo sobre la muestra
quarto::quarto_render("informe_corpus.qmd")   # reporte del corpus completo
```

O desde la terminal:

```bash
Rscript R/01_rds_a_csv.R
Rscript R/01b_tabla_analisis.R
Rscript R/02_explorar_muestra.R
quarto render informe_corpus.qmd
```

## Que hay en el corpus

| | |
|---|---|
| Filas | 6.119.933 tweets x 90 columnas (salida de `rtweet`) |
| Peso | 1,39 GB comprimido / 13,6 GB en RAM / 7,13 GB en CSV |
| Ventana | 11 al 29 de octubre de 2019 (hora de Chile) |
| Composicion | 87,5% retweets, 10,4% originales, 2,1% citas |
| Cuentas | 701.325 distintas publicando |
| Idioma | 92,7% `es`; 49 codigos en total, casi todo el resto sin texto clasificable |

## Parametros (en `R/00_setup.R`)

| Parametro       | Default  | Que hace                                             |
|-----------------|----------|------------------------------------------------------|
| `SEMILLA`       | `2019`   | semilla de la muestra aleatoria (reproducible)        |
| `N_MUESTRA`     | `200000` | filas de la muestra                                   |
| `EXPORTAR_CSV`  | `TRUE`   | `FALSE` se salta el CSV completo (es el paso lento)   |
| `COMPRIMIR_CSV` | `FALSE`  | `TRUE` escribe `.csv.gz` (mucho mas liviano)          |

## Notas

- El `.rds` se lee **una sola vez** en `01`: descomprimirlo cuesta minutos y
  varios GB de RAM, asi que el CSV completo y la muestra salen de esa lectura.
- Las columnas-lista de `rtweet` (hashtags, menciones, urls) se aplanan a texto
  separado por `|` antes de escribir el CSV.
- **Fechas:** el corpus viene en UTC. Al releer el CSV, `fread` puede entregar
  `created_at` como texto ISO (`2019-10-22T17:49:53Z`) y `as.POSIXct()` sin
  formato explicito NO reconoce la `T`: cae al patron `%Y-%m-%d` y trunca todo
  a medianoche sin avisar. `01b` parsea con formato explicito y aborta si las
  horas quedan degeneradas.
- El CSV se escribe en UTF-8 sin BOM. Si lo abres en Excel y ves los acentos
  rotos, usa Datos > Obtener datos > Desde texto y elige UTF-8.
- Los datos no se versionan: `.gitignore` excluye `*.rds`, `*.csv` y `output/`.

## Cuentas de diputados (08a y 08)

Fuentes de la cuenta de X, en orden de autoridad:

1. **Reseñas biográficas de la BCN** (`bcn.cl/historiapolitica/resenas_parlamentarias`),
   descargadas desde la nómina del período con `scripts/bajar_resenas_bcn.py`.
2. **Wikidata**, propiedad P2002, cargo `Q18067639` (diputado de Chile),
   período "LV periodo legislativo". Consulta en `scripts/wikidata_diputados.rq`.
3. **Búsqueda por nombre en el corpus**, solo aceptada si el perfil tiene marcas
   específicas de diputado ("diputad", "distrito", "cámara"...) y ninguna de
   cuenta extranjera.

Advertencias:

- La BCN y Wikidata pueden traer la cuenta **actual**, no la de 2019: Fidel
  Espinoza figura como `@fidelsenador` y en el corpus tuiteaba como
  `@fideldiputado`. Esos casos quedan marcados como "revisar: handle cambio".
- El partido viene de la nómina BCN y puede ser la afiliación **posterior** a
  2019 (Ibáñez y Winter figuran como IND; en 2019 eran Convergencia Social).
- `roster_politicos_2019.csv` salió de un resumen automático de Wikipedia y
  omite o confunde diputados (Bellolio, Hugo Gutiérrez, Garín, Melero). Para
  diputados usar `padron_diputados_2018_2022.csv`.
