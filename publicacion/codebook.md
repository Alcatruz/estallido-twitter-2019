# Codebook — Corpus de Twitter del estallido social chileno (octubre 2019)

Paquete de datos derivados para depósito con DOI. Generado por
[`R/13_paquete_publicacion.R`](../../../R/13_paquete_publicacion.R).

## Qué es este paquete y qué no es

**No contiene el corpus.** Los términos de servicio de X/Twitter prohíben
redistribuir el contenido de los tweets y los campos de perfil. Lo que se
publica son:

1. Los **identificadores** de los tweets, para rehidratación vía API.
2. **Derivados que no son contenido**: conteos temporales, estructura de red,
   frecuencias léxicas y registros de figuras públicas.

Advertencia práctica: desde que la API de X dejó de tener acceso gratuito, la
rehidratación a partir de identificadores es inviable para la mayoría de los
usuarios. Los derivados existen precisamente por eso. El precedente chileno más
cercano, [twConstitution](https://doi.org/10.17605/OSF.IO/73NDB)
(González-Bustamante, 2023), resolvió el mismo problema con acceso bajo
solicitud.

## Seudonimización

| Tipo de cuenta | Tratamiento |
|---|---|
| Cargo público en ejercicio en octubre de 2019 (ver `cuentas_politicas.csv`) | conserva su identificador real |
| Cualquier otra | reemplazada por hash estable `u` + 12 hex |

El hash usa `xxhash64` sobre el handle en minúsculas con sal fija
(`estallido2019`). Sin sal, el hash de un handle corto es reversible por fuerza
bruta. El mismo identificador seudónimo es consistente entre todos los archivos
del paquete, de modo que la estructura de la red se preserva.

Son 125 cuentas identificables de 702.103. El criterio es que un cargo público
actuando en público no tiene expectativa de anonimato, y la trazabilidad es
parte del valor analítico del dato.

## Archivos

### `tweet_ids.txt.gz` — 6.119.933 filas
Un identificador de estado por línea, sin encabezado. Para rehidratar.

### `serie_temporal.csv` — 1.244 filas
Volumen por día, hora y tipo de publicación.

| Variable | Tipo | Descripción |
|---|---|---|
| `fecha` | fecha | día en hora local de Chile (`America/Santiago`) |
| `hora` | entero 0–23 | hora local de Chile |
| `tipo` | texto | `original`, `cita` o `retweet` |
| `tweets` | entero | publicaciones en ese bloque |
| `cuentas` | entero | cuentas distintas que publicaron |

### `cuentas.csv` — 702.103 filas
Una fila por cuenta presente en el corpus, como emisora o como retuiteada.

| Variable | Tipo | Descripción |
|---|---|---|
| `id` | texto | handle real si es cargo público, si no hash seudónimo |
| `es_publica` | lógico | `TRUE` si está en el registro de cargos |
| `clase` | factor | `politica`, `medio` o `ciudadana` (ver nota abajo) |
| `tweets` | entero | publicaciones emitidas; `0` si solo aparece retuiteada |
| `anio_cuenta` | entero | año de creación de la cuenta |
| `seguidores_orden` | entero | orden de magnitud de seguidores (potencia de 10) |

La antigüedad va en **año** y los seguidores en **orden de magnitud** a
propósito: la fecha de creación al segundo junto con el conteo exacto de
seguidores vuelve a identificar a una cuenta aunque se oculte el nombre.

**Sobre `clase`:** `politica` proviene de un registro verificado contra la
Biblioteca del Congreso Nacional y Wikidata. `medio` es una heurística de
vocabulario y se equivoca en ambas direcciones (no detecta a `@teleSURtv`,
marca como medio a periodistas a título personal). Úsese `medio` como filtro
para que `ciudadana` no quede contaminada de prensa, no como censo de medios.

### `red_retweets_18oct.csv` — 407.387 filas
Red dirigida de retweets del 18 de octubre, agregada y ponderada.

| Variable | Tipo | Descripción |
|---|---|---|
| `hub` | texto | id de la cuenta que retuitea |
| `authority` | texto | id de la cuenta retuiteada |
| `peso` | entero | número de retweets de `hub` hacia `authority` ese día |

### `nodos_18oct.csv` — 56.724 filas
Nodos del componente gigante tras poda por k-core ≥ 2, con su comunidad
(Leiden, modularidad 0,505) y coordenadas de layout (sparse stress).

| Variable | Descripción |
|---|---|
| `id` | identificador consistente con `red_retweets_18oct.csv` |
| `comunidad` | id de comunidad |
| `ind`, `outd` | grado de entrada y de salida |
| `fuerza_in` | retweets recibidos (grado ponderado) |
| `x`, `y` | coordenadas del layout |

### `cuentas_politicas.csv` — 171 filas
Registro de cuentas de cargos en ejercicio en octubre de 2019: diputados,
senadores, ministros, Presidente y presidentes de partido. Incluye la fuente de
cada identificación (reseña BCN, Wikidata, o nombre validado contra la bio).

### `padron_diputados_2018_2022.csv` — 162 filas
Padrón reconciliado de quienes ejercieron como diputadas o diputados en el
período, con su cuenta y las fechas del cargo.

### `terminos_recoleccion.csv` — 40 filas
Secuencia de cobertura glotona que reconstruye los términos de recolección. El
corpus no venía con su consulta documentada.

| Variable | Descripción |
|---|---|
| `paso` | orden en la secuencia |
| `termino` | hashtag |
| `tweets_nuevos` | tweets que cubre y que ningún término anterior cubría |
| `pct_acumulado` | cobertura acumulada del corpus |
| `familia` | `fecha`, `evasion`, `ddhh`, `consigna`, `gobierno`, `movilizacion`, `ingles` |

### `18oct_contagio_comunidades.csv` — 8 filas
Índice de contagio por comunidad: qué fracción de los tweets de cada comunidad
trae hashtag de fecha pero ningún término chileno de la consulta.

### `hashtags_corpus.csv`, `global_por_dia.csv`, `global_por_hora.csv`
Frecuencias de los 300 hashtags más usados y series agregadas.

## Limitaciones del corpus que deben declararse en cualquier reutilización

1. **Días truncados.** El 11 y el 29 de octubre están cortados por la ventana de
   descarga (168 y 4.203 tweets). No son días de baja actividad.
2. **Sesgo hacia la primera semana.** El 25 de octubre, día de la marcha más
   grande de Chile, registra 112.812 tweets, menos que el día anterior. La
   recolección seguía vocabulario de la fase inicial que para esa fecha ya había
   sido reemplazado.
3. **Contaminación por hashtags de fecha.** Seis hashtags (`#17oct` a `#22oct`)
   explican 941.446 tweets, 15,4% del corpus, y los usa cualquiera en el mundo
   ese día. Tres comunidades del 18 de octubre entraron exclusivamente por esa
   puerta y no contienen vocabulario del estallido.
4. **Procedencia no establecida.** Quién recolectó el corpus, con qué
   herramienta y bajo qué permisos no está documentado y debe resolverse antes
   de cualquier depósito público.

## Licencia

Por definir. Los derivados admiten CC BY 4.0; los identificadores se rigen por
los términos de servicio de X.
