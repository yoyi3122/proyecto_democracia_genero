# =============================================================================
# Datos/depuracion.R
# Proyecto: Democracia e igualdad de género en el mundo (Gapminder)
# Módulo 8 · Reto 1 (diseño del dashboard) y Reto 2 (proyecto reproducible)
#
# Qué hace este script:
#   1. Descarga de GitHub los indicadores de Gapminder (repositorios open-numbers),
#      fijados a una versión concreta (commit) para que el resultado sea reproducible.
#   2. Guarda una copia de cada fichero original en Datos/original/.
#   3. Une todos los indicadores en una sola tabla país-año.
#   4. Depura los datos y crea nuevas variables útiles para el dashboard.
#   5. Guarda la tabla final en Datos/depurada/.
#
# Cómo ejecutarlo: abrir el proyecto (.Rproj) en RStudio y ejecutar el script.
# Las rutas se construyen con here(), que parte siempre de la carpeta del
# proyecto: no hay rutas locales absolutas y funciona en cualquier ordenador.
# =============================================================================

library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# 0. Parámetros
# -----------------------------------------------------------------------------

# Versión fija de cada repositorio (commit consultado el 2026-10-01).
# Si se quiere usar la versión más reciente, basta con sustituir por "master".
commit_fasttrack <- "85af9e1b82e5f5de64b2ad52c5fe2bd1c47b074b"
commit_sg        <- "ee190605fdec9d1c35c38e18e869a400a5818fe6"

url_base <- "https://raw.githubusercontent.com/open-numbers"
url_ft <- file.path(url_base, "ddf--gapminder--fasttrack", commit_fasttrack)
url_sg <- file.path(url_base, "ddf--gapminder--systema_globalis", commit_sg)

dir_original <- here("Datos", "original")   # ficheros tal como se descargan
dir_depurada <- here("Datos", "depurada")   # tabla final depurada
dir.create(dir_original, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_depurada, recursive = TRUE, showWarnings = FALSE)

# Catálogo de indicadores: nombre corto que usaremos, repositorio y ruta del fichero.
indicadores <- tribble(
  ~variable,          ~url,   ~ruta,
  # Democracia
  "polity",           url_sg, "countries-etc-datapoints/ddf--datapoints--democracy_score_use_as_color--by--geo--time.csv",
  "fh_libertad",      url_ft, "countries_etc_datapoints/ddf--datapoints--freedix_fh--by--country--time.csv",
  "fh_estatus",       url_ft, "countries_etc_datapoints/ddf--datapoints--freedstatus_fh--by--country--time.csv",
  "idea_libertades",  url_ft, "countries_etc_datapoints/ddf--datapoints--cliberties_idea--by--country--time.csv",
  "eiu_democracia",   url_ft, "countries_etc_datapoints/ddf--datapoints--demox_eiu--by--country--time.csv",
  # Igualdad de género
  "idea_igualdad_genero", url_ft, "countries_etc_datapoints/ddf--datapoints--gendereq_idea--by--country--time.csv",
  "mujeres_parlamento",   url_ft, "countries_etc_datapoints/ddf--datapoints--wn_bothhouses_c--by--country--time.csv",
  "jefa_estado",          url_ft, "countries_etc_datapoints/ddf--datapoints--female_hos--by--country--time.csv",
  "part_laboral_fem",     url_sg, "countries-etc-datapoints/ddf--datapoints--females_aged_15plus_labour_force_participation_rate_percent--by--geo--time.csv",
  "escolarizacion_ratio", url_sg, "countries-etc-datapoints/ddf--datapoints--mean_years_in_school_women_percent_men_25_to_34_years--by--geo--time.csv",
  # Contexto
  "pib_pc",           url_ft, "countries_etc_datapoints/ddf--datapoints--gdp_pcap--by--country--time.csv",
  "esperanza_vida",   url_ft, "countries_etc_datapoints/ddf--datapoints--lex--by--country--time.csv",
  "poblacion",        url_ft, "countries_etc_datapoints/ddf--datapoints--pop--by--country--time.csv"
)

# -----------------------------------------------------------------------------
# 1. Descarga (con copia local en Datos/original)
# -----------------------------------------------------------------------------

# Descarga un fichero solo si no existe ya en Datos/original (evita descargas repetidas).
descargar <- function(url, ruta, destino) {
  if (!file.exists(destino)) download.file(file.path(url, ruta), destino, quiet = TRUE)
  destino
}

# Lee un indicador y lo deja con tres columnas homogéneas: geo, anio, <variable>.
leer_indicador <- function(variable, url, ruta) {
  destino <- descargar(url, ruta, file.path(dir_original, basename(ruta)))
  read_csv(destino, show_col_types = FALSE) |>
    rename(geo = 1, anio = 2, !!variable := 3)
}

lista_datos <- pmap(indicadores, leer_indicador)

# Metadatos de los países: región, grupo de ingresos, coordenadas, etc.
paises <- read_csv(
  descargar(url_sg, "ddf--entities--geo--country.csv", file.path(dir_original, "paises.csv")),
  show_col_types = FALSE
)
# Nombres de los países en castellano (traducción oficial de Gapminder).
paises_es <- read_csv(
  descargar(url_sg, "lang/es-ES/ddf--entities--geo--country.csv", file.path(dir_original, "paises_es.csv")),
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 2. Unión en una tabla país-año
# -----------------------------------------------------------------------------

datos <- reduce(lista_datos, full_join, by = c("geo", "anio"))

# -----------------------------------------------------------------------------
# 3. Depuración
# -----------------------------------------------------------------------------

datos <- datos |>
  # Periodo de estudio: 1800-2022. Gapminder incluye proyecciones hasta 2100
  # (PIB, población, esperanza de vida) que no son datos observados.
  filter(anio >= 1800, anio <= 2022) |>
  # Polity5 solo tiene datos completos hasta 2018 (en 2019-2020 aparece un único
  # país), así que se anulan esos años para no mezclar coberturas distintas.
  mutate(polity = if_else(anio > 2018, NA_real_, polity)) |>
  # El indicador de jefas de Estado o de Gobierno es de mayo de 2020: en 2020-2021
  # solo aparecen unos 120 países y casi todos sin jefa, así que se anulan esos años.
  mutate(jefa_estado = if_else(anio > 2019, NA_character_, jefa_estado))

# Metadatos: nos quedamos con las variables de agrupación útiles para el dashboard.
meta <- paises |>
  select(geo = country, iso3 = iso3166_1_alpha3, region4 = world_4region,
         region6 = world_6region, grupo_ingresos = income_groups,
         latitud = latitude, longitud = longitude) |>
  left_join(paises_es |> select(geo = country, pais = name), by = "geo")

datos <- datos |>
  # inner_join: descarta agregados o territorios sin metadatos de país.
  inner_join(meta, by = "geo")

# -----------------------------------------------------------------------------
# 4. Recodificación y nuevas variables
# -----------------------------------------------------------------------------

datos <- datos |>
  mutate(
    # Tipo de régimen según los tramos estándar de Polity5.
    regimen = case_when(
      polity >= 6  ~ "Democracia",
      polity <= -6 ~ "Autocracia",
      !is.na(polity) ~ "Anocracia"
    ) |> factor(levels = c("Autocracia", "Anocracia", "Democracia")),

    # Freedom House usa una escala invertida (1 = más libre, 7 = menos libre).
    # La reorientamos para que valores altos signifiquen más libertad (1-7).
    fh_libertad_reorientada = 8 - fh_libertad,

    # Estatus de Freedom House como factor ordenado y en castellano.
    fh_estatus = factor(fh_estatus, levels = c("NF", "PF", "F"),
                        labels = c("No libre", "Parcialmente libre", "Libre"),
                        ordered = TRUE),

    # Jefa de Estado: variable lógica (TRUE si el país ha tenido alguna).
    jefa_estado = jefa_estado == "Had a female head of state",

    # Región y grupo de ingresos con etiquetas en castellano.
    region4 = recode(region4, africa = "África", americas = "América",
                     asia = "Asia", europe = "Europa"),
    grupo_ingresos = factor(
      grupo_ingresos,
      levels = c("low_income", "lower_middle_income", "upper_middle_income", "high_income"),
      labels = c("Bajos", "Medio-bajos", "Medio-altos", "Altos"),
      ordered = TRUE
    ),

    # Década, útil para agregar y para la animación temporal.
    decada = floor(anio / 10) * 10,

    # PIB per cápita en escala logarítmica (la distribución es muy asimétrica).
    log10_pib_pc = log10(pib_pc)
  ) |>
  relocate(geo, iso3, pais, region4, region6, grupo_ingresos, anio, decada)

# -----------------------------------------------------------------------------
# 5. Comprobaciones rápidas y guardado
# -----------------------------------------------------------------------------

# Número de países con dato por indicador en algunos años de referencia.
cobertura <- datos |>
  filter(anio %in% c(1900, 1950, 1975, 2000, 2018, 2021)) |>
  group_by(anio) |>
  summarise(across(c(polity, fh_libertad, idea_igualdad_genero,
                     mujeres_parlamento, part_laboral_fem, pib_pc),
                   ~ sum(!is.na(.x))))
print(cobertura)

write_csv(datos, file.path(dir_depurada, "democracia_genero.csv"))
saveRDS(datos, file.path(dir_depurada, "democracia_genero.rds"))  # conserva los factores

message("Tabla final: ", nrow(datos), " filas (país-año) y ", ncol(datos), " columnas.")
