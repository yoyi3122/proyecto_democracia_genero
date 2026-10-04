# =============================================================================
# Analisis_exploratorio/eda.R
# Análisis exploratorio de datos (EDA) — Reto 1, Tarea 1
#
# Entrada: Datos/depurada/democracia_genero.rds (generado por Datos/depuracion.R)
# Salida:  Analisis_exploratorio/figuras/eda_*.png y Analisis_exploratorio/resultados/
# =============================================================================

library(tidyverse)
library(here)

d <- readRDS(here("Datos", "depurada", "democracia_genero.rds"))
dir_figuras    <- here("Analisis_exploratorio", "figuras")
dir_resultados <- here("Analisis_exploratorio", "resultados")
dir.create(dir_figuras, showWarnings = FALSE)
dir.create(dir_resultados, showWarnings = FALSE)

# Año de referencia para los cortes transversales: último año con Polity5 completo.
anio_ref <- 2018

# Paletas aptas para daltonismo (Okabe-Ito).
col_region <- c("África" = "#E69F00", "América" = "#56B4E9",
                "Asia" = "#009E73", "Europa" = "#CC79A7")
col_regimen <- c("Autocracia" = "#D55E00", "Anocracia" = "#BBBBBB",
                 "Democracia" = "#0072B2")

theme_set(theme_minimal(base_size = 13) +
            theme(plot.title = element_text(face = "bold"),
                  plot.title.position = "plot",
                  panel.grid.minor = element_blank(),
                  plot.caption = element_text(colour = "grey40", hjust = 0)))
fuente <- "Fuente: Gapminder (open-numbers). Elaboración propia."

guardar <- function(nombre, ancho = 10, alto = 6) {
  ggsave(file.path(dir_figuras, paste0(nombre, ".png")),
         width = ancho, height = alto, dpi = 200, bg = "white")
}

# Etiquetas legibles de los indicadores numéricos.
etiquetas <- c(
  polity = "Democracia (Polity5, −10 a 10)",
  fh_libertad_reorientada = "Libertad (Freedom House, 1–7)",
  idea_libertades = "Libertades civiles (IDEA, 0–100)",
  eiu_democracia = "Democracia (EIU, 0–100)",
  idea_igualdad_genero = "Igualdad de género (IDEA, 0–100)",
  mujeres_parlamento = "Mujeres en el parlamento (%)",
  part_laboral_fem = "Participación laboral femenina (%)",
  escolarizacion_ratio = "Años de escuela mujeres / hombres (%)",
  log10_pib_pc = "PIB per cápita (log10, $ PPA 2021)",
  esperanza_vida = "Esperanza de vida (años)"
)

sink(file.path(dir_resultados, "eda_resumen.txt"))

# -----------------------------------------------------------------------------
# 1. Cobertura: % de países con dato por indicador y año
# -----------------------------------------------------------------------------
n_paises <- n_distinct(d$geo)
cobertura <- d |>
  group_by(anio) |>
  summarise(across(all_of(names(etiquetas)), ~ mean(!is.na(.x)) * 100)) |>
  pivot_longer(-anio, names_to = "indicador", values_to = "pct") |>
  mutate(indicador = factor(etiquetas[indicador], levels = rev(etiquetas)))

ggplot(cobertura, aes(anio, indicador, fill = pct)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#0072B2", limits = c(0, 100),
                      name = "% de países\ncon dato") +
  scale_x_continuous(breaks = seq(1800, 2020, 25), expand = c(0, 0)) +
  labs(title = "Cobertura temporal de los indicadores",
       subtitle = paste0("Porcentaje de los ", n_paises, " países con dato cada año, 1800–2022"),
       x = NULL, y = NULL, caption = fuente)
guardar("eda_01_cobertura", 11, 5.5)

cat("== Cobertura (n.º de países con dato) en años clave ==\n")
d |> filter(anio %in% c(1850, 1900, 1950, 1975, 2000, 2018, 2021)) |>
  group_by(anio) |>
  summarise(across(all_of(names(etiquetas)), ~ sum(!is.na(.x)))) |>
  as.data.frame() |> print()

# -----------------------------------------------------------------------------
# 2. Distribuciones en el año de referencia
# -----------------------------------------------------------------------------
corte <- d |> filter(anio == anio_ref)

dist <- corte |>
  select(geo, all_of(setdiff(names(etiquetas), "eiu_democracia"))) |>
  pivot_longer(-geo, names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor)) |>
  mutate(indicador = factor(etiquetas[indicador], levels = etiquetas))

ggplot(dist, aes(valor)) +
  geom_histogram(bins = 21, fill = "grey55", colour = "white") +
  facet_wrap(~ indicador, scales = "free", ncol = 3) +
  labs(title = paste0("Distribución de los indicadores por país, ", anio_ref),
       subtitle = "Escolarización: último dato disponible es 2015",
       x = NULL, y = "Número de países", caption = fuente)
guardar("eda_02_distribuciones", 11, 8)

cat("\n== Estadísticos descriptivos ", anio_ref, " ==\n")
dist |> group_by(indicador) |>
  summarise(n = n(), min = min(valor), q1 = quantile(valor, .25), mediana = median(valor),
            media = mean(valor), q3 = quantile(valor, .75), max = max(valor),
            asimetria = mean((valor - mean(valor))^3) / sd(valor)^3) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  as.data.frame() |> print()

cat("\n== Tipo de régimen (Polity5) en ", anio_ref, " ==\n")
print(table(corte$regimen))

# -----------------------------------------------------------------------------
# 3. Valores atípicos
# -----------------------------------------------------------------------------
cat("\n== Atípicos: % de mujeres en el parlamento, ", anio_ref, " (top 8) ==\n")
corte |> arrange(desc(mujeres_parlamento)) |>
  select(pais, region4, polity, regimen, mujeres_parlamento) |> head(8) |>
  as.data.frame() |> print()

cat("\n== Atípicos: PIB per cápita máximo, ", anio_ref, " ==\n")
corte |> arrange(desc(pib_pc)) |> select(pais, pib_pc, poblacion) |> head(5) |>
  as.data.frame() |> print()

cat("\n== Atípicos: esperanza de vida mínima de toda la serie ==\n")
d |> arrange(esperanza_vida) |> select(pais, anio, esperanza_vida) |> head(8) |>
  as.data.frame() |> print()

# Boxplots por región: dónde se concentran los atípicos.
corte |>
  select(pais, region4, mujeres_parlamento, idea_igualdad_genero) |>
  pivot_longer(c(mujeres_parlamento, idea_igualdad_genero),
               names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor)) |>
  mutate(indicador = etiquetas[indicador]) |>
  ggplot(aes(region4, valor, fill = region4)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2.5) +
  scale_fill_manual(values = col_region, guide = "none") +
  facet_wrap(~ indicador, scales = "free_y") +
  labs(title = paste0("Igualdad de género por región, ", anio_ref),
       subtitle = "Cada caja resume los países de la región; los puntos son valores atípicos",
       x = NULL, y = NULL, caption = fuente)
guardar("eda_03_boxplots_region", 11, 5.5)

# -----------------------------------------------------------------------------
# 4. Evolución temporal
# -----------------------------------------------------------------------------
# 4a. Proporción de países por tipo de régimen (el total de países cambia).
d |> filter(!is.na(regimen)) |>
  count(anio, regimen) |>
  group_by(anio) |> mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(anio, pct, fill = regimen)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = col_regimen, name = NULL) +
  scale_x_continuous(breaks = seq(1800, 2018, 25)) +
  labs(title = "Proporción de países según su tipo de régimen, 1800–2018",
       subtitle = "Polity5: democracia (6 a 10), anocracia (−5 a 5), autocracia (−10 a −6)",
       x = NULL, y = "% de países con dato", caption = fuente) +
  theme(legend.position = "top")
guardar("eda_04_regimenes_proporcion", 11, 5.5)

# 4b. Trayectorias de países que suelen trabajarse en clase.
seleccion <- c("esp", "prt", "deu", "chl", "zaf", "pol", "rus", "ind", "chn")
# Ojo: Gapminder no asigna a ningún país actual los estados históricos (URSS,
# RFA/RDA...), así que hay huecos (p. ej., Rusia 1923-1991, Alemania 1946-1989).
# Se completan los años con NA para que la línea se corte y el hueco sea visible.
d |> filter(geo %in% seleccion, anio >= 1900, anio <= 2018) |>
  select(pais, anio, polity) |>
  complete(pais, anio = 1900:2018) |>
  ggplot(aes(anio, polity)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 6, ymax = 10,
           fill = col_regimen["Democracia"], alpha = 0.12) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -10, ymax = -6,
           fill = col_regimen["Autocracia"], alpha = 0.12) +
  geom_step(linewidth = 0.8) +
  facet_wrap(~ pais) +
  scale_y_continuous(breaks = c(-10, -6, 0, 6, 10)) +
  labs(title = "Trayectorias de régimen político de nueve países, 1900–2018",
       subtitle = "Puntuación Polity5. Franja azul: democracia; franja naranja: autocracia",
       x = NULL, y = "Polity5", caption = fuente)
guardar("eda_05_trayectorias_paises", 11, 7.5)

# 4c. Mujeres en el parlamento: mediana por región.
d |> filter(!is.na(mujeres_parlamento)) |>
  group_by(region4, anio) |>
  summarise(mediana = median(mujeres_parlamento), n = n(), .groups = "drop") |>
  filter(n >= 5) |>
  ggplot(aes(anio, mediana, colour = region4)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = col_region, name = NULL) +
  labs(title = "Mujeres en los parlamentos nacionales por región, 1945–2021",
       subtitle = "Mediana del % de escaños ocupados por mujeres (regiones con al menos 5 países con dato)",
       x = NULL, y = "% de escaños (mediana)", caption = fuente) +
  theme(legend.position = "top")
guardar("eda_06_parlamento_region", 11, 5.5)

cat("\n== Mediana % mujeres en el parlamento por región ==\n")
d |> filter(anio %in% c(1950, 1975, 2000, 2021)) |>
  group_by(region4, anio) |>
  summarise(mediana = round(median(mujeres_parlamento, na.rm = TRUE), 1), .groups = "drop") |>
  pivot_wider(names_from = anio, values_from = mediana) |> as.data.frame() |> print()

# 4d. Países que han tenido alguna vez una jefa de Estado (acumulado).
jefas <- d |> filter(!is.na(jefa_estado)) |>
  group_by(anio) |> summarise(paises = sum(jefa_estado), total = n())
ggplot(jefas, aes(anio, paises)) +
  geom_step(linewidth = 1, colour = "#0072B2") +
  labs(title = "Países que han tenido alguna vez una jefa de Estado o de Gobierno, 1953–2019",
       subtitle = "Número acumulado de países. Mujeres elegidas o nombradas; no incluye monarcas hereditarias",
       x = NULL, y = "Número de países", caption = fuente)
guardar("eda_07_jefas_estado", 10, 5)
cat("\n== Jefas de Estado (acumulado) ==\n")
print(as.data.frame(jefas |> filter(anio %in% c(1960, 1980, 2000, 2019))))

# -----------------------------------------------------------------------------
# 5. Relaciones entre indicadores
# -----------------------------------------------------------------------------
vars_cor <- c("polity", "fh_libertad_reorientada", "idea_libertades",
              "idea_igualdad_genero", "mujeres_parlamento", "part_laboral_fem",
              "log10_pib_pc", "esperanza_vida")
m <- cor(corte[vars_cor], use = "pairwise.complete.obs", method = "spearman")
cat("\n== Correlaciones de Spearman, ", anio_ref, " ==\n")
print(round(m, 2))

as.data.frame(m) |> rownames_to_column("v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "rho") |>
  mutate(v1 = factor(etiquetas[v1], levels = etiquetas[vars_cor]),
         v2 = factor(etiquetas[v2], levels = rev(etiquetas[vars_cor]))) |>
  ggplot(aes(v1, v2, fill = rho)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3.6) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1),
                       name = "Coef. de\nSpearman") +
  labs(title = paste0("Correlaciones entre indicadores, ", anio_ref),
       x = NULL, y = NULL, caption = fuente) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
guardar("eda_08_correlaciones", 10, 8)

# Dispersión: democracia frente a igualdad de género y frente a mujeres en el parlamento.
disp <- corte |>
  select(pais, region4, poblacion, polity, idea_igualdad_genero, mujeres_parlamento) |>
  pivot_longer(c(idea_igualdad_genero, mujeres_parlamento),
               names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), !is.na(polity)) |>
  mutate(indicador = etiquetas[indicador])

# Etiquetamos los casos que rompen la relación general.
destacados <- disp |>
  group_by(indicador) |>
  mutate(residuo = resid(lm(valor ~ polity))) |>
  slice_max(abs(residuo), n = 6) |> ungroup()

ggplot(disp, aes(polity, valor)) +
  geom_jitter(aes(size = poblacion, fill = region4), shape = 21, alpha = 0.75,
              width = 0.25, height = 0, colour = "white") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey30", linewidth = 0.6) +
  geom_text(data = destacados, aes(label = pais), size = 3.2, vjust = -1) +
  scale_fill_manual(values = col_region, name = NULL) +
  scale_size_area(max_size = 12, guide = "none") +
  facet_wrap(~ indicador, scales = "free_y") +
  labs(title = paste0("Democracia e igualdad de género, ", anio_ref),
       subtitle = "Cada círculo es un país (tamaño = población). Se nombran los casos que más se alejan de la tendencia",
       x = "Democracia (Polity5)", y = NULL, caption = fuente) +
  theme(legend.position = "top")
guardar("eda_09_dispersion_democracia_genero", 12, 6.5)

cat("\n== Casos destacados (residuos mayores) ==\n")
print(as.data.frame(destacados |> select(indicador, pais, polity, valor) |>
                      mutate(valor = round(valor, 1))))

# Concordancia entre índices: Polity5 frente a Freedom House.
cat("\n== Tipo de régimen (Polity5) frente a estatus Freedom House, ", anio_ref, " ==\n")
print(table(corte$regimen, corte$fh_estatus))

sink()
message("EDA terminado: figuras y resumen en Analisis_exploratorio/")
