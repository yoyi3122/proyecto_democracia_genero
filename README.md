# Democracia e igualdad de género en el mundo

Proyecto de Ciencia de Datos reproducible · Módulo 8, Reto 2 · Idoia Garriz Galan

Analizamos con datos de [Gapminder](https://www.gapminder.org/) cómo han
evolucionado la democracia y la igualdad de género en el mundo, y si avanzan juntas.

## Objetivos

**Objetivo principal:** describir la evolución de la democracia y de la igualdad
de género en el mundo y estudiar su relación.

**Objetivos específicos:**

1. Ver cómo ha cambiado el número de democracias y autocracias desde 1800.
2. Ver cómo ha crecido el porcentaje de mujeres en los parlamentos por región desde 1945.
3. Comprobar si los países más democráticos son también los más igualitarios.

## Estructura

```
Datos/           datos originales, código de depuración y datos depurados
Dashboard/       código y dashboard en HTML
Informe/         informe técnico (knitr) en PDF
Presentacion/    presentación (R Markdown)
```

## Cómo reproducirlo

1. Abrir `proyecto_democracia_genero.Rproj` en RStudio.
2. Ejecutar `Datos/depuracion.R`.
3. Compilar los archivos de `Dashboard/`, `Informe/` y `Presentacion/`.

Paquetes necesarios: `tidyverse`, `here`, `flexdashboard`, `plotly`, `knitr` y `rmarkdown`.

Datos: Gapminder (repositorios de [open-numbers](https://github.com/open-numbers)), licencia CC BY 4.0.
