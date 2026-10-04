# Democracia e igualdad de género en el mundo (1800-2022)

Proyecto reproducible de Ciencia de Datos · Módulo 8, Reto 2.
Autora: Idoia Garriz.

Este proyecto estudia cómo han evolucionado la democracia y la igualdad de género
en los distintos países del mundo, y qué relación hay entre ambas. Usa datos
públicos de [Gapminder](https://www.gapminder.org/). Está pensado para el
profesorado de historia universal (bachillerato y universidad) y para
divulgadores.

## Objetivos

*(Pendiente: se completa en el paso 2.)*

## Estructura del repositorio

```
├── README.md
├── proyecto_democracia_genero.Rproj   <- abrir este archivo en RStudio
├── Datos/
│   ├── original/        <- ficheros CSV tal como se descargan de Gapminder
│   ├── depuracion.R     <- código de importación y depuración
│   └── depurada/        <- tabla final país-año (.rds y .csv)
├── Analisis_exploratorio/
│   ├── eda.R            <- análisis exploratorio (Reto 1)
│   ├── figuras/
│   └── resultados/
├── Dashboard/           <- código (.Rmd) y dashboard (.html)
├── Informe/             <- informe técnico con knitr (.Rnw y .pdf)
└── Presentacion/        <- presentación con R Markdown
```

## Cómo reproducir el proyecto

1. Clonar o descargar el repositorio y abrir `proyecto_democracia_genero.Rproj` en RStudio.
2. Instalar los paquetes necesarios:
   ```r
   install.packages(c("tidyverse", "here", "flexdashboard", "plotly",
                      "DT", "knitr", "kableExtra", "rmarkdown", "tinytex"))
   tinytex::install_tinytex()   # solo si no hay LaTeX instalado (para el PDF)
   ```
3. Ejecutar `Datos/depuracion.R`. Si los ficheros de `Datos/original/` ya existen,
   no se vuelven a descargar.
4. Compilar el dashboard, el informe y la presentación desde sus carpetas
   (botón *Knit* / *Compile PDF* de RStudio).

Todas las rutas se construyen con el paquete `here`, a partir de la carpeta del
proyecto, de modo que no hay referencias a directorios locales.

## Fuente de los datos

Indicadores de Gapminder distribuidos en los repositorios de
[open-numbers](https://github.com/open-numbers) (`ddf--gapminder--fasttrack` y
`ddf--gapminder--systema_globalis`), descargados en versiones fijas (commits)
para garantizar la reproducibilidad. Licencia de los datos:
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
