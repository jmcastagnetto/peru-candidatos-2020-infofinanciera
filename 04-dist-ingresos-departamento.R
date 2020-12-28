library(tidyverse)

ingresos <- readRDS("datos/proc/candidatos-2021-ingresos.rds")

df <- ingresos %>%
    mutate(
      genero = if_else(
        str_sexo == "1",
        "Hombre",
        "Mujer"
      ),
      str_domi_departamento = if_else(
        str_domi_departamento == "AMERICA",
        "_FUERA DEL PERU_",
        str_domi_departamento
      )
    )

por_dpto <- ggplot(
    df,
    aes(x = total_ingresos,
        y = genero,
        color = genero)
  ) +
    geom_violin(
      fill = NA,
      color = "grey60",
      trim = TRUE,
      scale = "width",
      show.legend = FALSE
    ) +
    geom_boxplot(
      outlier.colour = "red",
      varwidth = TRUE,
      show.legend = FALSE
    ) +
    scale_x_log10(labels = scales::comma) +
    annotation_logticks(sides = "b") +
    labs(
      x = "Ingresos totales (en Soles)",
      y = "",
      title = "Elecciones 2021: Distribución de ingresos declarados (2019) por Departamento de domicilio",
      subtitle = "Fuente: https://twitter.com/AniversarioPeru/status/1342705785269346305",
      caption = "@jmcastagnetto, Jesus M. Castagnetto, 2020-12-28"
    ) +
    theme_linedraw(16) +
    theme(
      plot.margin = unit(rep(1, 4), "cm"),
      axis.text.x = element_text(size = 10)
    ) +
    facet_wrap(~str_domi_departamento, ncol = 4)

ggsave(
    por_dpto,
    filename = "plots/distribucion-ingresos-por-genero-departamento.png",
    height = 16,
    width = 18
  )
