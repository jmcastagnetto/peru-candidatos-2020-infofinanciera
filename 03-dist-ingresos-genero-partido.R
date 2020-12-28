library(tidyverse)

ingresos <- readRDS("datos/proc/candidatos-2021-ingresos.rds")

df <- ingresos %>%
  mutate(
    str_organizacion_politica = factor(
      str_wrap(str_organizacion_politica, 22)
    ),
    genero = if_else(
      str_sexo == "1",
      "Hombre",
      "Mujer"
    )
  )

por_genero <- ggplot(
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
    title = "Elecciones 2021: Distribución de ingresos (2019) declarados por candidatos - La brecha de género.",
    subtitle = "Fuente: https://twitter.com/AniversarioPeru/status/1342705785269346305",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2020-12-27"
  ) +
  theme_linedraw(16) +
  theme(
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text.x = element_text(size = 10)
  ) +
  facet_wrap(~str_organizacion_politica, ncol = 6)

por_genero
ggsave(
  por_genero,
  filename = "plots/distribucion-ingresos-por-genero-partido.png",
  height = 14,
  width = 18
)
