library(tidyverse)

ingresos <- readRDS("datos/proc/candidatos-2021-ingresos.rds")

df <- ingresos %>%
  mutate(
    str_organizacion_politica = factor(str_organizacion_politica)
  )

por_partido <- ggplot(
  df,
  aes(x = total_ingresos,
      y = str_organizacion_politica)
) +
  geom_violin(
    fill = NA,
    color = "grey60",
    trim = TRUE,
    scale = "width"
  ) +
  geom_boxplot(
    outlier.colour = "red",
    varwidth = TRUE,
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_y_discrete(limits = rev(levels(df$str_organizacion_politica))) +
  annotation_logticks(sides = "b") +
  labs(
    x = "Ingresos totales (en Soles)",
    y = "",
    title = "Distribución de ingresos declarados por candidatos (2019)",
    subtitle = "Fuente: https://twitter.com/AniversarioPeru/status/1342705785269346305",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2020-12-27"
  ) +
  theme_bw(18) +
  theme(
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  por_partido,
  filename = "plots/distribucion-ingresos-por-partido.png",
  height = 14,
  width = 18
)
