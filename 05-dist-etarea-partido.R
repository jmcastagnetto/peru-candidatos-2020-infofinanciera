library(tidyverse)

ingresos <- readRDS("datos/proc/candidatos-2021-ingresos.rds")

df <- ingresos %>%
  mutate(
    edad_anhos = 2020 - lubridate::year(str_fecha_nacimiento),
    str_organizacion_politica = factor(str_organizacion_politica),
    genero = if_else(
      str_sexo == "1",
      "Hombre",
      "Mujer"
    )
  )

dist_etarea <- ggplot(
  df,
  aes(x = edad_anhos, y = str_organizacion_politica,
      fill = genero)
) +
  ggridges::stat_density_ridges(
    show.legend = FALSE,
    quantile_lines = TRUE,
    quantiles = 2,
    alpha = .7
  ) +
  scale_y_discrete(limits = rev(levels(df$str_organizacion_politica))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    y = "",
    x = "Edad estimada (años)",
    title = "Elecciones 2021: Distribución etárea por Agrupación Política y Sexo",
    subtitle = "Fuente: https://twitter.com/AniversarioPeru/status/1342705785269346305",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2020-12-28"
  ) +
  facet_wrap(~genero) +
  theme_classic(18) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title.position = "plot"
  )

ggsave(
  dist_etarea,
  filename = "plots/distribucion-edades-por-genero-partido.png",
  width = 16,
  height = 18
)
