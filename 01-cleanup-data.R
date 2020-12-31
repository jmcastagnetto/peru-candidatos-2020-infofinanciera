library(tidyverse)

fix_ubigeo <- function(ubigeo) {
  sprintf("%06d", as.integer(ubigeo))
}

fix_dni <- function(dni) {
  sprintf("%08d", as.integer(dni))
}

fix_total <- function(total) {
  total %>%
    str_remove_all(",") %>%
    as.numeric()
}

# Bienes ------------------------------------------------------------------

bienes <- read_csv(
  "datos/orig/candidatos 2021 bienes muebles e inmuebles - candidatos_por_bienes.csv",
  col_types =cols(
    .default = col_character(),
    intPosicion = col_number(),
    strFechaNacimiento = col_date(format = "%d/%m/%Y"),
    valor_bienes_muebles_e_inmuebles = col_number(),
    numero_bienes = col_number(),
    intEstadosCandPer = col_number(),
    strAnioPostula = col_number(),
    strUsuario = col_number(),
    decPorcentaje = col_number(),
    strFeTerminoRegistro = col_datetime(format = "%d/%m/%Y %H:%M:%S"),
    intPagina = col_number(),
    intCantidadPagina = col_number(),
    intCantidadReg = col_number(),
    intVotoPreferencial = col_number(),
    intTotalPersonero = col_number(),
    intOrden = col_number()
  )
) %>%
  janitor::clean_names() %>%
  mutate_at(
    vars(contains("ubi")),
    fix_ubigeo
  ) %>%
  mutate(
    str_documento_identidad = fix_dni(str_documento_identidad)
  ) %>%
  select(
    -state
  ) %>%
  distinct()

write_csv(
  bienes,
  file = "datos/proc/candidatos-2021-bienes.csv"
)

saveRDS(
  bienes,
  file = "datos/proc/candidatos-2021-bienes.rds"
)


# Ingresos ----------------------------------------------------------------

ingresos <- read_csv(
  "datos/orig/candidatos 2021 ingresos declarados año 2019 - candidatos_Ingresos.csv",
  col_types = cols(
    .default = col_character(),
    intPosicion = col_number(),
    intEstadosCandPer = col_number(),
    intAlertas = col_number(),
    intPagina = col_number(),
    intCantidadPagina = col_number(),
    intCantidadReg = col_number(),
    intItem = col_number(),
    intVotoPreferencial = col_number(),
    intTotalPersonero = col_number(),
    intOrden = col_number(),
    decRemuBrutaPublico = col_number(),
    decRemuBrutaPrivado = col_number(),
    decRentaIndividualPublico = col_number(),
    decRentaIndividualPrivado = col_number(),
    decOtroIngresoPublico = col_number(),
    decOtroIngresoPrivado = col_number(),
    decPorcentaje = col_number(),
    strFechaNacimiento = col_date(format = "%d/%m/%Y"),
    strFeTerminoRegistro = col_datetime(format = "%d/%m/%Y %H:%M:%S")
  )
) %>%
  select(
    -ends_with("_1")
  ) %>%
  janitor::clean_names() %>%
  mutate_at(
    vars(contains("ubi")),
    fix_ubigeo
  ) %>%
  mutate(
    str_documento_identidad = fix_dni(str_documento_identidad),
    total_ingresos = fix_total(total_ingresos),
    total_declarado = sum(
      dec_remu_bruta_publico,
      dec_remu_bruta_privado,
      dec_renta_individual_publico,
      dec_renta_individual_privado,
      dec_otro_ingreso_publico,
      dec_otro_ingreso_privado,
      na.rm = TRUE
    ),
    declarado_mayor = (total_ingresos < total_declarado)
  ) %>%
  select(
    -state
  ) %>%
  distinct()


write_csv(
  ingresos,
  file = "datos/proc/candidatos-2021-ingresos.csv"
)

saveRDS(
  ingresos,
  file = "datos/proc/candidatos-2021-ingresos.rds"
)


# Seleccionar y combinar --------------------------------------------------


bienes_sel <- bienes %>%
  select(
    str_documento_identidad,
    str_apellido_paterno,
    str_apellido_paterno,
    str_nombres,
    str_fecha_nacimiento,
    str_sexo,
    str_ubigeo_nacimiento,
    str_naci_departamento,
    str_naci_provincia,
    str_naci_distrito,
    str_ubigeo_domicilio,
    str_domi_departamento,
    str_domi_provincia,
    str_domi_distrito,
    str_organizacion_politica,
    numero_bienes,
    valor_bienes_muebles_e_inmuebles
  )

ingresos_sel <- ingresos %>%
  select(
    str_documento_identidad,
    dec_remu_bruta_publico,
    dec_remu_bruta_privado,
    dec_renta_individual_publico,
    dec_renta_individual_privado,
    dec_otro_ingreso_publico,
    dec_otro_ingreso_privado,
    total_ingresos
  )

combinado <- bienes_sel %>%
  left_join(
    ingresos_sel,
    by = "str_documento_identidad"
  ) %>%
  mutate(
    genero = if_else(
      str_sexo == "1",
      "Hombre",
      "Mujer"
    )
  ) %>%
  distinct()

write_csv(
  combinado,
  file = "datos/proc/candidatos-2021-bienes-ingresos.csv"
)

saveRDS(
  combinado,
  file = "datos/proc/candidatos-2021-bienes-ingresos.rds"
)