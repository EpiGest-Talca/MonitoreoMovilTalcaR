# Tabla 2: estadisticas diarias de PM2.5 y ratio agregadas por categorias
# de variables meteorologicas (humedad, temperatura, velocidad y direccion
# de viento). La agregacion espacial se hace primero a nivel de celda de
# 200 m y luego se promedia al dia completo para cada dia de campana.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

load("Data/Processed/med.RData")
load("Data/Processed/Sinca/sinca.RData") 
load("Data/Processed/Maps/EmptyRasters.RData")

if (!dir.exists("Tables")) dir.create("Tables")

r200 <- unwrap(r200_wrap)

# Filtrado de mediciones validas y calculo de ratio movil/central
# (ratios > 10 se descartan como outliers)
med_limpio <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr), sc_corr > 0) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

med_sf <- st_as_sf(med_limpio, coords = c("lon", "lat"), crs = 4326)
coords <- st_coordinates(st_transform(med_sf, 32719))

med_df <- st_drop_geometry(med_sf)
med_df$cell_200 <- cellFromXY(r200, coords)
med_df <- med_df %>% filter(!is.na(cell_200))

# Promedio por celda-dia (para no sesgar dias con muchos puntos)
celdas_diarias <- med_df %>%
  group_by(cell_200, date) %>%
  summarise(pm_cell = mean(mov_corr, na.rm = TRUE),
            ratio_cell = mean(ratio_pm25, na.rm = TRUE),
            .groups = "drop")

# Promedio espacial por dia
datos_diarios <- celdas_diarias %>%
  group_by(date) %>%
  summarise(PM_Dia = mean(pm_cell, na.rm = TRUE),
            Ratio_Dia = mean(ratio_cell, na.rm = TRUE),
            .groups = "drop")

datos_completos <- inner_join(datos_diarios, sinca, by = "date")

# Categorizacion meteorologica; los cortes se fijaron en el protocolo del estudio
datos_completos <- datos_completos %>%
  mutate(
    Temp_Cat = ifelse(mean_temp < 7.5, "<7.5 °C", ">=7.5 °C"),
    HR_Cat   = ifelse(is.na(mean_hr), "NA", ifelse(mean_hr < 95, "<95%", ">=95%")),
    WS_Cat   = ifelse(mean_ws < 1, "<1 m/s", ">=1 m/s"),
    WD_Cat   = case_when(
      mean_wd >= 45 & mean_wd < 135 ~ "E",
      mean_wd >= 135 & mean_wd < 225 ~ "S",
      mean_wd >= 225 & mean_wd < 315 ~ "W",
      TRUE ~ "N" 
    )
  )

# Devuelve una fila con N, p05, p25, mediana, media, p75, p95 para PM y Ratio.
# Si se pasan columna_categoria y valor_categoria filtra antes de agregar
calcular_fila <- function(df, columna_categoria = NULL, valor_categoria = NULL) {
  
  if(!is.null(columna_categoria)) {
    df <- df %>% filter(!!sym(columna_categoria) == valor_categoria)
  }
  
  if(nrow(df) == 0) {
    return(data.frame(
      N_PM = NA, P05_PM = NA, P25_PM = NA, Med_PM = NA, Mean_PM = NA, P75_PM = NA, P95_PM = NA,
      N_Rat = NA, P05_Rat = NA, P25_Rat = NA, Med_Rat = NA, Mean_Rat = NA, P75_Rat = NA, P95_Rat = NA
    ))
  }
  
  data.frame(
    N_PM    = nrow(df),
    P05_PM  = round(quantile(df$PM_Dia, 0.05, na.rm = TRUE), 2),
    P25_PM  = round(quantile(df$PM_Dia, 0.25, na.rm = TRUE), 2),
    Med_PM  = round(median(df$PM_Dia, na.rm = TRUE), 2),
    Mean_PM = round(mean(df$PM_Dia, na.rm = TRUE), 2),
    P75_PM  = round(quantile(df$PM_Dia, 0.75, na.rm = TRUE), 2),
    P95_PM  = round(quantile(df$PM_Dia, 0.95, na.rm = TRUE), 2),
    
    N_Rat    = nrow(df),
    P05_Rat  = round(quantile(df$Ratio_Dia, 0.05, na.rm = TRUE), 2),
    P25_Rat  = round(quantile(df$Ratio_Dia, 0.25, na.rm = TRUE), 2),
    Med_Rat  = round(median(df$Ratio_Dia, na.rm = TRUE), 2),
    Mean_Rat = round(mean(df$Ratio_Dia, na.rm = TRUE), 2),
    P75_Rat  = round(quantile(df$Ratio_Dia, 0.75, na.rm = TRUE), 2),
    P95_Rat  = round(quantile(df$Ratio_Dia, 0.95, na.rm = TRUE), 2)
  )
}

# Las filas "de titulo" (df vacio) se usan para crear los encabezados
# visuales de cada grupo en la tabla final (Humidity, Temperature, etc.)
filas <- list(
  cbind(Category = "Overall", calcular_fila(datos_completos)),
  cbind(Category = "Relative Humidity", calcular_fila(datos_completos[0, ])), 
  cbind(Category = "<95%", calcular_fila(datos_completos, "HR_Cat", "<95%")),
  cbind(Category = ">=95%", calcular_fila(datos_completos, "HR_Cat", ">=95%")),
  cbind(Category = "NA", calcular_fila(datos_completos, "HR_Cat", "NA")),
  cbind(Category = "Temperature", calcular_fila(datos_completos[0, ])),
  cbind(Category = "<7.5 °C", calcular_fila(datos_completos, "Temp_Cat", "<7.5 °C")),
  cbind(Category = ">=7.5 °C", calcular_fila(datos_completos, "Temp_Cat", ">=7.5 °C")),
  cbind(Category = "Wind Speed", calcular_fila(datos_completos[0, ])),
  cbind(Category = "<1 m/s", calcular_fila(datos_completos, "WS_Cat", "<1 m/s")),
  cbind(Category = ">=1 m/s", calcular_fila(datos_completos, "WS_Cat", ">=1 m/s")),
  cbind(Category = "Wind Direction", calcular_fila(datos_completos[0, ])),
  cbind(Category = "E", calcular_fila(datos_completos, "WD_Cat", "E")),
  cbind(Category = "N", calcular_fila(datos_completos, "WD_Cat", "N")),
  cbind(Category = "S", calcular_fila(datos_completos, "WD_Cat", "S")),
  cbind(Category = "W", calcular_fila(datos_completos, "WD_Cat", "W"))
)

tabla_export <- bind_rows(filas)

# Formato flextable
ft <- flextable(tabla_export)

ft <- set_header_labels(ft, 
                        Category = "Category", 
                        N_PM = "N (Days)", P05_PM = "p05", P25_PM = "p25", Med_PM = "Median", Mean_PM = "Mean", P75_PM = "p75", P95_PM = "p95",
                        N_Rat = "N (Days)", P05_Rat = "p05", P25_Rat = "p25", Med_Rat = "Median", Mean_Rat = "Mean", P75_Rat = "p75", P95_Rat = "p95")

# 7 columnas por bloque de metrica (N + 6 estadisticas)
ft <- add_header_row(ft, values = c("", "PM2.5 (ug m-3)", "PM2.5 ratio"), colwidths = c(1, 7, 7))
ft <- add_header_row(ft, values = c("", "Daily Aggregated Mobile Measurements"), colwidths = c(1, 14))

ft <- theme_booktabs(ft)
ft <- fontsize(ft, part = "all", size = 9) 
ft <- padding(ft, padding = 3, part = "all") 
ft <- flextable::align(ft, align = "center", part = "all")
ft <- flextable::align(ft, j = 1, align = "left", part = "all")

# Subindice/superindice en encabezados PM2.5 y PM2.5 ratio
ft <- flextable::compose(ft, i = 2, j = 9, part = "header", value = as_paragraph("PM", as_sub("2.5"), " ratio", as_sup("a")))
ft <- flextable::compose(ft, i = 2, j = 2, part = "header", value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ")"))

# Filas subrayadas = encabezados de seccion dentro del cuerpo de la tabla
ft <- flextable::compose(ft, i = 2, j = 1, part = "body", value = as_paragraph(as_chunk("Relative Humidity", props = fp_text_default(underlined = TRUE))))
ft <- flextable::compose(ft, i = 6, j = 1, part = "body", value = as_paragraph(as_chunk("Temperature", props = fp_text_default(underlined = TRUE))))
ft <- flextable::compose(ft, i = 9, j = 1, part = "body", value = as_paragraph(as_chunk("Wind Speed", props = fp_text_default(underlined = TRUE))))
ft <- flextable::compose(ft, i = 12, j = 1, part = "body", value = as_paragraph(as_chunk("Wind Direction", props = fp_text_default(underlined = TRUE))))

# Sangria a las filas de categoria (bajo cada encabezado de seccion)
ft <- padding(ft, i = c(3,4,5, 7,8, 10,11, 13,14,15,16), j = 1, padding.left = 15)

ft <- add_footer_lines(ft, values = "a Ratio compared to central site. p05: 5th percentile. p25: 25th percentile. p75: 75th percentile. p95: 95th percentile.")
ft <- autofit(ft)

# Export en hoja apaisada para que quepan las 14 columnas
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width = 11, height = 8.5),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(
  "Table 2: Summary statistics of daily aggregated mobile pollutant measurements by meteorological variables." = ft, 
  path = "Tables/Tabla2_Met_Stats.docx",
  pr_section = sect_properties
)

