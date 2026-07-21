# Tabla 3: estadisticas resumen de PM2.5 y ratio dentro de las celdas de
# cada una de las 4 resoluciones (50, 100, 200, 400 m).
# Solo se consideran celdas con >= 3 dias medidos para asegurar estabilidad
# del promedio de campana.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

load("Data/Processed/med.RData")
load("Data/Processed/Maps/EmptyRasters.RData")

if (!dir.exists("Tables")) dir.create("Tables")

r400 <- unwrap(r400_wrap)
r200 <- unwrap(r200_wrap)
r100 <- unwrap(r100_wrap)
r50  <- unwrap(r50_wrap)

med_limpio <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr), sc_corr > 0) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

med_sf <- st_as_sf(med_limpio, coords = c("lon", "lat"), crs = 4326)
med_utm <- st_transform(med_sf, 32719)
coords <- st_coordinates(med_utm)

med_df <- st_drop_geometry(med_utm)

# Asignar a cada punto su celda en las 4 resoluciones
med_df$cell_50  <- cellFromXY(r50, coords)
med_df$cell_100 <- cellFromXY(r100, coords)
med_df$cell_200 <- cellFromXY(r200, coords)
med_df$cell_400 <- cellFromXY(r400, coords)

# Para cada resolucion: promedio celda-dia -> promedio celda-campana -> estadisticas sobre celdas
calcular_estadisticas_grilla <- function(df, columna_celda, etiqueta_resolucion) {
  
  df_valido <- df %>% filter(!is.na(.data[[columna_celda]]))
  
  res_dia <- df_valido %>%
    group_by(.data[[columna_celda]], date) %>%
    summarise(
      pm_mean = mean(mov_corr, na.rm = TRUE),
      ratio_mean = mean(ratio_pm25, na.rm = TRUE), 
      .groups = "drop"
    )
  
  res_total <- res_dia %>%
    group_by(.data[[columna_celda]]) %>%
    summarise(
      PM_Total = mean(pm_mean, na.rm = TRUE),
      Ratio_Total = mean(ratio_mean, na.rm = TRUE),
      Dias_Medidos = n(),
      .groups = "drop"
    ) %>%
    filter(Dias_Medidos >= 3) 
  
  pm_vals <- res_total$PM_Total
  rat_vals <- res_total$Ratio_Total
  
  res_pm <- data.frame(
    Resolution = etiqueta_resolucion,
    N_cells = length(pm_vals),
    p05    = round(quantile(pm_vals, 0.05, na.rm = TRUE), 2),
    p25    = round(quantile(pm_vals, 0.25, na.rm = TRUE), 2),
    Median = round(median(pm_vals, na.rm = TRUE), 2),
    Mean   = round(mean(pm_vals, na.rm = TRUE), 2),
    p75    = round(quantile(pm_vals, 0.75, na.rm = TRUE), 2),
    p95    = round(quantile(pm_vals, 0.95, na.rm = TRUE), 2)
  )
  
  res_rat <- data.frame(
    Resolution = etiqueta_resolucion,
    N_cells = length(rat_vals),
    p05    = round(quantile(rat_vals, 0.05, na.rm = TRUE), 2),
    p25    = round(quantile(rat_vals, 0.25, na.rm = TRUE), 2),
    Median = round(median(rat_vals, na.rm = TRUE), 2),
    Mean   = round(mean(rat_vals, na.rm = TRUE), 2),
    p75    = round(quantile(rat_vals, 0.75, na.rm = TRUE), 2),
    p95    = round(quantile(rat_vals, 0.95, na.rm = TRUE), 2)
  )
  
  return(list(PM = res_pm, Ratio = res_rat))
}

res_50  <- calcular_estadisticas_grilla(med_df, "cell_50", "50 m")
res_100 <- calcular_estadisticas_grilla(med_df, "cell_100", "100 m")
res_200 <- calcular_estadisticas_grilla(med_df, "cell_200", "200 m")
res_400 <- calcular_estadisticas_grilla(med_df, "cell_400", "400 m")

tabla_pm <- bind_rows(res_50$PM, res_100$PM, res_200$PM, res_400$PM)
tabla_rat <- bind_rows(res_50$Ratio, res_100$Ratio, res_200$Ratio, res_400$Ratio)

# Filas-encabezado vacias para separar visualmente PM y ratio en la tabla
titulo_pm <- data.frame(Resolution = "PM2.5 (ug/m3)", N_cells = NA, p05 = NA, p25 = NA, Median = NA, Mean = NA, p75 = NA, p95 = NA)
titulo_rat <- data.frame(Resolution = "PM2.5 ratio", N_cells = NA, p05 = NA, p25 = NA, Median = NA, Mean = NA, p75 = NA, p95 = NA)

tabla_export <- bind_rows(titulo_pm, tabla_pm, titulo_rat, tabla_rat)
names(tabla_export) <- c("Resolution", "N cells", "p05", "p25", "Median", "Mean", "p75", "p95")

ft <- flextable(tabla_export)

ft <- add_header_row(ft, values = c("", "Statistics within cells"), colwidths = c(1, 7))
ft <- theme_booktabs(ft)
ft <- colformat_double(ft, digits = 2, na_str = "")
ft <- colformat_int(ft, na_str = "")

ft <- flextable::align(ft, align = "center", part = "all")
ft <- flextable::align(ft, j = 1, align = "left", part = "all")

# Titulo de bloque "PM2.5 (ug/m3)" con subindice/superindice y subrayado
ft <- flextable::compose(ft, i = 1, j = 1, part = "body",
              value = as_paragraph(
                as_chunk("PM", props = fp_text_default(underlined = TRUE)),
                as_chunk("2.5", props = fp_text_default(vertical.align = "subscript", underlined = TRUE)),
                as_chunk(" (µg/m", props = fp_text_default(underlined = TRUE)),
                as_chunk("3", props = fp_text_default(vertical.align = "superscript", underlined = TRUE)),
                as_chunk(")", props = fp_text_default(underlined = TRUE))
              ))

# Fila 6 = inicio del bloque Ratio (bloque PM ocupa filas 1-5)
ft <- flextable::compose(ft, i = 6, j = 1, part = "body",
              value = as_paragraph(
                as_chunk("PM", props = fp_text_default(underlined = TRUE)),
                as_chunk("2.5", props = fp_text_default(vertical.align = "subscript", underlined = TRUE)),
                as_chunk(" ratio", props = fp_text_default(underlined = TRUE)),
                as_chunk("a", props = fp_text_default(vertical.align = "superscript"))
              ))

ft <- add_footer_lines(ft, values = "a Ratio of measurement compared to central site.")
ft <- fontsize(ft, part = "footer", size = 9)
ft <- autofit(ft)

print(ft)

save_as_docx(
  "Table 3: Summary statistics by spatial resolution." = ft, 
  path = "Tables/Tabla3_Resolution_Stats.docx"
)

