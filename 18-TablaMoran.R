# Tabla 4: indice global de Moran para PM2.5 absoluto y para ratio
# mobile/central, calculado sobre cada una de las 4 resoluciones.
# Usa matriz de pesos por contiguedad reina y solo celdas con >= 3 dias.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(spdep)
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
coords <- st_coordinates(st_transform(med_sf, 32719))
med_df <- st_drop_geometry(med_sf)


calcular_moran_global <- function(df, plantilla_raster, res_num) {
  
  col_celda <- paste0("cell_", res_num)
  df[[col_celda]] <- cellFromXY(plantilla_raster, coords)
  
  # Agregacion dual (PM absoluto y ratio) a nivel celda-campana
  res_total <- df %>%
    filter(!is.na(.data[[col_celda]])) %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(
      pm_mean = mean(mov_corr, na.rm = TRUE),
      ratio_mean = mean(ratio_pm25, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    group_by(.data[[col_celda]]) %>%
    summarise(
      PM_Total = mean(pm_mean, na.rm = TRUE),
      Ratio_Total = mean(ratio_mean, na.rm = TRUE), 
      Dias = n(), 
      .groups = "drop"
    ) %>%
    filter(Dias >= 3)
  
  # Dos rasters (uno por metrica) combinados para crear poligonos consistentes
  r_pm <- plantilla_raster
  values(r_pm) <- NA
  r_pm[res_total[[col_celda]]] <- res_total$PM_Total
  
  r_rat <- plantilla_raster
  values(r_rat) <- NA
  r_rat[res_total[[col_celda]]] <- res_total$Ratio_Total
  
  r_temp <- c(r_pm, r_rat)
  
  # dissolve=FALSE mantiene cada celda como poligono individual
  poligonos <- as.polygons(r_temp, na.rm = TRUE, dissolve = FALSE) %>% st_as_sf()
  
  suppressWarnings({
    vecinos <- poly2nb(poligonos, queen = TRUE)
  })
  pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
  
  # Extraccion por posicion de columna (la primera es PM, la segunda es ratio)
  vec_pm  <- as.numeric(st_drop_geometry(poligonos)[[1]])
  vec_rat <- as.numeric(st_drop_geometry(poligonos)[[2]])
  
  test_pm <- moran.test(vec_pm, pesos, zero.policy = TRUE)
  p_val_pm <- ifelse(test_pm$p.value < 0.001, "<0.001", sprintf("%.3f", test_pm$p.value))
  
  test_rat <- moran.test(vec_rat, pesos, zero.policy = TRUE)
  p_val_rat <- ifelse(test_rat$p.value < 0.001, "<0.001", sprintf("%.3f", test_rat$p.value))
  
  res_pm <- data.frame(
    Pollutant = "", Resolution = paste(res_num, "m"),
    `Moran's I` = round(test_pm$estimate[1], 3), `p-value` = p_val_pm, check.names = FALSE
  )
  
  res_rat <- data.frame(
    Pollutant = "", Resolution = paste(res_num, "m"),
    `Moran's I` = round(test_rat$estimate[1], 3), `p-value` = p_val_rat, check.names = FALSE
  )
  
  return(list(PM = res_pm, Ratio = res_rat))
}

resultados_50  <- calcular_moran_global(med_df, r50, 50)
resultados_100 <- calcular_moran_global(med_df, r100, 100)
resultados_200 <- calcular_moran_global(med_df, r200, 200)
resultados_400 <- calcular_moran_global(med_df, r400, 400)

bloque_pm <- bind_rows(resultados_50$PM, resultados_100$PM, resultados_200$PM, resultados_400$PM)
bloque_rat <- bind_rows(resultados_50$Ratio, resultados_100$Ratio, resultados_200$Ratio, resultados_400$Ratio)

# El nombre del contaminante solo en la primera fila de cada bloque (estetica)
bloque_pm$Pollutant[1] <- "PM2.5 (ug m-3)"
bloque_rat$Pollutant[1] <- "PM2.5 ratio"

tabla_moran <- bind_rows(bloque_pm, bloque_rat)


ft <- flextable(tabla_moran)
ft <- theme_booktabs(ft)
ft <- flextable::align(ft, align = "center", part = "all")
ft <- flextable::align(ft, j = 1:2, align = "left", part = "all") 

ft <- flextable::compose(ft, i = 1, j = 4, part = "header", value = as_paragraph("p-value", as_sup("a")))
ft <- flextable::compose(ft, i = 1, j = 1, part = "body", value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ")"))
ft <- flextable::compose(ft, i = 5, j = 1, part = "body", value = as_paragraph("PM", as_sub("2.5"), " ratio"))

ft <- add_footer_lines(ft, values = "a Global Moran Test.")
ft <- fontsize(ft, part = "footer", size = 9)
ft <- autofit(ft)

print(ft)

save_as_docx(
  "Table 4: Global Moran's I results." = ft, 
  path = "Tables/Tabla4_Moran_Global.docx"
)

