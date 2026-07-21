# Version "ratio" del script 12: en vez de rasterizar la concentracion
# absoluta del movil, rasteriza el cociente movil/central (mov_corr/sc_corr).
# Un valor > 1 indica que la celda estuvo mas contaminada que el sitio
# central ese dia; < 1 lo contrario.
#
# Produce la Figura 4 con escala divergente (blanco centrado en 1.0),
# exporta GeoTIFFs para QGIS y guarda los stacks por hora/dia/total.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(OpenStreetMap) 

load("Data/Processed/med.RData")
load("Data/Processed/Maps/EmptyRasters.RData")
load("Data/Processed/Maps/TalcaMap.RData")

r400 <- unwrap(r400_wrap)
r200 <- unwrap(r200_wrap)
r100 <- unwrap(r100_wrap)
r50  <- unwrap(r50_wrap)

# Ratios > 10 se consideran outliers de sensor (probablemente celdas con
# muy pocos puntos y ruido) y se eliminan
med_limpio <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr), sc_corr > 0) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10) 

med_sf <- st_as_sf(med_limpio, coords = c("lon", "lat"), crs = 4326)
coords <- st_coordinates(st_transform(med_sf, 32719))
med_df <- st_drop_geometry(med_sf)

ancho_mapa <- map1$bbox$p2[1] - map1$bbox$p1[1]
alto_mapa  <- map1$bbox$p1[2] - map1$bbox$p2[2]
proporcion <- alto_mapa / ancho_mapa


inyectar_a_raster <- function(plantilla, matriz_datos, id_columna) {
  nombres_capas <- setdiff(names(matriz_datos), id_columna)
  r_stack <- rast(rep(plantilla, length(nombres_capas)))
  names(r_stack) <- nombres_capas
  
  celdas <- matriz_datos[[id_columna]]
  valores <- as.data.frame(matriz_datos[, nombres_capas])
  
  r_stack[celdas] <- valores
  return(r_stack)
}

procesar_ratio <- function(r_plantilla, resolucion_m) {
  
  col_celda <- paste0("cell_", resolucion_m)
  df <- med_df
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  df <- df %>% filter(!is.na(.data[[col_celda]]))
  
  # Agregacion por hora
  res_hora <- df %>%
    group_by(.data[[col_celda]], date, hour) %>%
    summarise(rat_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("H_", format(date, "%Y%m%d"), "_", sprintf("%02d", hour)))
  
  matriz_hora <- res_hora %>%
    select(all_of(col_celda), layer_name, rat_mean) %>%
    pivot_wider(names_from = layer_name, values_from = rat_mean)
  
  # Agregacion por dia
  res_dia <- df %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(rat_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("D_", format(date, "%Y%m%d")))
  
  matriz_dia <- res_dia %>%
    select(all_of(col_celda), layer_name, rat_mean) %>%
    pivot_wider(names_from = layer_name, values_from = rat_mean)
  
  # Total de campana filtrando celdas con menos de 3 dias medidos
  res_total <- res_dia %>%
    group_by(.data[[col_celda]]) %>%
    summarise(
      Total_Rat = mean(rat_mean, na.rm = TRUE),
      Dias_Medidos = n(), 
      .groups = "drop"
    ) %>%
    mutate(Total_Rat_N3 = ifelse(Dias_Medidos >= 3, Total_Rat, NA)) %>%
    select(all_of(col_celda), Total_Rat, Total_Rat_N3)
  
  stack_horas <- inyectar_a_raster(r_plantilla, matriz_hora, col_celda)
  stack_dias  <- inyectar_a_raster(r_plantilla, matriz_dia, col_celda)
  stack_total <- inyectar_a_raster(r_plantilla, res_total, col_celda)
  
  raster_final <- stack_total[["Total_Rat_N3"]]
  
  # GeoTIFF para QGIS
  archivo_qgis <- paste0("Data/Processed/Maps/Rasters_Finales/QGIS_Ratio_", resolucion_m, "m.tif")
  writeRaster(raster_final, filename = archivo_qgis, overwrite = TRUE)

  # Figura 4: escala divergente con blanco centrado en 0.9-1.1 (ratio ~ 1)
  raster_merc <- project(raster_final, "EPSG:3857")
  val_max <- max(values(raster_merc), na.rm = TRUE)
  
  techo <- max(val_max, 2.41) 
  cortes <- c(0, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 2.0, techo)
  
  etiquetas <- c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", 
                 "0.9 - 1.1", 
                 "1.1 - 1.3", "1.3 - 1.5", "1.5 - 1.7", "1.7 - 2.0", "> 2.0")

  # Paleta divergente: azules para ratio < 1, blanco en el centro, rojos para > 1
  mis_colores <- c(
    "#004385",
    "#0066b3",
    "#0083bf",
    "#cbe2f0",
    "#ffffff",
    "#ffccaa",
    "#ffab7f",
    "#f0755a",
    "#d9604b",
    "#ac0023"
  )

  nombre_archivo <- paste0("Figs/Fig4_RatioMap_", resolucion_m, "m.tif")
  
  ancho_pulgadas <- 10
  alto_pulgadas  <- ancho_pulgadas * proporcion
  
  tiff(nombre_archivo, units="in", res=300, width=ancho_pulgadas, height=alto_pulgadas)
  par(bg = "white", mar = c(0, 0, 2.5, 0), xaxs = "i", yaxs = "i") 
  
  plot(map1)
  plot(raster_merc, breaks = cortes, col = mis_colores, alpha = 0.90, add = TRUE, legend = FALSE)
  
  title(main = paste("PM2.5 Ratio (Mobile/Central)", resolucion_m, "m"), 
        col.main = "white", cex.main = 1.6)
  
  legend("topleft", 
         legend = etiquetas, 
         fill = mis_colores, 
         title = "Ratio", 
         bg = "white",       
         box.col = "black",  
         cex = 0.95, 
         inset = c(0.02, 0.11),
         border = "black") 
  
  box(col="black", lwd=4, which="figure")
  
  dev.off()
  
  return(list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total)))
}

resultados_400_rat <- procesar_ratio(r400, 400)
resultados_200_rat <- procesar_ratio(r200, 200)
resultados_100_rat <- procesar_ratio(r100, 100)
resultados_50_rat  <- procesar_ratio(r50, 50)

if (!dir.exists("Data/Processed/Maps/Rasters_Finales")) dir.create("Data/Processed/Maps/Rasters_Finales")
save(resultados_400_rat, resultados_200_rat, resultados_100_rat, resultados_50_rat, 
     file = "Data/Processed/Maps/Rasters_Finales/Stacks_Ratios_Full.RData")
