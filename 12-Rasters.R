# Agrega las mediciones moviles corregidas (mov_corr) a las 4 grillas
# (50/100/200/400 m) en 3 niveles temporales:
#   - por hora (H_YYYYMMDD_HH)
#   - por dia  (D_YYYYMMDD)
#   - total de campaña (Total_Mean), con version filtrada a celdas con N>=3 dias
#
# Genera la Figura 3 (mapa de PM2.5 por resolucion) y exporta GeoTIFF para QGIS.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(OpenStreetMap) 

load("Data/Processed/med.RData")
load("Data/Processed/Maps/EmptyRasters.RData")
load("Data/Processed/Maps/talcamap.RData")

r400 <- unwrap(r400_wrap)
r200 <- unwrap(r200_wrap)
r100 <- unwrap(r100_wrap)
r50  <- unwrap(r50_wrap)

if (!dir.exists("Figs")) dir.create("Figs")
if (!dir.exists("Data/Processed/Maps/Rasters_Finales")) dir.create("Data/Processed/Maps/Rasters_Finales")

# Filtrado de puntos validos y paso a UTM 19S para extraer celdas
med_limpio <- med[!is.na(med$lon) & !is.na(med$lat) & !is.na(med$mov_corr), ]
med_sf <- st_as_sf(med_limpio, coords = c("lon", "lat"), crs = 4326)
coords <- st_coordinates(st_transform(med_sf, 32719))

med_df <- st_drop_geometry(med_sf)

ancho_mapa <- map1$bbox$p2[1] - map1$bbox$p1[1]
alto_mapa  <- map1$bbox$p1[2] - map1$bbox$p2[2]
proporcion <- alto_mapa / ancho_mapa


# Inyecta valores de una tabla (celda -> valor) en un SpatRaster multicapa,
# usando como plantilla "plantilla" y como nombres de capa los del pivot_wider
inyectar_a_raster <- function(plantilla, matriz_datos, id_columna) {
  nombres_capas <- setdiff(names(matriz_datos), id_columna)
  r_stack <- rast(rep(plantilla, length(nombres_capas)))
  names(r_stack) <- nombres_capas
  
  celdas <- matriz_datos[[id_columna]]
  valores <- as.data.frame(matriz_datos[, nombres_capas])
  
  r_stack[celdas] <- valores
  return(r_stack)
}

procesar_resolucion <- function(r_plantilla, resolucion_m) {
  
  col_celda <- paste0("cell_", resolucion_m)
  df <- med_df
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  df <- df %>% filter(!is.na(.data[[col_celda]]))
  
  # Agregacion por hora
  res_hora <- df %>%
    group_by(.data[[col_celda]], date, hour) %>%
    summarise(pm_mean = mean(mov_corr, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("H_", format(date, "%Y%m%d"), "_", sprintf("%02d", hour)))
  
  matriz_hora <- res_hora %>%
    select(all_of(col_celda), layer_name, pm_mean) %>%
    pivot_wider(names_from = layer_name, values_from = pm_mean)
  
  # Agregacion por dia
  res_dia <- df %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(pm_mean = mean(mov_corr, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("D_", format(date, "%Y%m%d")))
  
  matriz_dia <- res_dia %>%
    select(all_of(col_celda), layer_name, pm_mean) %>%
    pivot_wider(names_from = layer_name, values_from = pm_mean)
  
  # Agregacion total de campana; Total_Mean_N3 exige al menos 3 dias en la celda
  res_total <- res_dia %>%
    group_by(.data[[col_celda]]) %>%
    summarise(
      Total_Mean = mean(pm_mean, na.rm = TRUE),
      Dias_Medidos = n(), 
      .groups = "drop"
    ) %>%
    mutate(Total_Mean_N3 = ifelse(Dias_Medidos >= 3, Total_Mean, NA)) %>%
    select(all_of(col_celda), Total_Mean, Total_Mean_N3)
  
  stack_horas <- inyectar_a_raster(r_plantilla, matriz_hora, col_celda)
  stack_dias  <- inyectar_a_raster(r_plantilla, matriz_dia, col_celda)
  stack_total <- inyectar_a_raster(r_plantilla, res_total, col_celda)
  
  raster_final <- stack_total[["Total_Mean_N3"]]
  
  # GeoTIFF para abrir en QGIS
  archivo_qgis <- paste0("Data/Processed/Maps/Rasters_Finales/QGIS_PM25_", resolucion_m, "m.tif")
  writeRaster(raster_final, filename = archivo_qgis, overwrite = TRUE)

  # Figura 3: mapa de PM2.5 por resolucion con escala discreta
  raster_merc <- project(raster_final, "EPSG:3857")
  val_max <- max(values(raster_merc), na.rm = TRUE)
  
  # El techo se asegura de ser >= 160.1 para que el ultimo bin "> 160" exista
  # incluso cuando los datos no superan 160
  techo <- max(val_max, 160.1) 
  
  cortes <- c(0, 20, 40, 60, 80, 100, 120, 140, 160, techo)
  etiquetas <- c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100", "100 - 120", "120 - 140", "140 - 160", "> 160")
  
  mis_colores <- colorRampPalette(c("white", "yellow", "orange", "red", "darkred"))(length(cortes) - 1)
  nombre_archivo <- paste0("Figs/Fig3_MapaBase_", resolucion_m, "m.tif")
  
  ancho_pulgadas <- 10
  alto_pulgadas  <- ancho_pulgadas * proporcion
  
  tiff(nombre_archivo, units="in", res=300, width=ancho_pulgadas, height=alto_pulgadas)
  par(bg = "white", mar = c(0, 0, 2.5, 0), xaxs = "i", yaxs = "i") 
  
  plot(map1)
  plot(raster_merc, breaks = cortes, col = mis_colores, alpha = 0.85, add = TRUE, legend = FALSE)
  
  title(main = paste("PM2.5 - ", resolucion_m, "m"), 
        col.main = "white", cex.main = 1.6)
  
  legend("topleft", 
         legend = etiquetas, 
         fill = mis_colores, 
         title = "PM2.5\n(ug/m3)", 
         bg = "white",       
         box.col = "black",  
         cex = 0.85,
         inset = c(0.02, 0.11))
  box(col="black", lwd=4, which="figure")
  
  dev.off()
  
  return(list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total)))
}

resultados_400 <- procesar_resolucion(r400, 400)
resultados_200 <- procesar_resolucion(r200, 200)
resultados_100 <- procesar_resolucion(r100, 100)
resultados_50  <- procesar_resolucion(r50, 50)

save(resultados_400, resultados_200, resultados_100, resultados_50, 
     file = "Data/Processed/Maps/Rasters_Finales/Stacks_TodasLasResoluciones.RData")
