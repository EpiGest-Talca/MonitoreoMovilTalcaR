# Figura 5: mapas LISA (Local Moran) del ratio PM2.5 mobile/central por
# resolucion. Clasifica cada celda con al menos 3 dias medidos en uno de
# los 4 cuadrantes de Moran + "Not Significant" (p > 0.05).
#   High-High / Low-Low : clusters de valores similares entre vecinos
#   High-Low  / Low-High: outliers espaciales
# Exporta tambien el shapefile de los hotspots para QGIS.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(spdep)
library(OpenStreetMap)

load("Data/Processed/med.RData")
load("Data/Processed/Maps/EmptyRasters.RData")
load("Data/Processed/Maps/TalcaMap.RData")

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

# Limite urbano para dibujo
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
limite_talca <- st_cast(subset(limite_urbano, N_COMUNA == "TALCA"), "POLYGON")
limite_talca <- limite_talca[which.max(st_area(limite_talca)), ]
limite_talca_merc <- st_transform(limite_talca, 3857)

generar_lisa_map <- function(df, plantilla_raster, res_num, map_base) {
  
  col_celda <- paste0("cell_", res_num)
  df[[col_celda]] <- cellFromXY(plantilla_raster, coords)
  
  # Promedio ratio por celda y campana (celdas con >= 3 dias)
  res_total <- df %>%
    filter(!is.na(.data[[col_celda]])) %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(ratio_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data[[col_celda]]) %>%
    summarise(Ratio_Total = mean(ratio_mean, na.rm = TRUE), Dias = n(), .groups = "drop") %>%
    filter(Dias >= 3)
  
  # Inyectar valores al raster plantilla
  r_ratio <- plantilla_raster
  values(r_ratio) <- NA
  r_ratio[res_total[[col_celda]]] <- res_total$Ratio_Total
  
  # dissolve=FALSE para mantener cada celda como poligono independiente
  poligonos <- as.polygons(r_ratio, na.rm = TRUE, dissolve = FALSE) %>% st_as_sf()
  
  # Vector de valores en el mismo orden que los poligonos
  vec_ratio <- as.numeric(st_drop_geometry(poligonos)[[1]])
  
  # Matriz de pesos espaciales por contiguedad tipo reina
  suppressWarnings({
    vecinos <- poly2nb(poligonos, queen = TRUE)
  })
  pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
  
  # Local Moran (LISA)
  lisa <- localmoran(vec_ratio, pesos, zero.policy = TRUE)
  
  # Clasificacion en los 4 cuadrantes + no significativo
  z_ratio <- as.numeric(scale(vec_ratio))   # valor estandarizado
  z_lag <- lag.listw(pesos, z_ratio, zero.policy = TRUE)  # promedio espacial de los vecinos
  p_values <- lisa[, 5]
  
  poligonos$Cluster <- "Not Significant"
  sig <- 0.05
  
  poligonos$Cluster[z_ratio > 0 & z_lag > 0 & p_values <= sig] <- "High-High"
  poligonos$Cluster[z_ratio < 0 & z_lag < 0 & p_values <= sig] <- "Low-Low"
  poligonos$Cluster[z_ratio > 0 & z_lag < 0 & p_values <= sig] <- "High-Low"
  poligonos$Cluster[z_ratio < 0 & z_lag > 0 & p_values <= sig] <- "Low-High"
  
  poligonos$Cluster <- factor(poligonos$Cluster, 
                              levels = c("High-High", "Low-Low", 
                                         "High-Low", "Low-High", "Not Significant"))
  
  # Shapefile para QGIS
  dir_shp <- "Data/Processed/Maps/Shapefiles_QGIS"
  if (!dir.exists(dir_shp)) dir.create(dir_shp, recursive = TRUE)
  
  archivo_shp <- paste0(dir_shp, "/LISA_Hotspots_", res_num, "m.shp")
  suppressWarnings(st_write(poligonos, archivo_shp, delete_dsn = TRUE, quiet = TRUE))

  # Figura 5: mapa de hotspots sobre imagen satelital
  if (!dir.exists("Figs")) dir.create("Figs")
  
  poligonos_merc <- st_transform(poligonos, 3857)

  colores_lisa <- c("High-High" = adjustcolor("red", alpha.f = 0.75), 
                    "Low-Low" = adjustcolor("blue", alpha.f = 0.75), 
                    "High-Low" = adjustcolor("pink", alpha.f = 0.75), 
                    "Low-High" = adjustcolor("lightblue", alpha.f = 0.75), 
                    "Not Significant" = adjustcolor("gray80", alpha.f = 0.35))
  
  ancho_mapa <- map_base$bbox$p2[1] - map_base$bbox$p1[1]
  alto_mapa  <- map_base$bbox$p1[2] - map_base$bbox$p2[2]
  proporcion <- alto_mapa / ancho_mapa
  
  nombre_archivo <- paste0("Figs/Fig5_LISA_Hotspots_", res_num, "m.tif")
  
  tiff(nombre_archivo, units="in", res=300, width=10, height=(10*proporcion))
  par(mar=c(1, 1, 3, 1))
  
  plot(map_base)
  
  # Borde mas fino en resoluciones chicas porque hay muchas mas celdas
  grosor_borde <- ifelse(res_num == 50, 0.05, ifelse(res_num == 100, 0.1, 0.3))
  
  plot(st_geometry(poligonos_merc), 
       col = colores_lisa[as.character(poligonos_merc$Cluster)], 
       border = adjustcolor("white", alpha.f = 0.5),
       lwd = grosor_borde, 
       add = TRUE)
       
  # Leyenda con colores opacos (sin alpha) para que el cuadrito se lea bien
  colores_leyenda <- c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "pink", "Low-High" = "lightblue", "Not Significant" = "gray80")
  
  legend("topleft", inset = 0.02, title = paste("LISA Clusters"), 
         legend = names(colores_leyenda), fill = colores_leyenda, 
         border = "black", bg = "white", cex = 1.1)
  
  title(main = paste("Local Moran's I PM2.5 -", res_num, "m"), 
        col.main="white", cex.main=1.5)
  
  dev.off()
}

generar_lisa_map(med_df, r400, 400, map1)
generar_lisa_map(med_df, r200, 200, map1)
generar_lisa_map(med_df, r100, 100, map1)
generar_lisa_map(med_df, r50,   50, map1)
