# Construye las 4 grillas rasterizadas vacias (50, 100, 200 y 400 m) que
# serviran de plantilla para todos los analisis por resolucion.
# Exporta el mapa visual de cada grilla y los rasters empaquetados para
# poder recargarlos luego con unwrap().

rm(list = ls())
gc()

library(terra)
library(OpenStreetMap)
library(sf)

load("Data/Processed/Maps/TalcaMap.RData") 

# Limite urbano: poligono mayor (zona urbana principal)
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
limite_talca_comuna <- subset(limite_urbano, N_COMUNA == "TALCA")

limite_talca_sep <- st_cast(limite_talca_comuna, "POLYGON")
limite_talca_sep$area <- st_area(limite_talca_sep)
limite_talca <- limite_talca_sep[which.max(limite_talca_sep$area), ]

limite_talca_utm <- st_transform(limite_talca, 32719)

# Bbox base para las 4 grillas en UTM 19S. Se fija manualmente para que
# las 4 resoluciones compartan exactamente la misma extension
xmin_real <- 251694.12 
ymin_real <- 6070639.17 
xmax_real <- 267781.78 
ymax_real <- 6081000.80 

ancho_lx <- xmax_real - xmin_real 
alto_ly  <- ymax_real - ymin_real  

# Construye un SpatRaster con resolucion fija, recortado al limite urbano
CreateRaster_Terra <- function(x, y, lx, ly, res, mascara_poligono) {
  r <- rast(xmin = x,               
            xmax = x + lx,          
            ymin = y,               
            ymax = y + ly,          
            resolution = res,         
            crs = "EPSG:32719")
  
  values(r) <- 0 
  r_recortado <- mask(r, vect(mascara_poligono))
  return(r_recortado)
}

r50  <- CreateRaster_Terra(xmin_real, ymin_real, ancho_lx, alto_ly, 50, limite_talca_utm)
r100 <- CreateRaster_Terra(xmin_real, ymin_real, ancho_lx, alto_ly, 100, limite_talca_utm)
r200 <- CreateRaster_Terra(xmin_real, ymin_real, ancho_lx, alto_ly, 200, limite_talca_utm)
r400 <- CreateRaster_Terra(xmin_real, ymin_real, ancho_lx, alto_ly, 400, limite_talca_utm)

# Dibuja cada grilla como puntos cyan sobre mapa satelital (Figura 2)
plot_grilla_individual <- function(raster_obj, res_label, map_base, pt_size) {
  
  ancho_mapa <- map_base$bbox$p2[1] - map_base$bbox$p1[1]
  alto_mapa  <- map_base$bbox$p1[2] - map_base$bbox$p2[2]
  proporcion <- alto_mapa / ancho_mapa
  
  nombre_archivo <- paste0("Figs/Fig2_EmptyRaster_", res_label, "m.tif")
  
  ancho_pulgadas <- 10
  alto_pulgadas  <- ancho_pulgadas * proporcion
  tiff(nombre_archivo, units="in", res=300, width=ancho_pulgadas, height=alto_pulgadas)
  
  par(mar=c(0, 0, 2.5, 0), xaxs="i", yaxs="i") 
  
  puntos_merc <- project(as.points(raster_obj), "EPSG:3857")
  coords <- crds(puntos_merc)
  
  plot(map_base)
  points(x = coords[, 1], y = coords[, 2], col = "cyan", pch = 19, cex = pt_size)
  plot(st_geometry(st_transform(limite_talca, 3857)), border="yellow", lwd=2, add=TRUE)
  
  title(main = paste(res_label, "m"), col.main = "white", cex.main = 1.8)
  
  box(col="black", lwd=4, which="figure")
  
  dev.off()
}

# Tamano del punto ajustado para que se vea razonable segun densidad de la grilla
plot_grilla_individual(r400, 400, map1, pt_size = 0.4)
plot_grilla_individual(r200, 200, map1, pt_size = 0.25)
plot_grilla_individual(r100, 100, map1, pt_size = 0.15)
plot_grilla_individual(r50,   50, map1, pt_size = 0.01) 


# Los SpatRaster no se serializan con save() directo; wrap() los convierte
# en objetos portable para guardar y luego recargar con unwrap()
r50_wrap  <- wrap(r50)
r100_wrap <- wrap(r100)
r200_wrap <- wrap(r200)
r400_wrap <- wrap(r400)

save(r50_wrap, r100_wrap, r200_wrap, r400_wrap, file = "Data/Processed/Maps/EmptyRasters.RData")
