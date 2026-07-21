# Genera la serie de 5 mapas satelitales con efecto cascada
# (urbano -> comuna -> provincia -> region -> pais) usando imagenes ESRI.
# Cada mapa destaca en amarillo el nivel interior sobre el contorno del nivel exterior.

library(OpenStreetMap)
library(sf)
library(prettymapr)
library(chilemapas)
library(grDevices)

if (!dir.exists("Figs")) dir.create("Figs")

# Capas base por nivel administrativo
# A. Chile continental (se recorta para evitar islas oceanicas)
chile_sf <- st_as_sf(generar_regiones()) %>% st_transform(4326)
chile_continental <- st_crop(st_union(chile_sf), 
                             st_bbox(c(xmin=-75.5, ymin=-57, xmax=-66, ymax=-17), crs=4326))

# B. Region del Maule (codigo 07)
region_maule <- subset(chile_sf, codigo_region == "07")

# C. Provincia de Talca (codigo 071)
prov_sf <- st_as_sf(generar_provincias()) %>% st_transform(4326)
prov_talca <- subset(prov_sf, codigo_provincia == "071")

# D. Comuna de Talca completa (urbano + rural, codigo 07101)
comunas_sf <- st_as_sf(mapa_comunas) %>% st_transform(4326)
comuna_talca_completa <- subset(comunas_sf, codigo_comuna == "07101")

# E. Limite urbano censal de Talca (poligono de mayor area)
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
talca_urbano <- st_cast(subset(limite_urbano, N_COMUNA == "TALCA"), "POLYGON")
talca_urbano <- talca_urbano[which.max(st_area(talca_urbano)), ] %>% st_transform(4326)


# Funcion generica: descarga el mapa base de ESRI, dibuja contorno exterior
# y achurado interior, y exporta TIFF con dimensiones proporcionales al bbox
exportar_mapa_satelital <- function(poligono_borde, poligono_achurado, nombre_archivo, nivel_zoom, titulo_mapa, color_exterior) {
  
  caja_4326 <- st_bbox(poligono_borde)
  margen_lon <- (caja_4326["xmax"] - caja_4326["xmin"]) * 0.05
  margen_lat <- (caja_4326["ymax"] - caja_4326["ymin"]) * 0.05
  
  map_base <- openmap(c(caja_4326["ymax"] + margen_lat, caja_4326["xmin"] - margen_lon),
                      c(caja_4326["ymin"] - margen_lat, caja_4326["xmax"] + margen_lon), 
                      type = "esri-imagery", zoom = nivel_zoom)
  
  borde_3857 <- st_transform(poligono_borde, 3857)
  achurado_3857 <- st_transform(poligono_achurado, 3857)
  
  # Ancho del TIFF proporcional al aspect ratio del bbox proyectado
  caja_3857 <- st_bbox(borde_3857)
  aspecto <- as.numeric((caja_3857["xmax"] - caja_3857["xmin"]) / (caja_3857["ymax"] - caja_3857["ymin"]))
  
  tiff(paste0("Figs/", nombre_archivo, ".tif"), units="in", res=300, width=max(10*aspecto, 3), height=10)
  par(mai=c(0,0,0,0), xaxs="i", yaxs="i")
  
  plot(map_base)
  
  # Achurado amarillo (seleccion interior)
  plot(st_geometry(achurado_3857), density=8, angle=45, col=adjustcolor("yellow", 0.4), lwd=2, border="yellow", add=TRUE)
  
  # En zooms lejanos se usa linea fina para no empastar el contorno
  grosor <- ifelse(nivel_zoom <= 5, 0.6, 3)
  plot(st_geometry(borde_3857), border=color_exterior, lwd=grosor, col=NA, add=TRUE)
  
  legend("top", legend=titulo_mapa, bty="n", text.col="white", cex=2.5, text.font=2, inset=0.02)
  
  prettymapr::addnortharrow(scale=0.8)
  try(prettymapr::addscalebar(label.col="white"), silent=TRUE)
  box(lwd=3)
  dev.off()
}


# Los 5 niveles de la cascada
exportar_mapa_satelital(talca_urbano, talca_urbano, 
                        "FigA_Urban_Talca", 13, "Urban Talca", "yellow")

exportar_mapa_satelital(comuna_talca_completa, talca_urbano, 
                        "FigB_Talca_Commune", 11, "Talca", "cyan")

exportar_mapa_satelital(prov_talca, comuna_talca_completa, 
                        "FigC_Talca_Province", 10, "Talca Province", "cyan")

exportar_mapa_satelital(region_maule, prov_talca, 
                        "FigD_Maule_Region", 8, "Maule Region", "cyan")

exportar_mapa_satelital(chile_continental, region_maule, 
                        "FigE_Chile", 5, " ", "cyan")

