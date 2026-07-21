# Procesa las Unidades Vecinales (UV) de Talca desde el shapefile IDEchile 2024
# y genera:
#   - mapa con UV dentro del limite urbano (numeradas)
#   - mapa con UV a nivel comuna completa
#   - leyendas aparte (urbana y comunal)
#   - diccionarios CSV (ID <-> nombre oficial / nombre amigable)

rm(list = ls())
graphics.off()
gc()

library(sf)
library(dplyr)
library(OpenStreetMap)
library(prettymapr)

# s2 desactivado para evitar errores de geometrias no validas en intersecciones
sf_use_s2(FALSE) 

load("Data/Processed/Maps/TalcaMap.RData")

# Limite urbano: poligono de mayor area dentro de la comuna de Talca
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
limite_urbano <- st_make_valid(limite_urbano)

suppressWarnings({
  limite_talca <- st_cast(subset(limite_urbano, N_COMUNA == "TALCA"), "POLYGON")
})
limite_talca <- limite_talca[which.max(st_area(limite_talca)), ]
limite_talca_merc <- st_transform(limite_talca, 3857)

# Unidades vecinales: se disuelven por nombre porque algunas llegan partidas en el shapefile
uvs <- st_read("Data/Raw/SHP_APC2023_R07/UnidadesVecinales_2024v4.shp", quiet = TRUE) 
uvs_talca <- subset(uvs, toupper(t_com_nom) == "TALCA") 
uvs_talca <- st_make_valid(uvs_talca)

uvs_talca <- uvs_talca %>% 
  group_by(t_uv_nom) %>% 
  summarise(geometry = st_union(geometry), .groups="drop")

uvs_talca$ID_Barrio <- sprintf("%03d", 1:nrow(uvs_talca))

# Centroides representativos (point on surface garantiza que esten dentro del poligono)
suppressWarnings({ puntos_comuna <- st_point_on_surface(uvs_talca) })
coords_comuna <- st_coordinates(st_transform(puntos_comuna, 3857))

# Subset urbano: interseccion con el limite, y filtro de areas muy chicas (<2ha)
# que aparecen como artefactos del corte
suppressWarnings({
  uvs_urbano <- st_intersection(uvs_talca, limite_talca)
})

uvs_urbano <- uvs_urbano %>%
  mutate(Area_m2 = as.numeric(st_area(geometry))) %>%
  filter(Area_m2 > 20000) 

# Nombres amigables para UV rurales (el shapefile trae codigos tipo "032R")
uvs_urbano <- uvs_urbano %>%
  mutate(Nombre_Final = case_when(
    t_uv_nom == "032R" ~ "Sector Industrial Ruta 5",
    t_uv_nom == "033R" ~ "Sector Periurbano 033R",   
    t_uv_nom == "034R" ~ "Sector El Tabaco / Sur",
    t_uv_nom == "035R" ~ "Sector Periurbano 035R",   
    t_uv_nom == "036R" ~ "Sector Aldea Campesina",
    t_uv_nom == "055R" ~ "Loteo Norte / Lircay",
    TRUE ~ t_uv_nom
  ))

suppressWarnings({ puntos_urbano <- st_point_on_surface(uvs_urbano) })
coords_urbano <- st_coordinates(st_transform(puntos_urbano, 3857))


# Diccionario comunal (nombres oficiales IDEchile)
dict_comuna <- uvs_talca %>% st_drop_geometry() %>% arrange(ID_Barrio)
etiquetas_comuna <- paste(dict_comuna$ID_Barrio, dict_comuna$t_uv_nom, sep = " - ")
write.csv(dict_comuna, "Data/Processed/Maps/Diccionario_UV_Comuna_Oficial.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Diccionario urbano (con los nombres amigables asignados arriba)
dict_urbano <- uvs_urbano %>% st_drop_geometry() %>% select(ID_Barrio, t_uv_nom, Nombre_Final) %>% arrange(ID_Barrio)
etiquetas_urbano <- paste(dict_urbano$ID_Barrio, dict_urbano$Nombre_Final, sep = " - ")
write.csv(dict_urbano, "Data/Processed/Maps/Diccionario_UV_Urbano_Modificado.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Dimensionamiento del TIFF proporcional al aspect ratio del mapa base
ancho_mapa <- map1$bbox$p2[1] - map1$bbox$p1[1]
alto_mapa  <- map1$bbox$p1[2] - map1$bbox$p2[2]
proporcion <- alto_mapa / ancho_mapa
ancho_pulgadas <- 10
alto_mapa_pulgadas <- ancho_pulgadas * proporcion

# Mapa 1: UV dentro del limite urbano con numeros
tiff("Figs/Mapa_Talca_UV_1_URBANO.tif", units="in", res=300, width=ancho_pulgadas, height=alto_mapa_pulgadas)
par(mai=c(0,0,0,0)) 

plot(map1)
plot(st_geometry(st_transform(uvs_urbano, 3857)), border="cyan", lwd=1.5, add=TRUE) 
plot(st_geometry(limite_talca_merc), border="yellow", lwd=3, add=TRUE) 

# Etiqueta doble (halo blanco + texto rojo) para lectura sobre imagen satelital
text(x = coords_urbano[,1], y = coords_urbano[,2], labels = uvs_urbano$ID_Barrio, cex = 0.85, col = "white", font = 2) 
text(x = coords_urbano[,1], y = coords_urbano[,2], labels = uvs_urbano$ID_Barrio, cex = 0.8, col = "red", font = 2) 

legend("topleft", inset=0.02, bg="white", title="Legend", legend = c("Neighborhood Unit", "City Limit"), col = c("cyan", "yellow"), lwd = c(2, 3), cex = 1.2)
prettymapr::addnortharrow(scale=1.2, lwd=2)
prettymapr::addscalebar(lwd=2, label.cex=1.2)
box(col="black", lwd=3, which="plot")
box(col="black", lwd=10, which="outer")
dev.off()

# Mapa 2: leyenda urbana (ID <-> nombre) en hoja aparte
tiff("Figs/Mapa_Talca_UV_2_LEYENDA_URBANA.tif", units="in", res=300, width=16, height=8)
par(mar=c(1, 1, 1, 1))
plot.new() 
legend("center", title = "Neighborhood Unit (Urban Zone)", legend = etiquetas_urbano, ncol = 5, cex = 0.9, bg = "white", box.col = "white", text.col = "black")
dev.off()

# Mapa 3: UV a nivel comuna (incluye sectores rurales)
bbox_comuna <- st_bbox(st_transform(uvs_talca, 4326))
upperleft_com <- c(bbox_comuna["ymax"], bbox_comuna["xmin"])
bottomright_com <- c(bbox_comuna["ymin"], bbox_comuna["xmax"])

mapa_comuna <- openmap(upperleft_com, bottomright_com, type="esri-imagery", zoom=13)

ancho_mapa_com <- mapa_comuna$bbox$p2[1] - mapa_comuna$bbox$p1[1]
alto_mapa_com  <- mapa_comuna$bbox$p1[2] - mapa_comuna$bbox$p2[2]
proporcion_com <- alto_mapa_com / ancho_mapa_com

tiff("Figs/Mapa_Talca_UV_3_COMUNA.tif", units="in", res=300, width=10, height=10 * proporcion_com)
par(mai=c(0,0,0,0)) 

plot(mapa_comuna)
plot(st_geometry(st_transform(uvs_talca, 3857)), border="cyan", lwd=1.2, add=TRUE)
plot(st_geometry(limite_talca_merc), border="yellow", lwd=3, add=TRUE)

text(x = coords_comuna[,1], y = coords_comuna[,2], labels = uvs_talca$ID_Barrio, cex = 0.85, col = "black", font = 2) 
text(x = coords_comuna[,1], y = coords_comuna[,2], labels = uvs_talca$ID_Barrio, cex = 0.75, col = "white", font = 2) 

legend("topleft", inset=0.02, bg="white", title="Macro-Location", legend = c("Neighborhood Unit", "Urban Boundary"), col = c("cyan", "yellow"), lwd = c(2, 3), cex = 1.2)
prettymapr::addnortharrow(scale=1.2, lwd=2)
prettymapr::addscalebar(lwd=2, label.cex=1.2)
box(col="black", lwd=3, which="plot")
box(col="black", lwd=10, which="outer")
dev.off()

# Leyenda comunal
tiff("Figs/Mapa_Talca_UV_LEYENDA_COMUNA.tif", units="in", res=300, width=16, height=8)
par(mar=c(1, 1, 1, 1))
plot.new() 
legend("center", title = "Neighborhood Unit (official nomenclature IDEchile)", legend = etiquetas_comuna, ncol = 5, cex = 0.9, bg = "white", box.col = "white", text.col = "black")
dev.off()
