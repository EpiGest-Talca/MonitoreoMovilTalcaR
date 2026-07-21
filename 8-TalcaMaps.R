# Construye el mapa base satelital de Talca (OSM / ESRI imagery), identifica
# el limite urbano y genera la Figura 1 del paper (dos paneles: estaciones
# de monitoreo y ruta movil GPS).
# Tambien exporta shapefiles para uso en QGIS.
rm(list = ls())
gc()

library(OpenStreetMap)
library(sf)
library(prettymapr)

load("Data/Processed/GPS/stack_gps.RData") 

if (!dir.exists("Data/Processed/Maps")){ 
  dir.create("Data/Processed/Maps")
}

# Bounding box del mapa base (centrado en Talca urbano)
centerlon <- -71.6440
centerlat <- -35.4180
spanlon <- 0.102
spanlat <- 0.05

upperleft <- c(centerlat + spanlat, centerlon - spanlon)
bottomright <- c(centerlat - spanlat, centerlon + spanlon)

map1 <- openmap(upperleft, bottomright, type="esri-imagery", zoom=13)
plot(map1)
save(map1, file="Data/Processed/Maps/TalcaMap.RData")

# Limite urbano censal 2023. Talca llega como multipoligono; nos quedamos
# con el poligono de mayor area (zona urbana principal)
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
limite_talca_comuna <- subset(limite_urbano, N_COMUNA == "TALCA")

limite_talca_sep <- st_cast(limite_talca_comuna, "POLYGON")
limite_talca_sep$area <- st_area(limite_talca_sep)
limite_talca <- limite_talca_sep[which.max(limite_talca_sep$area), ]

gps_sf <- st_as_sf(gps, coords = c("lon", "lat"), crs = 4326)

# Coordenadas de las tres estaciones de monitoreo en UTM 19S
sites <- data.frame(
  Site = c("U.C. Maule Station", "Talca University Station", "La Florida Station"),
  Easting = c(262216, 260878, 256889),
  Northing = c(6075477, 6078683, 6075395)
)
sites_sf <- st_as_sf(sites, coords = c("Easting", "Northing"), crs = 32719)


# Exportacion de capas vectoriales para QGIS
dir_qgis <- "Data/Processed/Maps/Shapefiles_Mapas_Base"
if (!dir.exists(dir_qgis)) dir.create(dir_qgis, recursive = TRUE)

suppressWarnings(st_write(limite_talca, paste0(dir_qgis, "/Limite_Urbano_Talca.shp"), delete_dsn = TRUE, quiet = TRUE))
suppressWarnings(st_write(sites_sf, paste0(dir_qgis, "/Estaciones_Monitoreo.shp"), delete_dsn = TRUE, quiet = TRUE))
suppressWarnings(st_write(gps_sf, paste0(dir_qgis, "/Ruta_Movil_GPS.shp"), delete_dsn = TRUE, quiet = TRUE))


# Figura 1: mapa superior con estaciones, mapa inferior con ruta movil
if (!dir.exists("Figs")) dir.create("Figs")

tiff("Figs/Fig1_Talca_TwoMaps.tif", units="in", res=300, width=10, height=12)
par(mfrow=c(2,1), mai=c(0,0,0,0)) 

# Panel superior: estaciones de monitoreo
plot(map1)

plot(st_geometry(st_transform(limite_talca, 3857)), 
     border="yellow", lwd=3, add=TRUE)

plot(st_geometry(st_transform(sites_sf, 3857)), 
     pch=c(21, 22, 24), cex=3.5, bg="red", col="white", lwd=2, add=TRUE)

sites_labels <- st_transform(sites_sf, 3857)
coords_sites <- st_coordinates(sites_labels)
text(x = coords_sites[,1], y = coords_sites[,2] + 800, 
     labels = sites$Site, cex = 1.3, col = "white", font = 2)

legend("topleft", inset=0.02, bg="white", title="Legend",
       legend = c("U.C. Maule Station", "Talca University Station", "La Florida Station", "City Limit"),
       pch=c(21, 22, 24, NA), pt.bg=c("red", "red", "red", NA),
       col=c("black", "black", "black", "yellow"), pt.cex=c(2, 2, 2, NA),
       lwd=c(NA, NA, NA, 3), cex=1.2)

prettymapr::addnortharrow(scale=1.2, lwd=2)
prettymapr::addscalebar(lwd=2, label.cex=1.2)
box(col="black", lwd=3, which="plot")

# Panel inferior: ruta GPS
plot(map1)

plot(st_geometry(st_transform(gps_sf, 3857)), 
     col="cyan", pch=16, cex=0.3, add=TRUE)

legend("topleft", inset=0.02, bg="white", title="Legend",
       legend = "Mobile Route", pch=15, col="cyan",
       pt.cex=2, lwd=NA, cex=1.2)

prettymapr::addnortharrow(scale=1.2, lwd=2)
prettymapr::addscalebar(lwd=2, label.cex=1.2)
box(col="black", lwd=3, which="plot")

box(col="black", lwd=10, which="outer")
dev.off()

