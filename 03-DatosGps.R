# Lectura de tracks GPS en formato GPX para cada dia de campaÑa.
# Se lee la capa "track_points" (o la equivalente disponible) y se
# conservan lon/lat/ele/time por punto.

rm(list = ls())
graphics.off()
gc()

library(sf)

leer_gpx <- function(archivo, ruta = "talca", capa = "track_points") {
  # Si la capa pedida no existe en el archivo, se busca una alternativa
  capas <- tryCatch(sf::st_layers(archivo)$name, error = function(e) character(0))
  if (length(capas) && !(capa %in% capas)) {
    cand <- intersect(c("track_points","tracks","route_points","routes","waypoints"), capas)
    if (length(cand)) capa <- cand[1]
  }
  temp <- sf::st_read(archivo, layer = capa, quiet = TRUE)
  coords <- sf::st_coordinates(temp)
  ele  <- if ("ele"  %in% names(temp)) temp$ele  else NA_real_
  time <- if ("time" %in% names(temp)) temp$time else NA
  df <- data.frame(lon = coords[,1], lat = coords[,2], ele = ele, time = time, ruta = ruta)
  # Elimina columnas totalmente NA y filas con NA
  df <- df[, colSums(is.na(df)) < nrow(df), drop = FALSE]
  df <- stats::na.omit(df)
  rownames(df) <- NULL
  df
}

if (!dir.exists("Data/Processed/GPS")) {
  dir.create("Data/Processed/GPS")
}

# 250707 se excluye por lluvia
archivos <- c(
  "Data/Raw/gps/GPS 250702.gpx","Data/Raw/gps/GPS 250703.gpx","Data/Raw/gps/GPS 250704.gpx",
  #"Data/Raw/gps/GPS 250707.gpx",
  "Data/Raw/gps/GPS 250708.gpx","Data/Raw/gps/GPS 250709.gpx",
  "Data/Raw/gps/GSP 250710.gpx","Data/Raw/gps/GPS 250711.gpx","Data/Raw/gps/GPS 250712.gpx",
  "Data/Raw/gps/GPS 250713.gpx","Data/Raw/gps/GPS 250714.gpx","Data/Raw/gps/GPS 250715.gpx",
  "Data/Raw/gps/GPS 250716.gpx","Data/Raw/gps/GPS 250717.gpx","Data/Raw/gps/GPS 250718.gpx",
  "Data/Raw/gps/GPS 250719.gpx","Data/Raw/gps/GPS 250720.gpx","Data/Raw/gps/GPS 250721.gpx",
  "Data/Raw/gps/GPS 250722.gpx","Data/Raw/gps/GPS 250723.gpx","Data/Raw/gps/GPS 250725.gpx",
  "Data/Raw/gps/GPS 250729.gpx","Data/Raw/gps/GPS 250805.gpx"
)

# Se extrae el YYMMDD del nombre del archivo para nombrar el objeto y el .RData
for (a in archivos) {
  id <- sub(".* (\\d{6})\\.gpx$", "\\1", a)
  obj <- paste0("gps_", id)
  df <- leer_gpx(a)
  assign(obj, df)
  save(list = obj, file = file.path("Data/Processed/GPS", paste0(obj, ".RData")))
}
