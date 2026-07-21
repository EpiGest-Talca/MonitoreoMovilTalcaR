# Cruza las tres bases apiladas (movil, central, GPS) en una sola tabla
# indexada por Datetime. Separa los tramos por tipo y aplica interpolacion
# espacial (lat/lon/ele) sobre el tramo de medicion en ruta.

rm(list = ls())
gc()

library(dplyr)
library(zoo)
library(tidyr) 

load("Data/Processed/GPS/stack_gps.RData")
load("Data/Processed/central/stack_central.RData")
load("Data/Processed/Movil/stack_movil.RData")
load("Data/Processed/Sinca/sinca.RData")

# Homologar nombres de columnas antes del merge
sapply(gps, class)
names(gps)[names(gps) == "time"] <- "Datetime"

sapply(sc, class)
names(sc)[names(sc) == "pm"] <- "pmsc"

sapply(m, class)
names(m)[names(m) == "pm"] <- "pmmov"

# Join movil + central por Datetime
raw0 <- merge(m, sc, by="Datetime", all.x=T)
raw0$tipo.y <- NULL
names(raw0)[names(raw0) == "tipo.x"] <- "tipo"

# Agregar GPS
raw <- merge(raw0, gps, by="Datetime", all.x=T)

# Definicion de "dia" desplazada en -4h para que la ruta nocturna
# se quede asociada al dia en que empezo (evita cortes a medianoche)
raw$date <- as.Date(trunc(raw$Datetime - 4*60*60, "day"), format="%Y-%m-%d")

# Segmentos: blancos, duplicados y medicion en ruta
b1  <- subset(raw, tipo=="0")
d1  <- subset(raw, tipo=="1")
d2  <- subset(raw, tipo=="3")
b2  <- subset(raw, tipo=="4")
med <- subset(raw, tipo=="2")

# Conversion de unidades (mg/m3 -> ug/m3)
med$pmmov <- med$pmmov * 1000
med$pmsc  <- med$pmsc  * 1000

summary(med)
med$pmsc[med$pmsc < 0]   <- NA
med$pmmov[med$pmmov < 0] <- NA

# Interpolacion espacial dentro de segmentos continuos.
# Regla: si entre dos puntos pasan mas de 3 minutos se considera un corte
# (no se interpola a traves); si pasan mas de 60 min es un viaje nuevo.
med <- med %>%
  arrange(Datetime) %>%
  mutate(
    ruta = "Talca",
    diff_minutos = as.numeric(difftime(Datetime, dplyr::lag(Datetime), units = "mins")),
    diff_minutos = replace_na(diff_minutos, 0),
    es_nuevo_viaje = diff_minutos > 60,
    tramo_id = cumsum(es_nuevo_viaje),
    corte_continuidad = diff_minutos > 3,
    grupo_interpolacion = cumsum(corte_continuidad)
  ) %>%
  group_by(grupo_interpolacion) %>% 
  mutate(
    # na.approx necesita al menos 2 valores no-NA por grupo; si no, deja el vector original
    lat = if(sum(!is.na(lat)) >= 2) na.approx(lat, maxgap = 300, rule = 2, na.rm = FALSE) else lat,
    lon = if(sum(!is.na(lon)) >= 2) na.approx(lon, maxgap = 300, rule = 2, na.rm = FALSE) else lon,
    ele = if(sum(!is.na(ele)) >= 2) na.approx(ele, maxgap = 300, rule = 2, na.rm = FALSE) else ele
  ) %>%
  ungroup() %>%
  select(-diff_minutos, -es_nuevo_viaje, -corte_continuidad, -grupo_interpolacion)

#elegir la primera opcion conflicts_prefer(dplyr::lag)

save(raw, med, b1, b2, d1, d2, file="Data/Processed/raw.RData")

