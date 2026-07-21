# Apila los .RData diarios (GPS, central y movil) en tres data.frames unicos:
# gps (stack_GPS), sc (stack_central) y m (stack_movil).

rm(list = ls())
graphics.off()
gc()

# ---- GPS ----

load("Data/Processed/GPS/gps_250702.RData")
load("Data/Processed/GPS/gps_250703.RData")
load("Data/Processed/GPS/gps_250704.RData")
# load("Data/Processed/GPS/gps_250707.RData")  # descartado por lluvia
load("Data/Processed/GPS/gps_250708.RData")
load("Data/Processed/GPS/gps_250709.RData")
load("Data/Processed/GPS/gps_250710.RData")  # archivo fuente GSP 250710.gpx, pero el RData se llama gps_250710
load("Data/Processed/GPS/gps_250711.RData")
load("Data/Processed/GPS/gps_250712.RData")
load("Data/Processed/GPS/gps_250713.RData")
load("Data/Processed/GPS/gps_250714.RData")
load("Data/Processed/GPS/gps_250715.RData")
load("Data/Processed/GPS/gps_250716.RData")
load("Data/Processed/GPS/gps_250717.RData")
load("Data/Processed/GPS/gps_250718.RData")
load("Data/Processed/GPS/gps_250719.RData")
load("Data/Processed/GPS/gps_250720.RData")
load("Data/Processed/GPS/gps_250721.RData")
load("Data/Processed/GPS/gps_250722.RData")
load("Data/Processed/GPS/gps_250723.RData")
load("Data/Processed/GPS/gps_250725.RData")
load("Data/Processed/GPS/gps_250729.RData")
load("Data/Processed/GPS/gps_250805.RData")

gps <- rbind(
  gps_250702, gps_250703, gps_250704,
  # gps_250707,  # lluvia
  gps_250708, gps_250709, gps_250710,
  gps_250711, gps_250712, gps_250713, gps_250714, gps_250715,
  gps_250716, gps_250717, gps_250718, gps_250719, gps_250720,
  gps_250721, gps_250722, gps_250723, gps_250725, gps_250729, gps_250805
)


save(gps, file="Data/Processed/GPS/stack_GPS.RData")
rm(list=ls())


# ---- Central ----
load("Data/Processed/central/central_250702.RData")
load("Data/Processed/central/central_250703.RData")
load("Data/Processed/central/central_250704.RData")
# load("Data/Processed/central/central_250707.RData")
load("Data/Processed/central/central_250708.RData")
load("Data/Processed/central/central_250709.RData")
load("Data/Processed/central/central_250710.RData")
load("Data/Processed/central/central_250711.RData")
load("Data/Processed/central/central_250712.RData")
load("Data/Processed/central/central_250713.RData")
load("Data/Processed/central/central_250714.RData")
load("Data/Processed/central/central_250715.RData")
load("Data/Processed/central/central_250716.RData")
load("Data/Processed/central/central_250717.RData")
load("Data/Processed/central/central_250718.RData")
load("Data/Processed/central/central_250719.RData")
load("Data/Processed/central/central_250720.RData")
load("Data/Processed/central/central_250721.RData")
load("Data/Processed/central/central_250722.RData")
load("Data/Processed/central/central_250723.RData")
load("Data/Processed/central/central_250725.RData")
load("Data/Processed/central/central_250729.RData")
load("Data/Processed/central/central_250805.RData")

sc <- rbind(a1, a2, a3, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
            a15, a16, a17, a18, a19, a20, a21, a22, a23)

save(sc, file="Data/Processed/central/stack_central.RData")
rm(list=ls())


# ---- Movil ----
load("Data/Processed/movil/movil_250702.RData")
load("Data/Processed/movil/movil_250703.RData")
load("Data/Processed/movil/movil_250704.RData")
# load("Data/Processed/movil/movil_250707.RData")
load("Data/Processed/movil/movil_250708.RData")
load("Data/Processed/movil/movil_250709.RData")
load("Data/Processed/movil/movil_250710.RData")
load("Data/Processed/movil/movil_250711.RData")
load("Data/Processed/movil/movil_250712.RData")
load("Data/Processed/movil/movil_250713.RData")
load("Data/Processed/movil/movil_250714.RData")
load("Data/Processed/movil/movil_250715.RData")
load("Data/Processed/movil/movil_250716.RData")
load("Data/Processed/movil/movil_250717.RData")
load("Data/Processed/movil/movil_250718.RData")
load("Data/Processed/movil/movil_250719.RData")
load("Data/Processed/movil/movil_250720.RData")
load("Data/Processed/movil/movil_250721.RData")
load("Data/Processed/movil/movil_250722.RData")
load("Data/Processed/movil/movil_250723.RData")
load("Data/Processed/movil/movil_250725.RData")
load("Data/Processed/movil/movil_250729.RData")
load("Data/Processed/movil/movil_250805.RData")

m <- rbind(d1, d2, d3, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14,
           d15, d16, d17, d18, d19, d20, d21, d22, d23)

save(m, file="Data/Processed/movil/stack_movil.RData")
