# Correccion de las mediciones movil y central usando los tramos duplicados
# (D1, D2) y la referencia diaria de SINCA.
#
# Formulas aplicadas:
#   sc_corr  = pmsc  * (mean_sinca / meansc)
#   mov_corr = pmmov * (mean_sinca / meansc) * corr_duplis
#
# donde:
#   meansc       = promedio diario del sitio central (su propia referencia)
#   mean_sinca   = promedio diario del mismo dia reportado por SINCA
#   corr_duplis  = promedio de los ratios pmsc/pmmov en D1 y D2 ese dia

rm(list = ls())
gc()

load("Data/Processed/raw.RData")
load("Data/Processed/Sinca/sinca.RData")

head(d1)
head(d2)

names(d1)

# Ratio pmsc/pmmov en cada duplicado; protege contra NA, division por cero e Inf
d1$ratio <- with(d1,
                 ifelse(is.na(pmmov) | pmmov == 0 | is.na(pmsc),
                        NA_real_,
                        pmsc / pmmov))

d2$ratio <- with(d2,
                 ifelse(is.na(pmmov) | pmmov == 0 | is.na(pmsc),
                        NA_real_,
                        pmsc / pmmov))

# Media diaria del ratio en el duplicado 1
corr0 <- aggregate(d1$ratio, list(d1$date), mean, na.rm = TRUE)
names(corr0)[names(corr0) == "x"] <- "meand1"
names(corr0)[names(corr0) == "Group.1"] <- "date"

# Media diaria del ratio en el duplicado 2
corr1 <- aggregate(d2$ratio, list(d2$date), mean, na.rm = TRUE)
names(corr1)[names(corr1) == "x"] <- "meand2"
names(corr1)[names(corr1) == "Group.1"] <- "date"

# mean() devuelve NaN si todo es NA; lo convertimos a NA normal
corr0$meand1[is.nan(corr0$meand1)] <- NA_real_
corr1$meand2[is.nan(corr1$meand2)] <- NA_real_

# Promedio entre ambos duplicados = factor de correccion movil
corr <- merge(corr0, corr1, by = "date", all.x = TRUE)
corr$corr <- (corr$meand1 + corr$meand2) / 2
names(corr)

# Incorporar promedio diario SINCA a med
names(med)
names(sinca)
names(corr)

med2 <- merge(med, sinca, by = "date", all.x = TRUE)
med_int <- NULL
med <- NULL
med <- med2

names(corr)
corr$meand1 <- NULL
corr$meand2 <- NULL

# Incorporar factor de correccion a med
med2 <- merge(med, corr, by = "date", all.x = TRUE)
med <- NULL
med <- med2
med2 <- NULL

# Promedio diario del sitio central (su propia media del dia de ruta)
names(med)
sc <- aggregate(med$pmsc, list(med$date), mean, na.rm = TRUE)
names(sc)[names(sc) == "x"] <- "meansc"
names(sc)[names(sc) == "Group.1"] <- "date"
str(sc)

med2 <- merge(med, sc, by = "date", all.x = TRUE)
med <- NULL
med <- med2
med2 <- NULL

# Correccion del sitio central hacia SINCA
med$sc_corr <- with(
  med,
  ifelse(
    is.na(meansc) | meansc == 0 | is.na(pmsc) | is.na(mean_sinca),
    NA_real_,
    pmsc * (mean_sinca / meansc)
  )
)

summary(med$sc_corr)

# Correccion del movil: factor SINCA/central + ajuste de los duplicados
med$mov_corr <- with(
  med,
  ifelse(
    is.na(meansc) | meansc == 0 | is.na(pmmov) | is.na(mean_sinca) | is.na(corr),
    NA_real_,
    pmmov * (mean_sinca / meansc) * corr
  )
)

summary(med$mov_corr)

# Eliminar Inf residuales en columnas numericas
med[] <- lapply(
  med,
  function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x)] <- NA_real_
    }
    x
  }
)

med$hour <- as.numeric(format(med$Datetime, "%H"))

save(med, file = "Data/Processed/med.RData")

# CSV con NA como vacio para compartir externamente
write.csv(med, file = "Data/Processed/med.csv", row.names = FALSE, na = "")
