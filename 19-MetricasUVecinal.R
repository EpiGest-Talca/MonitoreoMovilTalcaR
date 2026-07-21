# Tabla 5: estadisticas resumen de PM2.5 y ratio por Unidad Vecinal (UV).
# Cada punto movil se cruza espacialmente con el shapefile de UV y se
# promedia por UV-dia, luego por UV-campana. Se reportan mediana, media
# y Q1-Q3 para cada UV con >= 3 dias de mediciones.

rm(list = ls())
gc()

library(sf)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

sf_use_s2(FALSE)

load("Data/Processed/med.RData")

if (!dir.exists("Tables")) dir.create("Tables")

# Limite urbano (poligono mayor)
limite_urbano <- st_read("Data/Raw/SHP_APC2023_R07/Limite_Urbano_Censal.shp", quiet = TRUE)
limite_urbano <- st_make_valid(limite_urbano)
suppressWarnings({
  limite_talca <- st_cast(subset(limite_urbano, N_COMUNA == "TALCA"), "POLYGON")
})
limite_talca <- limite_talca[which.max(st_area(limite_talca)), ]

uvs <- st_read("Data/Raw/SHP_APC2023_R07/UnidadesVecinales_2024v4.shp", quiet = TRUE) 
uvs_talca <- subset(uvs, toupper(t_com_nom) == "TALCA") 
uvs_talca <- st_make_valid(uvs_talca)

# Nombres amigables para las UV rurales (mismo mapeo que en el script 9)
uvs_talca <- uvs_talca %>%
  mutate(Nombre_Final = case_when(
    t_uv_nom == "032R" ~ "Sector Industrial Ruta 5",
    t_uv_nom == "033R" ~ "Sector Periurbano 033R",   
    t_uv_nom == "034R" ~ "Sector El Tabaco / Sur",
    t_uv_nom == "035R" ~ "Sector Periurbano 035R",   
    t_uv_nom == "036R" ~ "Sector Aldea Campesina",
    t_uv_nom == "055R" ~ "Loteo Norte / Lircay",
    TRUE ~ t_uv_nom
  ))

# Algunas UV llegan partidas en el shapefile; se disuelven por nombre
uvs_talca <- uvs_talca %>% 
  group_by(Nombre_Final) %>% 
  summarise(geometry = st_union(geometry), .groups="drop")

# Recorte al area urbana y descarte de fragmentos muy pequenos (< 2 ha)
suppressWarnings({
  uvs_urbano <- st_intersection(uvs_talca, limite_talca)
})

uvs_urbano <- uvs_urbano %>%
  mutate(Area_m2 = as.numeric(st_area(geometry))) %>%
  filter(Area_m2 > 20000) 

# UTM 19S para el st_join con los puntos
uvs_utm <- st_transform(uvs_urbano, 32719)

# Puntos moviles validos con ratio calculado
med_limpio <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr), sc_corr > 0) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

med_sf <- st_as_sf(med_limpio, coords = c("lon", "lat"), crs = 4326)
med_utm <- st_transform(med_sf, 32719)

# Asignar a cada punto su UV por interseccion espacial
puntos_con_uv <- st_join(med_utm, uvs_utm, join = st_intersects)

df_analisis <- st_drop_geometry(puntos_con_uv) %>%
  filter(!is.na(Nombre_Final)) 

# Promedio UV-dia
res_diario <- df_analisis %>%
  group_by(Nombre_Final, date) %>%
  summarise(
    pm_mean = mean(mov_corr, na.rm = TRUE),
    ratio_mean = mean(ratio_pm25, na.rm = TRUE),
    .groups = "drop"
  )

# Estadisticas UV-campana; solo UV con >= 3 dias; orden descendente por mediana PM
res_total <- res_diario %>%
  group_by(Nombre_Final) %>%
  summarise(
    Dias_Medidos = n(),
    PM_Median = round(median(pm_mean, na.rm = TRUE), 1),
    PM_Mean   = round(mean(pm_mean, na.rm = TRUE), 1), 
    PM_Q1 = round(quantile(pm_mean, 0.25, na.rm = TRUE), 1),
    PM_Q3 = round(quantile(pm_mean, 0.75, na.rm = TRUE), 1),
    
    Rat_Median = round(median(ratio_mean, na.rm = TRUE), 2),
    Rat_Mean   = round(mean(ratio_mean, na.rm = TRUE), 2), 
    Rat_Q1 = round(quantile(ratio_mean, 0.25, na.rm = TRUE), 2),
    Rat_Q3 = round(quantile(ratio_mean, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  filter(Dias_Medidos >= 3) %>%
  mutate(
    PM_Q1_Q3 = sprintf("(%.1f; %.1f)", PM_Q1, PM_Q3),
    Rat_Q1_Q3 = sprintf("(%.2f; %.2f)", Rat_Q1, Rat_Q3)
  ) %>%
  arrange(desc(PM_Median))


tabla_export <- res_total %>%
  select(
    Neighborhood = Nombre_Final, 
    N_days = Dias_Medidos,
    PM_Median, PM_Mean, PM_Q1_Q3,   
    Rat_Median, Rat_Mean, Rat_Q1_Q3 
  )

ft <- flextable(tabla_export)

ft <- set_header_labels(ft, 
                        Neighborhood = "Neighborhood Unit",
                        N_days = "Days measured",
                        PM_Median = "Median", PM_Mean = "Mean", PM_Q1_Q3 = "(Q1; Q3)",
                        Rat_Median = "Median", Rat_Mean = "Mean", Rat_Q1_Q3 = "(Q1; Q3)")

# 3 columnas de metricas por variable (Median + Mean + Q1-Q3)
ft <- add_header_row(ft, 
                     values = c("", "", "PM2.5", "PM2.5 ratio"), 
                     colwidths = c(1, 1, 3, 3))

ft <- theme_booktabs(ft)
ft <- colformat_double(ft, na_str = "")
ft <- flextable::align(ft, align = "center", part = "all")
ft <- flextable::align(ft, j = 1, align = "left", part = "all")

# Formato de subindice y superindice en los encabezados de metrica
ft <- flextable::compose(ft, i = 1, j = 3, part = "header", 
              value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ")"))
ft <- flextable::compose(ft, i = 1, j = 6, part = "header", 
              value = as_paragraph("PM", as_sub("2.5"), " ratio", as_sup("a")))

ft <- add_footer_lines(ft, values = "a Ratio of mobile measurement compared to central site. Q1: 25th percentile. Q3: 75th percentile. Only neighborhoods with at least 3 days of measurements are included. Neighborhoods are ordered by median PM2.5 concentration.")
ft <- fontsize(ft, part = "footer", size = 9)

# Ancho extra a la primera columna para que quepan nombres largos de UV
ft <- autofit(ft)
ft <- flextable::width(ft, j = 1, width = 2.0) 

print(ft)

# Hoja apaisada
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width = 11, height = 8.5),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(
  "Table 5: Summary statistics of mobile PM2.5 and ratio by neighborhood unit." = ft, 
  path = "Tables/Tabla5_UnidadesVecinales_Stats.docx",
  pr_section = sect_properties
)
