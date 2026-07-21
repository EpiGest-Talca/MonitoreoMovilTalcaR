# Tabla 1: estadisticas descriptivas horarias del sitio SINCA
# (PM2.5, temperatura, humedad relativa, velocidad de viento) para las
# ventanas temporales de la campana.
# Incluye tambien boxplots horarios de cada variable.

rm(list = ls())
gc()

library(dplyr)
library(flextable) 
library(officer)   

load("Data/Processed/Sinca/sinca_hour.RData")

if (!dir.exists("Tables")) dir.create("Tables")
if (!dir.exists("Plots")) dir.create("Plots")

# PM2.5
stats_pm25 <- sinca_hour %>%
  filter(!is.na(SC_PM25)) %>%
  summarise(
    Variable = "PM2.5 (ug/m3)",
    N = n(),
    p05 = round(quantile(SC_PM25,0.05), 2),
    p25 = round(quantile(SC_PM25,0.25), 2),
    Median = round(median(SC_PM25), 2),
    Mean = round(mean(SC_PM25), 2),
    p75 = round(quantile(SC_PM25,0.75), 2),
    p95 = round(quantile(SC_PM25,0.95), 2)
  )

# Temperatura
stats_temp <- sinca_hour %>%
  filter(!is.na(Temp)) %>%
  summarise(
    Variable = "Temperature (°C)",
    N = n(),
    p05 = round(quantile(Temp,0.05), 2),
    p25 = round(quantile(Temp,0.25), 2),
    Median = round(median(Temp), 2),
    Mean = round(mean(Temp), 2),
    p75 = round(quantile(Temp,0.75), 2),
    p95 = round(quantile(Temp,0.95), 2)
  )

# Humedad relativa
stats_hr <- sinca_hour %>%
  filter(!is.na(HR)) %>%
  summarise(
    Variable = "Relative Humidity (%)",
    N = n(),
    p05 = round(quantile(HR,0.05), 2),
    p25 = round(quantile(HR,0.25), 2),
    Median = round(median(HR), 2),
    Mean = round(mean(HR), 2),
    p75 = round(quantile(HR,0.75), 2),
    p95 = round(quantile(HR,0.95), 2)
  )

# Velocidad del viento
stats_ws <- sinca_hour %>%
  filter(!is.na(WS)) %>%
  summarise(
    Variable = "Wind Speed (m/s)",
    N = n(),
    p05 = round(quantile(WS,0.05), 2),
    p25 = round(quantile(WS,0.25), 2),
    Median = round(median(WS), 2),
    Mean = round(mean(WS), 2),
    p75 = round(quantile(WS,0.75), 2),
    p95 = round(quantile(WS,0.95), 2)
  )

tabla1_final <- bind_rows(stats_pm25, stats_temp, stats_hr, stats_ws)


# Formato flextable
ft <- flextable(tabla1_final)
ft <- theme_booktabs(ft)
ft <- autofit(ft)

ft <- flextable::align(ft, align = "center", part = "all")
ft <- flextable::align(ft, j = 1, align = "left", part = "all")

# Superindices/subindices para N y PM2.5
ft <- flextable::compose(ft, j = "N", part = "header", value = as_paragraph("N", as_sup("a")))
ft <- flextable::compose(ft, i = 1, j = 1, part = "body", value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ")"))

ft <- add_footer_lines(ft, values = "a Hourly observations; p: Percentile.")
ft <- fontsize(ft, part = "footer", size = 9)
ft <- bold(ft, part = "header")


save_as_docx(
  "Table 1: Sinca site summary statistics." = ft, 
  path = "Tables/Tabla1_SincaSite_Stats.docx"
)


library(ggplot2)

# Se ordenan las horas en secuencia nocturna (18:00 del dia 1 -> 03:00 del dia 2)
# porque las rutas comienzan tarde y se extienden hasta la madrugada
orden_horas <- c("18","19", "20", "21", "22", "23", "0", "1", "2", "3")


# Boxplot PM2.5 por hora
plot_pm25_box <- sinca_hour %>%
  filter(!is.na(SC_PM25)) %>%
  mutate(Hora_Ord = factor(as.character(Hour), levels = orden_horas)) %>%
  filter(!is.na(Hora_Ord)) %>%
  ggplot(aes(x = Hora_Ord, y = SC_PM25)) +
  geom_boxplot(fill = "#4682B4", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "PM2.5 distribution by hour",
    x = "Hour",
    y = expression(paste("PM"[2.5], " (µg/m"^3, ")")),
    caption = "Source: National Air Quality Information System Data (SINCA)."
  )

ggsave("Plots/Boxplot_PM25_Ordenado.png", plot_pm25_box, width = 9, height = 6, dpi = 300)


# Boxplot Temperatura por hora
plot_temp_box <- sinca_hour %>%
  filter(!is.na(Temp)) %>%
  mutate(Hora_Ord = factor(as.character(Hour), levels = orden_horas)) %>%
  filter(!is.na(Hora_Ord)) %>%
  ggplot(aes(x = Hora_Ord, y = Temp)) +
  geom_boxplot(fill = "#E69F00", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Temperature distribution by hour",
    x = "Hour",
    y = "Temperature (°C)",
    caption = "Source: National Air Quality Information System Data  (SINCA)."
  )

ggsave("Plots/Boxplot_Temp_Ordenado.png", plot_temp_box, width = 9, height = 6, dpi = 300)


# Boxplot Humedad Relativa por hora
plot_hr_box <- sinca_hour %>%
  filter(!is.na(HR)) %>%
  mutate(Hora_Ord = factor(as.character(Hour), levels = orden_horas)) %>%
  filter(!is.na(Hora_Ord)) %>%
  ggplot(aes(x = Hora_Ord, y = HR)) +
  geom_boxplot(fill = "#56B4E9", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Relative humidity distribution by hour",
    x = "Hour",
    y = "Relative humidity (%)",
    caption = "Source: National Air Quality Information System Data (SINCA)."
  )

ggsave("Plots/Boxplot_HR_Ordenado.png", plot_hr_box, width = 9, height = 6, dpi = 300)


# Boxplot Velocidad del viento por hora
plot_ws_box <- sinca_hour %>%
  filter(!is.na(WS)) %>%
  mutate(Hora_Ord = factor(as.character(Hour), levels = orden_horas)) %>%
  filter(!is.na(Hora_Ord)) %>%
  ggplot(aes(x = Hora_Ord, y = WS)) +
  geom_boxplot(fill = "#009E73", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Wind speed distribution by hour",
    x = "Hour",
    y = "Wind speed (m/s)",
    caption = "Source: National Air Quality Information System Data (SINCA)."
  )

ggsave("Plots/Boxplot_WS_Ordenado.png", plot_ws_box, width = 9, height = 6, dpi = 300)
