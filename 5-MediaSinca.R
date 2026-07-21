# Procesa los datos horarios de la estacion SINCA (La Florida) y los cruza
# con las ventanas temporales de cada dia de campana.
#
# Produce dos salidas:
#   sinca_hour: observaciones horarias SINCA dentro de las ventanas de medicion
#   sinca     : promedios diarios (PM2.5, temp, HR, viento) por dia de ruta

rm(list = ls())
gc()

library(dplyr)
library(lubridate)
library(purrr)
library(conflicted) 

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("group_by", "dplyr")

if (!dir.exists("Data/Processed/Sinca")){ 
  dir.create("Data/Processed/Sinca")
}

sinca_raw <- read.csv("Data/Raw/sinca/pm25datos_250702_250805.csv", sep=";")

sinca_pm25_clean <- sinca_raw %>%
  mutate(
    # HHMM -> "HH:MM" (ej: 100 -> "01:00")
    hora_str = sprintf("%04d", HORA..HHMM.),
    hora_hh_mm = paste0(substr(hora_str, 1, 2), ":", substr(hora_str, 3, 4)),
    Datetime_sinca = ymd_hm(paste(FECHA..YYMMDD., hora_hh_mm), tz = "America/Santiago"),
    # Se prefiere el valor validado; si no existe, se usa el preliminar
    conc = coalesce(Registros.validados, Registros.preliminares)
  ) %>%
  filter(!is.na(conc)) %>%
  select(Datetime_sinca, conc)


# Lector generico para las variables meteorologicas de SINCA (T, HR, WS, WD).
# La columna de valor siempre esta en la posicion 3 del CSV.
limpiar_clima <- function(ruta, nombre_var) {
  if(!file.exists(ruta)) return(NULL)
  raw <- read.csv(ruta, sep=";", stringsAsFactors = FALSE)
  col_val <- names(raw)[3]
  raw %>%
    mutate(
      hora_str = sprintf("%04d", HORA..HHMM.),
      hora_hh_mm = paste0(substr(hora_str, 1, 2), ":", substr(hora_str, 3, 4)),
      Datetime_sinca = ymd_hm(paste(FECHA..YYMMDD., hora_hh_mm), tz = "America/Santiago"),
      valor_num = as.numeric(gsub(",", ".", .data[[col_val]]))
    ) %>%
    filter(!is.na(valor_num)) %>%
    select(Datetime_sinca, !!sym(nombre_var) := valor_num)
}

df_temp <- limpiar_clima("Data/Raw/sinca/Tempdatos_250702_250805.csv", "Temp")
df_hr   <- limpiar_clima("Data/Raw/sinca/HRdatos_250702_250805.csv", "HR")
df_ws   <- limpiar_clima("Data/Raw/sinca/VelVientodatos_250702_250805.csv", "WS")
df_wd   <- limpiar_clima("Data/Raw/sinca/DirVientodatos_250702_250805.csv", "WD")

# Merge de todas las variables SINCA por timestamp
lista_merge <- list(sinca_pm25_clean, df_temp, df_hr, df_ws, df_wd) %>% compact()

sinca_clean <- reduce(lista_merge, full_join, by = "Datetime_sinca") %>%
  arrange(Datetime_sinca)

# Para cada dia de ruta se define la ventana horaria [inicio, fin] combinando
# los timestamps del movil y del central, y se extraen las horas SINCA que caen
# dentro de esa ventana.
files_movil <- list.files("Data/Processed/movil", pattern = "movil_.*\\.RData", full.names = TRUE)

sinca_temp_full <- map_df(files_movil, function(path_m) {
  
  fecha_id <- gsub(".*movil_(\\d{6})\\.RData", "\\1", path_m)
  path_a <- file.path("Data/Processed/central", paste0("central_", fecha_id, ".RData"))
  
  if (!file.exists(path_a)) return(NULL)
  
  env_m <- new.env(); load(path_m, envir = env_m)
  env_a <- new.env(); load(path_a, envir = env_a)
  
  df_m <- env_m[[ls(env_m)[1]]] 
  df_a <- env_a[[ls(env_a)[1]]] 
  
  # Solo mediciones en ruta (tipo == 2), no los tramos B1/D1/D2/B2
  if("tipo" %in% colnames(df_m)) df_m <- df_m %>% filter(tipo == 2)
  if("tipo" %in% colnames(df_a)) df_a <- df_a %>% filter(tipo == 2)
  
  if(nrow(df_m) == 0 | nrow(df_a) == 0) return(NULL)
  
  inicio_real <- min(min(df_m$Datetime, na.rm = TRUE), min(df_a$Datetime, na.rm = TRUE))
  fin_real    <- max(max(df_m$Datetime, na.rm = TRUE), max(df_a$Datetime, na.rm = TRUE))
  
  attr(inicio_real, "tzone") <- "America/Santiago"
  attr(fin_real, "tzone") <- "America/Santiago"
  
  # Redondeo a hora completa para que la ventana cuadre con el timestamp SINCA
  win_inicio <- floor_date(inicio_real, unit = "hour")
  win_fin    <- floor_date(fin_real, unit = "hour")
  
  datos_sinca_ventana <- sinca_clean %>%
    filter(Datetime_sinca >= win_inicio, Datetime_sinca <= win_fin)
  
  if(nrow(datos_sinca_ventana) == 0) return(NULL)
  
  datos_sinca_ventana %>%
    mutate(
      Fecha_ID = fecha_id,        
      Trip_Start_Real = inicio_real
    )
})

# Resumen diario: media aritmetica para magnitudes escalares; media circular
# (atan2 sobre componentes sin/cos) para direccion de viento
sinca <- sinca_temp_full %>%
  group_by(Fecha_ID) %>%
  summarise(
    date = as.Date(min(Trip_Start_Real), tz = "America/Santiago"),
    mean_sinca = mean(conc, na.rm = TRUE),
    mean_temp  = mean(Temp, na.rm = TRUE),
    mean_hr    = mean(HR, na.rm = TRUE),
    mean_ws    = mean(WS, na.rm = TRUE),
    mean_wd    = atan2(mean(sin(WD * pi / 180), na.rm=TRUE), mean(cos(WD * pi / 180), na.rm=TRUE)) * 180 / pi,
    .groups = "drop"
  ) %>%
  mutate(mean_wd = ifelse(mean_wd < 0, mean_wd + 360, mean_wd)) %>%
  select(date, mean_sinca, mean_temp, mean_hr, mean_ws, mean_wd)

sinca_hour <- sinca_temp_full %>%
  mutate(
    Datetime = Datetime_sinca,       
    Hour     = hour(Datetime_sinca), 
    SC_PM25  = conc                  
  ) %>%
  select(Datetime, Hour, SC_PM25, Temp, HR, WS, WD, Fecha_ID) 

save(sinca_hour, file="Data/Processed/Sinca/sinca_hour.RData")
save(sinca, file="Data/Processed/Sinca/sinca.RData")
