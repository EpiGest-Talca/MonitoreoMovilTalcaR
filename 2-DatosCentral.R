# Lectura de archivos crudos del sensor del DT261395/DT261393.
# Misma logica de parseo que el script de moviles: cada TXT se segmenta en los
# tramos B1 / D1 / D2 / B2 y el resto queda como medicion (tipo = 2).

rm(list = ls())
gc()

abrir <- function(a, b, c, d, e, f, g, h, i) {
  data <- tryCatch(
    read.delim(a, header = TRUE, row.names = NULL,
               fill = TRUE, check.names = FALSE, comment.char = ""),
    error = function(e1) {
      tmp <- read.delim(a, header = FALSE, row.names = NULL,
                        fill = TRUE, check.names = FALSE, comment.char = "")
      names(tmp) <- paste0("V", seq_len(ncol(tmp)))
      tmp
    }
  )
  
  data$var1 <- data[[1]]
  data[[1]] <- NULL
  
  data$var <- substr(data$var1, 1, 3)
  data$tipo <- 2
  data[b:c, "tipo"] <- 0  # B1
  data[d:e, "tipo"] <- 1  # D1
  data[f:g, "tipo"] <- 3  # D2
  data[h:i, "tipo"] <- 4  # B2
  
  borrar <- function(pat, y) y[!grepl(pat, y$var), , drop = FALSE]
  patrones <- c(",Av", ",Ca", ",Da", ",Ma", ",Mi", ",Ti", ",Un",
                "Cal", "Dat", "dd-", "Log", "Mod", "Not", "Num", "Ser", "Sta", "Tes", "Dur")
  
  df <- data
  for (p in patrones) df <- borrar(p, df)
  
  idx <- grepl("^\\d{2}-\\d{2}-\\d{4},\\d{2}:\\d{2}:\\d{2},", df$var1)
  df <- df[idx, , drop = FALSE]
  
  df$var1 <- gsub("(\\d),(\\d+)$", "\\1.\\2", df$var1)
  
  parts <- strsplit(df$var1, ",", fixed = TRUE)
  mat <- do.call(rbind, parts)
  
  df$Datetime <- as.POSIXct(strptime(paste(mat[, 1], mat[, 2]), "%d-%m-%Y %H:%M:%S"))
  df$pm <- suppressWarnings(as.numeric(gsub(",", ".", mat[, 3])))
  
  df$var <- NULL
  df$var1 <- NULL
  rownames(df) <- NULL
  
  return(df)
}

if (!dir.exists("Data/Processed/central")) {
  dir.create("Data/Processed/central")
}

# ---- Dia 1 ----

a1 <- abrir(
  a = "Data/Raw/central/DT261395 250702 comma.txt",
  b = 25, c = 100, #B1
  d = 153, e = 359, #D1
  f = 19690, g = 19910, #D2
  h = 19914, i = 20019 #B2
)[-(72:80), ] # Filas excluidas: OBS excel lineas 129-137

save(a1, file = "Data/Processed/central/central_250702.RData")

# ---- Dia 2 ----

a2 <- abrir(
  a = "Data/Raw/central/DT261395 250703 comma.txt",
  b = 25, c = 92, #B1
  d = 111, e = 311, #D1
  f = 25352, g = 25554, #D2
  h = 25560, i = 25646 #B2
)

save(a2, file = "Data/Processed/central/central_250703.RData")

# ---- Dia 3 ----

a3 <- abrir(
  a = "Data/Raw/central/DT261395 250704 comma.txt",
  b = 25, c = 93, #B1
  d = 113, e = 306, #D1
  f = 26345, g = 26563, #D2
  h = 26571, i = 26662 #B2
)

save(a3, file = "Data/Processed/central/central_250704.RData")

# ---- Dia 4 (descartado por lluvia) ----
# a4 <- abrir(
#   a = "Data/Raw/central/DT261395 250707 comma.txt",
#   b = 30, c = 91, #B1
#   d = 120, e = 301, #D1
#   f = 12119, g = 12312, #D2
#   h = 12341, i = 12410 #B2
# )
# save(a4, file = "Data/Processed/central/central_250707.RData")

# ---- Dia 5 ----

a5 <- abrir(
  a = "Data/Raw/central/DT261395 250708 comma.txt",
  b = 25, c = 98, #B1
  d = 118, e = 318, #D1
  f = 26122, g = 26338, #D2
  h = 26343, i = 26431 #B2
)[-c(25510:25540,25569:25621), ] # Filas excluidas: OBS excel 25595-25625; 22654-22706

save(a5, file = "Data/Processed/central/central_250708.RData")

# ---- Dia 6 ----

a6 <- abrir(
  a = "Data/Raw/central/DT261395 250709 coma.txt",
  b = 25, c = 99,  # B1
  d = 118, e = 316,  # D1
  f = 28553, g = 28755,  # D2
  h = 28763, i = 28850   # B2
)

save(a6, file = "Data/Processed/central/central_250709.RData")

# ---- Dia 7 ----

a7 <- abrir(
  a = "Data/Raw/central/DT261393 250710 coma.txt",
  b = 25, c = 98, #B1
  d = 118, e = 321, #D1
  f = 26180, g = 26366, #D2
  h = 26391, i = 26487 #B2
)

save(a7, file = "Data/Processed/central/central_250710.RData")

# ---- Dia 8 ----

a8 <- abrir(
  a = "Data/Raw/central/DT261393 250711 comma.txt",
  b = 25, c = 94, #B1
  d = 114, e = 342, #D1
  f = 25903, g = 26106, #D2
  h = 26114, i = 26207 #B2
)

save(a8, file = "Data/Processed/central/central_250711.RData")

# ---- Dia 9 ----

a9 <- abrir(
  a = "Data/Raw/central/DT261395 250712 comma.txt",
  b = 25, c = 96, #B1
  d = 116, e = 309, #D1
  f = 23204, g = 23409, #D2
  h = 23417, i = 23506 #B2
)

save(a9, file = "Data/Processed/central/central_250712.RData")

# ---- Dia 10 ----

a10 <- abrir(
  a = "Data/Raw/central/DT261395 250713 comma.txt",
  b = 25, c = 100, #B1
  d = 195, e = 391, #D1
  f = 27304, g = 27513, #D2
  h = 27517, i = 27636 #B2
)[-(72:118), ] # Filas excluidas: OBS excel lineas 129-175

save(a10, file = "Data/Processed/central/central_250713.RData")

# ---- Dia 11 ----

a11 <- abrir(
  a = "Data/Raw/central/DT261395 250714 comma.txt",
  b = 25, c = 90, #B1
  d = 110, e = 302, #D1
  f = 27966, g = 28169, #D2
  h = 28177, i = 28258 #B2
)

save(a11, file = "Data/Processed/central/central_250714.RData")

# ---- Dia 12 ----

a12 <- abrir(
  a = "Data/Raw/central/DT261395 250715 comma.txt",
  b = 25, c = 97, #B1
  d = 117, e = 313, #D1
  f = 29045, g = 29249, #D2
  h = 29257, i = 29344 #B2
)

save(a12, file = "Data/Processed/central/central_250715.RData")

# ---- Dia 13 ----

a13 <- abrir(
  a = "Data/Raw/central/DT261395 250716 comma.txt",
  b = 25, c = 90, #B1
  d = 110, e = 299, #D1
  f = 28641, g = 28844, #D2
  h = 28852, i = 28937 #B2
)

save(a13, file = "Data/Processed/central/central_250716.RData")

# ---- Dia 14 ----

a14 <- abrir(
  a = "Data/Raw/central/DT261395 250717 comma.txt",
  b = 25, c = 95, #B1
  d = 115, e = 309, #D1
  f = 31065, g = 31288, #D2
  h = 31272, i = 31383 #B2
)[-(253:399), ] # Filas excluidas: OBS excel

save(a14, file = "Data/Processed/central/central_250717.RData")

# ---- Dia 15 ----

a15 <- abrir(
  a = "Data/Raw/central/DT261395 250718 comma.txt",
  b = 25, c = 104, #B1
  d = 126, e = 324, #D1
  f = 26641, g = 26840, #D2
  h = 26848, i = 26933 #B2
)

save(a15, file = "Data/Processed/central/central_250718.RData")

# ---- Dia 16 ----

a16 <- abrir(
  a = "Data/Raw/central/DT261395 250719 comma.txt",
  b = 25, c = 153, #B1
  d = 173, e = 369, #D1
  f = 24204, g = 24411, #D2
  h = 24419, i = 24512 #B2
)

save(a16, file = "Data/Processed/central/central_250719.RData")

# ---- Dia 17 ----

a17 <- abrir(
  a = "Data/Raw/central/DT261395 250720 comma.txt",
  b = 25, c = 123, #B1
  d = 143, e = 343, #D1
  f = 23755, g = 23957, #D2
  h = 23965, i = 24052 #B2
)

save(a17, file = "Data/Processed/central/central_250720.RData")

# ---- Dia 18 ----

a18 <- abrir(
  a = "Data/Raw/central/DT261395 250721 comma.txt",
  b = 25, c = 92, #B1
  d = 112, e = 309, #D1
  f = 27967, g = 28164, #D2
  h = 28172, i = 28254 #B2
)

save(a18, file = "Data/Processed/central/central_250721.RData")

# ---- Dia 19 ----

a19 <- abrir(
  a = "Data/Raw/central/DT261395 250722 comma.txt",
  b = 25, c = 121, #B1
  d = 141, e = 347, #D1
  f = 24480, g = 24685, #D2
  h = 24693, i = 24778 #B2
)

save(a19, file = "Data/Processed/central/central_250722.RData")

# ---- Dia 20 ----

a20 <- abrir(
  a = "Data/Raw/central/DT261395 250723 comma.txt",
  b = 25, c = 92, #B1
  d = 146, e = 339, #D1
  f = 27973, g = 28181, #D2
  h = 28185, i = 28279 #B2
)[-(64:69), ] # Filas excluidas: OBS excel

save(a20, file = "Data/Processed/central/central_250723.RData")

# ---- Dia 21 ----

a21 <- abrir(
  a = "Data/Raw/central/DT261395 250725 comma.txt",
  b = 25, c = 95, #B1
  d = 115, e = 317, #D1
  f = 26935, g = 27143, #D2
  h = 27150, i = 27234 #B2
)

save(a21, file = "Data/Processed/central/central_250725.RData")

# ---- Dia 22 ----

a22 <- abrir(
  a = "Data/Raw/central/DT261395 250729 comma.txt",
  b = 25, c = 92, #B1
  d = 112, e = 305, #D1
  f = 11906, g = 12104, #D2
  h = 12112, i = 12194 #B2
)

save(a22, file = "Data/Processed/central/central_250729.RData")

# ---- Dia 23 ----
a23 <- abrir(
  a = "Data/Raw/central/DT261395 250805 comma.txt",
  b = 25, c = 99, #B1
  d = 119, e = 325, #D1
  f = 6165, g = 6369, #D2
  h = 6377, i = 6461 #B2
)

save(a23, file = "Data/Processed/central/central_250805.RData")
