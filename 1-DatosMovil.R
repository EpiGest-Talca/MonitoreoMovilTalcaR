# Lectura de archivos crudos del sensor MOVIL (DT5203) para Talca.
#
# Cada archivo TXT contiene una ruta diaria dividida en 4 tramos:
#   B1 (blanco inicial)  -> tipo = 0
#   D1 (duplicado 1)     -> tipo = 1
#   D2 (duplicado 2)     -> tipo = 3
#   B2 (blanco final)    -> tipo = 4
#   Medicion en ruta     -> tipo = 2 
#
# Los limites (b,c,d,e,f,g,h,i) se determinan manualmente a partir del log
# de cada dia..

rm(list = ls())
gc()

if (!dir.exists("Data/Processed")) {
  dir.create("Data/Processed")
}

if (!dir.exists("Data/Processed/movil")) {
  dir.create("Data/Processed/movil")
}


# Lector para archivos con separador coma
abrir <- function(a, b, c, d, e, f, g, h, i) {
  # Lectura robusta: si falla el header, se lee sin header y se renombran columnas
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
  
  # Asignar etiqueta de tramo segun los rangos de fila
  data$var <- substr(data$var1, 1, 3)
  data$tipo <- 2
  data[b:c, "tipo"] <- 0  # B1
  data[d:e, "tipo"] <- 1  # D1
  data[f:g, "tipo"] <- 3  # D2
  data[h:i, "tipo"] <- 4  # B2
  
  # Eliminar filas de metadatos que el equipo escribe en el TXT
  borrar <- function(pat, y) y[!grepl(pat, y$var), , drop = FALSE]
  patrones <- c(",Av", ",Ca", ",Da", ",Ma", ",Mi", ",Ti", ",Un",
                "Cal", "Dat", "dd-", "Log", "Mod", "Not", "Num", "Ser", "Sta", "Tes", "Dur")
  
  df <- data
  for (p in patrones) df <- borrar(p, df)
  
  # Conservar solo filas con formato dd-mm-yyyy,HH:MM:SS,
  idx <- grepl("^\\d{2}-\\d{2}-\\d{4},\\d{2}:\\d{2}:\\d{2},", df$var1)
  df <- df[idx, , drop = FALSE]
  
  # Normalizar decimal con coma al final (ej: "12,34" -> "12.34")
  df$var1 <- gsub("(\\d),(\\d+)$", "\\1.\\2", df$var1)
  
  # Separar en Fecha / Hora / Valor
  parts <- strsplit(df$var1, ",", fixed = TRUE)
  mat <- do.call(rbind, parts)
  
  df$Datetime <- as.POSIXct(strptime(paste(mat[, 1], mat[, 2]), "%d-%m-%Y %H:%M:%S"))
  df$pm <- suppressWarnings(as.numeric(gsub(",", ".", mat[, 3])))
  
  df$var <- NULL
  df$var1 <- NULL
  rownames(df) <- NULL
  
  return(df)
}

# Variante para archivos con separador tabulacion (detecta el separador solo)
AbrirTabulado <- function(a, b, c, d, e, f, g, h, i) {
  data <- tryCatch(
    read.delim(a, header = TRUE, row.names = NULL, sep = "\n", 
               fill = TRUE, check.names = FALSE, comment.char = ""),
    error = function(e1) {
      tmp <- read.delim(a, header = FALSE, row.names = NULL, sep = "\n", 
                        fill = TRUE, check.names = FALSE, comment.char = "")
      names(tmp) <- paste0("V", seq_len(ncol(tmp)))
      tmp
    }
  )
  
  data$var1 <- data[[1]]
  data[[1]] <- NULL
  
  data$var <- substr(data$var1, 1, 3)
  data$tipo <- 2
  
  # min() protege contra indices fuera de rango cuando el archivo es mas corto
  nr <- nrow(data)
  if(b <= nr) data[b:min(c, nr), "tipo"] <- 0 
  if(d <= nr) data[d:min(e, nr), "tipo"] <- 1 
  if(f <= nr) data[f:min(g, nr), "tipo"] <- 3 
  if(h <= nr) data[h:min(i, nr), "tipo"] <- 4 
  
  borrar <- function(pat, y) y[!grepl(pat, y$var), , drop = FALSE]
  patrones <- c(",Av", ",Ca", ",Da", ",Ma", ",Mi", ",Ti", ",Un",
                "Cal", "Dat", "dd-", "Log", "Mod", "Not", "Num", "Ser", "Sta", "Tes", "Dur")
  
  df <- data
  for (p in patrones) df <- borrar(p, df)
  
  # El "." en el regex acepta coma o tabulacion como separador de fecha/hora
  idx <- grepl("^\\d{2}-\\d{2}-\\d{4}.\\d{2}:\\d{2}:\\d{2}", df$var1)
  df <- df[idx, , drop = FALSE]
  
  # Deteccion automatica de separador
  separador <- if (any(grepl("\t", df$var1))) "\t" else ","
  parts <- strsplit(df$var1, separador, fixed = TRUE)
  mat <- do.call(rbind, parts)
  
  df$Datetime <- as.POSIXct(strptime(paste(mat[, 1], mat[, 2]), "%d-%m-%Y %H:%M:%S"))

  valores_limpios <- trimws(mat[, 3]) 
  df$pm <- suppressWarnings(as.numeric(gsub(",", ".", valores_limpios)))
  
  df$var <- NULL
  df$var1 <- NULL
  rownames(df) <- NULL
  
  return(df)
}


# ---- Dia 1 ----
# Ruta incompleta; se complementa con datos del 29/7
d1 <- abrir(
  a = "Data/Raw/movil/DT5203 250702 comma.txt",
  b = 25, c = 98, #B1
  d = 148, e = 354, #D1
  f = 15304, g = 15529, #D2
  h = 15530, i = 15688 #B2
)[-c((70:75),(14560:14905)), ]
save(d1, file = "Data/Processed/movil/movil_250702.RData")

# ---- Dia 2 ----
# Ruta incompleta; se complementa con 5/8
d2 <- abrir(
  a = "Data/Raw/movil/DT5203 250703 comma.txt",
  b = 25, c = 91, #B1
  d = 110, e = 310, #D1
  f = 21724, g = 21933, #D2
  h = 21934, i = 22047 #B2
)[-(254:402), ]
save(d2, file = "Data/Processed/movil/movil_250703.RData")

# ---- Dia 3 ----
d3 <- abrir(
  a = "Data/Raw/movil/DT5203 250704 comma.txt",
  b = 25, c = 91, #B1
  d = 111, e = 305, #D1
  f = 22770, g = 22972, #D2
  h = 22973, i = 23112 #B2
)[-(22087:22529),]

save(d3, file = "Data/Processed/movil/movil_250704.RData")

# ---- Dia 4 (descartado por lluvia) ----
# d4 <- abrir(
#   a = "Data/Raw/movil/DT5203 250707 comma.txt",
#   b = 30, c = 92, #B1
#   d = 121, e = 304, #D1
#   f = 9512, g = 9706, #D2
#   h = 9735, i = 9804 #B2
# )
# save(d4, file = "Data/Processed/movil/movil_250707.RData")

# ---- Dia 5 ----
# Dias 5 al 7 de julio descartados por lluvia en la ruta
#eliminamos la obs 7427 ya que hay 2 mediciones en el mismo seg 21:05:11

d5 <- abrir(
  a = "Data/Raw/movil/DT5203 250708 comma.txt",
  b = 25, c = 100,
  d = 120, e = 318,
  f = 22687, g = 22909,
  h = 22910, i = 23027
)

d5 <- d5[-c(22245:22398, 7427), ]
save(d5, file = "Data/Processed/movil/movil_250708.RData")

# ---- Dia 6 ----
d6 <- abrir(
  a = "Data/Raw/movil/DT5203 250709 coma.txt",
  b = 25, c = 99,  # B1
  d = 119, e = 318,  # D1
  f = 22605, g = 22814,  # D2
  h = 22815, i = 22915   # B2
)

save(d6, file = "Data/Processed/movil/movil_250709.RData")

# ---- Dia 7 ----
d7 <- abrir(
  a = "Data/Raw/movil/DT5203 250710 coma.txt",
  b = 25, c = 100, #B1
  d = 120, e = 323, #D1
  f = 21774, g = 22005, #D2
  h = 21989, i = 22106 #B2
)

save(d7, file = "Data/Processed/movil/movil_250710.RData")

# ---- Dia 8 ----
d8 <- abrir(
  a = "Data/Raw/movil/DT5203 250711 comma.txt",
  b = 25, c = 98, #B1
  d = 118, e = 344, #D1
  f = 22176, g = 22363, #D2
  h = 22388, i = 22514 #B2
)

save(d8, file = "Data/Processed/movil/movil_250711.RData")

# ---- Dia 9 ----
d9 <- abrir(
  a = "Data/Raw/movil/DT5203 250712 comma.txt",
  b = 25, c = 97, #B1
  d = 117, e = 310, #D1
  f = 21640, g = 21868, #D2
  h = 21852, i = 21962 #B2
)

save(d9, file = "Data/Processed/movil/movil_250712.RData")

# ---- Dia 10 ----
d10 <- abrir(
  a = "Data/Raw/movil/DT5203 250713 comma.txt",
  b = 25, c = 97, #B1
  d = 186, e = 387, #D1
  f = 23554, g = 23768, #D2
  h = 23769, i = 23921 #B2
)[-(69:113), ] # Filas excluidas: OBS excel lineas 126-170

save(d10, file = "Data/Processed/movil/movil_250713.RData")

# ---- Dia 11 ----
d11 <- abrir(
  a = "Data/Raw/movil/DT5203 250714 comma.txt",
  b = 5, c = 91, #B1
  d = 111, e = 303, #D1
  f = 23406, g = 23616, #D2
  h = 23617, i = 23719 #B2
)

save(d11, file = "Data/Processed/movil/movil_250714.RData")

# ---- Dia 12 ----
d12 <- abrir(
  a = "Data/Raw/movil/DT5203 250715 comma.txt",
  b = 25, c = 98, #B1
  d = 118, e = 314, #D1
  f = 24174, g = 24386, #D2
  h = 24366, i = 24483 #B2
)

save(d12, file = "Data/Processed/movil/movil_250715.RData")

# ---- Dia 13 ----
d13 <- abrir(
  a = "Data/Raw/movil/DT5203 250716 comma.txt",
  b = 25, c = 92, #B1
  d = 112, e = 302, #D1
  f = 22497, g = 22704, #D2
  h = 22705, i = 22818 #B2
)

save(d13, file = "Data/Processed/movil/movil_250716.RData")

# ---- Dia 14 ----
d14 <- abrir(
  a = "Data/Raw/movil/DT5203 250717 comma.txt",
  b = 25, c = 97, #B1
  d = 117, e = 313, #D1
  f = 24304, g = 24540, #D2
  h = 24516, i = 24634 #B2
)

save(d14, file = "Data/Processed/movil/movil_250717.RData")

# ---- Dia 15 ----
d15 <- abrir(
  a = "Data/Raw/movil/DT5203 250718 comma.txt",
  b = 25, c = 105, #B1
  d = 125, e = 325, #D1
  f = 21468, g = 21674, #D2
  h = 21675, i = 21785 #B2
)

save(d15, file = "Data/Processed/movil/movil_250718.RData")

# ---- Dia 16 ----
d16 <- abrir(
  a = "Data/Raw/movil/DT5203 250719 comma.txt",
  b = 25, c = 154, #B1
  d = 174, e = 370, #D1
  f = 19742, g = 19953, #D2
  h = 19954, i = 20064 #B2
)

save(d16, file = "Data/Processed/movil/movil_250719.RData")

# ---- Dia 17 ----
d17 <- abrir(
  a = "Data/Raw/movil/DT5203 250720 comma.txt",
  b = 25, c = 112, #B1
  d = 132, e = 331, #D1
  f = 19967, g = 20195, #D2
  h = 20179, i = 20291 #B2
)

save(d17, file = "Data/Processed/movil/movil_250720.RData")

# ---- Dia 18 ----
d18 <- abrir(
  a = "Data/Raw/movil/DT5203 250721 comma.txt",
  b = 25, c = 94, #B1
  d = 114, e = 312, #D1
  f = 23124, g = 23347, #D2
  h = 23331, i = 23438 #B2
)

save(d18, file = "Data/Processed/movil/movil_250721.RData")

# ---- Dia 19 ----
d19 <- abrir(
  a = "Data/Raw/movil/DT5203 250722 comma.txt",
  b = 25, c = 122, #B1
  d = 142, e = 349, #D1
  f = 19530, g = 19743, #D2
  h = 19744, i = 19863 #B2
)

save(d19, file = "Data/Processed/movil/movil_250722.RData")

# ---- Dia 20 ----
# Este archivo viene tabulado, no con coma
d20 <- AbrirTabulado(
  a = "Data/Raw/movil/DT5203 250723 comma.txt",
  b = 25, c = 90, #B1
  d = 140, e = 337, #D1
  f = 22050, g = 22277, #D2
  h = 22278, i = 22378 #B2
)[-(62:67), ] # Filas excluidas: OBS excel

save(d20, file = "Data/Processed/movil/movil_250723.RData")

# ---- Dia 21 ----
# 24/7 descartado por lluvia
d21 <- abrir(
  a = "Data/Raw/movil/DT5203 250725 comma.txt",
  b = 25, c = 96, #B1
  d = 116, e = 318, #D1
  f = 19733, g = 19948, #D2
  h = 19949, i = 20057 #B2
)

save(d21, file = "Data/Processed/movil/movil_250725.RData")

# ---- Dia 22 ----
d22 <- abrir(
  a = "Data/Raw/movil/DT5203 250729 comma.txt",
  b = 25, c = 90, #B1
  d = 110, e = 303, #D1
  f = 9269, g = 9475, #D2
  h = 9476, i = 9565 #B2
)

save(d22, file = "Data/Processed/movil/movil_250729.RData")

# ---- Dia 23 ----
d23<- abrir( 
  a = "Data/Raw/movil/DT5203 250805 comma.txt",
  b = 25, c = 98, #B1
  d = 118, e = 327, #D1
  f = 4143, g = 4361, #D2
  h = 4369, i = 4456 #B2
)

save(d23, file = "Data/Processed/movil/movil_250805.RData")