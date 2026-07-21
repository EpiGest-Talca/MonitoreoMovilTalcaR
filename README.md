# Monitoreo Móvil de PM2.5 en Talca — Pipeline de Análisis en R

Código y datos de la campaña de monitoreo móvil de material particulado fino (PM2.5) realizada en la zona urbana de Talca, Región del Maule, Chile (23 días de medición, julio–agosto de 2025).

Este repositorio acompaña el artículo:

> **[Gallardo et al. (año)]. [Título del artículo]. [Revista].** DOI: [pendiente]

Departamento de Salud Pública, Universidad de Talca.

## Descripción general

La campaña combinó un sensor móvil (DustTrak DT5203) montado en una ruta diaria por la ciudad, un sensor fijo de referencia (DT261395/DT261393) y un track GPS por día. Las mediciones móviles se corrigieron contra los tramos duplicados y contra la estación regulatoria SINCA La Florida, y luego se agregaron a grillas de 50, 100, 200 y 400 m para el análisis espacial (rásters de concentración, ratios móvil/central, autocorrelación espacial de Moran e indicadores por Unidad Vecinal).

## Estructura del repositorio

```
├── Data/
│   ├── Raw/                          # Datos crudos de la campaña, sin procesar
│   │   ├── movil/                    #   TXT diarios del sensor móvil DT5203 (23 días)
│   │   ├── central/                  #   TXT diarios del sensor fijo DT261395/DT261393 (23 días)
│   │   ├── gps/                      #   Tracks GPS diarios .gpx (22 días; 250707 excluido por lluvia)
│   │   ├── sinca/                    #   CSV/TXT horarios de la estación SINCA La Florida (PM2.5, T°, HR, viento)
│   │   ├── SHP_APC2023_R07/          #   Shapefiles: límite urbano censal y Unidades Vecinales 2024 (IDE Chile)
│   │   └── DPA_2023/                 #   Shapefiles División Político-Administrativa 2023 (IDE Chile)
│   │       ├── REGIONES/ · COMUNAS/ · PROVINCIAS/   # Capas usadas en los mapas de contexto en cascada
│   │       └── DOCUMENTACION/        #   Metadatos y metodología del DPA 2023
│   └── Processed/                    # Salidas intermedias del pipeline (.RData), generadas al ejecutar
│       ├── movil/                    #   Un .RData por día + stack_movil.RData
│       ├── central/                  #   Un .RData por día + stack_central.RData
│       ├── GPS/                      #   Un .RData por día + stack_GPS.RData
│       ├── Sinca/                    #   sinca.RData, sinca_hour.RData
│       ├── Maps/                     #   TalcaMap.RData, diccionarios UV, rásters vacíos
│       │   ├── Shapefiles_Mapas_Base/    # Límite urbano, estaciones, ruta GPS (para QGIS)
│       │   ├── Rasters_Finales/          # Stacks por resolución (PM2.5 y ratio) + GeoTIFF para QGIS
│       │   └── Shapefiles_QGIS/          # Hotspots LISA por resolución (para QGIS)
│       ├── raw.RData
│       └── med.RData / med.csv       #   Base final integrada (script 7)
├── Figs/                             # Figuras del paper — se generan al ejecutar
│   ├── Fig1_Talca_TwoMaps.tif                    # Fig. 1: estaciones + ruta móvil
│   ├── FigA_Urban_Talca.tif … FigE_Chile.tif     # Mapas de contexto en cascada (script 9)
│   ├── Mapa_Talca_UV_*.tif                       # Mapas de Unidades Vecinales (script 10)
│   ├── Fig2_EmptyRaster_{50,100,200,400}m.tif    # Fig. 2: grillas vacías por resolución
│   ├── Fig3_MapaBase_{50,100,200,400}m.tif       # Fig. 3: PM2.5 por resolución
│   ├── Fig4_RatioMap_{50,100,200,400}m.tif       # Fig. 4: ratio móvil/central por resolución
│   └── Fig5_LISA_Hotspots_{50,100,200,400}m.tif  # Fig. 5: clusters LISA por resolución
├── Plots/                            # Boxplots exploratorios (script 14)
├── Tables/                           # Tablas del paper en .docx
│   ├── Tabla1_SincaSite_Stats.docx
│   ├── Tabla2_Met_Stats.docx
│   ├── Tabla3_Resolution_Stats.docx
│   ├── Tabla4_Moran_Global.docx      # (pendiente de generar — ver nota abajo)
│   └── Tabla5_UnidadesVecinales_Stats.docx
├── MonitoreoMovilTalcaR.Rproj
└── *.R                                # Scripts del pipeline (ejecutar en orden numérico, 1 a 19)
```

## Pipeline (orden de ejecución)

| # | Script | Descripción |
|---|--------|-------------|
| 1 | `1-DatosMovil.R` | Lee los TXT crudos del sensor móvil. Cada archivo diario se segmenta manualmente en tramos: blanco inicial (B1), duplicados (D1, D2), blanco final (B2) y medición en ruta. Guarda un `.RData` por día. |
| 2 | `2-DatosCentral.R` | Misma lógica de parseo para el sensor fijo (central). |
| 3 | `3-DatosGps.R` | Lee los tracks GPS (.gpx, capa `track_points` con detección automática de capa alternativa) de cada día. |
| 4 | `4-ApilarDias.R` | Apila los `.RData` diarios en tres bases únicas: `gps` (stack_GPS), `sc` (stack_central) y `m` (stack_movil). |
| 5 | `5-MediaSinca.R` | Procesa los datos horarios SINCA y calcula promedios (PM2.5, temperatura, HR, viento) dentro de las ventanas temporales de cada día de ruta. |
| 6 | `6-MergeBases.R` | Cruza las tres bases apiladas por `Datetime`, separa los tramos por tipo e interpola espacialmente (lat/lon/ele) el tramo de medición en ruta. |
| 7 | `7-CorreccionDuplis.R` | Corrección de las mediciones: `sc_corr = pmsc × (mean_sinca / meansc)` y `mov_corr = pmmov × (mean_sinca / meansc) × corr_duplis`, usando los tramos duplicados y la referencia diaria SINCA. |
| 8 | `8-TalcaMaps.R` | Mapa base satelital de Talca, límite urbano y **Figura 1** (estaciones + ruta móvil). Exporta shapefiles para QGIS. |
| 9 | `9-MapsChile.R` | Serie de 5 mapas de contexto en cascada (urbano → comuna → provincia → región → país) usando las capas de `DPA_2023`. |
| 10 | `10-UnidadesVecinales.R` | Procesa las Unidades Vecinales (UV) de Talca (IDE Chile 2024): mapas numerados, leyendas y diccionarios CSV. |
| 11 | `11-GrillasTalca.R` | Construye las 4 grillas raster vacías (50 / 100 / 200 / 400 m) usadas como plantilla en todo el análisis. |
| 12 | `12-Rasters.R` | Agrega `mov_corr` a las 4 grillas por hora, día y campaña (celdas con ≥ 3 días). **Figura 3** y GeoTIFFs. |
| 13 | `13-Ratios.R` | Versión ratio del script 12: rasteriza `mov_corr / sc_corr` (>1 = celda más contaminada que el sitio central). **Figura 4**. |
| 14 | `14-TablaResumenSinca.R` | **Tabla 1**: descriptivos horarios SINCA (PM2.5, T°, HR, viento) + boxplots horarios. |
| 15 | `15-EstadisticasMeteo.R` | **Tabla 2**: PM2.5 y ratio agregados por categorías meteorológicas (agregación a celda de 200 m y luego a día). |
| 16 | `16-EstadisticasResolucion.R` | **Tabla 3**: estadísticas por celda para cada resolución (celdas con ≥ 3 días). |
| 17 | `17-Moran.R` | **Figura 5**: mapas LISA (Moran local) del ratio por resolución; exporta shapefile de hotspots. |
| 18 | `18-TablaMoran.R` | **Tabla 4**: índice global de Moran (PM2.5 y ratio) por resolución, contigüidad reina. |
| 19 | `19-MetricasUVecinal.R` | **Tabla 5**: mediana, media y Q1–Q3 de PM2.5 y ratio por Unidad Vecinal (UV con ≥ 3 días). |

Cada script asume que el directorio de trabajo es la raíz del repositorio (rutas relativas `Data/`, `Figs/`, `Plots/`, `Tables/`). Abrir `MonitoreoMovilTalcaR.Rproj` en RStudio/Positron sitúa el working directory correctamente de forma automática.

## Requisitos

R ≥ 4.5 y los siguientes paquetes (versiones exactas registradas en `renv.lock`):

```r
install.packages(c(
  "dplyr", "tidyr", "purrr", "lubridate", "zoo",
  "sf", "terra", "spdep",
  "OpenStreetMap", "prettymapr", "chilemapas",
  "ggplot2", "flextable", "officer", "conflicted"
))
```

Nota: `OpenStreetMap` requiere Java (JRE) instalado.

### Entorno reproducible (renv)

El proyecto usa [`renv`](https://rstudio.github.io/renv/) para fijar versiones exactas de todos los paquetes (R 4.5.3). Para restaurar el entorno exacto usado en el análisis:

```r
install.packages("renv")
renv::restore()
```

Esto lee `renv.lock` e instala cada paquete en la versión exacta registrada, sin necesitar los binarios de `renv/library/` (que no se versionan).

## Notas sobre los datos

- Los límites de segmentación de cada archivo diario (parámetros `b`–`i` en los scripts 1 y 2) se determinaron manualmente a partir de la bitácora de campo de cada día; los comentarios en el código documentan filas excluidas y observaciones de terreno.
- Los días 5–7 de julio fueron descartados por lluvia durante la ruta (el TXT y GPX de esos días puede no tener procesamiento asociado).
- Datos SINCA: estación La Florida, Sistema de Información Nacional de Calidad del Aire (sinca.mma.gob.cl).
- Shapefiles de límite urbano censal y Unidades Vecinales 2024, y de la División Político-Administrativa 2023 (regiones/comunas/provincias): IDE Chile.

## Pendientes antes de publicar

- `Tables/Tabla4_Moran_Global.docx` no está generada todavía — correr `18-TablaMoran.R` para producirla.
- Hay un archivo `17- grafico para paper moran.R` dentro de `Data/Raw/SHP_APC2023_R07/`; no debería vivir ahí — moverlo a la raíz del repo (si es una versión válida del script 17) o eliminarlo si es una copia obsoleta.

## Licencia

Código bajo licencia MIT (ver `LICENSE`). Los datos de la campaña se publican con fines de reproducibilidad del artículo; citar el paper al reutilizarlos.

## Contacto

Martín Soto Cabezas — Departamento de Salud Pública, Universidad de Talca.
