# =============================================================================
# CONFIGURACIÓN GLOBAL DEL PROYECTO
# Proyecto: Predicción de precios de vuelos según aerolínea y ruta
# Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
# =============================================================================

# Limpiar entorno
rm(list = ls())

# ----------------------------- OPCIONES GLOBALES -----------------------------
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica en impresiones

# ----------------------------- LIBRERÍAS DEL PROYECTO ------------------------
library(readr)       # leer CSV, TSV o archivos de ancho fijo
library(readxl)      # leer excels
library(here)        # rutas reproducibles (relativas)
  
library(tidyverse)   # manipulación y visualización ordenada/"limpia" de datos
library(dplyr)       # manipulación eficiente y sencilla de datos 
library(tidyr)       # organizaión y transformación de datos
library(janitor)     # limpiar y examinar datos
library(broom)       # ordenar resultados de modelos en tablas

library(lubridate)   # fechas y horas

library(ggplot2)     # gráficos complejos
library(scales)      # escala y formato de visualización de datos
library(ggrepel)     # personalizar las etiquetas de gráficos
library(grid)        # dibujar cuadrículas en gráficos existentes
library(gridExtra)   # organizar y combinar múltiples gráficos, anoaciones

library(stargazer)   # tablas de resultados estadísticos y regresión
library(wooldridge)  # acceder a los datasets de econometría
library(caret)       # modelos predictivos, regesión
library(skimr)       # resumen estadístico detallado
library(margins)     # calcular efectos marginales en modelos de regresión
library(sandwich)    # estimadores robustos de la matriz de covarianza
library(lmtest)      # pruebas diagnósticas para modelos de regresión lineal
library(pROC)        # curva ROC
library(car)
# ----------------------------- DIRECTORIO DEL PROYECTO -----------------------
# Anclar el proyecto en la carpeta raíz
here::i_am("config/parametros.R")
proyecto_dir <- here::here()


# ----------------------------- RUTAS PRINCIPALES -----------------------------
# DATA
dir_data       <- file.path(proyecto_dir, "data")
dir_data_raw   <- file.path(dir_data, "raw")        # datos originales (Kaggle)
dir_data_proc  <- file.path(dir_data, "processed")  # datos limpios/procesados
dir_data_ext   <- file.path(dir_data, "external")   # tablas auxiliares, catálogos

# SCRIPTS Y FUNCIONES
dir_scripts    <- file.path(proyecto_dir, "scripts")
dir_functions  <- file.path(proyecto_dir, "functions")
dir_config     <- file.path(proyecto_dir, "config")

# OUTPUTS
dir_outputs          <- file.path(proyecto_dir, "outputs")
dir_outputs_figures  <- file.path(dir_outputs, "figures")
dir_outputs_tables   <- file.path(dir_outputs, "tables")
dir_outputs_reports  <- file.path(dir_outputs, "reports")

# ----------------------------- CREAR DIRECTORIOS SI NO EXISTEN -----------------------------
dirs_crear <- c(
  dir_data, dir_data_raw, dir_data_proc, dir_data_ext,
  dir_scripts, dir_functions, dir_config,
  dir_outputs, dir_outputs_figures, dir_outputs_tables, dir_outputs_reports
)

for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# ----------------------------- PARÁMETROS DEL PROYECTO -----------------------

NOMBRE_PROYECTO      <- "Predicción de precios de vuelos"
TEMA_PROYECTO        <- "Transporte aéreo - tarifas según aerolínea y ruta"
FUENTE_DATOS         <- "Kaggle - Flight Fare Prediction MH"
ARCHIVO_PRINCIPAL    <- "Data_Train.xlsx"   # en data/raw
VARIABLE_OBJETIVO    <- "Price"

# Campos clave del dataset (para usar en filtros / checks)
VARIABLES_CLAVE <- list(
  aerolinea   = "Airline",
  origen      = "Source",
  destino     = "Destination",
  ruta        = "Route",
  duracion    = "Duration",
  escalas     = "Total_Stops",
  fecha_viaje = "Date_of_Journey"
)

# Fechas importantes del TP
FECHA_ENTREGA_TP     <- as.Date("2025-12-05")
FECHA_ULTIMA_EDICION <- Sys.Date()

# ----------------------------- FUNCIONES DE MENSAJES -------------------------
mensaje_exito <- function(texto) {
  cat("✅", texto, "\n")
}

mensaje_proceso <- function(texto) {
  cat("🔄", texto, "...\n")
}

mensaje_alerta <- function(texto) {
  cat("⚠️", texto, "\n")
}

mensaje_info <- function(texto) {
  cat("ℹ️", texto, "\n")
}

# ----------------------------- RESUMEN RÁPIDO --------------------------------
cat("\n=============================================\n")
cat("CONFIGURACIÓN DEL PROYECTO CARGADA\n")
cat("Proyecto:", NOMBRE_PROYECTO, "\n")
cat("Tema:    ", TEMA_PROYECTO, "\n")
cat("Datos:   ", FUENTE_DATOS, "\n")
cat("Entrega: ", as.character(FECHA_ENTREGA_TP), "\n")
cat("Hoy es:  ", as.character(FECHA_ULTIMA_EDICION), "\n")
cat("=============================================\n\n")

mensaje_exito("Configuración cargada correctamente para el proyecto de vuelos")
# -----------------------------------------------------------------------------