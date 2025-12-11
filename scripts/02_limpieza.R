# =============================================================================
# SCRIPT 02: LIMPIEZA Y PREPARACIÓN DE DATOS
# Proyecto: Predicción de precios de vuelos según aerolínea y ruta
# Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
# Fecha: 2025-12-05
# =============================================================================

# ------------------------- CONFIGURACIÓN INICIAL -----------------------------
source(here::here("config", "parametros.R"))
source(here::here("functions", "limpieza_funciones.R"))

# ----------------------- CARGAR DATOS INTERMEDIOS ----------------------------
mensaje_proceso("Cargando datos intermedios desde RDS")
datos_raw <- readRDS(file.path(dir_data_proc, "datos_raw.rds"))
mensaje_exito("Datos intermedios cargados correctamente")

# ------------------------- LIMPIEZA DE NOMBRES -------------------------------
datos_clean <- limpiar_nombres(datos_raw)
mensaje_exito("Nombres de columnas estandarizados")

# ------------------------ LIMPIEZA DE FECHAS ---------------------------------
datos_clean$date_of_journey <- limpiar_fecha(datos_clean$date_of_journey)
mensaje_exito("Variable date_of_journey convertida a formato Date")

# ------------------------ LIMPIEZA DE DURACIÓN -------------------------------
datos_clean$duration_min <- limpiar_duracion(datos_clean$duration)
mensaje_exito("Variable duration convertida a minutos")

# ------------------------ LIMPIEZA DE ESCALAS --------------------------------
datos_clean$total_stops_num <- limpiar_escalas(datos_clean$total_stops)
mensaje_exito("Variable total_stops convertida a número entero")

# ------------------------ CHEQUEO DE NA --------------------------------------
mensaje_proceso("Chequeando valores faltantes")
print(resumen_na(datos_clean))

# ------------------------ DETECCIÓN DE OUTLIERS ------------------------------
mensaje_proceso("Chequeando outliers en la variable price")
print(detectar_atipicos(datos_clean, "price"))

# ------------------------ GUARDAR INTERMEDIO ---------------------------------
saveRDS(datos_clean, file.path(dir_data_proc, "datos_clean.rds"))
mensaje_exito("Datos limpios guardados en formato RDS para uso posterior")
# -----------------------------------------------------------------------------
