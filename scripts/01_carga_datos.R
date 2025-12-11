  # =============================================================================
  # SCRIPT 01: CARGA Y VALIDACIÓN INICIAL DE DATOS
  # Proyecto: Predicción de precios de vuelos según aerolínea y ruta
  # Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
  # Fecha: 2025-12-05
  # =============================================================================
  
  # ------------------------- CONFIGURACIÓN INICIAL -----------------------------
  source(here::here("config", "parametros.R"))
  source(here::here("functions", "carga_funciones.R"))
  
  mensaje_proceso("Iniciando carga de datos")
  
  # ------------------------ CARGAR DATOS PRINCIPALES ---------------------------
  archivo_raw <- file.path(dir_data_raw, ARCHIVO_PRINCIPAL)
  datos_raw   <- leer_excel_seguro(archivo_raw)
  
  # ------------------------ VALIDACIÓN INICIAL ---------------------------------
  if (!is.null(datos_raw)) {
    mostrar_dimensiones(datos_raw)                  # Dimensiones
    validar_objetivo(datos_raw, VARIABLE_OBJETIVO)  # Variable objetivo
    validar_claves(datos_raw, VARIABLES_CLAVE)      # Variables clave
  }
  # ------------------------ PRIMER VISTAZO -------------------------------------
  if (!is.null(datos_raw)) {
    skimr::skim(datos_raw)                 # resumen estadístico detallado
    janitor::tabyl(datos_raw$Airline)      # distribución de aerolíneas (ejemplo)
  }
  # ------------------------ GUARDAR INTERMEDIO ---------------------------------
  if (!is.null(datos_raw)) {
    saveRDS(datos_raw, file.path(dir_data_proc, "datos_raw.rds"))
    mensaje_exito("Datos crudos guardados en formato RDS para uso posterior")
  }
  # -----------------------------------------------------------------------------
