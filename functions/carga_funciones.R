# =============================================================================
# FUNCIONES PARA CARGAR DATOS
# Y VALIDACIONES INICIALES
# =============================================================================

# --------------------------- CARGAR EXCEL SEGURO -----------------------------
# Con validación de existencia del archivo y mensajes
leer_excel_seguro <- function(path) {
  mensaje_proceso(paste("Intentando leer archivo:", path))
  if (!file.exists(path)) {
    mensaje_alerta(paste("No se encontró el archivo:", path))
    return(NULL)
  }
  datos <- readxl::read_excel(path)
  mensaje_exito("Archivo cargado correctamente")
  return(datos)
}
# ------------------------ VALIDAR VARIABLE OBJETIVO --------------------------
validar_objetivo <- function(df, var_objetivo) {
  if (!(var_objetivo %in% names(df))) {
    mensaje_alerta("La variable objetivo no está en el dataset")
  } else {
    mensaje_exito(paste("Variable objetivo encontrada:", var_objetivo))
  }
}
# ------------------------- VALIDAR VARIABLES CLAVES --------------------------
validar_claves <- function(df, claves) {
  vars_faltantes <- setdiff(unlist(claves), names(df))
  if (length(vars_faltantes) > 0) {
    mensaje_alerta(paste("Faltan variables clave:", paste(vars_faltantes, collapse = ", ")))
  } else {
    mensaje_exito("Todas las variables clave están presentes")
  }
}
# -------------------------- MOSTRAR DIMENSIONES ------------------------------
mostrar_dimensiones <- function(df) {
  mensaje_info(paste("El dataset tiene", nrow(df), "filas y", ncol(df), "columnas"))
}
# -----------------------------------------------------------------------------

