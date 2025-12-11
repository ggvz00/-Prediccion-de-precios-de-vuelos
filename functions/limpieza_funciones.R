# =============================================================================
# FUNCTIONS 01: Funciones de limpieza de datos
# Proyecto: Predicción de precios de vuelos según aerolínea y ruta
# Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
# =============================================================================

# ------------------------- LIMPIAR NOMBRE COLUMNAS ---------------------------
#'   Convierte a minúsculas, 
#'   Reemplaza espacios por guiones bajos.
#'   Elimina caracteres especiales.
#' @param df Dataframe sucio
#' @return Dataframe con nombres limpios (snake_case)
limpiar_nombres <- function(df) {
  nombres_nuevos <- names(df) %>%
    str_to_lower() %>%                    # Todo a minúsculas
    str_replace_all("\\s+", "_") %>%      # Espacios a guiones bajos
    str_replace_all("[^a-z0-9_]", "") %>% # Eliminar símbolos raros
    str_replace_all("_{2,}", "_") %>%     # Eliminar guiones duplicados
    str_remove("^_|_$")                   # Eliminar guiones al inicio/final
  
  names(df) <- nombres_nuevos
  return(df)
}

# ------------------------- CONVERTIR FECHAS A DATE ---------------------------
limpiar_fecha <- function(x) {
  lubridate::dmy(x)
}

# ----------------------- CONVERTIR DURACIÓN A MINUTOS ------------------------
#' Convierte strings de duración (ej: "2h 50m", "19h") a minutos totales numéricos
#' @param vector_duracion Vector de caracteres con la duración
#' @return Vector numérico con minutos totales
limpiar_duracion <- function(vector_duracion) {
  
  # Extraer horas (busca dígitos antes de la 'h')
  horas <- str_extract(vector_duracion, "\\d+(?=h)") %>% 
    as.numeric() %>% 
    replace_na(0) # Si no hay horas, es 0
  
  # Extraer minutos (busca dígitos antes de la 'm')
  minutos <- str_extract(vector_duracion, "\\d+(?=m)") %>% 
    as.numeric() %>% 
    replace_na(0) # Si no hay minutos, es 0
  
  # Calcular total
  total_minutos <- (horas * 60) + minutos
  return(total_minutos)
}

# -------------------------- NORMALIZAR ESCALAS -------------------------------
#' Convierte el texto de escalas a número entero
#' @param vector_escalas Vector de caracteres (ej: "non-stop", "1 stop")
#' @return Vector numérico
limpiar_escalas <- function(vector_escalas) {
  case_when(
    vector_escalas == "non-stop" ~ 0,
    vector_escalas == "1 stop"   ~ 1,
    vector_escalas == "2 stops"  ~ 2,
    vector_escalas == "3 stops"  ~ 3,
    vector_escalas == "4 stops"  ~ 4,
    TRUE ~ NA_real_
  )
}

# ------------------- DETECCIÓN DE OUTLIERS (Método IQR) ----------------------
#' Detecta valores atípicos usando el Rango Intercuartílico
#' @param df Dataframe
#' @param variable String con el nombre de la variable numérica
#' @return Lista con estadísticas, límites y conteo de outliers
detectar_atipicos <- function(df, variable) {
  datos <- df[[variable]]
  
  # Calcular cuartiles ignorando NAs
  Q1 <- quantile(datos, 0.25, na.rm = TRUE)
  Q3 <- quantile(datos, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  # Definir límites (Bigotes del Boxplot)
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  
  # Identificar outliers
  es_outlier <- datos < limite_inferior | datos > limite_superior
  cantidad <- sum(es_outlier, na.rm = TRUE)
  porcentaje <- round((cantidad / length(datos)) * 100, 2)
  
  # Retornar lista de diagnóstico
  list(
    variable = variable,
    limite_inf = limite_inferior,
    limite_sup = limite_superior,
    n_outliers = cantidad,
    pct_outliers = porcentaje
  )
}

# ---------------------- CHECK VALORES FALTANTES (NA) -------------------------
# Cuántos valores NA hay por columna
resumen_na <- function(df) {
  colSums(is.na(df))
}

# -------------------- TABLA DE VALORES FALTANTES (NA) ------------------------
#' Genera tabla con el conteo y porcentaje de NAs por columna
#' @param df Dataframe
#' @return Dataframe con columnas: variable, n_na, porcentaje
tabla_na_resumen <- function(df) {
  conteo <- resumen_na(df) %>% tibble::enframe(name = "variable", value = "n_na")
  
  conteo %>%
    mutate(porcentaje = round(100 * n_na / nrow(df), 2)) %>%
    filter(n_na > 0) %>%
    arrange(desc(n_na))
}

# ---------------------- ESTADÍSTICAS DESCRIPTIVAS ----------------------------
#' Calcula estadísticas descriptivas para variables numéricas clave
#' @param df Dataframe con columnas price y duration_min
#' @return Dataframe con estadísticas (media, mediana, moda, sd, iqr, min, max)
resumen_numericas <- function(df) {
  
# Función auxiliar para calcular la moda
  calcular_moda <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  df %>%
    summarise(
      n             = n(),
      # Precio
      price_mean    = mean(price, na.rm = TRUE),
      price_median  = median(price, na.rm = TRUE),
      price_mode    = calcular_moda(price),
      price_sd      = sd(price, na.rm = TRUE),
      price_iqr     = IQR(price, na.rm = TRUE),
      price_min     = min(price, na.rm = TRUE),
      price_max     = max(price, na.rm = TRUE),
      # Duración
      dur_mean      = mean(duration_min, na.rm = TRUE),
      dur_median    = median(duration_min, na.rm = TRUE),
      dur_sd        = sd(duration_min, na.rm = TRUE),
      dur_min       = min(duration_min, na.rm = TRUE),
      dur_max       = max(duration_min, na.rm = TRUE)
    )
}

# ---------------------- RENDER TABLA OUTLIERS --------------------------------
#' Genera una imagen PNG con la tabla de outliers
#' @param tabla_outliers Dataframe con resumen de outliers
#' @param ruta_salida Ruta del archivo PNG
render_tabla_outliers <- function(tabla_outliers, ruta_salida) {
  
  tabla_visual <- tabla_outliers %>%
    mutate(
      limite_inf = round(limite_inf, 1),
      limite_sup = round(limite_sup, 1),
      pct_outliers = paste0(pct_outliers, "%")
    ) %>%
    rename(
      "Variable" = variable,
      "Límite Inf" = limite_inf,
      "Límite Sup" = limite_sup,
      "Cant. Outliers" = n_outliers,
      "% Muestra" = pct_outliers
    )
  
  png(ruta_salida, width = 650, height = 200, bg = "white")
  gridExtra::grid.table(tabla_visual)
  dev.off()
}


# -----------------------------------------------------------------------------
