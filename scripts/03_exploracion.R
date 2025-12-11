# =============================================================================
# SCRIPT 03: ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# Proyecto: Predicción de precios de vuelos según aerolínea y ruta
# Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
# Fecha: 2025-12-05
# Descripción:
#   - Análisis de estructura y valores faltantes (NAs)
#   - Estadísticas descriptivas (media, mediana, desvío)
#   - Detección y exportación de Outliers
#   - Visualizaciones exploratorias (Histogramas y Boxplots)
# =============================================================================

# ------------------------- 1. CONFIGURACIÓN INICIAL --------------------------
source(here::here("config", "parametros.R"))
source(here::here("functions", "limpieza_funciones.R"))

mensaje_proceso("Iniciando Script 03: EDA...")


# ----------------------- 2. CARGA DE DATOS LIMPIOS ---------------------------
archivo_entrada <- file.path(dir_data_proc, "datos_clean.rds")

if (!file.exists(archivo_entrada)) {
  stop("❌ No se encuentra 'datos_clean.rds'. Por favor, ejecuta el Script 02 primero.")
}

datos_clean <- readRDS(archivo_entrada)
mensaje_exito("Datos limpios cargados correctamente")


# ----------------------- 3. ESTRUCTURA Y NAs ---------------------------------
mensaje_proceso("Analizando estructura y valores faltantes...")

# Estructura general
glimpse(datos_clean)

# Tabla de NAs por columna
tabla_na <- tabla_na_resumen(datos_clean)
print(tabla_na)

# Guardar tabla de NAs
write_excel_csv2(tabla_na, file.path(dir_outputs_tables, "03_resumen_valores_faltantes.csv"))


# ----------------------- 4. ESTADÍSTICAS DESCRIPTIVAS ------------------------
mensaje_proceso("Generando estadísticas descriptivas...")

# A. Variables Numéricas (Precio y Duración)
resumen_num <- resumen_numericas(datos_clean)

print(t(resumen_num)) # Transponer para ver mejor en consola
write_excel_csv2(resumen_num, file.path(dir_outputs_tables, "03_resumen_estadistico.csv"))


# B. Variables Categóricas (Aerolínea)
freq_airline <- datos_clean %>%    # Cuántos vuelos tiene cada aerolinea y ordenamos de mayor a menor
  count(airline, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2))

write_excel_csv2(freq_airline, file.path(dir_outputs_tables, "03_frecuencia_aerolineas.csv"))


# ----------------------- 5. ANÁLISIS DE OUTLIERS -----------------------------
mensaje_proceso("Detectando y exportando outliers...")

# Usamos la función 'detectar_atipicos' que definimos en functions/
out_price <- detectar_atipicos(datos_clean, "price")
out_dur   <- detectar_atipicos(datos_clean, "duration_min")

# Convertimos las listas a una tabla resumen
tabla_outliers <- bind_rows(out_price, out_dur)

print(tabla_outliers)
# Guardamos el CSV
write_excel_csv2(tabla_outliers, file.path(dir_outputs_tables, "03_resumen_outliers.csv"))


# ----------------------- EXTRA - TABLA VISUAL (IMAGEN) -----------------------
mensaje_proceso("Generando imagen profesional de la tabla de outliers...")

render_tabla_outliers(
  tabla_outliers,
  file.path(dir_outputs_tables, "03_tabla_outliers_render.png")
)

mensaje_exito("Imagen de tabla guardada en outputs/tables/")


# ----------------------- 6. VISUALIZACIÓN EXPLORATORIA -----------------------
mensaje_proceso("Generando gráficos exploratorios...")

# Definimos un tema base con fondo BLANCO
tema_reporte <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black")
  )

# Gráfico 1: Histograma de Precio
g1 <- ggplot(datos_clean, aes(x = price)) +
  geom_histogram(bins = 50, fill = "#2E86C1", color = "white", alpha = 0.9) +
  geom_vline(aes(xintercept = mean(price, na.rm=TRUE)), color = "red", linetype = "dashed") +
  labs(title = "Distribución del Precio", 
       subtitle = "La línea roja indica el promedio", 
       x = "Precio (Rupias)", y = "Frecuencia") +
  tema_reporte # Usamos el tema corregido
#El histograma revela que la variable Precio
#no sigue una distribución normal (campana de Gauss), sino que presenta una fuerte asimetría positiva
#Esto justifica metodológicamente la necesidad de aplicar una transformación logarítmica

# Nota: Agregamos bg = "white" en ggsave para asegurar el fondo blanco al grafico
ggsave(file.path(dir_outputs_figures, "03_hist_precio.png"), g1, 
       width = 8, height = 5, bg = "white") 

# Gráfico 2: Precio por Aerolínea (Boxplot)
g2 <- ggplot(datos_clean, aes(x = reorder(airline, price, FUN = median), y = price, fill = airline)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "₹")) +
  labs(title = "Distribución de Precios por Aerolínea", 
       subtitle = "Ordenado por mediana de precio",
       x = NULL, y = "Precio") +
  tema_reporte

ggsave(file.path(dir_outputs_figures, "03_boxplot_precio_aerolinea.png"), g2, 
       width = 10, height = 6, bg = "white")
##Las aerolíneas de bajo costo (SpiceJet, IndiGo, GoAir) concentran precios más bajos y con menor dispersión, 
#mientras que Jet Airways, Air India y ‘Multiple carriers’ presentan precios medianos y máximos significativamente más 
#altos, con numerosos outliers. Jet Airways Business se ubica claramente como un segmento premium. Esto respalda la 
#hipótesis de que el precio del ticket depende fuertemente de la aerolínea

# Gráfico 3: Duración vs Precio (Scatterplot)
g3 <- ggplot(datos_clean, aes(x = duration_min, y = price)) +
  geom_point(alpha = 0.1, color = "#566573") + # Gris oscuro para los puntos
  geom_smooth(method = "lm", color = "#C0392B", se = FALSE) + # Línea roja oscura
  scale_y_continuous(labels = scales::dollar_format(prefix = "₹")) +
  labs(title = "Relación Duración vs Precio", 
       subtitle = "Correlación lineal",
       x = "Duración (minutos)", y = "Precio") +
  tema_reporte

ggsave(file.path(dir_outputs_figures, "03_scatter_duracion_precio.png"), g3, 
       width = 8, height = 5, bg = "white")
#Si bien la duración influye (la línea sube), el hecho de que haya tanta dispersión sugiere que otra variable (como la
#Aerolínea) es la  que determina realmente si el vuelo es caro o barato. 
#Esto justifica usar regresión múltiple: necesitamos incluir aerolínea,
#ruta y número de escalas para explicar mejor el precio, no alcanza con la duración sola


mensaje_exito("Script 03 completado exitosamente.")