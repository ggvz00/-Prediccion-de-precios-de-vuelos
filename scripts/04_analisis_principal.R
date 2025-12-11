# =============================================================================
# SCRIPT 04: ANÁLISIS PRINCIPAL
# Proyecto: Predicción de precios de vuelos
# Autores: Gonzalo Vazquez (Registro: 904203) ; Rocío Perez Gregorini (Registro: 905323)
# Fecha: 2025-12-05
# Descripción:
#   - Preparación de datos
#   - Ajuste del modelo ANOVA en bloques
#   - Comparaciones post-hoc (Tukey HSD)
#   - Inferencias y regresiones
#   - Visualización e interpretación de resultados
# =============================================================================

# ------------------------- 1. CONFIGURACIÓN INICIAL --------------------------
source(here::here("config", "parametros.R"))

source(here::here("functions", "limpieza_funciones.R"))
source(here::here("functions", "visualizacion_funciones.R"))

mensaje_proceso("Iniciando Script 04...")



# ----------------------- 2. CARGA DE DATOS LIMPIOS ---------------------------

archivo_entrada <- file.path(dir_data_proc, "datos_clean.rds")

if (!file.exists(archivo_entrada)) {
  stop("❌ No se encuentra 'datos_clean.rds'. Por favor, ejecuta el Script 02 primero.")
}

datos <- readRDS(archivo_entrada)
mensaje_exito("Datos limpios cargados correctamente")



# ------------------------ 3. PREPARACIÓN DE DATOS ----------------------------

mensaje_proceso("Feature Engineering...")

datos_modelos <- datos %>%
  drop_na(route, total_stops) %>%
  mutate(
    log_price = log(price),
    duration_block = cut(duration_min, breaks = c(0, 120, 240, 360, 600, Inf),
                         labels = c("0-2h", "2-4h", "4-6h", "6-10h", "10h+"), right = FALSE),
    hora_salida = as.numeric(str_extract(dep_time, "^\\d{1,2}")),
    momento_dia = factor(case_when(
      hora_salida < 6  ~ "Madrugada", hora_salida < 12 ~ "Mañana",
      hora_salida < 18 ~ "Tarde", TRUE ~ "Noche"
    ), levels = c("Madrugada", "Mañana", "Tarde", "Noche")),
    tipo_aerolinea = if_else(airline %in% c("Jet Airways", "Jet Airways Business", "Air India", "Vistara"), 
                             "Full-Service", "Low-Cost")
  )

mensaje_exito("Datos preparados: variables respuesta log_price, hora_salida, momento_dia y tipo_aerolineas definidas.")



# --------------------- 4. CORRELACIÓN, ANOVA Y TURKEY -------------------------

mensaje_proceso("Analizando correlaciones...")

# 3. CORRELACIÓN
mensaje_proceso("Calculando Correlación...")
test_correlacion <- cor.test(datos_modelos$duration_min, datos_modelos$log_price)
write_excel_csv2(broom::tidy(test_correlacion), file.path(dir_outputs_tables, "04_resultado_correlacion.csv"))

# 4. ANOVA y TUKEY (post-hoc)
mensaje_proceso("Ejecutando ANOVA y Tukey...")
modelo_anova <- aov(log_price ~ airline + duration_block, data = datos_modelos)
write_excel_csv2(broom::tidy(modelo_anova), file.path(dir_outputs_tables, "04_resultado_anova.csv"))

tukey_airline <- TukeyHSD(modelo_anova, "airline")
# Guardamos tabla completa
write_excel_csv2(as.data.frame(tukey_airline$airline) %>% rownames_to_column("pair"), 
                 file.path(dir_outputs_tables, "04_tukey_airline.csv"))

mensaje_exito("Tablas de ANOVA y Tukey guardadas en outputs/tables/")


# ------------------------ 6. VISUALIZACIÓN DE RESULTADOS ---------------------

mensaje_proceso("Generando gráficos de diagnóstico y comparaciones...")

grafico_anova_residuos(modelo_anova, file.path(dir_outputs_figures, "04_residuos_anova.png"))
grafico_tukey(tukey_airline, file.path(dir_outputs_figures, "04_tukey_airline.png"))

mensaje_exito("Gráficos guardados en outputs/figures/")


# ------------------------- 7. REGRESION MÚLTIPLE -----------------------------

# 5. REGRESIÓN MÚLTIPLE
mensaje_proceso("Ajustando Regresión...")
modelo_regresion <- lm(log_price ~ airline + duration_min + total_stops_num + momento_dia, data = datos_modelos)
write_excel_csv2(broom::tidy(modelo_regresion), file.path(dir_outputs_tables, "04_coeficientes_regresion.csv"))

r2 <- round(summary(modelo_regresion)$adj.r.squared, 4)
mensaje_exito(paste("R2 Ajustado:", r2))

# 6. GRÁFICO PERFORMANCE (Estilo Azul Manual)
datos_diag <- broom::augment(modelo_regresion)
g_perf <- ggplot(datos_diag, aes(x = .fitted, y = log_price)) +
  geom_point(alpha = 0.2, color = "#2980B9") + # Azul profesional
  geom_abline(color = "#C0392B", linetype = "dashed", size = 1) + # Rojo oscuro
  labs(title = "Modelo Final: Predicción vs Realidad", 
       subtitle = paste("R2 Ajustado:", r2), x = "Predicho", y = "Real") +
  tema_azul_pro # Usamos el tema nuevo

ggsave(file.path(dir_outputs_figures, "04_performance_modelo.png"), g_perf, width = 7, height = 6)

mensaje_exito("Script 04. Gráficos azules y limpios generados.")


# --- VERIFICACIÓN DE INDEPENDENCIA (Durbin-Watson) ---
mensaje_proceso("Verificando independencia de errores...")

# Ejecutamos el test
dw_test <- lmtest::dwtest(modelo_regresion)

# Mostramos el resultado en consola
print(dw_test)

# Interpretación automática para guardar en un txt
interpretacion_dw <- paste0(
  "Estadístico DW: ", round(dw_test$statistic, 4), "\n",
  "P-value: ", round(dw_test$p.value, 4), "\n",
  "Conclusión: ", if_else(dw_test$statistic > 1.5 & dw_test$statistic < 2.5, 
                          "Los errores son independientes (Cumple supuesto).", 
                          "Existe autocorrelación (Precaución con los p-values).")
)

writeLines(interpretacion_dw, file.path(dir_outputs_tables, "04_diagnostico_independencia.txt"))

# --- VERIFICACIÓN DE MULTICOLINEALIDAD (VIF) ---
mensaje_proceso("Verificando Multicolinealidad (VIF)...")


# Calculamos el VIF
vif_valores <- car::vif(modelo_regresion)

print(vif_valores)

# Guardamos el resultado en un txt para el informe
capture.output(print(vif_valores), file = file.path(dir_outputs_tables, "04_diagnostico_vif.txt"))