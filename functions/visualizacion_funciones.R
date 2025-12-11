# =============================================================================
# FUNCIONES DE VISUALIZACIÓN (VERSIÓN CORRECTIDA - AZUL PRO)
# =============================================================================


# 1. TEMA GLOBAL: AZUL PROFESIONAL (Fondo Blanco Puro)
tema_azul_pro <- theme_minimal(base_size = 12) +
  theme(
    # Fondo blanco absoluto (para que no salga gris ni crema)
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Textos en gris oscuro (más elegante que negro puro)
    text = element_text(color = "#2C3E50"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0), # Título alineado izq
    plot.subtitle = element_text(size = 11, color = "#7F8C8D"),
    
    # Ejes y Grillas
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.major = element_line(color = "#ECF0F1"), # Grilla muy suave
    panel.grid.minor = element_blank(), # Sin grilla menor
    axis.line = element_line(color = "#BDC3C7")
  )

# 2. GRÁFICO DE RESIDUOS (Diagnóstico)
grafico_anova_residuos <- function(modelo, ruta_salida) {
  
  df <- data.frame(
    fitted = fitted(modelo),
    resid = resid(modelo)
  )
  
  g <- ggplot(df, aes(x = fitted, y = resid)) +
    geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed", size = 1) +
    geom_point(alpha = 0.4, color = "#2980B9", size = 2) + # Azul Transparente
    geom_smooth(method = "loess", se = FALSE, color = "#2C3E50", size = 0.8) +
    labs(
      title = "Diagnóstico: Residuos vs Ajustados",
      subtitle = "Verificación de patrones de error (Homocedasticidad)",
      x = "Valores Ajustados (Log Price)",
      y = "Residuos (Error)"
    ) +
    tema_azul_pro
  
  ggsave(ruta_salida, g, width = 8, height = 6)
}

# 3. GRÁFICO DE TUKEY (CORREGIDO: Solo Top 20)
grafico_tukey <- function(tukey_obj, ruta_salida) {
  
  # Convertir a data frame
  df <- as.data.frame(tukey_obj$airline) %>%
    rownames_to_column(var = "comparacion")
  
  # --- LA CORRECCIÓN CLAVE ---
  # Filtramos para mostrar SOLO las diferencias significativas y grandes
  df_clean <- df %>%
    filter(`p adj` < 0.05) %>%       # Solo las significativas
    arrange(desc(abs(diff))) %>%     # Ordenar por tamaño de la diferencia
    head(20)                         # SOLO LAS 20 MEJORES (Evita la mancha negra)
  
  g <- ggplot(df_clean, aes(x = reorder(comparacion, diff), y = diff)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "#7F8C8D") +
    geom_point(aes(color = diff > 0), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    coord_flip() + # Barras horizontales para leer bien
    scale_color_manual(values = c("#E74C3C", "#27AE60"), guide = "none") + # Rojo/Verde
    labs(
      title = "Top 20 Diferencias de Precio entre Aerolíneas",
      subtitle = "Comparaciones Post-Hoc (Tukey) más relevantes",
      caption = "Verde: El primero es más caro | Rojo: El primero es más barato",
      x = NULL,
      y = "Diferencia de Precio Logarítmica"
    ) +
    tema_azul_pro
  
  ggsave(ruta_salida, g, width = 10, height = 8)
}