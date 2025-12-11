# =============================================================================
# SCRIPT 05: VISUALIZACIÓN FINAL Y STORYTELLING
# Proyecto: Predicción de precios de vuelos
# Autores: Gonzalo Vazquez, Rocio Gregorini
# Descripción: Generación de gráficos  para el informe final.
# =============================================================================

# 1. CONFIGURACIÓN
source(here::here("config", "parametros.R"))
source(here::here("functions", "limpieza_funciones.R"))
source(here::here("functions", "visualizacion_funciones.R")) 

mensaje_proceso("Iniciando Script 05: Storytelling...")

# Cargar datos limpios
datos <- readRDS(file.path(dir_data_proc, "datos_clean.rds"))

# Recreamos las variables clave (Feature Engineering) para graficar
datos_plot <- datos %>%
  drop_na(route, total_stops) %>%
  mutate(
    log_price = log(price),
    tipo_aerolinea = if_else(airline %in% c("Jet Airways", "Jet Airways Business", "Air India", "Vistara"), 
                             "Full-Service (Premium)", "Low-Cost (Económica)"),
    # Convertimos escalas a factor con etiquetas 
    escalas_cat = factor(total_stops_num, labels = c("Directo", "1 Escala", "2 Escalas", "3 Escalas", "4 Escalas"))
  )
# -----------------------------------------------------------------------------
# GRÁFICO 1: EL "PRECIO DE LA MARCA" (Impacto de Aerolínea en el Bolsillo)
# -----------------------------------------------------------------------------
mensaje_proceso("Generando Gráfico 1: Impacto de la Aerolínea...")

# Calculamos el precio medio por aerolínea para ordenar el gráfico
resumen_aerolineas <- datos_plot %>%
  group_by(airline, tipo_aerolinea) %>%
  summarise(precio_medio = median(price), .groups = 'drop') %>%
  arrange(desc(precio_medio))

# Gráfico de Barras Horizontal 
g1_story <- ggplot(resumen_aerolineas, aes(x = reorder(airline, precio_medio), y = precio_medio, fill = tipo_aerolinea)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::dollar(precio_medio, prefix = "₹")), 
            hjust = -0.1, size = 3.5, color = "#2C3E50") + # Etiqueta de precio
  scale_fill_manual(values = c("#2980B9", "#95A5A6")) + # Azul para Premium, Gris para LowCost
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Margen para que entren los números
  coord_flip() +
  
  labs(
    title = "El Costo de la Marca: Premium vs Low-Cost",
    # AQUI ESTÁ EL CAMBIO SOLICITADO:
    subtitle = "Las aerolíneas Premium son sistemáticamente más costosas (Jet Airways lidera el mercado).",
    x = NULL,
    y = "Precio Mediano (Rupias)",
    fill = "Segmento",
    caption = "Fuente: Elaboración propia base Kaggle | Script 05"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    # Agregamos un pequeño margen al subtítulo para que respire mejor
    plot.subtitle = element_text(size = 11, color = "#5D6D7E", margin = margin(b = 10)),
    panel.grid.major.y = element_blank(), # Limpiamos líneas horizontales
    legend.position = "bottom"
  )

ggsave(file.path(dir_outputs_reports, "05_storytelling_ranking_marcas.png"), g1_story, 
       width = 10, height = 7, bg = "white")

# -----------------------------------------------------------------------------
# GRÁFICO 2: (Duración vs Precio por Segmento)
# -----------------------------------------------------------------------------
mensaje_proceso("Generando Gráfico 2: Segmentación de Mercado...")

g2_story <- ggplot(datos_plot, aes(x = duration_min, y = price, color = tipo_aerolinea)) +
  
  # 1. PUNTOS: Suaves de fondo
  geom_point(alpha = 0.2, size = 1.5) +
  
  # 2. LÍNEAS DE TENDENCIA: Bien gruesas para destacar la diferencia de pendientes
  geom_smooth(method = "lm", se = FALSE, linewidth = 2.5) +
  
  # 3. COLORES Y ESCALAS
  scale_color_manual(values = c("#C0392B", "#27AE60")) + # Rojo (Premium), Verde (LowCost)
  scale_y_continuous(labels = scales::dollar_format(prefix = "₹")) +
  scale_x_continuous(breaks = seq(0, 1500, by = 180)) + # Marcas cada 3 horas
  
  # 4. ETIQUETAS (Con el hallazgo en el subtítulo)
  labs(
    title = "Dinámica de Precios: ¿Cuánto cuesta el tiempo de vuelo?",
    # Aquí va la frase que pediste como subtítulo principal:
    subtitle = "Podemos ver que las aerolíneas premium muestran una pendiente más pronunciada (mayor costo por hora).",
    x = "Duración del Vuelo (Minutos)",
    y = "Precio del Pasaje",
    color = "Tipo de Servicio",
    caption = "Fuente: Elaboración propia base Kaggle | Script 05"
   
  ) +
  
  # 5. TEMA
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    # Damos estilo al subtítulo para que se lea bien
    plot.subtitle = element_text(size = 11, color = "#5D6D7E", margin = margin(b = 10)),
    legend.position = "top"
  )

# Guardamos
ggsave(file.path(dir_outputs_reports, "05_storytelling_segmentacion.png"), g2_story, 
       width = 10, height = 7, bg = "white")

# -----------------------------------------------------------------------------

# EXTRA: CONCLUSIONES (Para  el informe)
# -----------------------------------------------------------------------------
mensaje_proceso("Generando texto de conclusiones...")

# Calculamos la diferencia porcentual real
media_premium <- mean(datos_plot$price[datos_plot$tipo_aerolinea == "Full-Service (Premium)"])
media_lowcost <- mean(datos_plot$price[datos_plot$tipo_aerolinea == "Low-Cost (Económica)"])
brecha <- round(((media_premium - media_lowcost) / media_lowcost) * 100, 1)

  conclusiones <- c(
    "El precio presenta fuerte asimetría positiva → se justifica la transformación logarítmica.",
    "Las aerolíneas low-cost concentran precios más bajos y menos dispersión.",
    "Jet Airways Business se ubica como segmento premium.",
    "La duración influye, pero la aerolínea explica mejor la variación en precios.",
    paste("El modelo final alcanza un R2 ajustado cercano al 0.65")
  )
  
writeLines(conclusiones, file.path(dir_outputs_reports, "05_conclusiones_clave.txt"))
  
  mensaje_exito("Reportes finales generados en outputs/reports/")
  