library(dplyr)
library(readr)
datos_estaciones_meteorologicas_tema9 <- read_csv("C:/Users/VICTUS/OneDrive/Desktop/UNSAAC/UNSAAC 2026-II/PROGRAMACION ESTADISTICA/Tema 9/datos_estaciones_meteorologicas_tema9.xlsx")
View(datos_estaciones_meteorologicas_tema9)

# 1. Carga de datos multivariados
df <- read.csv("C:/Users/VICTUS/OneDrive/Desktop/UNSAAC/UNSAAC 2026-II/PROGRAMACION ESTADISTICA/Tema 9/datos_estaciones_meteorologicas_tema9.xlsx", row.names = 1, stringsAsFactors = FALSE)
num_cols <- c('temperatura_c', 'humedad_pct', 'precipitacion_mm', 'altitud_m')

# 2. Imputación de valores faltantes con la mediana
df_clean <- df[, num_cols]
for (col in num_cols) {
  df_clean[is.na(df_clean[, col]), col] <- median(df_clean[, col], na.rm = TRUE)
}

# 3. Estandarización Z-Score nativa con scale()
df_z <- as.data.frame(scale(df_clean))

# 4. Escalamiento Min-Max personalizado [0, 1]
min_max_scale <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df_minmax <- as.data.frame(lapply(df_clean, min_max_scale))
rownames(df_minmax) <- rownames(df_clean)

cat("--- Matriz Estandarizada Z-Score en R ---\n")
print(head(df_z))

library(pheatmap)
library(RColorBrewer)

# Definición de paleta divergente personalizada centrada en azul-blanco-rojo
palette_div <- colorRampPalette(c("navy", "white", "firebrick3"))(100)

pheatmap(as.matrix(df_z),
         color = palette_div,
         cluster_rows = FALSE,      # Sin clustering en esta hora
         cluster_cols = FALSE,
         display_numbers = TRUE,    # Mostrar valores en celdas
         number_format = "%.2f",
         fontsize_number = 9,
         border_color = "gray80",
         main = "Mapa de Calor de Indicadores Meteorológicos (Z-Score)")
# Pheatmap con Clustering Jerárquico Automático (Distancia Euclidiana y Método Ward.D2)
pheatmap(as.matrix(df_z),
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         color = colorRampPalette(c("purple", "black", "yellow"))(100),
         display_numbers = TRUE,
         number_format = "%.1f",
         main = "Heatmap con Clustering Jerárquico en R (Ward.D2)")
# Crear DataFrame de anotación para las filas
annotation_df <- data.frame(Zona = factor(df$tipo_zona))
rownames(annotation_df) <- rownames(df)

# Colores personalizados para las anotaciones
ann_colors <- list(
  Zona = c(Urbana = "crimson", Valle = "forestgreen", Altoandina = "royalblue")
)

pheatmap(as.matrix(df_z),
         annotation_row = annotation_df,
         annotation_colors = ann_colors,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Heatmap con Anotaciones de Filas en R")
# Exportación directa desde pheatmap a archivo vectorial/ráster HD
corr_matrix <- cor(df_clean)

pheatmap(corr_matrix,
         display_numbers = TRUE,
         number_format = "%.3f",
         color = colorRampPalette(c("darkblue", "white", "darkred"))(100),
         breaks = seq(-1, 1, length.out = 101),
         main = "Matriz de Correlación Climatológica de Pearson",
         filename = "mapa_calor_correlacion_hd.png",
         width = 7, 
         height = 6, 
         res = 300)

cat("Imagen R exportada exitosamente como 'mapa_calor_correlacion_hd.png'.\n")
