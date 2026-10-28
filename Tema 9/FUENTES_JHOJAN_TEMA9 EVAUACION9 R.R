library(readxl)

df <- as.data.frame(read_excel("C:\Users\VICTUS\OneDrive\Desktop\UNSAAC\UNSAAC 2026-II\PROGRAMACION ESTADISTICA\Tema 9\datos_ambiental_evaluacion_tema9.xlsx"))
rownames(df) <- df$estacion
num_cols <- c("pm10_ugm3", "no2_ppb", "ruido_db")

# Imputación con la mediana
df_clean <- df[, num_cols]
for (col in num_cols) {
  df_clean[is.na(df_clean[, col]), col] <- median(df_clean[, col], na.rm = TRUE)
}

# Z-score por columna (scale() usa desviación muestral, n-1)
df_z <- as.data.frame(scale(df_clean))

# Min-Max [0, 1]
min_max_scale <- function(x) (x - min(x)) / (max(x) - min(x))
df_mm <- as.data.frame(lapply(df_clean, min_max_scale))
rownames(df_mm) <- rownames(df_clean)

print(round(df_z, 2))


library(pheatmap)
library(RColorBrewer)

mat <- as.matrix(df_z)
lim <- max(abs(mat))                                   # simetría alrededor de 0
brks <- seq(-lim, lim, length.out = 101)
paleta <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)  # azul (bajo) -> rojo (alto)

pheatmap(mat,
         color = paleta,
         breaks = brks,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Parte C – Mapa de calor Z-score (R)")

library(pheatmap)
library(RColorBrewer)

mat <- as.matrix(df_z)
lim <- max(abs(mat))
brks <- seq(-lim, lim, length.out = 101)
paleta <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)

pheatmap(mat,
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         color = paleta,
         breaks = brks,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Parte D – Clustermap Ward.D2 / Euclidiana (R)",
         filename = "parteD_clustermap_R.png",
         width = 8,
         height = 8,
         res = 300)
