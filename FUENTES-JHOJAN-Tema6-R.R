#---primera celda---

# Completa: Reconstruye el degradado con matrix()
# Creamos la secuencia de 5 valores entre 0 y 255
fila_degradado <- seq(0, 255, length.out = 5)

# Creamos la matriz de 5x5 repitiendo la fila. 
# byrow = TRUE es equivalente a np.tile(..., (5, 1)) en Python
gris <- matrix(fila_degradado, nrow = 5, ncol = 5, byrow = TRUE)

# Completa: dim() y mean()
print(dim(gris))
print(mean(gris))

# 1. Convertimos 'gris' a una matriz 2D de 5x5 si es un vector 1D
if (is.null(dim(gris))) {
  # Si no tiene dimensiones, es un vector 1D. Lo repetimos 5 veces hacia abajo.
  # byrow = TRUE es equivalente a np.tile(gris, (5, 1)) en Python.
  gris_2d <- matrix(gris, nrow = 5, ncol = 5, byrow = TRUE)
} else {
  gris_2d <- gris
}

# 2. Guardar la imagen en un archivo PNG
png("gris_tema6.png")

# 3. Visualizar la imagen
# NOTA: En R, image() dibuja las filas de abajo hacia arriba.
# Para que se vea igual que en Python (origen arriba a la izquierda),
# transponemos t() e invertimos el orden de las filas [5:1, ].
image(t(gris_2d)[nrow(gris_2d):1, ], col = gray.colors(256), main = "Imagen en escala de grises")

# 4. Cerrar el dispositivo gráfico para que el archivo se guarde correctamente
dev.off()

# 5. Mostrar la imagen en pantalla (equivalente a plt.show())
# En RStudio, esto aparecerá en el panel "Plots".
image(t(gris_2d)[nrow(gris_2d):1, ], col = gray.colors(256), main = "Imagen en escala de grises")



#---segunda celda---
# Completa: extrae cada canal
# En R, los canales están en la 3ra dimensión: 1=Rojo, 2=Verde, 3=Azul
canal_r <- rgb[, , 1]
canal_g <- rgb[, , 2]
canal_b <- rgb[, , 3]

# Mostrar los canales (opcional, pero recomendado para verificar)
# Configuramos el área de gráficos: 1 fila, 3 columnas
par(mfrow = c(1, 3))

# --- Canal Rojo ---
# Usamos colorRampPalette para crear un degradado de blanco a rojo
image(t(canal_r)[nrow(canal_r):1, ], 
      col = colorRampPalette(c("white", "red"))(256), 
      main = "Canal R (Rojo)")

# --- Canal Verde ---
image(t(canal_g)[nrow(canal_g):1, ], 
      col = colorRampPalette(c("white", "green"))(256), 
      main = "Canal G (Verde)")

# --- Canal Azul ---
image(t(canal_b)[nrow(canal_b):1, ], 
      col = colorRampPalette(c("white", "blue"))(256), 
      main = "Canal B (Azul)")

# Restaurar la configuración de gráficos por defecto (1 gráfico a la vez)
par(mfrow = c(1, 1))

#---tercera celda---

# Completa: gris_promedio y gris_luminosidad
gris_promedio <- (canal_r + canal_g + canal_b) / 3
gris_luminosidad <- 0.299 * canal_r + 0.587 * canal_g + 0.114 * canal_b

print("--- Gris Promedio ---")
print(gris_promedio)
print("\n--- Gris Luminosidad ---")
print(gris_luminosidad)
