# ============================================================
library(imager)


ruta <- "C:/Users/VICTUS/OneDrive/Documentos/calca_satelital.png"
if (!file.exists(ruta)) ruta <- file.choose()

# img = imread('calca_satelital.png')
img <- load.image(ruta)
if (spectrum(img) == 4) img <- rm.alpha(img)   # quita el canal alfa si existe

# gray = rgb2gray(img)
gray <- grayscale(img)

# crop = gray[10:150, 20:200]
# Python: indices desde 0 y el final se excluye -> filas 10..149, columnas 20..199
# R: indices desde 1 y el final se incluye      -> y = 11..150, x = 21..200
crop <- imsub(gray, x >= 21 & x <= 200, y >= 11 & y <= 150)

cat("Dimensiones del recorte:", paste(dim(crop), collapse = " x "), "\n")
# 180 x 140 x 1 x 1  (ancho x alto x Z x canales)

plot(crop, main = "Recorte B1")


# ============================================================
# PARTE C - Canales, luminancia y ROI (R / imager) - para RStudio
# ============================================================
library(imager)

ruta <- "C:/Users/VICTUS/OneDrive/Documentos/calca_satelital.png"
if (!file.exists(ruta)) ruta <- file.choose()
carpeta <- dirname(ruta)

img <- load.image(ruta)
if (spectrum(img) == 4) img <- rm.alpha(img)   # quita canal alfa si existe

# ---- 1. Carga y metadatos ----
cat("Ruta de acceso   :", normalizePath(ruta), "\n")
cat("Clase del objeto :", class(img), "\n")
cat("Dimensiones      :", paste(dim(img), collapse = " x "), " [X, Y, Z, C]\n")
cat("Ancho x Alto     :", width(img), "x", height(img), "px\n")
cat("Canales          :", spectrum(img), "\n")
cat("Rango de valores :", paste(round(range(img), 3), collapse = " - "), "\n")

# ---- 2. Matrices de los canales R y B ----
canal_R <- as.matrix(R(img))
canal_B <- as.matrix(B(img))
cat("Dim matriz R:", dim(canal_R), "| media R:", round(mean(canal_R), 4), "\n")
cat("Dim matriz B:", dim(canal_B), "| media B:", round(mean(canal_B), 4), "\n")

# ---- 3. Luminancia BT.601 y su promedio ----
Y <- 0.299 * R(img) + 0.587 * G(img) + 0.114 * B(img)
cat("Luminancia media Y =", round(mean(Y), 4), "\n")
cat("Control grayscale():", round(mean(grayscale(img)), 4), "\n")

# ---- 4. ROI central de 150 x 150 px a PNG ----
if (width(img) < 150 || height(img) < 150) {
  stop("La imagen mide ", width(img), " x ", height(img),
       " px; se necesita al menos 150 x 150 px.")
}
x0 <- floor((width(img)  - 150) / 2) + 1
y0 <- floor((height(img) - 150) / 2) + 1

roi <- imsub(img, x >= x0 & x <= x0 + 149,
             y >= y0 & y <= y0 + 149)
stopifnot(width(roi) == 150, height(roi) == 150)

cat("ROI central:", width(roi), "x", height(roi), "px\n")

# Muestra la ROI en el panel Plots (sin guardar archivo)
plot(roi, main = "ROI central 150 x 150 px")









# ============================================================
# PARTE D - Suavizado, Otsu, histograma del canal Rojo (300 DPI)
# Version independiente, con la misma ruta y carga que el script completo
# ============================================================
library(imager)

# ------------------------------------------------------------
# RUTA DE LA IMAGEN (barras normales "/" para evitar el error "\U")
# ------------------------------------------------------------
ruta <- "C:/Users/VICTUS/OneDrive/Documentos/calca_satelital.png"

if (!file.exists(ruta)) ruta <- file.choose()

carpeta <- dirname(ruta)   # aqui se guardan las salidas

img <- load.image(ruta)
if (spectrum(img) == 4) img <- rm.alpha(img)   # quita canal alfa si existe

cat("\n===== PARTE D =====\n")

# ---- 1. Escala de grises + suavizado Gaussiano sigma = 2 (1.5 pts) ----
gris <- grayscale(img)
blur <- isoblur(gris, sigma = 2)

# ---- 2. Umbral automatico de Otsu y segmentacion (2 pts) ----
otsu <- function(x, nbins = 256) {
  h  <- hist(x, breaks = seq(min(x), max(x), length.out = nbins + 1),
             plot = FALSE)
  p  <- h$counts / sum(h$counts)
  w0 <- cumsum(p)
  mu <- cumsum(p * h$mids)
  sb <- (tail(mu, 1) * w0 - mu)^2 / (w0 * (1 - w0))  # varianza entre clases
  h$mids[which.max(sb)]
}

t_opt <- otsu(as.vector(blur))
mask  <- blur > t_opt                 # TRUE = zona clara (nubes / cobertura)

cat(sprintf("Umbral optimo (Otsu) = %.4f\n", t_opt))
cat(sprintf("Cobertura segmentada = %.1f %%\n", 100 * mean(mask)))

# Control con la funcion nativa de imager
mask_auto <- threshold(blur, "auto")
cat("Coincidencia con threshold('auto'):",
    round(100 * mean(mask == mask_auto), 1), "%\n")

# ---- 3 y 4. Figura compuesta y guardado a 300 DPI (2.5 pts) ----
rojo <- as.vector(R(img))

# Funcion que dibuja la figura compuesta (se usa en pantalla y en el PNG)
dibujar_figura <- function() {
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  plot(img, main = "Imagen original", axes = FALSE)
  plot(as.cimg(mask), main = "Mascara binaria (Otsu)", axes = FALSE)
  hist(rojo, breaks = 64, col = "firebrick", border = "white",
       main = "Histograma canal Rojo",
       xlab = "Intensidad (0-1)", ylab = "Frecuencia")
  abline(v = t_opt, col = "black", lwd = 2, lty = 2)
  legend("topright", legend = sprintf("Umbral Otsu = %.3f", t_opt),
         lty = 2, lwd = 2, bty = "n")
}

# (a) Mostrar en el panel Plots de RStudio
dibujar_figura()

# (b) Guardar a 300 DPI (lo exige el punto 4 de la guia)
salida_fig <- file.path(carpeta, "resultado_D.png")
png(salida_fig, width = 10, height = 3.8, units = "in", res = 300)
dibujar_figura()
dev.off()
cat("Figura guardada en:", salida_fig,
    "(10 x 3.8 in a 300 DPI = 3000 x 1140 px)\n")