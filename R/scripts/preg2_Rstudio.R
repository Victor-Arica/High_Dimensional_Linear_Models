#Preg2: Sobreajuste
rm(list = ls())

# Configuración inicial
set.seed(12345)
#install.packages("")  [Por si es necesario descargar los paquetes]
library(ggplot2)
#library(gridExtra) Por si acaso llega a ser necesario

#Setear directorio (comentado para evitar errores)
setwd("C:\\Users\\Julio\\Desktop\\R\\Output")

#Generacion de datos sin intercepto
generar_datos <- function(n, p) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p) #Matriz X con p caracteristicas
  
  # Y = X1 + X2 + error (intercepto = 0)
  if (p == 1) {
    y <- X[,1] + rnorm(n, 0, 1)
  } else {
    y <- X[,1] + X[,2] + rnorm(n, 0, 1)
  }
  
  return(list(X = X, y = y))
}

# Dividir datos en entrenamiento (75%) y prueba (25%)
dividir_datos <- function(X, y, prop_train = 0.75) {
  n <- nrow(X)
  indices_train <- sample(1:n, size = floor(prop_train * n))
  X_train <- X[indices_train, , drop = FALSE]
  y_train <- y[indices_train]
  X_test <- X[-indices_train, , drop = FALSE]
  y_test <- y[-indices_train]
  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

# Función para calcular R cuadrado
calcular_r2 <- function(y_real, y_pred) {
  ss_res <- sum((y_real - y_pred)^2)
  ss_tot <- sum((y_real - mean(y_real))^2)
  return(1 - (ss_res / ss_tot))
}

#Parámetros del experimento según la actividad
n <- 1000  # n° observaciones (fijo)
p_valores <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)  #n° de características

# Matriz para almacenar resultados
resultados <- matrix(NA, nrow = length(p_valores), ncol = 4)
colnames(resultados) <- c("R2_train", "R2_adj_train", "R2_test", "p")

#Simulación principal
cat("Iniciando simulación de overfitting...\n")
cat("Número de observaciones fijo: n =", n, "\n\n")

for (i in seq_along(p_valores)) {
  p <- p_valores[i]
  cat("Procesando p =", p, "características...\n")
  
  # Generar datos con p características
  datos <- generar_datos(n, p)
  X <- datos$X
  y <- datos$y
  
  # Solo dividir en train/test para el caso out-of-sample (p = 1000)
  if (p == 1000) {
    division <- dividir_datos(X, y)
    X_train <- division$X_train
    y_train <- division$y_train
    X_test <- division$X_test
    y_test <- division$y_test
  } else {
    # Para otros casos, usar todos los datos para entrenamiento
    X_train <- X
    y_train <- y
    X_test <- NULL
    y_test <- NULL
  }
  
  # Crear data frame para el modelo
  if (p == 1) {
    # Modelo simple: Y = β₁X₁ + ε
    df_train <- data.frame(y = y_train, x1 = X_train[,1])
    modelo <- lm(y ~ x1 - 1, data = df_train)  # Sin intercepto
  } else {
    # Modelo múltiple: Y = β₁X₁ + β₂X₂ + ... + βₚXₚ + ε
    df_train <- as.data.frame(cbind(y = y_train, X_train))
    names(df_train)[-1] <- paste0("x", 1:p)
    
    # Crear fórmula dinámicamente
    formula_str <- paste("y ~", paste(paste0("x", 1:p), collapse = " + "), "- 1")
    modelo <- lm(as.formula(formula_str), data = df_train)
  }
  
  # Predicciones en entrenamiento
  pred_train <- predict(modelo)
  
  # Calcular R² de entrenamiento
  r2_train <- calcular_r2(y_train, pred_train)
  
  # Calcular R² ajustado
  r2_adj <- summary(modelo)$adj.r.squared
  
  # Calcular R² de prueba (solo para p = 1000)
  r2_test <- NA
  if (!is.null(X_test)) {
    if (p == 1) {
      df_test <- data.frame(x1 = X_test[,1])
      pred_test <- predict(modelo, newdata = df_test)
    } else {
      df_test <- as.data.frame(X_test)
      names(df_test) <- paste0("x", 1:p)
      pred_test <- predict(modelo, newdata = df_test)
    }
    r2_test <- calcular_r2(y_test, pred_test)
  }
  
  # Almacenar resultados
  resultados[i, ] <- c(r2_train, r2_adj, r2_test, p)
  
  # Mostrar progreso
  cat(sprintf("  R² entrenamiento: %.4f\n", r2_train))
  cat(sprintf("  R² ajustado: %.4f\n", r2_adj))
  if (!is.na(r2_test)) {
    cat(sprintf("  R² prueba (out-of-sample): %.4f\n", r2_test))
  }
  cat("\n")
}

# Convertir a data frame para ggplot
df_resultados <- as.data.frame(resultados)

# Gráfico 1: R² vs Número de Características
p1 <- ggplot(df_resultados, aes(x = p)) +
  geom_line(aes(y = R2_train, color = "R² Entrenamiento"), linewidth = 1, na.rm = TRUE) +
  geom_line(aes(y = R2_adj_train, color = "R² Ajustado"), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = R2_train, color = "R² Entrenamiento"), na.rm = TRUE) +
  geom_point(aes(y = R2_adj_train, color = "R² Ajustado"), na.rm = TRUE) +
  scale_color_manual(values = c("R² Entrenamiento" = "blue", 
                                "R² Ajustado" = "red")) +
  labs(title = "Overfitting: R² vs Número de Características",
       subtitle = "n = 1000 observaciones fijas",
       x = "Número de Características (p)",
       y = "R²",
       color = "Tipo de R²") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_log10()

# Gráfico 2: Zoom en características bajas (≤ 50)
df_zoom <- df_resultados[df_resultados$p <= 50, ]
p2 <- ggplot(df_zoom, aes(x = p)) +
  geom_line(aes(y = R2_train, color = "R² Entrenamiento"), linewidth = 1, na.rm = TRUE) +
  geom_line(aes(y = R2_adj_train, color = "R² Ajustado"), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = R2_train, color = "R² Entrenamiento"), na.rm = TRUE) +
  geom_point(aes(y = R2_adj_train, color = "R² Ajustado"), na.rm = TRUE) +
  scale_color_manual(values = c("R² Entrenamiento" = "blue", 
                                "R² Ajustado" = "red")) +
  labs(title = "Detalle: R² vs Características (p ≤ 50)",
       subtitle = "Comportamiento con pocas características",
       x = "Número de Características (p)",
       y = "R²",
       color = "Tipo de R²") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico 3: Diferencia entre R² entrenamiento y R² ajustado
df_resultados$diferencia <- df_resultados$R2_train - df_resultados$R2_adj_train
p3 <- ggplot(df_resultados, aes(x = p, y = diferencia)) +
  geom_line(color = "purple", linewidth = 1, na.rm = TRUE) +
  geom_point(color = "purple", size = 2, na.rm = TRUE) +
  labs(title = "Penalización por Complejidad: R² Entrenamiento - R² Ajustado",
       subtitle = "Diferencia aumenta con más características",
       x = "Número de Características (p)",
       y = "Diferencia en R²") +
  theme_minimal() +
  scale_x_log10()

#Exportando los gráficos como imágenes
ggsave("P1_overfitting_caracteristicas.png", plot = p1,
       width = 8, height = 6, dpi = 200,
       bg = "white", device = "png")

ggsave("P2_detalle_caracteristicas.png", plot = p2,
       width = 8, height = 6, dpi = 200,
       bg = "white", device = "png")

ggsave("P3_penalizacion_complejidad.png", plot = p3,
       width = 8, height = 6, dpi = 200,
       bg = "white", device = "png")

# Mostrar los gráficos dentro de RStudio
print(p1)
print(p2)
print(p3)

# Mostrar tabla de resultados
cat("RESULTADOS FINALES:\n")
print(round(df_resultados, 4))

cat("\nANÁLISIS DE RESULTADOS:\n")

cat("1. DEMOSTRACIÓN DE OVERFITTING:\n")
cat("- El R² de entrenamiento aumenta monotónicamente con más características\n")
cat("- El R² ajustado inicialmente aumenta pero luego disminuye\n")
cat("- Esto confirma el fenómeno de overfitting cuando p es grande\n\n")

# Encontrar el punto óptimo
max_adj_idx <- which.max(df_resultados$R2_adj_train)
cat("2. PUNTO ÓPTIMO SEGÚN R² AJUSTADO:\n")
cat(sprintf("   - Mejor R² ajustado: %.4f con p = %d características\n", 
            df_resultados$R2_adj_train[max_adj_idx], 
            df_resultados$p[max_adj_idx]))

cat(sprintf("   - R² de entrenamiento en ese punto: %.4f\n",
            df_resultados$R2_train[max_adj_idx]))

cat("COMPORTAMIENTO EXTREMO (p = 1000):")
resultado_extremo <- df_resultados[df_resultados$p == 1000, ]
cat(sprintf("   - R² entrenamiento: %.4f\n", resultado_extremo$R2_train))
cat(sprintf("   - R² ajustado: %.4f\n", resultado_extremo$R2_adj_train))
if (!is.na(resultado_extremo$R2_test)) {
  cat(sprintf("   - R² prueba (out-of-sample): %.4f\n", resultado_extremo$R2_test))
  cat(sprintf("   - Diferencia train-test: %.4f\n", 
              resultado_extremo$R2_train - resultado_extremo$R2_test))
  cat("Esta diferencia confirma overfitting")
}

sink("Comentarios finales.txt")
cat("INTERPRETACIÓN TEÓRICA\n")
cat("- Con p pequeño: Modelo puede tener underfitting (sesgo alto)\n")
cat("- Con p grande: Modelo hace overfitting (varianza alta)\n")
cat("- R² ajustado ayuda a encontrar el equilibrio sesgo-varianza óptimo\n")
cat("- La relación real solo involucra 2 características (X1 + X2)\n")

cat("Algunas conclusiones del ejercicio:\n")
cat("- Más parámetros ≠ mejor modelo\n")
cat("- R² de entrenamiento es optimista con muchas características\n")
cat("- R² ajustado penaliza la complejidad innecesaria\n")
cat("- Validación out of sample es crucial para detectar overfitting\n")
sink()

# Guardar resultados
write.csv(df_resultados, "resultados_overfitting_caracteristicas.csv", row.names = FALSE)

