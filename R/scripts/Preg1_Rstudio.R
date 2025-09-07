# Preg1: DEMOSTRACIÓN DEL TEOREMA DE FRISCH-WAUGH-LOVELL (FWL) [Version de Rstudio]

# Librerías necesarias (por si acaso no se tienen instaladas)
library(tidyverse)
library(broom)
library(ggplot2)

setwd("C:\\Users\\Julio\\Desktop\\R\\Output")


# 1. GENERAR DATOS SIMULADOS (podemos demostrar el teorema utilizando una regresion y un par de variables creadas :D)

set.seed(123) # Para reproducibilidad
n <- 1000     # Número de observaciones

# Generar variables
X2_1 <- rnorm(n, mean = 10, sd = 2)    # Primera variable de control
X2_2 <- rnorm(n, mean = 5, sd = 1.5)   # Segunda variable de control
X1 <- 2*X2_1 + 3*X2_2 + rnorm(n, sd = 0.5)  # Variable de interés (correlacionada con controles)

# Crear matrices
X2 <- cbind(1, X2_1, X2_2)  # Matriz de controles (incluye intercepto)
X1_mat <- cbind(X1)         # Matriz de variable de interés

# Parámetros reales
beta1_true <- 1.5
beta2_true <- c(2, 0.8, -1.2)

# Generar v. dependiente
u <- rnorm(n, sd = 2)  # Término de error
y <- X1_mat %*% beta1_true + X2 %*% beta2_true + u

# 2. ESTIMACIÓN OLS COMPLETA (y en [X1, X2])

X_full <- cbind(X1_mat, X2)
beta_hat_full <- solve(t(X_full) %*% X_full) %*% t(X_full) %*% y

# Extraer beta1 de la regresión completa
beta1_full <- beta_hat_full[1]

cat("Estimación OLS completa de beta1:", round(beta1_full, 4), "\n")

# 3. PROCEDIMIENTO FWL (DOS ETAPAS)

# Paso 1: Regresionar y en X2 y obtener residuos
M_X2 <- diag(n) - X2 %*% solve(t(X2) %*% X2) %*% t(X2)  # Matriz de aniquilación
y_tilde <- M_X2 %*% y  # Residuos de y después de controlar por X2

# Paso 2: Regresionar X1 en X2 y obtener residuos
X1_tilde <- M_X2 %*% X1_mat  # Residuos de X1 después de controlar por X2

# Paso 3: Regresionar y_tilde en X1_tilde
beta1_fwl <- solve(t(X1_tilde) %*% X1_tilde) %*% t(X1_tilde) %*% y_tilde

cat("Estimación FWL de beta1:", round(beta1_fwl, 4), "\n")

# 4. VERIFICACIÓN DE EQUIVALENCIA
cat("\nDIFERENCIA ENTRE LAS DOS ESTIMACIONES:", round(beta1_full - beta1_fwl, 10), "\n")
cat("¿SON IGUALES?", all.equal(beta1_full, beta1_fwl), "\n")

# 5. VISUALIZACIÓN

# Crear dataframe para visualización
df_visual <- data.frame(
  X1_original = X1_mat[,1],
  X1_tilde = X1_tilde[,1],
  y_original = y,
  y_tilde = y_tilde[,1]
)

#Creamos graficos para que pueda verse la demostracion de FWL de forma visual

# Gráfico 1: Relación original entre X1 e y
p1 <- ggplot(df_visual, aes(x = X1_original, y = y_original)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación original entre X1 e y",
       x = "X1 (sin controlar)", y = "y") +
  theme_minimal()

# Gráfico 2: Relación parcial (después de controlar por X2)
p2 <- ggplot(df_visual, aes(x = X1_tilde, y = y_tilde)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación parcial entre X1 e y (controlando por X2)",
       x = "X1_tilde (residual)", y = "y_tilde (residual)") +
  theme_minimal()

# Exportar gráficos a la carpeta Output
ggsave("grafico1_relacion_original.png", plot = p1, path = "C:/Users/Julio/Desktop/R/Output", 
       width = 8, height = 6, dpi = 300)
ggsave("grafico2_relacion_parcial.png", plot = p2, path = "C:/Users/Julio/Desktop/R/Output", 
       width = 8, height = 6, dpi = 300)

# Mostrar gráficos en R
print(p1)
print(p2)

# 6. COMPARACIÓN CON LM() DE R
# Regresión completa usando lm()
lm_full <- lm(y ~ X1 + X2_1 + X2_2) #Resultados de regresion completa

print(summary(lm_full))

# Procedimiento FWL manual usando lm()
# Paso 1: Residualizar y respecto a X2
lm_y_X2 <- lm(y ~ X2_1 + X2_2)
y_resid <- resid(lm_y_X2)

# Paso 2: Residualizar X1 respecto a X2
lm_X1_X2 <- lm(X1 ~ X2_1 + X2_2)
X1_resid <- resid(lm_X1_X2)

# Paso 3: Regresión de residuos
lm_fwl <- lm(y_resid ~ X1_resid - 1)  # -1 para omitir intercepto

#Resultados de procedimiento FWL:
print(summary(lm_fwl))

# 7. DEMOSTRACIÓN MATEMÁTICA

#Se tiene M_X2 = I - X2(X2'X2)^{-1}X2' (matriz de proyección ortogonal)
#Ademas: y_tilde = M_X2 y (y residualizada respecto a X2)
# también X1_tilde = M_X2 X1 (X1 residualizada respecto a X2)\n")
# y por ultimo beta1_FWL = (X1_tilde' X1_tilde)^{-1} X1_tilde' y_tilde
# Por tanto se puede demostrar que beta1_FWL = beta1_OLS de la regresión completa
#Y aquello muestra que beta1 captura el efecto de X1 sobre y después de controlar por el efecto de X2.

# 8. VERIFICACIÓN NUMÉRICA ADICIONAL

# Calcular R-cuadrado parcial
ss_total <- sum((y_tilde - mean(y_tilde))^2)
ss_residual <- sum((y_tilde - X1_tilde %*% beta1_fwl)^2)
r_squared_partial <- 1 - (ss_residual/ss_total)

cat("\nR-cuadrado parcial de X1 después de controlar por X2:", 
    round(r_squared_partial, 4), "\n")

# 9. EXPLICACIÓN DE LA DEMOSTRACIÓN DEL TEOREMA FWL

#Este script demuestra el Teorema de Frisch-Waugh-Lovell (FWL) mediante
# La estimación OLS completa que incluye todas las variables X1 y X2 y el procedimiento FWL en dos etapas que
#Residualiza tanto y como X1 respecto a X2, además, estima la relación entre estos residuos.
#La comparación muestra que ambas estimaciones de beta1 son idénticas. Esto prueba que el coeficiente de X1 en la regresión múltiple puede
#obtenerse mediante regresión parcial después de 'limpiar' tanto la variable dependiente como la independiente de los efectos de X2.
#Los gráficos ilustran visualmente la diferencia entre la relación "cruda" (sin controlar) y la relación parcial (controlando por X2).