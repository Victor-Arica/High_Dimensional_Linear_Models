rm(list = ls())
# Leer archivo y abrir base
setwd("C:\\Users\\Julio\\Desktop\\R\\Input")
datos <- read_csv("apartments.csv",show_col_types = FALSE)

setwd("C:\\Users\\Julio\\Desktop\\R\\Output")

#3a Limpieza de la data base

# a1. Crear área variable al cuadrado y dar un poco de limpieza a los NA
datos$area2 <- datos$area^2

datos$buildingmaterial[is.na(datos$buildingmaterial)] <- "Otros" #Algunas obs de buildingmaterial están marcadas como NA, mejor lo ponemos como "Otros"
datos$type[is.na(datos$type)] <- "Otros" #Algunas obs de type están marcadas como NA, mejor lo ponemos como "Otros"
datos$haselevator[is.na(datos$haselevator)] <- "no" #Algunas obs de haselevator están marcadas como NA, se asumen como no


# a2. Convertir variables booleanas en dummies (0/1)
binary_columns <- c('hasparkingspace', 'hasbalcony', 'haselevator', 'hassecurity', 'hasstorageroom')

for(col in binary_columns) {
  datos[[col]] <- ifelse(datos[[col]] == "yes", 1, 0)
}

#a3 Crear variable en función del último dígito
datos$ultimo_digito <- datos$area %% 10
# Crear variables dummy para cada dígito (0-9)
for(digit in 0:9) {
  datos[paste0("end_", digit)] <- as.integer(datos$ultimo_digito == digit)
}

datos$ultimo_digito <- NULL     # Eliminar la columna temporal datos$ultimo_digito











#3b Estimacion del modelo lineal
#Hacemos un vector para simplicar la regresión :D

Vector <- paste(
  "price ~",  # Variables de último dígito (excluyendo end_9 como base)
  "end_0 + end_1 + end_2 + end_3 + end_4 + end_5 + end_6 + end_7 + end_8 +", # Variables de área
  "area + area2 +", # Distancias a puntos de interés (ajusta según tus columnas)
  "kindergartendistance + restaurantdistance + collegedistance + pharmacydistance +", # Variables binarias
  "hasparkingspace + hasbalcony + haselevator + hassecurity + hasstorageroom +",# Variables categóricas
  "month + type + rooms + ownership + buildingmaterial")

#Realizar la regresion
reg3b <- lm(Vector, data = datos)

#Imprimir los resultados de nuestra regresion (reg3b)
summary_reg3b <- summary(reg3b)
print(summary_reg3b)

#Exportar resultados de mi regresion como csv

resultados_reg3b <- data.frame(
  Variable = names(coef(reg3b)),
  Coeficiente = coef(reg3b),
  Error_Estándar = summary(reg3b)$coefficients[, 2],
  Valor_t = summary(reg3b)$coefficients[, 3],
  Valor_p = summary(reg3b)$coefficients[, 4]
)

# Exportar a CSV
write.csv(resultados_reg3b, "resultados_resultados_reg3b.csv", row.names = FALSE)

#Interpretacion del coeficiente "end_0" dentro de la regresion (se usa cat para que no se trunque el comentario)
cat("El coeficiente de la variable end_0 es estadísticamente significativo (p-value = 0.0000718) y positivo, indicando que, manteniendo constantes todas las demás variables del modelo, las viviendas con áreas que terminan en el dígito 0 tienen un precio aproximadamente 26,342 unidades monetarias mayor en comparación con aquellas que terminan en 9 (categoría base). Este resultado sugiere la existencia de un efecto psicológico o de percepción en el mercado, donde los valores redondos (como los que terminan en 0) pueden ser asociados con propiedades más premium, lo que se refleja en un precio significativamente superior incluso después de controlar por características físicas, de ubicación y otros.")

#####    Regresion particionada para end_0
#llamar librerias (por si acaso, para que no nos salga errores)
library(dplyr)
library(broom)

#a) regresion de end_0
lm_end_0 <- lm(
  end_0 ~ end_1 + end_2 + end_3 + end_4 + end_5 + end_6 + end_7 + end_8 +
    area + area2 +
    kindergartendistance + restaurantdistance + collegedistance + pharmacydistance +
    hasparkingspace + hasbalcony + haselevator + hassecurity + hasstorageroom +
    month + type + rooms + ownership + buildingmaterial,
  data = datos)

residuos_end0 <- residuals(lm_end_0) #Obtenemos los residuos de end_0

#b) Regresion para el precio
lm_precio <- lm(
  price ~ end_1 + end_2 + end_3 + end_4 + end_5 + end_6 + end_7 + end_8 +
    area + area2 +
    kindergartendistance + restaurantdistance + collegedistance + pharmacydistance +
    hasparkingspace + hasbalcony + haselevator + hassecurity + hasstorageroom +
    month + type + rooms + ownership + buildingmaterial,
  data = datos)
  
residuos_precio <- residuals(lm_precio) #Obtenemos los residuos de end_0

#c) Regresion de los residuos (esta es la regresion del partialling-out)
lm_residuos <- lm(
  residuos_precio ~ residuos_end0,
  data = datos
)
#Mostrar los resultados de la reg parcial

#d) Comparacion con la regresion original
coef_end_0 <- coef(reg3b)["end_0"] #Extraer el coeficiente de end_0 de la regresion original para compararlo
print(coef_end_0)
cat("COMPARACIÓN DE COEFICIENTES")
cat("Coeficiente de end_0 en regresión completa:", coef_end_0)
cat("Coeficiente de partialling-out:", coef(lm_residuos)["residuos_end0"])
cat("¿Son iguales?", coef_end_0,coef(lm_residuos)["residuos_end0"],"
    LO SON")













#3c ¿El terminar en 0 implica que el departamento sea premium? ¿Ello otorga una prima?

#c1 Entrenar el modelo
df <- datos #Creamos un nuevo data base para ser más organizados
#Iniciamos el modelo entonces, primero:
df <- df[df$end_0 == 0 | is.na(df$end_0), ]  # Excluir áreas que terminan en 0


Vector2 <- paste(
  "price ~",  # Variables de último dígito (excluyendo end_9 como base)
  "end_1 + end_2 + end_3 + end_4 + end_5 + end_6 + end_7 + end_8 +", # Variables de área
  "area + area2 +", # Distancias a puntos de interés (ajusta según tus columnas)
  "kindergartendistance + restaurantdistance + collegedistance + pharmacydistance +", # Variables binarias
  "hasparkingspace + hasbalcony + haselevator + hassecurity + hasstorageroom +",# Variables categóricas
  "month + type + rooms + ownership + buildingmaterial")

reg_c1 <- lm(Vector2, data = df) #Esta regresion es muy parecida a la primera realiza durante el ej, pero no incluye a los dep cuya area termina en 0

#Imprimir los resultados de nuestra regresion (regc1)
summary_reg_c1 <- summary(reg_c1)
print(summary_reg_c1)

#Exportar resultados de esta regresion como csv

resultados_reg_c1 <- data.frame(
  Variable = names(coef(reg_c1)),
  Coeficiente = coef(reg_c1),
  Error_Estándar = summary(reg_c1)$coefficients[, 2],
  Valor_t = summary(reg_c1)$coefficients[, 3],
  Valor_p = summary(reg_c1)$coefficients[, 4]
)

# Exportar a CSV
write.csv(resultados_reg_c1, "resultados_reg_c1.csv", row.names = FALSE)



#c2 Probar el modelo y tratar de predecir precios
datos$precio_predicho <- predict(reg_c1, newdata = datos)
#Se puede crear una base comparativa de los precios reales y los predichos
comparativa <- data.frame(datos$id,datos$price,datos$precio_predicho)
#OJO: Cuando el tipo de material de construccion es NA, el modelo tmb manda NA


#c3 Comparar los promedios de los precios (reales y predichos)

#Creamos una nueva base por orden
comparativa2 <- comparativa
comparativa2$end_0 <- datos$end_0
comparativa2 <- comparativa2[comparativa2$end_0 == 1,]

summary(comparativa2) #Extraemos los valores promedio de las variables de precios y precios predichos
cat("Precio promedio real:", mean(comparativa2$datos.price))
#Con el summary se sabe que el precio promedio predicho es 858420
cat("Precio precio promedio predicho: 858420")


#Comentarios finales sobre los resultados del modelo de Machine Learning
cat("En mi opinion, y a traves de mis resultados, puedo indicar que el hecho de que un departamento
    tenga su area redonda (que termine en 0), no está afectando o sobreestimando su mando. Digo
    aquello puesto que la comparativa final gracias al modelo empleado mando un precio promedio no muy
    distante del real, e incluso siendo un poco inferior al mismo.")
