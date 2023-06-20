#'Regresion Multiple Cualitativa
#'
#'Realiza una prediccion de los datos.
#'
#'@param Xn (Datos con valores para predecir)
#'@param Ajuste (valores ajustados para hacer las predicciones)
#'@return Lista de valores de prediccion.
#'@export
prediccion <- function(Xn, ajuste) {
  # Crear las variables dummy
  dummy <- model.matrix(~.-1, data = Xn)
  X <- dummy[, -1]

  # Extraer las betas o coeficientes de predicción
  betas <- ajuste$resultados[, 1]

  # Crear la matriz de variables predictoras
  X <- as.matrix(cbind(1, X))

  # Calcular las predicciones
  y_predicciones <- X %*% betas
  return(y_predicciones)
}
