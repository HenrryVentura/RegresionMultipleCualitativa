#'Regresion Multiple Cualitativa
#'
#'Realiza una regresion lineal multiple con datos cualitativos.
#'
#'@param data (archivo de datos)
#'@return Lista con coeficientes de regresion, error estandar, r ajustado y r cuadrado.
#'@param Xn (valores para prediccion).
#'@param ajuste (modelo obtenido para prediccion).
#'@export
RLM_qual<- function(data) {
  # Crear variable dummy
  dummy <- model.matrix(~.-1, data = data)
  dummy <- dummy[, -1]
  # Crear matriz de dise?o X y vector de respuesta Y
  X <- as.matrix(dummy)
  Y <- data[, 1]  # Suponiendo que la variable de respuesta esta en la primera columna
  # Calcular XtX, su inversa y XtY
  XtX <- t(X) %*% X
  inv_XtX <- solve(XtX)
  XtY <- t(X) %*% Y
  # Calcular coeficientes de regresi?n
  betas <- inv_XtX %*% XtY
  # Valores de prediccion
  y_pred <- X %*% betas
  # errores Residuales
  error <- Y - y_pred
  # Residual de varianza
  var_residual <- sum(error^2) / (length(Y) - ncol(X))
  # Covarianza de la matriz de beta
  cov_beta <- var_residual * (solve(t(X) %*% X))
  # Estandarizar errores de los coeficientes de beta
  se_beta <- sqrt(diag(cov_beta))
  # t-valores de coeficientes de beta
  t_beta <- betas / se_beta
  p_beta <- 2 * pt(abs(t_beta), df = length(Y) - ncol(X), lower.tail = FALSE)
  # Generar tabla de resultados
  results <- data.frame(coeficientes = betas, error_estandar = se_beta, t = t_beta, p_valor = p_beta)
  row.names(results) <- c("intercepto", colnames(X)[-1])
  # Calcular medidas de ajuste
  Minimo <- min(error)
  Maximo <- max(error)
  RSE <- sqrt(var_residual)
  SCE <- sum(error^2)
  SST <- sum((Y - mean(Y))^2)
  R_cuadrado <- 1 - (SCE / SST)
  R_cuadrado_ajustado <- 1 - ((1 - R_cuadrado) * ((length(Y) - 1) / (length(Y) - ncol(X) - 1)))

  valores <- list(
    Minimo = Minimo,
    Maximo = Maximo,
    Residual_Standard_error = RSE,
    R_cuadrado = R_cuadrado,
    Ajustada_R_cuadrado = R_cuadrado_ajustado,
    resultados = results
  )
  return(valores)
}




