
# Inicializamos las columnas para los resultados
df_counts$p_value <- NA
df_counts$success_rate <- NA
df_counts$chi_sq_p_value <- NA

for(i in 1:nrow(df_counts)) {
  # Extraemos los dos grupos más prevalentes para la fila actual
  prevalences <- unlist(df_counts[i, 1:k])  # Convertimos el data frame a un vector
  # Tomamos los dos valores más altos, asegurándonos de manejar NAs correctamente
  max_values <- head(sort(prevalences, decreasing = TRUE), 2)
  # Si hay menos de dos valores no-NA, completamos con 0
  if(length(max_values) < 2) {
    max_values <- c(max_values, 0)
  }
  # Calculamos el número total de asignaciones
  n <- sum(prevalences) # HIPOTESIS B y C
  # Extraemos el número de asignaciones a los dos grupos más prevalentes
  #x <- max_values         # HIPOTESIS B
  m<-n-max_values[1]     # HIPOTESIS C
  x<-c(max_values[1],m)  # HIPOTESIS C
  # Realizamos el test binomial
  if (n > 0) {
    test <- binom.test(x, n, alternative = "greater")
    # Guardamos los resultados en el dataframe
    df_counts$p_value[i] <- test$p.value
    df_counts$success_rate[i] <- max(max_values) / n
  } else {
    df_counts$p_value[i] <- NA
    df_counts$success_rate[i] <- NA
  }
}


for(i in 1:nrow(df_counts)) {
  # Frecuencias observadas: asignaciones a cada grupo para el paciente i
  observed <- df_counts[i, 1:k]
  
  # Frecuencias esperadas: total de asignaciones dividido por el número de grupos
  total_assignments <- sum(observed)
  expected <- rep(total_assignments / k, k)
  
  # Realizamos el test de chi-cuadrado
  test <- chisq.test(x = observed, p = expected, rescale.p = TRUE)
  
  # Guardamos el valor p en el dataframe
  df_counts$chi_sq_p_value[i] <- test$p.value
}

















