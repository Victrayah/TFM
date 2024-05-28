informe_datos <- function(data) {
  # Verificar variables con NAs
  vars_na <- colnames(data)[colSums(is.na(data)) > 0]
  
  if (length(vars_na) > 0) {
    cat("Variables con valores faltantes (NAs):\n")
    for (var in vars_na) {
      cat(paste("- ", var, "\n"))
    }
    
    # Informar cantidad de NAs por variable
    cat("\nCantidad de NAs por variable:\n")
    for (var in vars_na) {
      cat(paste("- ", var, ": ", sum(is.na(data[[var]])), "\n"))
    }
  } else {
    cat("No hay variables con valores faltantes (NAs).\n")
  }
  
  # Tabla de frecuencias de cada variable
  cat("\nTabla de frecuencias de cada variable:\n")
  for (var in colnames(data)) {
    freq_table <- table(data[[var]])
    cat(paste("- ", var, ":\n"))
    print(freq_table)
    cat("\n")
  }
}

