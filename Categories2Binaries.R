Categories2Binaries <- function(data) {
  for (col in names(data)) {
    unique_values <- unique(data[[col]])
    if (all(unique_values %in% c(0, 1, NA))) {
      data[[col]] <- as.integer(data[[col]])
    }
  }
  
  categorical_vars <- sapply(data, is.character)
  
  # Crear nuevas variables binarias para cada categoría
  for (var in names(data)[categorical_vars]) {
    categories <- unique(data[[var]])
    
    for (category in categories) {
      new_var <- paste0(var, "_", category)
      data[[new_var]] <- as.integer(data[[var]] == category)
    }
  }
  
  # Borrar las variables categóricas originales
  data <- data[, !categorical_vars]
  
  return(data)
}