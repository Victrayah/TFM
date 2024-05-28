# Función para encontrar el valor más repetido o indicar indecisión
get_most_frequent_or_tie <- function(row) {
  # Contar la frecuencia de cada valor en la fila, excluyendo NA
  counts <- sort(table(row, useNA = "no"), decreasing = TRUE)
  # Si la fila solo tiene NA, retornar NA
  if(length(counts) == 0) { 
    return(NA)
  }
  # Si hay un empate en el máximo, indicar indeciso
  if(length(counts[counts == max(counts)]) > 1) {
    return(paste("indeciso", paste(names(counts)[counts == max(counts)], collapse = "+")))
  } else {
    # De lo contrario, devolver el valor más frecuente
    return(names(which.max(counts)))
  }
}

# Función para obtener el valor más frecuente si cumple con un porcentaje deseado
get_value_if_desired_percent <- function(row, desired_percentage) {
  percentage_threshold <- desired_percentage / 100
  total = sum(row, na.rm = TRUE)
  max_count = max(row, na.rm = TRUE)
  percentage = max_count / total
  
  # Devolver el valor más frecuente si cumple con el umbral de porcentaje
  if(percentage >= percentage_threshold) {
    value_names = names(row)
    most_frequent = value_names[which.max(row)]
    return(most_frequent)
  } else {
    return(NA)
  }
}

# Función para calcular la distancia euclidiana entre centroides de dos experimentos
calcular_distancias <- function(centroides_exp1, centroides_exp2) {
  n1 <- nrow(centroides_exp1)
  n2 <- nrow(centroides_exp2)
  distancias <- matrix(nrow = n1, ncol = n2)
  
  # Calcular la distancia entre cada par de centroides
  for (i in 1:n1) {
    for (j in 1:n2) {
      distancias[i, j] <- sqrt(sum((centroides_exp1[i, ] - centroides_exp2[j, ])^2))
    }
  }
  
  return(distancias)
}

# Función para encontrar la asignación óptima usando el algoritmo Húngaro
encontrar_asignacion_optima <- function(distancias) {
  asignacion <- solve_LSAP(distancias) # Aplicar el algoritmo Húngaro
  return(asignacion)
}

# Función para calcular la media de los centroides dado una asignación óptima
calcular_media_centroides_df <- function(centroides_exp1, centroides_exp2, asignaciones, i) {
  if (!is.data.frame(centroides_exp1)) {
    centroides_exp1 <- as.data.frame(centroides_exp1)
  }
  if (!is.data.frame(centroides_exp2)) {
    centroides_exp2 <- as.data.frame(centroides_exp2)
  }
  
  centroids_cum <- data.frame(matrix(ncol = ncol(centroides_exp1), nrow = nrow(centroides_exp1)))
  
  # Calcular la media de los centroides según el valor de 'i'
  if (i == 1) {
    for (j in 1:nrow(centroides_exp1)) {
      centroids_cum[j, ] <- (centroides_exp1[asignacion_optima[j], ] + centroides_exp2[j, ]) / 2
    }
  } else {
    for (j in 1:nrow(centroides_exp1)) {
      centroids_cum[j, ] <- (centroides_exp1[asignacion_optima[j], ] + centroides_exp2[j, ]) / i
    }
  }
  colnames(centroids_cum) <- colnames(centroides_exp1)
  
  return(centroids_cum)
}

# Función para verificar si una pareja ya existe en una lista
pareja_existe <- function(lista, pareja) {
  for (item in lista) {
    if (all(sort(item) == sort(pareja))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para obtener el porcentaje del valor más frecuente en cada fila de 'x'
get_percentage <- function(x, k) {
  percentage <- c()
  for (i in 1:nrow(x)) {
    total = sum(x[i, 1:k], na.rm = TRUE)
    max_count = max(x[i, 1:k], na.rm = TRUE)
    percentage[i] = max_count / total
  }
  return(percentage)
}

# Función para asignar grupo en casos de indecisión
assign_tie_breaker <- function(value) {
  if(startsWith(value, "indeciso")) {
    # Extraer los valores en caso de indecisión
    values_in_tie <- strsplit(sub("indeciso ", "", value), "\\+")[[1]]
    # Contar cuántas veces aparece cada valor involucrado en el empate
    counts <- value_counts[names(value_counts) %in% values_in_tie]
    # Ordenar y obtener el valor con menos casos
    least_common_value <- names(counts)[which.min(counts)]
    return(least_common_value)
  } else {
    # Si no hay indecisión, mantener el valor original
    return(value)
  }
}






