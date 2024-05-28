# Definir función para calcular la inversa o pseudoinversa de una matriz.
calcular_inversa_o_pseudoinversa <- function(A) {
  # Intentar calcular la inversa utilizando la función solve().
  # La función tryCatch() maneja cualquier error que pueda surgir al intentar calcular la inversa.
  inversa <- tryCatch({
    solve(A)  # Intenta calcular la inversa de A.
  }, error = function(e) {
    # En caso de error (por ejemplo, si la matriz es singular y no tiene inversa),
    # se captura el error y se devuelve NULL para indicar el fallo.
    NULL
  })
  
  # Verificar si el intento de calcular la inversa falló, devolviendo NULL.
  if (is.null(inversa)) {
    # Si solve() falló porque la matriz es singular, se calcula la pseudoinversa.
    # Se usa la función ginv() del paquete MASS para calcular la pseudoinversa de A.
    inversa <- MASS::ginv(A)
    # Mensaje indicando que se calculó la pseudoinversa.
    #cat("La matriz es singular, se calculó la pseudoinversa.\n")
  } else {
    # Si la inversa fue calculada exitosamente, imprimir un mensaje indicándolo.
    #cat("Se calculó la inversa de la matriz.\n")
  }
  
  # Devolver la inversa o la pseudoinversa de la matriz.
  return(inversa)
}

# Definir una función para calcular el determinante de una matriz de covarianza.
calcular_determinante <- function(mat_cov){
  # Calcular el rango de la matriz utilizando la descomposición QR.
  # qr() devuelve una lista cuyo componente 'rank' indica el rango estimado de la matriz.
  rango <- qr(mat_cov)$rank
  
  # Comparar el rango de la matriz con su dimensión.
  # Si el rango es igual a la cantidad de filas (o columnas, ya que es cuadrada),
  # entonces la matriz es de rango completo y se puede calcular el determinante.
  if (rango == dim(mat_cov)[1]){
    determinante <- det(mat_cov)  # Calcular el determinante con la función det().
  } else if (rango < dim(mat_cov)[1]){
    # Si el rango es menor que la dimensión de la matriz,
    # esto implica que la matriz es singular y su determinante es cero.
    determinante <- 0
  }
  # Devolver el valor del determinante calculado (o cero si la matriz es singular).
  return(determinante)
}

# Definir una función para calcular la distancia de Jensen-Shannon entre dos distribuciones.
jensen_shannon_distance <- function(media1, media2, mat_cov1, mat_cov2){
  # Calcular la media conjunta de las dos distribuciones.
  media_conjunta <- matrix((media1 + media2) / 2, nrow = length(media1), ncol = 1)
  
  # Calcular la matriz de covarianza conjunta como el promedio de las dos matrices de covarianza.
  cov_conjunta <- (mat_cov1 + mat_cov2) / 2
  
  # Calcular la inversa (o pseudoinversa) de la matriz de covarianza conjunta.
  inversa <- calcular_inversa_o_pseudoinversa(cov_conjunta)
  
  # Calcular los determinantes de las matrices de covarianza individuales y conjunta.
  det_1 <- calcular_determinante(mat_cov1)
  det_2 <- calcular_determinante(mat_cov2)
  det_conjunto <- calcular_determinante(cov_conjunta)
  
  # Calcular la divergencia KL de la primera distribución a la distribución conjunta.
  KL1 <- 1/2 * (log(det_conjunto / det_1) - dim(mat_cov1)[1] + round(sum(diag(inversa %*% mat_cov1)))
                + t(media_conjunta - media1) %*% inversa %*% (media_conjunta - media1))
  
  # Calcular la divergencia KL de la segunda distribución a la distribución conjunta.
  KL2 <- 1/2 * (log(det_conjunto / det_2) - dim(mat_cov2)[1] + round(sum(diag(inversa %*% mat_cov2)))
                + t(media_conjunta - media2) %*% inversa %*% (media_conjunta - media2))
  
  # Verificar si los valores de KL1 y KL2 son válidos y calcular la distancia de Jensen-Shannon.
  if (!is.na(KL1) && !is.na(KL2) && KL1 == Inf && KL2 == Inf) {
    JS <- 0
  } else if (!is.na(KL1) && !is.na(KL2)) {
    JS <- sqrt(1/2 * KL1 + 1/2 * KL2)
  } else {
    JS <- NA  # Manejar el caso de valores no disponibles (NA) de otra manera adecuada.
  }
  
  # Devolver la distancia de Jensen-Shannon calculada.
  return(JS)
}

## Funciones Scatters_post_JS

# Función para encontrar el valor más repetido o indicar indeciso
get_most_frequent_or_tie <- function(row) {
  # Ordenar y contar los valores, excluyendo NAs
  counts <- sort(table(row, useNA = "no"), decreasing = TRUE)
  if(length(counts) == 0) { # Si solo hay NAs
    return(NA)
  }
  # Si el máximo se repite, indicamos indeciso
  if(length(counts[counts == max(counts)]) > 1) {
    return(paste("indeciso", paste(names(counts)[counts == max(counts)], collapse = "+")))
  } else {
    # De lo contrario, devolvemos el valor más frecuente
    return(names(which.max(counts)))
  }
}


get_value_if_desired_percent <- function(row, desired_percentage) {
  percentage_threshold <- desired_percentage / 100
  total = sum(row, na.rm = TRUE)
  max_count = max(row, na.rm = TRUE)
  percentage = max_count / total
  
  if(percentage >= percentage_threshold) {
    value_names = names(row)
    most_frequent = value_names[which.max(row)]
    return(most_frequent)
  } else {
    return(NA)
  }
}


get_percentage <- function(x,k) {
  percentage <- c()
  for (i in 1:nrow(x)) {
    total = sum(x[i,1:k], na.rm = TRUE)
    max_count = max(x[i,1:k], na.rm = TRUE)
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

##
## CENTROIDS

# Definir la función para calcular los centroides
calculateCentroids <- function(data, cluster_labels) {
  centroids <- aggregate(data, by=list(cluster=cluster_labels), FUN=mean)
  return(centroids[, -1]) # Eliminar la columna de etiquetas de cluster
}

# Definir la función para mapear los clusters
mapClusters <- function(centroids1, centroids2) {
  distance_matrix <- as.matrix(dist(rbind(centroids1, centroids2)))
  distance_matrix <- distance_matrix[1:nrow(centroids1), (nrow(centroids1)+1):(nrow(centroids1)+nrow(centroids2))]
  
  mapping <- apply(distance_matrix, 1, which.min)
  return(mapping)
}

applyMapping <- function(df_mapped, mapping, experiment_column) {
  
  # Aplicar el mapeo a cada fila
  for (m in 1:nrow(df_mapped)) {
    original_cluster <- df_mapped[m, experiment_column]
    # Revisa si el cluster original está en el mapeo
    if (original_cluster %in% mapping) {
      df_mapped[m, experiment_column] <- mapping[original_cluster]
    }
  }
  
  return(df_mapped)
}

## Otras funciones

# Convertir las asignaciones de clusters en probabilidades relativas.
convert_to_prob <- function(column) {
  table(column) / sum(!is.na(column))
}

# Creamos un gráfico 3D interactivo de dispersión para visualizar los resultados del PCA.
fig <- plot_ly(
  type = 'scatter3d',
  mode = 'markers',
  showlegend = T
)
fig <- fig %>% add_trace(data= datosplot, x = datosplot$Dim.1, y = datosplot$Dim.2, z = datosplot$Dim.3)
fig <- fig %>% layout(
  title = "Scatterplot PCA",
  scene = list(
    xaxis = list(title = 'Dim.1'),
    yaxis = list(title = 'Dim.2'),
    zaxis = list(title = 'Dim.3'),
    aspectratio = list(x = 1, y = 1, z = 1)
  )
)