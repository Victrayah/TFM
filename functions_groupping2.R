


calcular_distancias <- function(centroides_exp1, centroides_exp2) {
  # Inicializar la matriz de distancias con dimensiones adecuadas
  n1 <- nrow(centroides_exp1)
  n2 <- nrow(centroides_exp2)
  distancias <- matrix(nrow = n1, ncol = n2)
  
  # Calcular distancias entre cada par de centroides
  for (i in 1:n1) {
    for (j in 1:n2) {
      distancias[i, j] <- sqrt(sum((centroides_exp1[i, ] - centroides_exp2[j, ])^2))
    }
  }
  
  # Devolver la matriz de distancias
  return(distancias)
}

encontrar_asignacion_optima <- function(distancias) {
  # Aplicar el algoritmo Húngaro
  asignacion <- solve_LSAP(distancias)
  
  # Devolver los índices de los grupos en 'exp1' asignados a cada grupo en 'exp2'
  return(asignacion)
}

calcular_media_centroides_df <- function(centroides_exp1, centroides_exp2, asignaciones, i) {
  # Asegurarse de que los centroides sean data.frames o convertirlos si son matrices
  if (!is.data.frame(centroides_exp1)) {
    centroides_exp1 <- as.data.frame(centroides_exp1)
  }
  if (!is.data.frame(centroides_exp2)) {
    centroides_exp2 <- as.data.frame(centroides_exp2)
  }
  
  # Inicializar un data.frame para almacenar las medias de los centroides
  centroids_cum <- data.frame(matrix(ncol = ncol(centroides_exp1), nrow = nrow(centroides_exp1)))
  
  if (i == 1) {
  # Calcular la media para cada pareja de centroides y almacenarla en el data.frame
  for (j in 1:nrow(centroides_exp1)) {
    centroids_cum[j, ] <- (centroides_exp1[asignacion_optima[j], ] + centroides_exp2[j, ]) / 2
  }
  }else{
  # Calcular la media para cada pareja de centroides y almacenarla en el data.frame
  for (j in 1:nrow(centroides_exp1)) {
    centroids_cum[j, ] <- (centroides_exp1[asignacion_optima[j], ] + centroides_exp2[j, ]) / i
  }
  }
  # Asignar nombres de columnas adecuados
  colnames(centroids_cum) <- colnames(centroides_exp1)
  
  return(centroids_cum)
}

# Función para verificar si una pareja ya existe en la lista, independientemente del orden
pareja_existe <- function(lista, pareja) {
  for (item in lista) {
    if (all(sort(item) == sort(pareja))) {
      return(TRUE)
    }
  }
  return(FALSE)
}






















