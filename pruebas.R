library(dplyr)
library(ggalluvial)
library(tidyr)
library(reshape2)
library(stats)
library(clue)

# OPCION CALCULANDO CENTROIDES

num_experiments <- 50
bestk <- 7
set.seed(112) # Establecemos una semilla para la reproducibilidad de los resultados.

# Realizamos un análisis de componentes principales (PCA) sobre los datos escalados sin gráfica.
res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE, ncp = 3)
datosplot<-data.frame(res.pca$ind$coord) # Convertimos las coordenadas de los individuos a un dataframe para graficar.
bs <- bootstrap(datosplot, num_experiments, .8)
results_ordered_centroids <- list()

results_list <- list()
centroids_list <- list()

# Definir la función para calcular los centroides
calculateCentroids <- function(tsne_data, cluster_labels) {
  centroids <- aggregate(tsne_data, by=list(cluster=cluster_labels), FUN=mean)
  return(centroids[, -1]) # Eliminar la columna de etiquetas de cluster
}

# Definir la función para mapear los clusters
mapClusters <- function(centroids1, centroids2) {
  distance_matrix <- as.matrix(dist(rbind(centroids1, centroids2)))
  distance_matrix <- distance_matrix[1:nrow(centroids1), (nrow(centroids1)+1):(nrow(centroids1)+nrow(centroids2))]
  
  mapping <- apply(distance_matrix, 1, which.min)
  return(mapping)
}

for (k in bestk){
  results <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
  
  # Realizar los experimentos y calcular los centroides
  for (i in 1:num_experiments) {
    
    sample_rows <- sort(bs[[1]][[i]][["id"]])
    sample_data <- datosplot[sample_rows, ]
  
    dist_matrix <- dist(sample_data)
    
    resultsClustering <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(resultsClustering, k = bestk)
    
    # Guardar los resultados en la lista
    results_list[[i]] <- clusters
    results[sample_rows, i] <- clusters
    
    # Calcular y almacenar los centroides
    centroids_list[[i]] <- calculateCentroids(sample_data, clusters)
  }
  
  # Configurar los nombres de filas y columnas del dataframe de resultados.
  rownames(results) <- rownames(data_inputs)
  names(results) <- paste('Experimento-', 1:num_experiments)
  
  
  # Comparar y mapear los clusters entre la primera ejecución y todas las demás
  cluster_mappings <- list()
  nombres_experimentos <- names(results[1:length(results)])
  
  for (i in 1:num_experiments) {
    cluster_mappings[[i]] <- mapClusters(centroids_list[[1]], centroids_list[[i]])
  }
  
  # Función para aplicar el mapeo de grupos a los resultados de los experimentos.
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
  
  # Aplicar el mapeo a los resultados de cada experimento.
  df_mapped <- results
  for (i in 1:num_experiments){
    df_mapped <- applyMapping(df_mapped, cluster_mappings[[i]], i)
  }
  results_ordered_centroids[[k]] <- df_mapped
}



######################################################################
# OPCION CALCULANDO INDICE DE JACCARD ENTRE EXPERIMENTOS

calculateJaccardIndex <- function(df_clusters, exp1, exp2) {
  clusters_exp1 <- sort(unique(df_clusters[[exp1]]))
  clusters_exp2 <- sort(unique(df_clusters[[exp2]]))
  
  jaccard_matrix <- matrix(0, nrow=length(clusters_exp1), ncol=length(clusters_exp2))
  
  for (i in clusters_exp1) {
    patients_in_cluster_i <- df_clusters[[exp1]] == i
    
    for (j in clusters_exp2) {
      patients_in_cluster_j <- df_clusters[[exp2]] == j
      
      intersection <- sum(patients_in_cluster_i & patients_in_cluster_j)
      union <- sum(patients_in_cluster_i | patients_in_cluster_j)
      
      jaccard_index <- ifelse(union == 0, 0, intersection / union) # Evita división por cero
      jaccard_matrix[i, j] <- jaccard_index
    }
  }
  
  return(jaccard_matrix)
}



mapClustersHungarian <- function(jaccard_matrix) {
  # El algoritmo húngaro funciona minimizando el coste, por lo que convertimos la similitud (índice de Jaccard) 
  # en un coste restando los valores de similitud del valor máximo (1).
  cost_matrix <- 1 - jaccard_matrix
  # Usar el algoritmo húngaro para encontrar la asignación óptima
  assignment <- solve_LSAP(cost_matrix)
  
  return(assignment)
}


applyMapping <- function(df_clusters, mapping, experiment_column) {
  # Crear una copia para no modificar el DataFrame original
  df_mapped <- df_clusters
  
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

df_clusters <- resultados
mapping_list <- list()
experiment_names <- colnames(df_clusters)

for(w in 2:length(experiment_names)) {
  
  exp1 <- experiment_names[1]
  exp2 <- experiment_names[w]

  # Ejemplo de uso:
  jaccard_matrix <- calculateJaccardIndex(df_clusters, exp1, exp2)
  
  # Ejemplo de uso:
  mapping <- mapClustersHungarian(jaccard_matrix)
  mapping_list[[exp2]] <- mapping
  
  # Ejemplo de uso
  # Suponiendo que 'Experimento2' es la columna que quieres ajustar y que mapping es un vector nombrado
  df_clusters <- applyMapping(df_clusters, mapping, experiment_column = exp2)
}

results_df <- df_clusters

library(tidyverse)

# Convertir results_df a un tibble para trabajar mejor con tidyverse
results_df <- as_tibble(results_df, rownames = "Patient")

# Usar pivot_longer para "fundir" los datos
long_df <- results_df %>%
  pivot_longer(
    cols = -Patient, # Excluir la columna Patient para que no sea transformada
    names_to = "Experiment", # Nombre de la nueva columna para los nombres de las columnas antiguas
    values_to = "Group" # Nombre de la nueva columna para los valores
  )

# Ver las primeras filas del nuevo dataset
print(head(long_df))

df_alluvial2<-long_df
for (l in 1:num_experiments) {
  print(head(df_clusters[[l]], n = 41))
}

head(df_alluvial2)
# MONTAR ALUVIAL PLOT 2
ggplot(df_alluvial2, aes(x = Experiment,
                         stratum = Group,
                         alluvium = Patient,
                         fill = Group)) +
  geom_flow() +
  geom_stratum(alpha = 0.5) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  theme(legend.position = "none") +
  ggtitle("Plot ICU HCLUST data_inputs")

#################################################################################
# OPCION PERMUTACION

library(combinat)

applyMapping <- function(df_clusters, mapping, experiment_column) {
  # Crear una copia para no modificar el DataFrame original
  df_mapped <- df_clusters
  
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

# Función para calcular el índice de Jaccard
calculateJaccard <- function(exp1, exp2) {
  clusters_exp1 <- sort(unique(exp1))
  clusters_exp2 <- sort(unique(exp2))
  
  jaccard_matrix <- matrix(0, nrow=length(clusters_exp1), ncol=length(clusters_exp2))
  
  for (i in clusters_exp1) {
    patients_in_cluster_i <- exp1 == i
    
    for (j in clusters_exp2) {
      patients_in_cluster_j <- exp2 == j
      
      intersection <- sum(patients_in_cluster_i & patients_in_cluster_j)
      union <- sum(patients_in_cluster_i | patients_in_cluster_j)
      
      jaccard_index <- ifelse(union == 0, 0, intersection / union) # Evita división por cero
      jaccard_matrix[i, j] <- jaccard_index
      
    }
  }
  
  return(jaccard_index)
}

mapClustersHungarian <- function(jaccard_matrix) {
  # El algoritmo húngaro funciona minimizando el coste, por lo que convertimos la similitud (índice de Jaccard) 
  # en un coste restando los valores de similitud del valor máximo (1).
  cost_matrix <- 1 - jaccard_matrix
  # Usar el algoritmo húngaro para encontrar la asignación óptima
  assignment <- solve_LSAP(cost_matrix)
  
  return(assignment)
}


df_clusters <- resultados
mapping_list <- list()
experiment_names <- colnames(df_clusters)

for(w in 2:length(experiment_names)) {
  
  # Supongamos que df_clusters es tu DataFrame
  exp1_data <- resultados[[1]] # Datos del Experimento 1
  exp2_data <- resultados[[w]] # Datos del Experimento 2
  unique_groups <- unique(exp2_data)
  permutations <- permn(unique_groups)
  
  # Inicializar variables para almacenar el mejor resultado
  best_jaccard <- 0
  best_permutation <- NULL
  
  # Evaluar cada permutación
  for (perm in permutations) {
    permuted_data <- match(exp2_data, perm)
    jaccard_index <- calculateJaccard(exp1_data, permuted_data)
    
    if (jaccard_index > best_jaccard) {
      best_jaccard <- jaccard_index
      best_permutation <- perm
    }
  }
  mapping_list[[experiment_names[w]]] <- best_permutation
  
  df_clusters <- applyMapping(df_clusters, best_permutation, experiment_column = w)
}


################################################################################
library(parallel)
library(combinat)

# Supongamos que df_clusters es tu DataFrame
exp1_data <- resultados[[1]] # Datos del Experimento 1
exp2_data <- resultados[[2]] # Datos del Experimento 2
unique_groups <- unique(exp2_data)
permutations <- permn(unique_groups)

# Función para calcular el índice de Jaccard
calculateJaccard <- function(group1, group2) {
  intersection <- length(intersect(group1, group2))
  union <- length(union(group1, group2))
  return(intersection / union)
}

# Preparar el entorno paralelo
num_cores <- detectCores() - 1 # Dejar un núcleo libre para el sistema
cl <- makeCluster(num_cores)

# Exportar variables necesarias al entorno de los trabajadores
clusterExport(cl, varlist=c("exp1_data", "exp2_data", "calculateJaccard", "unique_groups"))

# Función paralela para evaluar un conjunto de permutaciones
evaluatePermutations <- function(permutations) {
  best_jaccard <- 0
  best_permutation <- NULL
  
  for (perm in permutations) {
    permuted_data <- match(exp2_data, perm)
    jaccard_index <- calculateJaccard(exp1_data, permuted_data)
    
    if (jaccard_index > best_jaccard) {
      best_jaccard <- jaccard_index
      best_permutation <- perm
    }
  }
  return(list(jaccard=best_jaccard, permutation=best_permutation))
}

# Dividir las permutaciones entre los núcleos
permutation_chunks <- split(permutations, cut(1:length(permutations), num_cores, labels = FALSE))

# Ejecutar en paralelo
results <- parLapply(cl, permutation_chunks, evaluatePermutations)

# Detener el cluster
stopCluster(cl)

# Encontrar el mejor resultado global
best_result <- do.call(what=rbind, args=results)
best_overall <- best_result[which.max(best_result[, "jaccard"]),]


################################################################################
# ARI
library(mclust)
library(combinat)

df_clusters <- resultados
mapping_list <- list()
experiment_names <- colnames(df_clusters)

for(w in 2:length(experiment_names)) {
    
  # Supongamos que df_clusters es tu DataFrame
  exp1_data <- resultados[[1]] # Datos del Experimento 1
  exp2_data <- resultados[[2]] # Datos del Experimento 2
  unique_groups <- unique(exp2_data)
  permutations <- permn(unique_groups)
  
  # Inicializar variables para almacenar el mejor resultado
  best_ari <- -1 # ARI puede ser negativo, así que empezamos con -1
  best_permutation <- NULL
  
  # Evaluar cada permutación
  for (perm in permutations) {
    permuted_data <- match(exp2_data, perm)
    ari_value <- adjustedRandIndex(exp1_data, permuted_data)
    
    if (ari_value > best_ari) {
      best_ari <- ari_value
      best_permutation <- perm
    }
  }
  
  mapping_list[[experiment_names[w]]] <- best_permutation
  
  df_clusters <- applyMapping(df_clusters, best_permutation, experiment_column = w)
}

#########################################

# Visualización de los clusters resultantes en un gráfico 3D.
sample_rows <- sort(bs[[1]][[1]][["id"]])
sample_data <- datosplot[sample_rows, ]
clusters <- results[[1]]
clusters <- na.omit(clusters)
sample_data <- cbind(sample_data, clusters)

fig <- plot_ly(
  type = 'scatter3d',
  mode = 'markers',
  showlegend = T
)
fig <- fig %>% add_trace(data= sample_data[, 1:3], x = sample_data$Dim.1, y = sample_data$Dim.2, z = sample_data$Dim.3,
                         color = sample_data$clusters,
                         name = sample_data$clusters,
                         legendgroup = sample_data$clusters,
                         marker = list(opacity = 0.7))
fig <- fig %>% layout(
  title = "Scatterplot PCA",
  scene = list(
    xaxis = list(title = 'Dim.1'),
    yaxis = list(title = 'Dim.2'),
    zaxis = list(title = 'Dim.3'),
    aspectratio = list(x = 1, y = 1, z = 1)
  )
)


###
# intento de utilizar más CPU
# Definimos el número de experimentos a realizar y el mejor número de clusters k.
num_experiments <- 1000
bestk =2:4 
set.seed(112) # Establecemos una semilla para la reproducibilidad de los resultados.

# Realizamos un análisis de componentes principales (PCA) sobre los datos escalados sin gráfica.
res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE,ncp = 10)
datosplot<-data.frame(res.pca$ind$coord) # Convertimos las coordenadas de los individuos a un dataframe para graficar.

source('jensen_shanon_distance.R')
#fig

########################################################################################################
# Realizar remuestreo bootstrap en el conjunto de datos.
bs <- bootstrap(datosplot, num_experiments, .8)
clusters_list <- list()

# Iterar sobre un conjunto predefinido de valores de 'k' para probar diferentes números de clusters.
library(parallel)

# Define la función que realizará el clustering para un valor de k dado
cluster_func <- function(k) {
  results_list <- list()
  # Crear un dataframe para almacenar los resultados de los experimentos.
  results <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
  
  # Realizar experimentos de remuestreo 'num_experiments' veces.
  for (i in 1:num_experiments) {
    
    # Obtener las filas muestreadas para el experimento actual.
    sample_rows <- sort(bs[[1]][[i]][["id"]])
    sample_data <- datosplot[sample_rows, ]
    
    ### Realizar el agrupamiento (clustering)
    
    # Calcular la matriz de distancias euclidianas entre las muestras.
    dist_matrix <- dist(sample_data, method = 'euclidean')
    
    # Realizar agrupamiento jerárquico y cortar el árbol para obtener 'k' grupos.
    resultsClustering <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(resultsClustering, k = k)
    
    # Almacenar los resultados del agrupamiento en la lista y el dataframe de resultados.
    results_list[[i]] <- clusters
    results[sample_rows, i] <- clusters
  }
  
  # Configurar los nombres de filas y columnas del dataframe de resultados.
  rownames(results) <- rownames(data_inputs)
  names(results) <- paste('Experimento-', 1:num_experiments)
  
  ## CALCULAR DISTRIBUCIONES Y DISTANCIA DE JS
  list_distributions <- apply(results, 2, convert_to_prob)
  
  # Calcular la distancia de Jensen-Shannon entre los grupos de cada experimento.
  nombres_experimentos <- names(results[1:length(results)])
  resultados_distancias <- list()
  
  for (exp in nombres_experimentos){
    grupos_exp <- sort(unique(results[[exp]]))
    matriz_distancias <- matrix(NA, nrow = length(grupos_exp), ncol = length(grupos_exp))
    for (i in grupos_exp){
      for (j in grupos_exp){
        # Calcular las medias y covarianzas para los grupos comparados.
        grupos_base <- which(results[[1]] == i)
        pacientes_agrupados_base <- datosplot[grupos_base, ]
        media_base <- matrix(colMeans(pacientes_agrupados_base), nrow = ncol(datosplot), ncol = 1)
        matriz_covarianzas_base <- cov(pacientes_agrupados_base, use = "complete.obs")
        
        grupos_comp <- which(results[[exp]]==j)
        pacientes_agrupados_comp <- datosplot[grupos_comp, ]
        media_comp <- matrix(colMeans(pacientes_agrupados_comp), nrow = ncol(datosplot), ncol = 1)
        matriz_covarianzas_comp <- cov(pacientes_agrupados_comp, use = "complete.obs")
        
        # Calcular la distancia de Jensen-Shannon entre los grupos.
        matriz_distancias[i,j] <- jensen_shannon_distance(media_base, media_comp, matriz_covarianzas_base, matriz_covarianzas_comp)
      }
    }
    resultados_distancias[[exp]] <- matriz_distancias
  }
  
  # Mapear los grupos más similares entre los diferentes experimentos.
  mapeo_final <- list()
  for (exp in nombres_experimentos){
    matriz_js <- resultados_distancias[[exp]]
    mapeo <- numeric(nrow(matriz_js))
    for (i in 1:nrow(matriz_js)) {
      mapeo[i] <- which.min(matriz_js[i, ])
    }
    mapeo_final[[exp]] <- mapeo
  }
  
  # Función para aplicar el mapeo de grupos a los resultados de los experimentos.
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
  
  # Aplicar el mapeo a los resultados de cada experimento.
  df_mapped <- results
  for (i in 1:num_experiments){
    df_mapped <- applyMapping(df_mapped, mapeo_final[[i]], i)
  }
  
  return(df_mapped)
}

num_cores <- detectCores() - 2

# Inicializa el clúster
cl <- makeCluster(num_cores)

# Exporta los objetos necesarios al entorno de los clústeres
clusterExport(cl, c("bestk", "datosplot", "cluster_func", "jensen_shannon_distance", "convert_to_prob","num_experiments","bs","data_inputs","calcular_inversa_o_pseudoinversa","calcular_determinante"))


# Ejecuta el bucle utilizando parLapply para paralelizar en Windows
clusters_list <- parLapply(cl, bestk, cluster_func)

# Cierra el clúster después de haber terminado
stopCluster(cl)

# Crea una lista para almacenar los dataframes generados fuera del bucle
df_list <- lapply(clusters_list, as.data.frame)


##########################################################################


df_counts_numeric_list <- list()
for (k in 2:3) {
  df_mappedF <- as.data.frame(lapply(df_list[[k]], factor),row.names = row.names(df_mapped))
  
  # Función para encontrar el valor más repetido o indicar indeciso
  # Crear un nuevo dataframe con los conteos
  df_counts <- as.data.frame(t(apply(df_mappedF, 1, function(row) {
    # Contar los valores únicos por fila, excluyendo NAs
    as.vector(table(factor(row, levels = unique(unlist(df_mappedF[!is.na(df_mappedF)]))), useNA = "no"))
  })))
  
  # Nombrar las columnas del nuevo df con los valores únicos (excluyendo NAs)
  colnames(df_counts) <- unique(unlist(df_mappedF[!is.na(df_mappedF)]))
  
  # Añadir una columna con el valor más repetido o indicar "indeciso"
  df_counts$MostFrequentValue <- apply(df_mappedF, 1, get_most_frequent_or_tie)
  
  # Asegurarse de que los rownames sean los mismos que en df_mappedF
  rownames(df_counts) <- rownames(df_mappedF)
  
  # Calcular la frecuencia de cada valor en MostFrequentValue
  value_counts <- table(df_counts$MostFrequentValue)
  
  # Función para asignar grupo en casos de indecisión
  
  # Aplicar la función para resolver indecisiones y crear una nueva variable
  df_counts$ResolvedValue <- sapply(df_counts$MostFrequentValue, assign_tie_breaker)
  
  #####################################
  # DEFINIR PACIENTES REPRESENTATIVOS #
  #####################################
  
  # Establecer el porcentaje deseado
  desired_percentage <- 90
  
  # Generar el nombre de la columna de manera que sea válido y sin espacios
  variable_name <- paste("Value", gsub("\\.", "_", as.character(desired_percentage)), "Percent", sep = "")
  
  # Asegurar que las columnas de conteo son numéricas
  df_counts_numeric <- df_counts[, 1:bestk]
  df_counts_numeric <- apply(df_counts_numeric, 2, as.numeric)
  
  # Función ajustada para calcular el valor según el porcentaje deseado
  
  # Aplicar la función a cada fila del dataframe numérico
  df_counts_numeric <- as.data.frame(df_counts_numeric)
  df_counts[[variable_name]] <- apply(df_counts_numeric, 1, get_value_if_desired_percent, desired_percentage)
  rownames(df_counts_numeric)<-rownames(df_counts)
  #
  
  df_counts_numeric_list[[k]] <- df_counts_numeric
  print("proceso terminado para k=")
  print(k)
}

df_listPC5_JS
df_counts_numeric_listPC5_JS<-df_counts_numeric_list

