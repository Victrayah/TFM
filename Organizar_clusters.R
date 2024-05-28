

# Cargamos scripts externos que contienen funciones y datos necesarios.
source("Main9.R") 
source("spain_outputs_PCAv5.R")
source('functions_groupping.R')

# Definimos el número de experimentos a realizar y el mejor número de clusters k.
num_experiments <- 10
bestk =5
set.seed(112) # Establecemos una semilla para la reproducibilidad de los resultados.

# Realizamos un análisis de componentes principales (PCA) sobre los datos escalados sin gráfica.
res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE,ncp = 25)
datosplot<-data.frame(res.pca$ind$coord) # Convertimos las coordenadas de los individuos a un dataframe para graficar.


fig

########################################################################################################
# Realizar remuestreo bootstrap en el conjunto de datos.
bs <- bootstrap(datosplot, num_experiments, .8)
clusters_list <- list()

# Iterar sobre un conjunto predefinido de valores de 'k' para probar diferentes números de clusters.
for (k in bestk){
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
  clusters_list[[k]] <- df_mapped
  print("Clustering terminado para k =")
  print(k)
}


########################################################################################
# Crear una lista para almacenar los dataframes generados fuera del bucle
df_list <- list()

# Iterar sobre cada elemento de clusters_list
for (k in bestk) {
  # Convertir clusters_list[[k]] a dataframe y almacenarlo en df_list
  df_list[[k]] <- as.data.frame(clusters_list[[k]])
}

### GUARDAR OBJETOS GRANDES
#df_listPC25_JS <-df_list

beep(sound = 3)

l=5
count_not_7 <- 0  # Inicializar contador

for (i in 1:num_experiments) {
  column_name <- paste("Experimento-",i,sep = " ")
  n <- length(unique(df_list[[l]][[i]]))
  
  if (n != l+1) {
    count_not_7 <- count_not_7 + 1  # Aumentar contador
    print(paste("El número de valores únicos para el", column_name, "no es igual a 3."))
  }
}

count_not_7

