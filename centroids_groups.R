# Cargamos scripts externos que contienen funciones y datos necesarios.

source("Main9.R") 
source("spain_outputs_PCAv5.R")
source('functions_groupping.R')

# Definimos el número de experimentos a realizar y el mejor número de clusters k.
num_experiments <- 10
bestk = 4:6
set.seed(112) # Establecemos una semilla para la reproducibilidad de los resultados.

# Realizamos un análisis de componentes principales (PCA) sobre los datos escalados sin gráfica.
res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE, ncp = 7)
datosplot<-data.frame(res.pca$ind$coord) # Convertimos las coordenadas de los individuos a un dataframe para graficar.


########################################################################################################
# Configuración inicial ##################################################
bootstrapYES <- T  # Cambiar a FALSE para no usar bootstrap              #
hclustYES <- T     # Cambiar a FALSE para usar kmeans                    #
centroid_chain<-F  # Cambiar a FALSE para que todos se comparen con exp1 #
##########################################################################

bs <- bootstrap(datosplot, num_experiments, 0.8)
clusters_list <- list()
clusters_list_POST<- list()
centroids_list <- list()

for (k in bestk){
  results_list <- list()
  centroids_experiment_list <- list()
  results <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
  row.names(results) <- row.names(datosplot) # Asegurarse de que results tenga los mismos nombres de fila que datosplot
  
  for (i in 1:num_experiments) {
    
    ### MUESTREO: BOOTSTRAP O 100% MUESTRA
    if (bootstrapYES) {
      sample_rows <- sort(bs[[1]][[i]][["id"]])
      sample_data <- datosplot[sample_rows, ]
    } else {
      sample_data <- datosplot
      sample_rows <- 1:nrow(datosplot) # Todos los índices se utilizan si no es bootstrap
    }
    
    ### CLUSTERING: KMEANS O HCLUST
    if (hclustYES) {
      dist_matrix <- dist(sample_data, method = "euclidean")
      hc <- hclust(dist_matrix, method = "ward.D2")
      clusters <- cutree(hc, k = k)
      centroids <- aggregate(sample_data, by=list(cluster=clusters), FUN=mean)
      centroids <- centroids[,-1]  # Eliminar la columna de cluster
    } else {
      kmeans_result <- kmeans(sample_data, centers = k, nstart = 25)
      clusters <- kmeans_result$cluster
      centroids <- kmeans_result$centers
    }
    
    ### CREAR UN VECTOR DE RESULTADOS CON LONGITUD IGUAL A DATOSPLOT Y NAs PARA LAS FILAS NO USADAS
    full_results <- rep(NA, nrow(datosplot))
    names(full_results) <- row.names(datosplot)
    full_results[sample_rows] <- clusters
    
    ### ALMACENAR RESULTADOS
    results_list[[i]] <- full_results
    centroids_experiment_list[[i]] <- centroids
    results[sample_rows, i] <- clusters
  }
  
  clusters_list[[k]] <- results_list
  centroids_list[[k]] <- centroids_experiment_list
  

###########################################################################################

### A) COMPARACIÓN DE CENTROIDES ENCADENADO
if (centroid_chain == T) {

## DETECTAR CUÁL ES LA MEJOR COMBINACIÓN DE ASIGNACIÓN DE GRUPO SEGÚN LOS CENTROIDES  

# Asumiendo la misma estructura de datos que antes
centroides_exp1 <- centroids_list[[k]][[1]] # Centroides de los grupos del experimento 1
centroides_exp2 <- centroids_list[[k]][[2]] # Centroides de los grupos del experimento 2

# Inicializar una matriz para almacenar las distancias entre los grupos de los dos experimentos
distancias <- matrix(nrow = nrow(centroides_exp1), ncol = nrow(centroides_exp2))

# Calcular todas las distancias
for (i in 1:nrow(centroides_exp1)) {
  for (j in 1:nrow(centroides_exp2)) {
    distancias[i, j] <- sqrt(sum((centroides_exp1[i, ] - centroides_exp2[j, ])^2))
  }
}

# Inicializar un dataframe para almacenar el grupo más similar y la distancia
resultados_similares <- data.frame(Grupo_Exp1 = integer(),
                                   Grupo_Exp2_Mas_Similar = integer(),
                                   Distancia_Minima = numeric())

# Copiar la matriz de distancias para manipularla
distancias_mod <- distancias

# Iterar sobre cada grupo del experimento 1
for (i in 1:nrow(centroides_exp1)) {
  # Encontrar el grupo más similar y la distancia mínima para el grupo actual del experimento 1
  min_dist <- min(distancias_mod[i,])
  grupo_similar <- which.min(distancias_mod[i,])
  
  # Añadir al dataframe
  resultados_similares[nrow(resultados_similares) + 1, ] <- c(i, grupo_similar, min_dist)
  
  # Asegurar que este grupo del experimento 2 no sea seleccionado de nuevo como el más cercano al mismo nivel de distancia
  # Poniendo NA en la columna correspondiente a este grupo en todas las filas
  distancias_mod[, grupo_similar] <- Inf
  
  # Reordenar el dataframe basado en la distancia mínima para asegurar la asignación correcta
  resultados_similares <- resultados_similares[order(resultados_similares$Distancia_Minima),]
}

# Reasignar los índices de los grupos más similares para que coincidan con los requerimientos
for (i in unique(resultados_similares$Grupo_Exp2_Mas_Similar)) {
  indices <- which(resultados_similares$Grupo_Exp2_Mas_Similar == i)
  if (length(indices) > 1) {
    distancias_minimas <- resultados_similares$Distancia_Minima[indices]
    indice_menor_distancia <- indices[which.min(distancias_minimas)]
    # Poner Inf en los demás grupos que no tienen la menor distancia
    for (j in indices) {
      if (j != indice_menor_distancia) {
        resultados_similares$Grupo_Exp2_Mas_Similar[j] <- NA
      }
    }
  }
}

# Imprimir los resultados finales
#print(resultados_similares)

### CAMBIOS DE ETIQUETA DE GRUPO

# Inicializar clusters_list_POST justo después de calcular clusters_list y centroids_list
clusters_list_POST <- clusters_list

# Función para calcular las distancias y reasignar los grupos
ajustar_grupos <- function(exp_num_anterior, exp_num_actual) {
  centroides_exp_anterior <- centroids_list[[k]][[exp_num_anterior]]
  centroides_exp_actual <- centroids_list[[k]][[exp_num_actual]]
  clusters_exp_actual <- clusters_list[[k]][[exp_num_actual]]
  
  # Ajustar los grupos basados en los centroides más cercanos
  clusters_exp_actual_post <- clusters_exp_actual # Esto se reemplaza con el nuevo código de ajuste
  
  # Actualizar clusters_list_POST con los grupos ajustados
  clusters_list_POST[[k]][[exp_num_actual]] <- clusters_exp_actual_post
}

# Aplicar la función iterativamente para ajustar los grupos de cada experimento en base al anterior
for (exp_num in 1:num_experiments) {
  ajustar_grupos(exp_num - 1, exp_num)
}
} else {
  
  ### B) COMPARAR CENTROIDES CON EXPERIMENTO 1
  # Inicializar clusters_list_POST justo después de calcular clusters_list y centroids_list
  clusters_list_POST[[k]] <- clusters_list[[k]]
  
  # Función para calcular las distancias y reasignar los grupos en base al Experimento 1
  ajustar_grupos_con_exp1 <- function(exp_num_actual) {
    centroides_exp_referencia <- centroids_list[[k]][[1]]  # Centroides del Experimento 1
    centroides_exp_actual <- centroids_list[[k]][[exp_num_actual]]
    clusters_exp_actual <- clusters_list[[k]][[exp_num_actual]]
    
    # Calcular las distancias entre los centroides del experimento actual y el de referencia
    distancias <- matrix(nrow = nrow(centroides_exp_referencia), ncol = nrow(centroides_exp_actual))
    for (i in 1:nrow(centroides_exp_referencia)) {
      for (j in 1:nrow(centroides_exp_actual)) {
        distancias[i, j] <- sqrt(sum((centroides_exp_referencia[i, ] - centroides_exp_actual[j, ])^2)) 
      }
    }
    
    # Encontrar los grupos más cercanos en el experimento de referencia para cada grupo del experimento actual
    grupo_mas_cercano <- apply(distancias, 2, which.min)
    
    # Ajustar los clusters del experimento actual basándose en el grupo más cercano encontrado
    clusters_exp_actual_post <- ifelse(is.na(clusters_exp_actual), NA, sapply(clusters_exp_actual, function(cluster) if (!is.na(cluster)) grupo_mas_cercano[cluster] else NA))
    
    # Actualizar clusters_list_POST con los grupos ajustados
    clusters_list_POST[[k]][[exp_num_actual]] <- clusters_exp_actual_post
  }
  
  # Aplicar la función iterativamente para ajustar los grupos de cada experimento en base al Experimento 1
  for (exp_num in 2:length(clusters_list_POST[[k]])) {  # Comienza en 2 porque el experimento 1 es el de referencia
    ajustar_grupos_con_exp1(exp_num)
  }
}
  print("Proceso terminado para k =")
  print(k)
}
  

beep(sound = 3)

# FIN

clusters_list_POST_7_PC<-clusters_list_POST

# EXTRA
#########################################################################
# # Inicializar df_mapped con el número correcto de filas y columnas apropiadas
# df_mapped <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
# names(df_mapped) <- paste("Experimento", 1:num_experiments, sep = "")
# 
# # Rellenar df_mapped con los datos de clusters_list_POST
# for (i in 1:num_experiments) {
#   # Cuando bootstrapYES es TRUE, inicializamos todas las entradas de ese experimento como NA
#   if (bootstrapYES) {
#     df_mapped[, i] <- NA
#   }
#   
#   # Extraemos los clusters mapeados para el experimento actual
#   clusters_actual <- clusters_list_POST[[k]][[i]]
#   
#   # Rellenamos df_mapped con los valores de clusters, teniendo en cuenta el muestreo bootstrap si aplica
#   if (bootstrapYES) {
#     # Solo asignamos valores a los índices muestreados, el resto permanece como NA
#     sampled_rows <- sort(bs[[1]][[i]][["id"]])
#     df_mapped[sampled_rows, i] <- clusters_actual
#   } else {
#     # Cuando no usamos bootstrap, asignamos directamente ya que todos los pacientes están presentes
#     df_mapped[, i] <- clusters_actual
#   }
# }


  
  
  
  
  
  
  
  
  
  








































