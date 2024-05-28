# setwd("D:/2023_icu_delirium")
# source("Procesamiento_datos.R")
# source("functions_groupping2.R")
# source("functions_groupping.R")

#####
# 1 # Primera fase: Hacemos experimentos
#####

################################# * Cogemos los datos del primer PCA, pero con los outliers quitados
datosplot<-clean_dfs$CP14Scores # Según los filtros de t2 hotelling. De esta forma, al cambiar CP,
################################# cambiamos los sujetos de los pacientes, pero los CPs tienen el mismo contenido de variabilidad.

# Control de tiempo de ejecución
# start_time <- Sys.time()

# Definimos el número de experimentos a realizar y el mejor número de clusters k.
num_experiments <- 10 
bestk = 3:6   
set.seed(112) # Establecemos una semilla para la reproducibilidad de los resultados.

# Bootstrap
bs <- bootstrap(datosplot, num_experiments, 0.8)

# Listas necesarias
clusters_list <- list()
centroids_list <- list()
centroids_cum_list<- list()
asignaciones<-list()


for (k in bestk){
  results_list <- list()
  centroids_experiment_list<-list()
  results <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
  row.names(results) <- row.names(datosplot) # Asegurarse de que results tenga los mismos nombres de fila que datosplot
  for (i in 1:num_experiments) {

    ### MUESTREO: BOOTSTRAP
    sample_rows <- sort(bs[[1]][[i]][["id"]])
    sample_data <- datosplot[sample_rows, ]

    ### CLUSTERING: HCLUST
    dist_matrix <- dist(sample_data, method = "euclidean")
    hc <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(hc, k = k)
    centroids <- aggregate(sample_data, by=list(cluster=clusters), FUN=mean)
    centroids <- centroids[,-1]  # Eliminar la columna de cluster

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

  ################################################################

  centroids_cum_list_i<- list()
  asignaciones_i<- list()

  for (i in 1:num_experiments) {
    if (i==1) {
      # Asumiendo la misma estructura de datos que antes
      centroides_exp1 <- centroids_list[[k]][[1]] # Centroides de los grupos del experimento 1
      centroides_exp2 <- centroids_list[[k]][[1]] # Centroides de los grupos del experimento 2
    } else {
      centroides_exp1 <- centroids_cum_list_i[[i-1]] # Centroides de los grupos del experimento 1
      centroides_exp2 <- centroids_list[[k]][[i]] # Centroides de los grupos del experimento 2
    }

    # Inicializar una matriz para almacenar las distancias entre los grupos de los dos experimentos
    distancias <- matrix(nrow = nrow(centroides_exp1), ncol = nrow(centroides_exp2))

    distancias<-calcular_distancias(centroides_exp1,centroides_exp2)

    asignacion_optima <- encontrar_asignacion_optima(distancias)

    asignacion_optima<-as.integer(asignacion_optima)

    centroids_cum <- calcular_media_centroides_df(centroides_exp1, centroides_exp2, asignaciones, i)

    centroids_cum_list_i[[i]]<-centroids_cum
    asignaciones_i[[i]]<-asignacion_optima

  }

  centroids_cum_list[[k]]<-centroids_cum_list_i
  asignaciones[[k]]<-asignaciones_i

  print("Primera fase completada de k = ")
  print(k)
}


#####
# 2 # Segunda fase: Cambiar etiquetas de grupo
#####

PREmap <- list()
POSTmap <- list()
# bestk=4

for (k in bestk){
  # df_mapped
  PREmap_i <- as.data.frame(clusters_list[[k]])
  nombres_columnas <- paste0("Experimento_", seq_along(PREmap_i))
  colnames(PREmap_i) <- nombres_columnas

  mapa<- as.data.frame(asignaciones[[k]])
  nombres_columnas <- paste0("Experimento_", seq_along(mapa))
  colnames(mapa) <- nombres_columnas

  POSTmap_i <- PREmap_i

  PREmap[[k]]<-PREmap_i

  reemplazos_i <- list()

  # Bucle a través de los experimentos
  for (i in 1:num_experiments) {
    # Lista temporal para almacenar los reemplazos de este experimento
    reemplazos_temp <- list()

    # Bucle a través de las filas de 'mapa'
    for (j in 1:nrow(mapa)) {
      # Encuentra el índice en 'mapa' para el reemplazo
      valor_pre_reemplazo <- j
      valor_post_reemplazo <- mapa[j, i]

      # Verificar si los valores son diferentes
      if (valor_pre_reemplazo != valor_post_reemplazo) {
        # Crear la pareja de reemplazo
        pareja_actual <- c(valor_pre_reemplazo, valor_post_reemplazo)

        # Verificar si la pareja ya existe en la lista temporal
        if (!pareja_existe(reemplazos_temp, pareja_actual)) {
          # Agregar la pareja de reemplazo a la lista temporal
          reemplazos_temp[[length(reemplazos_temp) + 1]] <- pareja_actual
        }
      }
    }

    reemplazos_i[[i]] <- reemplazos_temp

    for (j in 1:length(reemplazos_temp)) {
      if (length(reemplazos_i[[i]]) >= 1) {
        A <- reemplazos_i[[i]][[j]][[1]]
        B <- reemplazos_i[[i]][[j]][[2]]

        pacientes_grupo_A <- POSTmap_i[,i] == A
        pacientes_grupo_B <- POSTmap_i[,i] == B

        # Cambia los valores de Group
        POSTmap_i[,i][pacientes_grupo_A] <- B
        POSTmap_i[,i][pacientes_grupo_B] <- A
      }
    }
  }

  # POSTmap_i<-as.data.frame(POSTmap_i)
  # POSTmap_i

  POSTmap[[k]]<-POSTmap_i
}

# * Reinicio del environment (demasiado pesado, frecuentemente la terminal crashea si no se hace)

# source("Main9.R")
# source("functions_groupping2.R")
source("functions_groupping.R")
# # save(POSTmap, file = "POSTmapPC21.RData")

# Guardar el objeto POSTmap en un archivo .RData
save(POSTmap, file = "POSTmap3PCmuestra2.RData")
end_time <- Sys.time()
time_difference <- end_time - start_time
print(paste("Tiempo de ejecución del bucle:", time_difference))

# # # Listar todos los objetos en el entorno de trabajo
# all_objects <- ls()
# # 
# # # Eliminar todos excepto POSTmap
# rm(list = all_objects[all_objects != "POSTmap"])

# SONIDO FINAL EJECUCIÓN 
# beep(sound = 3)

#######################
# POST K Y PC OPTIMUM # 
#######################

bestk=3
start_time <- Sys.time()

#####
# 3 # Tercera fase: Análisis de robustez de los resultados
#####

######################
# PREPARAR DF_COUNTS #
######################

df_counts_list <- list()
df_counts_numeric_list <- list()
percent_list<- list()
for (k in bestk) {

  #VÍA CENTROIDES_FINAL
  df_mappedF <- POSTmap[[k]]

  # Función para encontrar el valor más repetido o indicar indeciso
  # Crear un nuevo dataframe con los conteos
  df_counts <- as.data.frame(t(apply(df_mappedF, 1, function(row) {
    # Contar los valores únicos por fila, excluyendo NAs
    as.vector(table(factor(row, levels = unique(unlist(df_mappedF[!is.na(df_mappedF)]))), useNA = "no"))
  })))

  #####################################
  # DEFINIR PACIENTES REPRESENTATIVOS # *  Este método al final NO se ha utilizado para analizar la robustez
  ##################################### *  Se incluye para que el subsiguiente código a continuación funcione

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

  # Establecer el porcentaje deseado
  desired_percentage <- 80

  # Generar el nombre de la columna de manera que sea válido y sin espacios
  variable_name <- paste("Value", gsub("\\.", "_", as.character(desired_percentage)), "Percent", sep = "")

  # Asegurar que las columnas de conteo son numéricas
  df_counts_numeric <- df_counts[, 1:k]
  df_counts_numeric <- apply(df_counts_numeric, 2, as.numeric)

  # Función ajustada para calcular el valor según el porcentaje deseado

  # Aplicar la función a cada fila del dataframe numérico
  df_counts_numeric <- as.data.frame(df_counts_numeric)
  df_counts[[variable_name]] <- apply(df_counts_numeric, 1, get_value_if_desired_percent, desired_percentage)
  rownames(df_counts_numeric)<-rownames(df_counts)


  df_counts_numeric_list[[k]] <- df_counts_numeric
  df_counts_list[[k]] <-df_counts

  ########################################
  # INFORME DE RESULTADOS PARA DF_COUNTS # * Esta figura tampoco se incluirá finalmente
  ########################################

  # Bucle para generar cada gráfico
  ps <- get_percentage(df_counts, k)
  kmain<-paste0("densidad para k=",k)
  plot(density(ps),main =kmain)
  percent_list[[k]]<-ps

  print("proceso terminado para k=")
  print(k)
}

### RING
# beep(sound = 3)

###########################
# BINOMIAL Y CHI CUADRADO # * Al final se optó por Binomial, más restrictiva.
###########################

# Fuente del script binomial_chi2_paciente.R
source("binomial_chi2_paciente.R")

# Contar el número de valores menores que 0.05 en df_counts$chi_sq_p_value
# Imprimir la distribución de valores en la columna 'ResolvedValue'
cat("Distribución de valores en 'ResolvedValue':\n")
print(table(df_counts$ResolvedValue))

# Calcular el número de p-valores menores a 0.05
num_valores_menores <- sum(df_counts$p_value < 0.05)
cat("Hay", num_valores_menores, "Pacientes significativos")

# Crear una nueva columna 'Significativo' que indique si el p-valor es menor a 0.05
df_counts$Significativo <- ifelse(df_counts$p_value < 0.05, "Sí", "No")

# Agrupar los datos por 'ResolvedValue' y contar cuántos pacientes son significativos en cada grupo
pacientes_por_valor_resuelto <- aggregate(Significativo ~ ResolvedValue, data = df_counts, FUN = function(x) sum(x == "Sí"))
print(pacientes_por_valor_resuelto)

# Crear una tabla de frecuencias para la columna 'ResolvedValue'
tabla_frecuencias <- table(df_counts$ResolvedValue)

# Guardar cuántos pacientes son significativos en cada grupo de 'ResolvedValue'
pacientes_significativos_por_grupo <- aggregate(Significativo ~ ResolvedValue, data = df_counts, FUN = function(x) sum(x == "Sí"))

# Calcular el porcentaje de pacientes significativos respecto al tamaño total de pacientes de cada grupo
pacientes_significativos_por_grupo$Porcentaje <- pacientes_significativos_por_grupo$Significativo / tabla_frecuencias[pacientes_significativos_por_grupo$ResolvedValue] * 100

# Unir la tabla de frecuencias y los porcentajes en un solo dataframe
df_final <- merge(
  x = data.frame(ResolvedValue = names(tabla_frecuencias), Frecuencia = as.numeric(tabla_frecuencias)),
  y = pacientes_significativos_por_grupo,
  by = "ResolvedValue",
  all.x = TRUE
)

# Reemplazar los valores NA con 0 en caso de que haya grupos sin pacientes significativos
df_final$Porcentaje[is.na(df_final$Porcentaje)] <- 0

# Imprimir el dataframe final
print(df_final)


write.csv(df_final,"df_final_PC3_k4.csv")

# end_time <- Sys.time()
# time_difference <- end_time - start_time
# print(paste("Tiempo de ejecución del bucle:", time_difference))
# beep(sound = 3)













