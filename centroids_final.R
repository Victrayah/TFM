# setwd("D:/2023_icu_delirium")
# source("Procesamiento_datos.R")
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


# Creamos una paleta de colores que va de verde a rojo con 101 matices
colores <- colorRampPalette(c("red", "green"))(101)

# Usamos el porcentaje como índice para los colores, asegurándonos de que sea al menos 1
df_final$IndiceColor <- pmax(1, ceiling(df_final$Porcentaje))

# Creamos el gráfico de barras con ggplot2
ggplot(df_final, aes(x = ResolvedValue, y = Frecuencia, fill = IndiceColor)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = colores,
                       name = "% Pacientes Sig",
                       breaks = c(1, 25, 50, 75, 100),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
  labs(title = "Distribución muestra  - 14 Componentes Principales",
       x = "Grupo", y = "Tamaño muestral") +
  theme_minimal() +
  theme(legend.position = "right")

############
# SCATTERS #
############
df_counts$robpac <- ifelse(df_counts$p_value < 0.05, df_counts$ResolvedValue, NA)

# Suponiendo que df_counts es tu dataframe

# Crear la variable plotly
df_counts$plotly <- ifelse(!is.na(df_counts$robpac), paste("Robust patients group", df_counts$robpac),
                           paste("No robust patients group", df_counts$ResolvedValue))

table(df_counts$plotly)

colores<-c(
  "lightgreen",
  "lightpink",
  "lightblue",
  "green",
  "purple",
  "blue"
)

axx <- list(
  title = "1<sup>st</sup> component"
)
axy <- list(
  title = "2<sup>nd</sup> component"
)
axz <- list(
  title = "3<sup>rd</sup> component"
)


pClusters3d<-plot_ly(x = datosplot[,1],
                     y = datosplot[,2],
                     z = datosplot[,3],
                     color = df_counts$plotly,
                     #color = df_counts$ResolvedValue,
                     colors = colores,
                     type = "scatter3d", mode = "markers",
                     marker = list(sizemode = 'area'),
                     scene = 'sceneClusters') %>%
  layout(title = "Scatter 3D", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pClusters3d

pClusters2d <- plot_ly(
  x = datosplot[,1],
  y = datosplot[,2],
  color = df_counts$plotly,
  colors = colores,  # Utiliza la paleta Set3 modificada
  type = "scatter",
  mode = "markers",
  marker = list(sizemode = 'area'),
) %>%
  layout(title = "Scatter 2D", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pClusters2d


#######################
# TABLAS PACIENTES ER #
#######################

df_ER <- df_counts
# Filtrar solo los pacientes que no tienen NA en la columna prescatter
df_ER <- df_ER[complete.cases(df_ER$robpac), ]

#df_ER <- df_ER[complete.cases(df_ER$ResolvedValue), ]

# Subconjugar dataExperiment usando los rownames de df_ER
dataWithClusters <- dataExperiment[rownames(dataExperiment) %in% rownames(df_ER), ]
dataWithClusters$ClusterGroup<-df_ER$robpac

#dataWithClusters$ClusterGroup<-df_ER$ResolvedValue

source("Funciones_tabla.R")

#####
# 1 # DF con medias/ proporciones e intervalos de confianza
#####
PREmpCIdf <- mpCIfunction(dataWithClusters, ClusterGroup)
mpCIdf <- convertir_PREmpCIdf(PREmpCIdf)

#####
# 2 # DF con P.values
#####
Pvaluesdataframe <- data.frame(
  Variable = c(names(chi_squared_results), names(kruskal_wallis_results)),
  Test = c(rep("Chi-squared", length(chi_squared_results)),
           rep("Kruskal-Wallis", length(kruskal_wallis_results))),
  P_Value = c(sapply(chi_squared_results, function(res) if (!is.null(res)) res$p.value else NA),
              sapply(kruskal_wallis_results, function(res) res$p.value))
)
p_values <- c(
  sapply(chi_squared_results, function(res) if (!is.null(res)) res$p.value else NA),
  sapply(kruskal_wallis_results, function(res) res$p.value)
)
Pvaluesdataframe <- data.frame(P_Value = p_values)
Pvaluesdataframe <- head(Pvaluesdataframe, n = nrow(Pvaluesdataframe) - 1)

#####
# 3 # Integrar DFs en resultsTable
#####
rownames(Pvaluesdataframe) <- as.character(rownames(Pvaluesdataframe))
rownames(mpCIdf) <- as.character(rownames(mpCIdf))
order_Pvaluesdataframe <- match(rownames(mpCIdf), rownames(Pvaluesdataframe))

# Fusionamos los dataframes por los rownames conservando el orden original de mpCIdf
merged_df <- cbind(Pvaluesdataframe[order_Pvaluesdataframe, ], mpCIdf)
merged_df <- merged_df[, c(2:ncol(merged_df), 1)]
colnames(merged_df)[ncol(merged_df)] <- "P.Value"
merged_df$P.Value <- round(merged_df$P.Value, 4)
merged_df$P.Value <- ifelse(merged_df$P.Value < 0.05, paste0(merged_df$P.Value, "***"), merged_df$P.Value)

##########################
resultsTable<-merged_df #
##########################

#########################################
# Poner la N del subgrupo en el colname #
#########################################

#################
# MONTAR TABLAS #
#################

source("Tabla 3.R")

#tTable
#ITable
#OTable











