######################
# PREPARAR DF_COUNTS #
######################
par(mfrow=c(1,3))
df_counts_list <- list()
df_counts_numeric_list <- list()
percent_list<- list()
for (k in 4:6) {
  
  #VÍA JS
  # df_mappedF <- as.data.frame(lapply(df_listPC5_JS[[k]], #Cambia aquí cuantas PC quieres !!!
  #                                    factor),row.names = row.names(df_mapped))
  #VÍA CENTROIDES
  # df_mappedF <- as.data.frame(lapply(clusters_list_POST_7_PC[[k]], #Cambia aquí cuantas PC quieres !!!
  #                                    factor),row.names = row.names(clusters_list_POST[[k]]))
  # 
  #VÍA CENTROIDES_FINAL
  df_mappedF <- POSTmap[[k]]

  # Función para encontrar el valor más repetido o indicar indeciso
# Crear un nuevo dataframe con los conteos
df_counts <- as.data.frame(t(apply(df_mappedF, 1, function(row) {
  # Contar los valores únicos por fila, excluyendo NAs
  as.vector(table(factor(row, levels = unique(unlist(df_mappedF[!is.na(df_mappedF)]))), useNA = "no"))
})))

#####################################
# DEFINIR PACIENTES REPRESENTATIVOS #
#####################################

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

#

df_counts_numeric_list[[k]] <- df_counts_numeric
df_counts_list[[k]] <-df_counts

########################################
# INFORME DE RESULTADOS PARA DF_COUNTS #
########################################


# Bucle para generar cada gráfico
  ps <- get_percentage(df_counts, k)
  kmain<-paste0("densidad para k=",k)
  plot(density(ps),main =kmain)
  percent_list[[k]]<-ps 

print("proceso terminado para k=")
print(k)
}


### GUARDAR OBJETOS
df_counts_listPC7_JS_centroid<-df_counts_list
df_counts_numeric_listPC7_centroid<-df_counts_numeric_list 
beep(sound = 3)
###

###########################
# BINOMIAL Y CHI CUADRADO #
###########################
k=4
df_counts<-df_counts_listPC20_JS_centroid[[k]]
source("binomial_chi2_paciente.R")
# Contar el número de valores menores que 0.05 en df_counts$chi_sq_p_value


cat("Distribución de valores en 'ResolvedValue':\n")
print(table(df_counts$ResolvedValue))

num_valores_menores <- sum(df_counts$p_value < 0.05)
cat("Hay",num_valores_menores,"Pacientes significativos")


df_counts$Significativo <- ifelse(df_counts$p_value < 0.05, "Sí", "No")
pacientes_por_valor_resuelto <- aggregate(Significativo ~ ResolvedValue, data = df_counts, FUN = function(x) sum(x == "Sí"))
print(pacientes_por_valor_resuelto)


############
# SCATTERS #
############



#######################
# TABLAS PACIENTES ER #
#######################


df_ER <- df_counts
rownames(df_ER) <- rownames(df_mapped)
# Filtrar solo los pacientes que no tienen NA en la columna prescatter
df_ER <- df_ER[complete.cases(df_ER$Value80Percent), ]



# Subconjugar dataExperiment usando los rownames de df_ER
dataWithClusters <- dataExperiment[rownames(dataExperiment) %in% rownames(df_ER), ]
dataWithClusters$ClusterGroup<-df_ER$Value80Percent
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

#Poner la N del subgrupo en el colname
source("colnameswithN.R")

##########
# TTABLE #
##########
if (CharlsonOUT == T) {
  source("TablasCharlsonOUT.R")
}


if (CharlsonOUT == F) {
  source("TablasCharlsonIN.R")  
}

#ITable
#OTable
tTable


