library(dplyr)
library(ggalluvial)
library(tidyr)
library(reshape2)
### 1. HACEMOS TSNEs 

# Número de experimentos
num_experiments <- 50
bestk=7
# Crear una lista para almacenar los resultados de cada experimento

#tsne <- Rtsne((scale(data_inputs)), dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)
#datosplot<-tsne$Y[,1:3]

#res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE)
#datosplot<-res.pca$ind$coord

results_list <- list()

for (i in 1:num_experiments) {
  set.seed(i)

  ### Hacer 1 tSNE / clustering
  tsne <- Rtsne((scale(data_inputs)), dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)
  datosplot<-tsne$Y[,1:3]
  
  ### Hacer 1 PCA / clustering
  #res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE)
  #datosplot<-res.pca$ind$coord
  #datosplot<- data_inputs
  ############# Clustering 
  dist_matrix <- dist(datosplot)
  
  #method HCLUST
  resultsClustering <- hclust(dist_matrix, method = "ward.D2")
  resultsClustering$cluster <- cutree(resultsClustering, k = bestk)
  
  #method KMEANS
  #resultsClustering <- kmeans(datosplot[,1:3], center = bestk)
  
  #####
  groups <- resultsClustering$cluster
  
  
  # Guardar los resultados en la lista
  results_list[[i]] <- list(
    groups = groups
  )
}

### GUARDAR EN DF 
results_df <- data.frame(matrix(ncol = num_experiments, nrow = nrow(data_inputs)))
colnames(results_df) <- paste("Experimento", 1:num_experiments)
for (i in 1:num_experiments) {
  results_df[, i] <- results_list[[i]]$groups
}
rownames(results_df) <- rownames(data_inputs)
head(results_df)

## GUARDAR EN DF FORMATO PARA ALLUVIAL FUNCIONA MAL!!!!!!!!!!!!!!!!
#num_pacientes <- nrow(results_df)
#num_experimentos <- ncol(results_df)
#paciente <- rep(rownames(results_df), each = num_experimentos)
#experimento <- rep(colnames(results_df), times = num_pacientes)
#grupo <- as.vector(as.matrix(results_df))
#nuevo_df <- data.frame(Patient = paciente, Experiment = experimento, Group = grupo)
#head(nuevo_df)
#df_alluvial2<-nuevo_df
##########################
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
for (i in 1:4) {
  print(head(results_list[[i]]$groups, n = 41))
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
  


############################################
# CAMBIOS MANUALES DE POSICIONES DE GRUPOS #
############################################
df_alluvial3 <- df_alluvial2

df_alluvial3 <- df_alluvial3 %>%
  mutate(Experiment = 
           ifelse(Experiment == "V02", "V05",
                  ifelse(Experiment == "V05", "V02",
                         Experiment)))

df_alluvial3

#CAMBIA GRUPOS

A = 4
B = 1

filas_grupo_A <- df_alluvial3$Experiment == "V10" & df_alluvial3$Group == A
filas_grupo_B <- df_alluvial3$Experiment == "V10" & df_alluvial3$Group == B

# Cambia los valores de Group
df_alluvial3$Group[filas_grupo_A] <- B
df_alluvial3$Group[filas_grupo_B] <- A


#PLOT FINAL
df_alluvial3 <- transform(df_alluvial3,
                          Group = factor(Group, rev(levels(Group))))
ggplot(df_alluvial3,
       aes(x = Experiment, stratum = Group, alluvium = Patient,
           y = Patient,
           fill = Group, label = Group)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("Plot ICU")


table(df_alluvial3$Group)
table(df_alluvial3$Experiment)



###############
# ALTERNATIVA 4

df_alluvial4<-df_alluvial2

# Eliminar filas con Experiment igual a "V01" o "V02"
df_alluvial4 <- subset(df_alluvial4, Experiment != "V01" & Experiment != "V02")

# Eliminar filas con Experiment igual a "V01" o "V02"
df_alluvial4 <- df_alluvial4[!(df_alluvial4$Experiment == "V01" | df_alluvial4$Experiment == "V02"), ]

df_alluvial4 <- transform(df_alluvial4,
                          Group = factor(Group, rev(levels(Group))))
ggplot(df_alluvial4,
       aes(x = Experiment, stratum = Group, alluvium = Patient,
           y = Patient,
           fill = Group, label = Group)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("Plot ICU")


#################
# CHI CUADRADO POR PAREJAS
variable_pairs <- combn(names(results_df), 2)

# Realizar pruebas de chi-cuadrado para cada par de variables
for (i in 1:ncol(variable_pairs)) {
  var1 <- as.factor(results_df[[variable_pairs[1, i]]])
  var2 <- as.factor(results_df[[variable_pairs[2, i]]])
  
  # Crear una tabla de contingencia para el par de variables
  contingency_table <- table(var1, var2)
  
  # Realizar la prueba de chi-cuadrado
  chi_squared <- chisq.test(contingency_table)
  
  # Imprimir el p-valor de la prueba de chi-cuadrado
  cat("Variables:", variable_pairs[1, i], "y", variable_pairs[2, i], "- p-valor:", chi_squared$p.value, "\n")
}

##################
########
# COEFICIENTES DE SOLAPAMIENTO


# Crear un vector para almacenar los coeficientes de solapamiento
overlap_coefficients <- c()

# Obtener todas las combinaciones de pares de variables
variable_pairs <- combn(names(results_df), 2)

# Calcular el coeficiente de solapamiento para cada par de variables
for (i in 1:ncol(variable_pairs)) {
  var1 <- results_df[[variable_pairs[1, i]]]
  var2 <- results_df[[variable_pairs[2, i]]]
  
  # Contar la cantidad de filas donde las dos variables tienen el mismo valor
  same_values_count <- sum(var1 == var2, na.rm = TRUE)
  
  # Calcular el coeficiente de solapamiento como porcentaje
  overlap_coefficient <- 100- ((same_values_count / nrow(results_df)) * 100)
  
  # Almacenar el coeficiente de solapamiento en el vector
  overlap_coefficients <- c(overlap_coefficients, overlap_coefficient)
  
  # Imprimir el coeficiente de solapamiento
  cat("Variables:", variable_pairs[1, i], "y", variable_pairs[2, i], 
      "- Coeficiente de solapamiento:", overlap_coefficient, "%\n")
}

# Calcular la media y la desviación estándar de los coeficientes de solapamiento
mean_overlap <- mean(overlap_coefficients)
std_dev_overlap <- sd(overlap_coefficients)

# Mostrar la media y la desviación estándar de los coeficientes de solapamiento
cat("Media de los coeficientes de solapamiento:", mean_overlap, "%\n")
cat("Desviación estándar de los coeficientes de solapamiento:", std_dev_overlap, "%\n")


######################
# PRUEBA TODOS LOS MÉTODOS DE HCLUST

# Lista de métodos para hclust
methods <- c("single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid", "median")

# Lista para almacenar los gráficos
plots_list <- list()

# Realizar el clustering y generar los plots para cada método
for (method in methods) {
  # Clustering
  dist_matrix <- dist(datosplot)
  resultsClustering <- hclust(dist_matrix, method = method)
  resultsClustering$cluster <- cutree(resultsClustering, k = bestk)
  
  groups <- resultsClustering$cluster
  
  # Crear un DataFrame con los resultados
  results_df <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))
  for (i in 1:num_experiments) {
    results_df[, i] <- groups
  }
  rownames(results_df) <- rownames(datosplot)
  
  # Preparar datos para el plot alluvial
  num_pacientes <- nrow(results_df)
  num_experimentos <- ncol(results_df)
  paciente <- rep(rownames(results_df), each = num_experimentos)
  experimento <- rep(colnames(results_df), times = num_pacientes)
  grupo <- as.vector(as.matrix(results_df))
  df_alluvial <- data.frame(Paciente = paciente, Experimento = experimento, Grupo = grupo)
  
  # Generar el plot alluvial y guardarlo en la lista
  plot <- ggplot(df_alluvial,
                 aes(x = Experimento, stratum = Grupo, alluvium = Paciente,
                     y = Paciente,
                     fill = Grupo, label = Grupo)) +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 3) +
    theme(legend.position = "none") +
    ggtitle(paste("Plot ICU - Método:", method))
  
  plots_list[[method]] <- plot
}

# Ejemplo de cómo acceder a un gráfico específico
plots_list

