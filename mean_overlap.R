 
library(dplyr)
library(ggalluvial)
library(tidyr)

bestk <- 7
perplexity_values <- c(10,20,30,40,50,60,70,80,90,100)

# Número de experimentos por cada valor de perplexity_value
num_experiments <- 10

# Crear una lista para almacenar los resultados de cada experimento para diferentes perplexity_values
results_list <- list()

for (perplexity_value in perplexity_values) {
  overlap_coefficients <- c()  # Almacenar coeficientes de solapamiento para cada valor de perplexity_value
  
  # Realizar los experimentos para el valor actual de perplexity_value
  for (i in 1:num_experiments) {
    set.seed(i)
    
    tsne <- Rtsne(scale(data_inputs), dims = 3, perplexity = perplexity_value, verbose = TRUE, max_iter = 1000)
    datosplot <- tsne$Y[, 1:3]
    
    # Clustering
    dist_matrix <- dist(datosplot)
    resultsClustering <- hclust(dist_matrix, method = "ward.D2")
    resultsClustering$cluster <- cutree(resultsClustering, k = bestk)
    groups <- resultsClustering$cluster
    
    # Guardar los resultados en la lista correspondiente al valor de perplexity_value actual
    results_list[[as.character(perplexity_value)]] <- c(results_list[[as.character(perplexity_value)]], list(groups = groups))
  }
  
  # Obtener todas las combinaciones de pares de variables para calcular los coeficientes de solapamiento
  results_df <- as.data.frame(sapply(results_list[[as.character(perplexity_value)]], unlist))
  colnames(results_df) <- paste("Experimento", 1:num_experiments)
  rownames(results_df) <- rownames(data_inputs)
  
  variable_pairs <- combn(names(results_df), 2)
  
  # Calcular el coeficiente de solapamiento para cada par de variables
  for (i in 1:ncol(variable_pairs)) {
    var1 <- results_df[[variable_pairs[1, i]]]
    var2 <- results_df[[variable_pairs[2, i]]]
    
    same_values_count <- sum(var1 == var2, na.rm = TRUE)
    overlap_coefficient <- ((same_values_count / nrow(results_df)) * 100)
    
    overlap_coefficients <- c(overlap_coefficients, overlap_coefficient)
  }
  
  # Calcular la media y la desviación estándar de los coeficientes de solapamiento para el valor de perplexity_value actual
  mean_overlap <- mean(overlap_coefficients)
  std_dev_overlap <- sd(overlap_coefficients)
  
  # Guardar la media y la desviación estándar en la lista para el valor de perplexity_value actual
  results_list[[as.character(perplexity_value)]][["mean_overlap"]] <- mean_overlap
  results_list[[as.character(perplexity_value)]][["std_dev_overlap"]] <- std_dev_overlap
}

# Mostrar los resultados de medias y desviaciones estándar para cada perplexity_value
for (perplexity_value in perplexity_values) {
  cat("Para perplexity_value =", perplexity_value, "\n")
  cat("Media de los coeficientes de solapamiento:", results_list[[as.character(perplexity_value)]][["mean_overlap"]], "%\n")
  cat("Desviación estándar de los coeficientes de solapamiento:", results_list[[as.character(perplexity_value)]][["std_dev_overlap"]], "%\n\n")
}

# Crear un vector para almacenar los valores de mean_overlap
mean_overlap_values <- c(
  results_list[["10"]][["mean_overlap"]],
  results_list[["20"]][["mean_overlap"]],
  results_list[["30"]][["mean_overlap"]],
  results_list[["40"]][["mean_overlap"]],
  results_list[["50"]][["mean_overlap"]],
  results_list[["60"]][["mean_overlap"]],
  results_list[["70"]][["mean_overlap"]],
  results_list[["80"]][["mean_overlap"]],
  results_list[["90"]][["mean_overlap"]],
  results_list[["100"]][["mean_overlap"]]
)

# Crear un vector para almacenar los valores de i
i_values <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# Crear el gráfico de barras con ajuste en el eje y
barplot(mean_overlap_values, names.arg = i_values,
        xlab = "Perplexity value", ylab = "Valor de mean_overlap",
        col = "skyblue", main = "Valores de mean_overlap para cada perplexity value",
        ylim = c(0, 100),
        width = 0.85)

# Agregar barras de error (desviaciones estándar)
arrows(
  x0 = seq_along(mean_overlap_values),
  y0 = mean_overlap_values - std_dev_values,
  x1 = seq_along(mean_overlap_values),
  y1 = mean_overlap_values + std_dev_values,
  angle = 90, code = 3, length = 0.1
)

# Mostrar el gráfico
legend("topright", legend = "Desviación estándar",
       bty = "n", pch = 15, col = "black", pt.cex = 1)



# Valores de mean_overlap
mean_overlap_values <- c(
  results_list[["10"]][["mean_overlap"]],
  results_list[["20"]][["mean_overlap"]],
  results_list[["30"]][["mean_overlap"]],
  results_list[["40"]][["mean_overlap"]],
  results_list[["50"]][["mean_overlap"]],
  results_list[["60"]][["mean_overlap"]],
  results_list[["70"]][["mean_overlap"]],
  results_list[["80"]][["mean_overlap"]],
  results_list[["90"]][["mean_overlap"]],
  results_list[["100"]][["mean_overlap"]]
)

# Crear el marco de datos
df_mean_overlap <- data.frame(
  mean_overlap = mean_overlap_values,
  row.names = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
)

# Mostrar el marco de datos creado
print(df_mean_overlap)

std_dev_overlap_values <- c(
  results_list[["10"]][["std_dev_overlap"]],
  results_list[["20"]][["std_dev_overlap"]],
  results_list[["30"]][["std_dev_overlap"]],
  results_list[["40"]][["std_dev_overlap"]],
  results_list[["50"]][["std_dev_overlap"]],
  results_list[["60"]][["std_dev_overlap"]],
  results_list[["70"]][["std_dev_overlap"]],
  results_list[["80"]][["std_dev_overlap"]],
  results_list[["90"]][["std_dev_overlap"]],
  results_list[["100"]][["std_dev_overlap"]]
)

# Añadir la columna std_dev_overlap al marco de datos existente
df_mean_overlap$std_dev_overlap <- std_dev_overlap_values

# Mostrar el marco de datos actualizado
print(df_mean_overlap)

# Calculando los límites superior e inferior
# Suponiendo que 'mean_overlap' es la columna de medias y 'std_dev_overlap' es la columna de desviaciones estándar
df_mean_overlap$LI <- df_mean_overlap$mean_overlap - 1.96 * (df_mean_overlap$std_dev_overlap / sqrt(nrow(df_mean_overlap)))
df_mean_overlap$LS <- df_mean_overlap$mean_overlap + 1.96 * (df_mean_overlap$std_dev_overlap / sqrt(nrow(df_mean_overlap)))

# Mostrar el dataframe con las nuevas columnas LI (límite inferior) y LS (límite superior)
print(df_mean_overlap)

library(dplyr)
library(ggplot2)

# Crear un índice numérico para las filas
df_mean_overlap$row_index <- 1:nrow(df_mean_overlap)
df_mean_overlap$row_index <- rownames(df_mean_overlap)

# Convertir los datos al formato largo (tidy)
df_plot <- df_mean_overlap %>%
  select(row_index, mean_overlap, LI, LS) %>%
  tidyr::gather(key = "interval", value = "value", -row_index)

# Crear el gráfico de barras
ggplot(df_plot, aes(x = as.factor(row_index), y = value, fill = interval)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Filas", y = "Valor") +
  ggtitle("Valores de mean_overlap, LI y LS por fila") +
  theme_minimal() +
  theme(legend.position = "top")








