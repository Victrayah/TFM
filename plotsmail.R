library(FactoMineR)
library(cluster)
library(factoextra)

# Configuración inicial
num_experiments <- 1000
bestk <- 3:6
ncp_values <- c(3, 8, 14, 21)
set.seed(112)

# Array para almacenar promedios de silhouette para cada combinación de ncp y k
silhouette_averages <- matrix(nrow = length(ncp_values), ncol = length(bestk))
rownames(silhouette_averages) <- paste("ncp", ncp_values, sep="=")
colnames(silhouette_averages) <- paste("k", bestk, sep="=")


library(cluster)  # Para la función silhouette

for (ncp_idx in seq_along(ncp_values)) {
  ncp <- ncp_values[ncp_idx]
  # Uso de clean_dfs que contiene los datos originales limpios de outliers después de PCA
  datosplot <- clean_dfs[[paste("CP", ncp, "Original", sep="")]]
  
  # Bootstrap y análisis de clusterización sobre datos limpios
  bs <- bootstrap(datosplot, num_experiments, 0.8)
  
  for (k_idx in seq_along(bestk)) {
    k <- bestk[k_idx]
    silhouette_scores <- numeric(num_experiments)
    
    for (i in 1:num_experiments) {
      sample_rows <- sort(bs[[1]][[i]][["id"]])
      sample_data <- datosplot[sample_rows, ]
      
      if (nrow(sample_data) >= k) {  # Asegurar que hay suficientes datos para el número de clusters
        dist_matrix <- dist(sample_data)
        hc <- hclust(dist_matrix, method = "ward.D2")
        clusters <- cutree(hc, k)
        
        if (length(unique(clusters)) > 1) {  # Silhouette requiere al menos 2 clusters
          silhouette_info <- silhouette(clusters, dist_matrix)
          silhouette_scores[i] <- mean(silhouette_info[, "sil_width"])
        } else {
          silhouette_scores[i] <- NA  # NA para casos no calculables
        }
      }
    }
    
    # Guardar el promedio de los valores silhouette
    silhouette_averages[ncp_idx, k_idx] <- mean(silhouette_scores, na.rm = TRUE)
  }
  print(paste("Proceso terminado para k=", k, " y PC=", ncp, sep=""))
}

# Ver los resultados
print(silhouette_averages)



library(pheatmap)
pheatmap(silhouette_averages,
         color = colorRampPalette(c("white", "red"))(50),
         main = "Silhouette Scores Heatmap Muestra 1",
         display_numbers = TRUE,
         fontsize = 10,
         fontsize_row = 12,
         fontsize_col = 12,
         border_color = NA,
         cluster_rows = FALSE, 
         cluster_cols = FALSE)


















