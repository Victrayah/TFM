# Initializing a vector to store Silhouette values
silhouette_values <- numeric(length = 11)

# Loop for different values of bestk
for (bestk in 2:12) {
  dist_matrix <- dist(datosplot)
  hierarchical_clusters <- hclust(dist_matrix, method = "ward.D2")
  clusters <- cutree(hierarchical_clusters, k = bestk)
  
  # Calculate silhouette value for each iteration
  silhouette <- silhouette(clusters, dist_matrix)
  silhouette_values[bestk - 1] <- mean(silhouette[, "sil_width"])  # Store mean silhouette width
}

# Create a dataframe to store results
silhouette_data <- data.frame(bestk = 2:12, Silhouette = silhouette_values)

# Plotting Silhouette values for each 'bestk'
silhouette_plot <- ggplot(silhouette_data, aes(x = as.factor(bestk), y = Silhouette)) +
  geom_point(color = "blue", size = 3) +  
  scale_x_discrete(labels = as.character(2:12)) +  
  labs(title = "Silhouette Values for Different Numbers of Clusters",
       x = "Number of Clusters (bestk)", y = "Silhouette Value")
silhouette_plot

