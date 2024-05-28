############# Clustering using t-SNE
set.seed(1) # for reproducibility
perplexity_value = 30
if (nrow(dataExperiment)<200){
  perplexity_value = 10
}
tsne <- Rtsne(scale(dataExperiment[,X_vars.numeric], center = T, scale = T), dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)
# tsne <- Rtsne(scale(dataExperiment[,X_vars.numeric], center = T, scale = T), pca = FALSE, dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)



resultsClustering <- kmeans(tsne$Y[,1:3], center = bestk)
#############



groups <- resultsClustering$cluster
aux <- dataExperiment[,c(X_vars, Y_vars)]
# Silhouette <- intCriteria(as.matrix(dataExperiment[,X_vars.numeric]), resultsClustering$cluster, c("Silhouette"))
# a <- fviz_nbclust(x = dataExperiment[,X_vars.numeric], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
a <- fviz_nbclust(x =tsne$Y[,1:3], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
Silhouette <- a$data$y[bestk]