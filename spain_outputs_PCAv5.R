######################################
# Mando exclusión variables Charlson #
######################################
CharlsonOUT<-T


source("Main9.R")

#####
# K #
#####
bestk=4
set.seed((1))

##############
# PCA # tSNE # - CHOOSE INPUT CLUSTER
##############
res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE)
plot(res.pca)

#INPUTS PCA ANALYSIS
Biplot<-fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

############ Clustering using tSNE
#perplexity_value = sqrt(nrow(dataExperiment))
#tsne <- Rtsne((scale(data_inputs)), dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)

############## CHOOSE BETWEEN PCA // tSNE
datosplot<-res.pca$ind$coord
#datosplot<-tsne$Y[,1:3]

############# Clustering - HCLUST

# resultsClustering <- kmeans(datosplot[,1:3], center = bestk)
# groups <- resultsClustering$cluster
dist_matrix <- dist(datosplot)
resultsClustering <- hclust(dist_matrix, method = "ward.D2")
resultsClustering$cluster <- cutree(resultsClustering, k = bestk)

groups <- resultsClustering$cluster
aux <- data_inputs
a <- fviz_nbclust(x =datosplot[,1:3], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
Silhouette <- a$data$y[bestk]
Silhouette

 source("Silhouette_plot.R")


#####################################################################
######### 1. Calcular medias/proporciones e intervalos de confianza #
# TABLA # 2. Calcular chi2/ kruskal y guardo P.values               #
######### 3. Integrar tabla en kable                                #
#####################################################################

dataWithClusters <- dataExperiment
dataWithClusters$ClusterGroup <- as.factor(groups)  
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

ITable
OTable
tTable
#########################
#file.edit("compile K.R")
######################################################################################################
################
# PLOTLY PLOTS #
################

INDICES <- c(1:dim(dataExperiment)[1])
markerSize2d = 35
markerSize2ddiamond = 25
markerSize3d = 300
markerSize3ddiamond = 250

axx <- list(
  title = "1<sup>st</sup> component"
)
axy <- list(
  title = "2<sup>nd</sup> component"
)
axz <- list(
  title = "3<sup>rd</sup> component"
)
#######
if (bestk >= 2 && bestk <= 11) {
  colors_set3_cluster <- brewer.pal(n = bestk + 1, name = "Set3")[2:(bestk + 1)]
} else if (bestk == 12) {
  colors_set3_cluster <- c(
    "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", 
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", 
    "#FFED6F", "#8DD3C7"
  )
}

pClusters2d <- plot_ly(
  x = datosplot[INDICES, 1],
  y = datosplot[INDICES, 2],
  color = as.factor(paste("Subgroup", format(resultsClustering$cluster[INDICES], digits = 2))),
  colors = colors_set3_cluster,  # Utiliza la paleta Set3 modificada
  type = "scatter",
  mode = "markers",
  text = paste0("Patient ID: ", rownames(dataExperiment[INDICES, ])),
  marker = list(sizemode = 'area'),
) %>%
  layout(title = "Subgroups", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pClusters3d <- plot_ly(x = datosplot[INDICES,1], 
                       y = datosplot[INDICES,2], 
                       z = datosplot[INDICES,3],
                       color = as.factor(paste("Subgroup",format(resultsClustering$cluster[INDICES], digits=2))),
                       colors = colors_set3_cluster,
                       type = "scatter3d", mode = "markers",
                       text = paste0("Patient ID: ",rownames(dataExperiment[INDICES, ])),
                       marker = list(sizemode = 'area'),
                       scene = 'sceneClusters') %>%
  layout(title = "Subgroups", xaxis = axx, yaxis = axy) %>% 
  config(displaylogo = FALSE)


pClusters2d
 

#POUTCOME con color+simbolo regulado
pOutcome3d <- plot_ly(data = dataExperiment, 
                      x = res.pca$ind$coord[INDICES, 1], 
                      y = res.pca$ind$coord[INDICES, 2], 
                      z = res.pca$ind$coord[INDICES, 3],
                      color = (dataExperiment$del_or_coma_duration_exp),
                      colors = c("blue","yellow"),
                      symbol = as.factor( ifelse(dataExperiment$ever_del_or_coma[INDICES] == 1, "Yes", "No")),
                      symbols = c("square","circle"),
                      type = "scatter3d", 
                      mode = "markers",
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES, ])),
                      scene = "sceneOutcome") %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pOutcome3d



#################################################################
# RESTO CÓDIGO
#VICTOR
pOutcome2d <- plot_ly(data = dataExperiment, 
                      x = datosplot[INDICES, 1], 
                      y = datosplot[INDICES, 2], 
                      color = as.factor(ifelse(is.na(dataExperiment$died_28_yn[INDICES]), "NA", ifelse(dataExperiment$died_28_yn[INDICES] == 1, "Deceased", "Non-Deceased"))),
                      colors = c("firebrick", "darkseagreen2"),
                      type = "scatter",
                      mode = "markers",
                      text = paste0("Patient ID: ", rownames(dataExperiment[INDICES, ]), "\n Age:", dataExperiment$died_28_yn[INDICES]),
                      marker = list(sizemode = "area", symbol = "diamond"),
                      showlegend = FALSE) %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)
pOutcome2d
#VICTOR

pOutcome3d <- plot_ly(data = dataExperiment, 
                      x = res.pca$ind$coord[INDICES, 1], 
                      y = res.pca$ind$coord[INDICES, 2], 
                      z = res.pca$ind$coord[INDICES, 3],
                      color = (dataExperiment$Supervivencia),
                      colors = c("blue","yellow"),
                      #symbol = as.factor( ifelse(dataExperiment$ever_del_or_coma[INDICES] == 1, "Yes", "No")),
                      #symbols = c("square","circle"),
                      type = "scatter3d", 
                      mode = "markers",
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES, ])),
                      scene = "sceneOutcome") %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pOutcome3d
pOutcome3d <- plot_ly(data = dataExperiment, 
                      x = res.pca$ind$coord[INDICES, 1], 
                      y = res.pca$ind$coord[INDICES, 2], 
                      z = res.pca$ind$coord[INDICES, 3],
                      color = as.factor(ifelse(is.na(dataExperiment$died_28_yn[INDICES]), "NA", ifelse(dataExperiment$died_28_yn[INDICES] == 1, "Deceased", "Non-Deceased"))),
                      colors = c("firebrick", "white", "darkseagreen2"),
                      type = "scatter3d", 
                      mode = "markers",
                      text = paste0("Patient ID: ", rownames(dataExperiment[INDICES, ]), "\n Age:", dataExperiment$died_28_yn[INDICES]),
                      marker = list(sizemode = "area", symbol = "diamond"),
                      scene = "sceneOutcome") %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pOutcome3d



