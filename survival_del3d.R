# Crear la nueva variable "survival_del" basada en las condiciones dadas
dataExperiment$survival_del <- ifelse(dataExperiment$ever_del == 0 & dataExperiment$died_28_yn == 0, "alive_never_delirium",
                                      ifelse(dataExperiment$ever_del == 0 & dataExperiment$died_28_yn == 1, "dead_never_delirium",
                                             ifelse(dataExperiment$ever_del == 1 & dataExperiment$died_28_yn == 0, "alive_delirium",
                                                    ifelse(dataExperiment$ever_del == 1 & dataExperiment$died_28_yn == 1, "dead_delirium", NA))))



pOutcome3d <- plot_ly(data = dataExperiment, 
                      x = res.pca$ind$coord[INDICES, 1], 
                      y = res.pca$ind$coord[INDICES, 2], 
                      z = res.pca$ind$coord[INDICES, 3],
                      color = (dataExperiment$survival_del),
                      colors = c("blue","yellow","green","red"),
                      type = "scatter3d", 
                      mode = "markers",
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES, ])),
                      scene = "sceneOutcome") %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

pOutcome3d

dataHistograma <- dataWithClusters

dataHistograma %>%
  group_by(ClusterGroup) %>%
  mutate(percentage = n() / sum(n())) %>%
  ggplot(aes(x = survival_del, y = percentage, fill = survival_del)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Muestra de survival_del por CLUSTER",
       x = "survival_del",
       y = "Muestra") +
  theme_minimal() +
  facet_wrap(~rango_edad, scales = "free_y")