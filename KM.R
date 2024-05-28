
library(survival)
library(survminer)

na_counts <- table(dataWithClusters$ClusterGroup, is.na(dataWithClusters$days_to_death_28))

# Crear un objeto de supervivencia
surv_obj <- Surv(dataWithClusters$days_to_death_28, event = !is.na(dataWithClusters$days_to_death_28))

# Realizar el análisis de Kaplan-Meier para cada grupo
km_fit <- survfit(surv_obj ~ dataWithClusters$ClusterGroup)

# Crear la gráfica de Kaplan-Meier para los cuatro grupos
ggsurvplot(km_fit, data = dataWithClusters, risk.table = TRUE, pval = TRUE)



# Contar valores NA en cada grupo
na_counts <- table(dataWithClusters$ClusterGroup, is.na(dataWithClusters$days_to_death_28))

# Imprimir la tabla de conteo de valores NA
print(na_counts)



