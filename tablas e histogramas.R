

#PLOT RAÇA

dataHistograma <- subset(dataExperiment, !(enr_race_final %in% c('Other', 'Not Reported')))


library(dplyr)

dataHistograma %>%
  group_by(enr_race_final) %>%
  mutate(percentage = n() / sum(n())) %>%
  ggplot(aes(x = daily_vaso, y = percentage, fill = daily_vaso)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Porcentaje de daily_vaso por enr_race_final",
       x = "daily_vaso",
       y = "Porcentaje") +
  theme_minimal() +
  facet_wrap(~enr_race_final, scales = "free_y")

#PLOT EDAT


# Crear la variable cualitativa
rangos_edad <- c(0, 40, 50, 60, 70, 80, 90)
dataExperiment$rango_edad <- cut(dataExperiment$enr_age_final, breaks = rangos_edad, 
                                 labels = c("0-39", "40-49", "50-59", "60-69", "70-79", "80-90"), 
                                 include.lowest = TRUE)
dataHistograma <- dataWithClusters

# binaries
dataHistograma %>%
  group_by(ClusterGroup) %>%
  mutate(percentage = n() / sum(n())) %>%
  ggplot(aes(x = survival_del, y = percentage, fill = factor(survival_del))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Muestra de survival_del por rango de edad",
       x = "survival_del",
       y = "Muestra") +
  theme_minimal() +
  facet_wrap(~rango_edad, scales = "free_y") +
  scale_fill_manual(values = c("0" = "#BFEFFF", "1" = "#EE6363"), 
                    name = "benzo_day1_None",
                    labels = c("0" = "Valor 0", "1" = "Valor 1"))

# cualitatives (recorda llevar el categories2binaries)
dataHistograma %>%
  group_by(ClusterGroup) %>%
  mutate(percentage = n() / sum(n())) %>%
  ggplot(aes(x = survival_del, y = percentage, fill = survival_del)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Muestra de survival_del por rango de edad",
       x = "survival_del",
       y = "Muestra") +
  theme_minimal() +
  facet_wrap(~ClusterGroup, scales = "free_y")


#PLOT CLUSTER - SURVIVAL












