###############
## lIBRARIES ##
###############
library(text2vec)
library(MVN)
library(lsa)
library(ape)
library(survival)
library(survminer)
library(stringr)
library(varhandle)
library(glue) # glue is equivalent as .format in python
library(doParallel) # Leverage multiple cores
library(foreach)
library(cluster)
library(fastcluster)
library(graphics)

library(factoextra)
library(FactoMineR)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(Rtsne)
library(RColorBrewer)
library(kableExtra)
library(ggalluvial)
library(reshape2)
library(philentropy)
library(sjstats)
library(beepr)
library(clue)
library(irr)

########
## BD ##
########

### Set my current working directory 
#setwd("D:/BDSLab_2023/2023_ICU_dellirium_sars_cov_2")
source("informe_datos.R")
BD<- read.csv("covid_del_sum.csv",sep = ";")
#BD_paper<- read.csv("covid_delirium_data.csv")

######################## MUESTRA INICIAL ALEATORIA
#n <- nrow(BD)  # Obtener el número total de filas
#set.seed(123)  # Establecer una semilla para reproducibilidad
#filas_aleatorias <- sample(1:n, 500)  # Seleccionar 100 filas al azar

# Crear un nuevo dataframe con las 100 filas aleatorias
#BD <- BD[filas_aleatorias, ]



#Poner rownames
rownames(BD)<-BD$study_id

#BINARIZAR VARIABLES
BD[BD == "Yes"] <- 1
BD[BD == "No"] <- 0
BD[BD == "Unknown"] <- NA
BD <- BD %>%
  mutate(position_day1.y = ifelse(position_day1.y == "Prone", 0, 1))

#PONER RAZAS EN OTHER
frequencies <- table(BD$enr_race_final.y)
categories_to_change <- names(frequencies[frequencies < 25])
BD$enr_race_final.y[BD$enr_race_final.y %in% categories_to_change] <- "Other"

#GCS_low 98---> NAs
BD$daily_gcs_low.x[BD$daily_gcs_low.x == 98] <- NA

######################
# Crear una nueva variable SUPERVIVENCIA en el dataframe BD
BD$SUPERVIVENCIA <- ifelse(BD$ever_del_or_coma == 1, 
                           BD$dcfd_final.y + BD$del_or_coma_duration_exp.y, 
                           BD$dcfd_final.y)

######################

BD$SUPERVIVENCIA2 <- ifelse(is.na(BD$del_or_coma_duration_exp), 0, BD$del_or_coma_duration_exp)

###########################################
# FILTRO - PACIENTES SIN NA EN dcfd_final #
###########################################

#DELIRIUM 
#evitar NAs
filas_sin_NA <- complete.cases(BD[, c("dcfd_final.y", 
                                      "ever_del_or_coma.y"
                                      #"ever_hypodel_f.y"
)])
BD <- BD[filas_sin_NA, ]

#############################
# FILTRO PACIENTES OUTLIERS #
#############################

# FILTRO PACIENTES MUESTRA RAZA INPUTS + TODA LA MUESTRA
# BD <- subset(BD, !(rownames(BD) %in% c("015-081", 
#                                        "018-019"
#                                        )))

# # FILTRO PACIENTES MUESTRA RAZA OUTCOMES + TODA LA MUESTRA
# BD <- subset(BD, !(rownames(BD) %in% c("028-002",
#                                        "028-003",
#                                        "028-092",
#                                        "068-005",
#                                        "068-033",
#                                        "068-017",
#                                        "068-020",
#                                        "068-032",
#                                        "068-036",
#                                        "074-008",
#                                        "018-019"
#                                        )))

# FILTRO PACIENTES MUESTRA RAZA OUTCOMES + SOLO DELIRIUM
# BD <- subset(BD, !(rownames(BD) %in% c("074-008",
#                                        "068-033",
#                                        "028-002",
#                                        "028-003"
#                                        )))

###################################
# QUITAR PACIENTES SIN DEL / COMA #
###################################
BD <- BD %>%
  filter(ever_del_or_coma.y == 1)

############
# OUTCOMES #
############

#Neurological_status
Neurological_status <- data.frame(
  row.names = rownames(BD),
  ever_del_or_coma = BD$ever_del_or_coma.y,
  del_or_coma_duration_exp = BD$del_or_coma_duration_exp.y,
  ever_del = BD$ever_del.y,
  del_duration_exp = BD$del_duration_exp.y,
  del_duration = BD$del_duration.y,
  ever_hyperdel_f = BD$ever_hyperdel_f.y,
  hyperdel_days_exp = BD$hyperdel_days_exp.y,
  ever_hypodel_f = BD$ever_hypodel_f.y,
  hypodel_days_exp = BD$hypodel_days_exp.y,
  ever_coma = BD$ever_coma.y,
  coma_duration_exp = BD$coma_duration.y,
  del_or_coma_free_days = BD$dcfd_final.y,
  enr_race_final = BD$enr_race_final.y, #######AÑADIDA!!!
  persistent_coma = BD$persistent_coma.y
)

# Ventilatory support
Ventilatory_support <- data.frame(
  row.names = rownames(BD),
  ever_vent = BD$ever_vent.y,
  ever_on_imv = BD$ever_on_imv.y,
  total_mv_los_icu_28 = BD$total_mv_los_icu_28.y,
  mv_free_days = BD$mv_free_days.y
)

# Survival
Survival <- data.frame(
  row.names = rownames(BD),
  Duración_coma_delirium_todos = BD$SUPERVIVENCIA2, ## AÑADIDA
  Supervivencia = BD$SUPERVIVENCIA,
  status_28_f = BD$status_28_f.y,
  died_28_yn = BD$died_28_yn.y,
  days_to_death_28 = BD$days_to_death_28.y,
  enr_status_days_alive = BD$enr_status_days_alive.x,
  enr_status_where = BD$enr_status_where.x,
  enr_status_days_dead = BD$enr_status_days_dead.x
)

# Length of Stay
Length_of_Stay <- data.frame(
  row.names = rownames(BD),
  total_icu_los_28 = BD$total_icu_los_28.y,
  index_icu_los_28 = BD$index_icu_los_28.y,
  enr_hosp_los = BD$enr_hosp_los.x
)

##########
# INPUTS #
##########

# Epidemiological data
Epidemiological_data <- data.frame(
  row.names = rownames(BD),
  enr_age_final = BD$enr_age_final.y,
  #enr_race_final = BD$enr_race_final.y,
  enr_sex = BD$enr_sex.x,
  enr_hear = BD$enr_hear.x,
  enr_see = BD$enr_see.x
)

# Previous disease
Previous_disease <- data.frame(
  row.names = rownames(BD),
  enr_saps_final = BD$enr_saps_final.y,
  enr_smoke = BD$enr_smoke.x,
  enr_alc = BD$enr_alc.x,
  enr_hos_pre_los = BD$enr_hos_pre_los.x,
  enr_char_1___0 = BD$enr_char_1___0.x,
  enr_char_1___1 = BD$enr_char_1___1.x,
  enr_char_1___2 = BD$enr_char_1___2.x,
  enr_char_1___3 = BD$enr_char_1___3.x,
  enr_char_1___4 = BD$enr_char_1___4.x,
  enr_char_1___5 = BD$enr_char_1___5.x,
  enr_char_1___6 = BD$enr_char_1___6.x,
  enr_char_1___7 = BD$enr_char_1___7.x,
  enr_char_1___8 = BD$enr_char_1___8.x,
  enr_char_1___9 = BD$enr_char_1___9.x,
  enr_char_1___10 = BD$enr_char_1___10.x,
  enr_char_2___0 = BD$enr_char_2___0.x,
  enr_char_2___1 = BD$enr_char_2___1.x,
  enr_char_2___2 = BD$enr_char_2___2.x,
  enr_char_2___3 = BD$enr_char_2___3.x,
  enr_char_2___4 = BD$enr_char_2___4.x,
  enr_char_2___5 = BD$enr_char_2___5.x,
  enr_char_2___6 = BD$enr_char_2___6.x,
  enr_char_3___0 = BD$enr_char_3___0.x,
  enr_char_3___1 = BD$enr_char_3___1.x,
  enr_char_4___0 = BD$enr_char_4___0.x,
  enr_char_4___1 = BD$enr_char_4___1.x,
  enr_char_4___2 = BD$enr_char_4___2.x
)

# ICU details
ICU_details <- data.frame(
  row.names = rownames(BD),
  enr_icu = BD$enr_icu.x
)

# Support
Support <- data.frame(
  row.names = rownames(BD),
  daily_vent_type___0 = BD$daily_vent_type___0.x,
  daily_vent_type___3 = BD$daily_vent_type___3.x,
  daily_vent_type___4 = BD$daily_vent_type___4.x,
  daily_vent_type___2 = BD$daily_vent_type___2.x,
  daily_vent_type___1 = BD$daily_vent_type___1.x,
  daily_pos___1 = BD$daily_pos___1.x,
  daily_pos___2 = BD$daily_pos___2.x,
  daily_sbt = BD$daily_sbt.x,
  daily_vaso = BD$daily_vaso.x
)

# Sedation
Sedation <- data.frame(
  row.names = rownames(BD),
  sedation_level_final_day1 = BD$sedation_level_final_day1.y,
  benzo_day1 = BD$benzo_day1.y,
  opioids_day1 = BD$opioids_day1.y,
  antipsychotics_day1 = BD$antipsychotics_day1.y,
  anx_hyp_day1 = BD$anx_hyp_day1.y
)

# Anti delirium measures
Anti_delirium_measures <- data.frame(
  row.names = rownames(BD),
  daily_pr = BD$daily_pr.x,
  daily_mobile = BD$daily_mobile.x,
  daily_visit = BD$daily_visit.x,
  daily_virtual_contact = BD$daily_virtual_contact.x
)

# Neurological
Neurological <- data.frame(
  row.names = rownames(BD),
  daily_gcs_low = BD$daily_gcs_low,
  daily_del_yn = BD$daily_del_yn,
  daily_del_type = BD$daily_del_type
)


#####################################
# DATA INPUTS / DATA OUTCOMES cbind #
#####################################

data_inputs<-cbind(Epidemiological_data,
                   Previous_disease,
                   ICU_details,
                   Support,
                   Sedation,
                   Anti_delirium_measures,
                   Neurological
                   )

data_outcomes<-cbind(Neurological_status,
                    Ventilatory_support,
                    Survival,
                    Length_of_Stay
                    )

###
naValue = NA
N = dim(data_inputs)[1]
completenessMatrix <- !is.na(data_inputs)
completenessMatrix <- completenessMatrix & !data_inputs == ""
sumCompletenessMatrix <-apply(completenessMatrix,2,sum)
completenessByColumn <-round(sumCompletenessMatrix/N*100,4)

sortResults = sort(completenessByColumn, index.return = TRUE, decreasing = TRUE)
plot_ly(x = sortResults$x*100, y = reorder(names(sortResults$x),sortResults$x), name = 'Completeness per variable',
        type = 'bar', orientation = 'h',
        marker = list(color = 'rgba(50, 171, 96, 0.6)',
                      line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(title = "Completeness per variable (sorted)",
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 100)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE))

# Vector variables con NAs
variables_with_NAs <- colnames(data_inputs)[colMeans(is.na(data_inputs)) > 0]

#Quitar variables mal anotadas
data_inputs <- data_inputs[, colMeans(is.na(data_inputs)) <= 0]

una_sola_clase <- sapply(data_inputs, function(x) length(unique(x))) == 1
data_inputs <- data_inputs[, !una_sola_clase]
data_inputs <- data_inputs %>% 
  mutate_if(~ all(. %in% c(0, 1)), as.character)

# Obtener las variables/columnas de data_inputs
columnas_experiment <- colnames(data_inputs)


####################################
# BINARIZAR VARIABLES CATEGÓRICAS ##
##### EN DATA inputs ###########
####################################
source("Categories2Binaries.R")
data_inputs <- Categories2Binaries(data_inputs)
data_outcomes<-Categories2Binaries(data_outcomes)
dataExperiment<-cbind(data_inputs,data_outcomes)

##############################
# Quitar variables Charlson # 
#############################

CharlsonOUT=T

if (CharlsonOUT == T) {
  # Selecciona las columnas que deseas quitar
  data_inputs <- subset(data_inputs, select = -c(
    enr_char_1___1,
    enr_char_1___2,
    enr_char_1___3,
    enr_char_1___4,
    enr_char_1___5,
    enr_char_1___6,
    enr_char_1___7,
    enr_char_1___8,
    enr_char_1___9,
    enr_char_1___10,
    enr_char_2___1, 
    enr_char_2___2,
    enr_char_2___3,
    enr_char_2___4,
    enr_char_2___5,
    enr_char_2___6,
    enr_char_3___1,
    enr_char_4___1,
    enr_char_4___2
  ))
  dataExperiment<-cbind(data_inputs,data_outcomes)
}

####################################
# Quitar variables correlacionadas # 
####################################

varcor=T

if (varcor == T) {
  # Selecciona las columnas que deseas mantener
  data_inputs <- subset(data_inputs, select = -c(
    sedation_level_final_day1_Coma,
    sedation_level_final_day1_Normal,
    benzo_day1_None
  ))
  dataExperiment<-cbind(data_inputs,data_outcomes)
}


# PCA PRELIMINAR

res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE)
plot(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,# Avoid text overlapping
             geom = "text"
            
)

# res.pca <- PCA(dataExperiment_num, scale.unit = T, graph = FALSE)
# plot(res.pca)
# 
# dataExperiment_ch_num = dataExperiment_ch %>% mutate_all(as.numeric)
# res.pca <- PCA(dataExperiment_ch_num, scale.unit = T, graph = FALSE)
# plot(res.pca)

########################
# ANÁLISIS DE OUTLIERS # T2 HOTELLING
########################

# Función modificada para devolver el dataframe original y las puntuaciones PCA limpios
clean_data_with_pca <- function(data, num_components) {
  # Realizar PCA
  pca_result <- PCA(data, scale.unit = TRUE, graph = FALSE, ncp = num_components)
  
  # Calcular las puntuaciones de los componentes principales
  scores <- pca_result$ind$coord[, 1:num_components]
  
  # Calcular las distancias de T² de Hotelling
  t2_scores <- rowSums((scores %*% solve(cov(scores))) * scores)
  
  # Umbral usando la distribución Chi-cuadrado
  threshold <- qchisq(0.99, df = num_components)
  
  # Identificar outliers
  outliers <- which(t2_scores > threshold)
  
  # Crear nuevo dataframe sin outliers del original
  data_clean <- data[-outliers, ]
  
  # Crear dataframe de puntuaciones PCA sin outliers
  scores_clean <- scores[-outliers, ]
  
  # Retornar ambos dataframes en una lista
  return(list(original = data_clean, pca_scores = scores_clean))
}

components_to_analyze <- c(3, 8, 14, 21)  # Números de componentes principales a analizar
clean_dfs <- list()

for (num_cp in components_to_analyze) {
  clean_data <- clean_data_with_pca(data_inputs, num_cp)
  clean_dfs[[paste("CP", num_cp, "Original", sep = "")]] <- clean_data$original
  clean_dfs[[paste("CP", num_cp, "Scores", sep = "")]] <- clean_data$pca_scores
}


####################
# PLOT N según CPs #
####################

# # Crear un vector con el número de filas de cada dataframe
# num_rows <- sapply(clean_dfs, nrow)
# 
# # Crear un dataframe para el gráfico
# df_for_plot <- data.frame(
#   Components = names(num_rows),
#   NumRows = num_rows
# )
# 
# # Crear el gráfico de barras
# plot <- ggplot(df_for_plot, aes(x = Components, y = NumRows, fill = Components)) +
#   geom_bar(stat = "identity") +
#   labs(title = "N según CPs",
#        x = "Número de Componentes Principales",
#        y = "Número de Observaciones") +
#   theme_minimal()
# 
# # Mostrar el gráfico
# print(plot)

# seguir con centroids_final.R ---->>>










