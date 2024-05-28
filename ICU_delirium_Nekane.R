######################################
## CÓDIGO PARA NEKANE ###############
####################################

# Este código pierde algunas funciones del código original, 
# pero es más fácil de usar a la hora de quitar o poner variables,
# para ver rápidamente qué resultados salen en la tabla con cada conjunto de variables.

###############
## LIBRARIES ##
###############
library(factoextra)
library(FactoMineR)
library(readr)
library(dplyr)
library(plotly)
library(Rtsne)
library(RColorBrewer)
library(kableExtra)

########
## BD ## NO TOCAR NADA DE ESTE APARTADO
########

BD<- read.csv("covid_del_sum.csv",sep = ";")

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

###########################################
# FILTRO - PACIENTES SIN NA EN dcfd_final # (No hay ningún caso así)
###########################################

#DELIRIUM 
#evitar NAs
filas_sin_NA <- complete.cases(BD[, c("dcfd_final.y", 
                                      "ever_del_or_coma.y"
                                      #"ever_hypodel_f.y"
)])
BD <- BD[filas_sin_NA, ]

##############
## OUTCOMES ## VARIABLES QUE QUEREMOS QUE SALGAN EN LAS TABLAS,
############## PERO NO SE UTILICEN PARA HACER EL CLUSTERING

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
  enr_race_final = BD$enr_race_final.y,
  enr_sex = BD$enr_sex.x,
  enr_saps_final = BD$enr_saps_final.y,
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
  enr_icu = BD$enr_icu.x,
  enr_hos_pre_los = BD$enr_hos_pre_los.x
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


#OJO!!!!!!!!!!!!!!! SI SE DESEA QUITAR ALGUNO DE LOS ANTERIORES GRUPOS DE VARIABLES,
#SE PUEDE HACER A CONTINUACIÓN, DONDE SE MONTA EL data_inputs POR LOS GRUPOS ANTERIORES.

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

#######################################
# TRATAMIENTO DE DATOS PRE - ANÁLISIS #
#######################################

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


dataExperiment<-cbind(data_inputs,data_outcomes) #DataExperiment = INPUTS + OUTPUTS


####################################################
                    #          #
                    # ANÁLISIS # 
                    #          # 
####################################################


# ELEGIR Nº SUBGRUPOS DEL CLUSTERING (CAMBIAR NÚMERO)

bestk = 4 

# ELEGIR PERPLEXITY VALUE 

perplexity_value = 47

# PCA 

res.pca <- PCA(data_inputs, scale.unit = T, graph = FALSE)
plot(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,# Avoid text overlapping
             geom = "text"
             
)

# Clustering using tSNE - Método de clustering de preferencia

tsne <- Rtsne((scale(data_inputs)), dims = 3, perplexity=perplexity_value, verbose=TRUE, max_iter = 1000)


############## CHOOSE BETWEEN PCA // tSNE #### SI QUIERES VER LOS RESULTADOS DEL 
                                          #### CLUSTERING DE PCA EN VEZ DE tSNE,
                                          #### PUEDES INTERCAMBIAR EL "#" EN LAS 
                                          #### SIGUIENTES LINEAS
#datosplot<-res.pca$ind$coord
datosplot<-tsne$Y[,1:3]
#############

############# Clustering

resultsClustering <- kmeans(datosplot[,1:3], center = bestk)
groups <- resultsClustering$cluster
aux <- data_inputs
a <- fviz_nbclust(x =datosplot[,1:3], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
Silhouette <- a$data$y[bestk]

Silhouette #Coeficiente Silhouette para valorar calidad del clustering

############


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
source("TablasKableExtra.R")


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



#POUTCOME con color+simbolo regulado
pOutcome2d <- plot_ly(data = dataExperiment, 
                      x = datosplot[INDICES, 1], 
                      y = datosplot[INDICES, 2], 
                      color = (dataExperiment$del_or_coma_duration_exp),
                      colors = c("blue","yellow"),
                      type = "scatter",
                      mode = "markers",
                      text = paste0("Patient ID: ", rownames(dataExperiment[INDICES, ])),
                      marker = list(sizemode = "area", symbol = "diamond"),
                      showlegend = FALSE) %>%
  layout(title = "Outcomes (Del/comma - Duration)", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

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
  layout(title = "Outcomes (Del/comma - Duration)", xaxis = axx, yaxis = axy) %>%
  config(displaylogo = FALSE)

