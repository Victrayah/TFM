# setwd('C:/Users/Lexin Zhou/COVID_MEXICO/Spain Clustering')
source('./2020_covid19sdetool_generator/R/pCI.R')
source('./2020_covid19sdetool_generator/R/mCI.R')

library(readr)
library(Rtsne)
######
### The final PCA result with no severe or atypical outliers
DIAGN_vars = colnames(dataExperiment)[grep('DIAGN', colnames(dataExperiment))]
DIAGN_vars = setdiff(DIAGN_vars, 'DIAGN_covid')
MED_vars = colnames(dataExperiment)[grep('MED_', colnames(dataExperiment))]

dataExperiment[,MED_vars] = dataExperiment[,MED_vars] %>% replace(is.na(.), FALSE)
dataExperiment[,MED_vars] = dataExperiment[,MED_vars] %>% replace(is.na(.), FALSE)
dataExperiment = dataExperiment[!is.na(dataExperiment$ageCat),]
for (variable in c(MED_vars, DIAGN_vars)){
  dataExperiment[,variable] = ifelse(dataExperiment[,variable] == TRUE, 1, 0)
}

X_vars <- c("DATOS_Edad", "AN_ING_Leucocitos", "AN_ING_Neutrofilos", "AN_ING_Linfocitos", "AN_ING_Monocitos", "AN_ING_Eosinofilos", "AN_ING_Basofilos", 
            "AN_ING_Hematies", "AN_ING_Hemoglobina", "AN_ING_Hto", "AN_ING_Plaquetas", "DATOS_Sexo", "SOURCE", 'DATOS_Fecha_ingreso_URG', "ageCat", DIAGN_vars, MED_vars)
X_vars.numeric <-  c("DATOS_Edad","AN_ING_Leucocitos", "AN_ING_Neutrofilos", "AN_ING_Linfocitos", "AN_ING_Monocitos", "AN_ING_Eosinofilos", "AN_ING_Basofilos", 
                     "AN_ING_Hematies", "AN_ING_Hemoglobina", "AN_ING_Hto", "AN_ING_Plaquetas")

Y_vars <- c("MORTALITY_RAW_SINCE_FECHAI_UCI", "MORTALITY15_URG","MORTALITY30_URG", "MORTALITY15_ADM", "MORTALITY30_ADM", "MORTALITY7_UCI","MORTALITY30_UCI")
######

############# Clustering using PCA
res.pca <- PCA(dataExperiment[,X_vars.numeric], scale.unit = T, graph = FALSE, quali.sup = length(X_vars.numeric))
resultsClustering <- kmeans(res.pca$ind$coord[,1:3], center = bestk)
#############


groups <- resultsClustering$cluster
aux <- dataExperiment[,c(X_vars, Y_vars)]
# Silhouette <- intCriteria(as.matrix(dataExperiment[,X_vars.numeric]), resultsClustering$cluster, c("Silhouette"))
# a <- fviz_nbclust(x = dataExperiment[,X_vars.numeric], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
a <- fviz_nbclust(x =res.pca$ind$coord[,1:3], FUNcluster = kmeans, method = "silhouette",  k.max = bestk, verbose = FALSE) + labs(title = "Num. clusters")
Silhouette <- a$data$y[bestk]

######


######
INDICES <- intersect(INDICES, 1:nrow(dataExperiment))
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

pClusters2d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2],
                       color = as.factor(paste("Subgroup",format(resultsClustering$cluster[INDICES], digits=2))),
                       colors = brewer.pal(n = bestk, name = "Set3"),
                       type = "scatter", mode = "markers",
                       text = paste0("Patient ID: ",rownames(dataExperiment[INDICES,]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                       marker = list(sizemode = 'area'),
                       scene = 'sceneClusters') %>%
  layout(title = "Subgroups (obtained at 3D, switch to 3D for better display)", xaxis = axx, yaxis = axy) %>% 
  config(displaylogo = FALSE)


pClusters3d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2], z = res.pca$ind$coord[INDICES,3],
                       color = as.factor(paste("Subgroup",format(resultsClustering$cluster[INDICES], digits=2))),
                       colors = brewer.pal(n = bestk, name = "Set3"),
                       type = "scatter3d", mode = "markers",
                       text = paste0("Patient ID: ",rownames(resultsClustering$ind$coord[INDICES]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                       marker = list(sizemode = 'area'),
                       scene = 'sceneClusters') %>%
  layout(title = "Subgroups (obtained at 3D, switch to 3D for better display)", xaxis = axx, yaxis = axy) %>% 
  config(displaylogo = FALSE)


pOutcome2d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2],
                      color = as.factor(ifelse(dataExperiment$MORTALITY30_URG[INDICES]=='TRUE', 'Deceased', 'Non-Deceased')),
                      # colors = brewer.pal(n = 2, name = "Dark2"),
                      colors = c("firebrick", "darkseagreen2"),
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES,]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                      marker = list(sizemode = 'area', symbol = "diamond"),
                      scene = 'sceneOutcome') %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  # add_markers() %>% 
  config(displaylogo = FALSE)



pOutcome3d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2],z = res.pca$ind$coord[INDICES,3],
                      color = as.factor(ifelse(dataExperiment$MORTALITY30_URG[INDICES]=='TRUE', 'Deceased', 'Non-Deceased')),
                      # colors = brewer.pal(n = 2, name = "Dark2"),
                      colors = c("firebrick", "darkseagreen2"),
                      type = "scatter3d", mode = "markers",
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES,]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                      marker = list(sizemode = 'area', symbol = "diamond"),
                      scene = 'sceneOutcome') %>%
  layout(title = "Outcome", xaxis = axx, yaxis = axy) %>%
  # add_markers() %>% 
  config(displaylogo = FALSE)


pCountry2d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2],
                      color = factor(dataExperiment$ageCat[INDICES], levels = c('<18', '18-49', '50-64', '>64')),
                      # colors = brewer.pal(n = 2, name = "Dark2"),
                      colors = c("darkolivegreen2", "darkorange1"),
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES,]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                      marker = list(sizemode = 'area', symbol = "diamond"),
                      scene = 'sceneOutcome') %>%
  layout(title = "SOURCE", xaxis = axx, yaxis = axy) %>%
  # add_markers() %>% 
  config(displaylogo = FALSE)

pCountry3d <- plot_ly(x = res.pca$ind$coord[INDICES,1], y = res.pca$ind$coord[INDICES,2],z = res.pca$ind$coord[INDICES,3],
                      color = factor(dataExperiment$ageCat[INDICES], levels = c('<18', '18-49', '50-64', '>64')),
                      # colors = brewer.pal(n = 2, name = "Dark2"),
                      colors = c("darkolivegreen2", "darkorange1"),
                      type = "scatter3d", mode = "markers",
                      text = paste0("Patient ID: ",rownames(dataExperiment[INDICES,]),"\n Age:",dataExperiment$DATOS_Edad[INDICES]),
                      marker = list(sizemode = 'area', symbol = "diamond"),
                      scene = 'sceneOutcome') %>%
  layout(title = "SOURCE", xaxis = axx, yaxis = axy) %>%
  # add_markers() %>% 
  config(displaylogo = FALSE)


######### TABLE #########
library(dplyr)
library(kableExtra)
alphaci = 0.05
uniqueGroups = 1:length(unique(resultsClustering$cluster))
resultsTable = data.frame(matrix(nrow = length(X_vars)+5, ncol = length(uniqueGroups)))

# Note: this (whihout make.names) returns an error when same text is in symptoms and comorbidities, a solution might be using a column for names, or avoid repated terms
rownames(resultsTable) <- c(sprintf('No. of individuals (n<sub>total</sub> = %d)',nrow(dataExperiment)), "Age","Age<18","Age18-49","Age50-64","Age>64", "Sex=Female", "AN_ING_Leucocitos", "AN_ING_Neutrofilos", "AN_ING_Linfocitos", "AN_ING_Monocitos", "AN_ING_Eosinofilos", "AN_ING_Basofilos", 
                            "AN_ING_Hematies", "AN_ING_Hemoglobina", "AN_ING_Hto", "AN_ING_Plaquetas" ,  DIAGN_vars, MED_vars, "Source=H12O", "MORTALITY7_UCI", "MORTALITY30_UCI")
colnames(resultsTable) <- paste('Subgroup',uniqueGroups)

data6 <- dataExperiment[,c( "AN_ING_Leucocitos", "AN_ING_Neutrofilos", "AN_ING_Linfocitos", "AN_ING_Monocitos", "AN_ING_Eosinofilos", "AN_ING_Basofilos", 
                            "AN_ING_Hematies", "AN_ING_Hemoglobina", "AN_ING_Hto", "AN_ING_Plaquetas" , DIAGN_vars, MED_vars)]

for (i in 1:length(uniqueGroups)){
  patientGroupIdx = resultsClustering$cluster %in% sort(uniqueGroups)[i]
  nPatientsGroup = sum(patientGroupIdx)
  # comorbidity statistics
  data_analysis_subgroup = data6[patientGroupIdx,,drop = FALSE]
  nind = nrow(data_analysis_subgroup)
  data_analysis_subgroupT = t(data_analysis_subgroup)
  
  resultsS = sapply(data.frame(data_analysis_subgroup[, grep('AN', colnames(data_analysis_subgroup))]), function(x) mCI(x, alphaci))
  
  resultsDIAGN = sapply(data.frame(data_analysis_subgroup[,DIAGN_vars]), function(x) pCI(x, alphaci)*100)
  
  resultsMED = sapply(data.frame(ifelse(data_analysis_subgroup[,MED_vars] == 1, TRUE, FALSE)), function(x) pCI(x, alphaci)*100)
  
  # subgroupResultColumn = apply(resultsS, 2, function(x) sprintf('%.3f (%.3f-%.3f)',x[1],x[2],x[3]))
  
  # blood analytics ED (Emergency Department)
  data_analysis_metadata_subgroup = dataExperiment[patientGroupIdx,]
  
  # Age
  AgeStats = mCI(data_analysis_metadata_subgroup$DATOS_Edad, alphaci)
  
  #Age range stats
  Age17Stats = pCI(data_analysis_metadata_subgroup$ageCat == '<18', alphaci)*100
  Age18Stats = pCI(data_analysis_metadata_subgroup$ageCat == '18-49', alphaci)*100
  Age50Stats = pCI(data_analysis_metadata_subgroup$ageCat == '50-64', alphaci)*100
  Age65Stats = pCI(data_analysis_metadata_subgroup$ageCat == '>64', alphaci)*100
  
  femaleStats = pCI(data_analysis_metadata_subgroup$DATOS_Sexo=='Female', alphaci)*100
  sourceStats =  pCI(data_analysis_metadata_subgroup$SOURCE == 'H12O', alphaci)*100
  recovered15Stats = pCI(data_analysis_metadata_subgroup$MORTALITY7_UCI == 'TRUE', alphaci) * 100
  recovered30Stats = pCI(data_analysis_metadata_subgroup$MORTALITY30_UCI == 'TRUE', alphaci) * 100
  
  # compile final result column
  subgroupResultColumn = c(as.character(nPatientsGroup), sprintf('%.2f (%.2f-%.2f)',AgeStats[1],AgeStats[2],AgeStats[3]), sprintf('%.2f (%.2f-%.2f)',Age17Stats[1],Age17Stats[2],Age17Stats[3]), sprintf('%.2f (%.2f-%.2f)',Age18Stats[1],Age18Stats[2],Age18Stats[3]), sprintf('%.2f (%.2f-%.2f)',Age50Stats[1],Age50Stats[2],Age50Stats[3]), sprintf('%.2f (%.2f-%.2f)',Age65Stats[1],Age65Stats[2],Age65Stats[3]), sprintf('%.2f (%.2f-%.2f)',femaleStats[1],femaleStats[2],femaleStats[3]), apply(resultsS, 2, function(x) sprintf('%.3f (%.3f-%.3f)',x[1],x[2],x[3])), apply(resultsDIAGN, 2, function(x) sprintf('%.2f (%.2f-%.2f)',x[1],x[2],x[3])),apply(resultsMED, 2, function(x) sprintf('%.2f (%.2f-%.2f)',x[1],x[2],x[3])), sprintf('%.2f (%.2f-%.2f)',sourceStats[1],sourceStats[2],sourceStats[3]),
                           sprintf('%.2f (%.2f-%.2f)',recovered15Stats[1],recovered15Stats[2],recovered15Stats[3]),
                           sprintf('%.2f (%.2f-%.2f)',recovered30Stats[1],recovered30Stats[2],recovered30Stats[3]))
  
  resultsTable[i] <- subgroupResultColumn
}

# resultsTable$variables <- rownames(resultsTable)
# write_xlsx(resultsTable,"C:\\Users\\Lexin Zhou\\11Meta_subgroups_table.xlsx")

# names(resultsTable) <- cell_spec(names(resultsTable), background = "yellow")
tTable = kable(resultsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  pack_rows("Features (%|x, CI 95%)", 2, 7) %>%
  pack_rows("Blood tests (%|x, CI 95%)", 8, 17) %>%
  pack_rows("Diagnoses (%|x, CI 95%)", 18, 22+4) %>%
  pack_rows("Medicines (%|x, CI 95%)", 23+4, 30+4+2) %>%
  pack_rows("Outcomes (%|x, CI 95%)", 31+4+2, 33+4+2) 


groupColors = brewer.pal(n = ncol(resultsTable), name = "Set3")

for (i in 1:ncol(resultsTable)) {
  # tTable = tTable %>% column_spec(i+1, background = groupColors[i], include_thead = TRUE) %>% column_spec(i+1, background = "inherit")
  tTable = tTable
}



### Ages plot ###
groupColors = brewer.pal(n = bestk, name = "Set3")

pAges <- plot_ly(x = resultsClustering$cluster, y = dataExperiment$DATOS_Edad,
                 type = 'box',
                 color = as.factor(resultsClustering$cluster),
                 colors = groupColors
) 

# plot_ly(data_analysis_metadata, y=~ageNum,x=~ClusterGroup,color=~ClusterGroup,type="box")

pAges <- pAges %>%
  layout(
    title = "Age by cluster",
    xaxis = list(
      title = "cluster"
      
    ),
    yaxis = list(
      title = "Age",
      zeroline = F
    )
  ) %>% 
  config(displaylogo = FALSE)






######### BARPLOTS #########

alphaci = 0.05
uniqueGroups = unique(resultsClustering$cluster)
nSubgroups = length(uniqueGroups)
resultsBySubgroup = vector("list",length(uniqueGroups))
# colnames(data_analysis) = substr(colnames(data_analysis),3,nchar(colnames(data_analysis)))

featuresVector = data6[,MED_vars]
data_analysis = data6[, c(MED_vars, DIAGN_vars)]

for (i in 1:length(uniqueGroups)){
  
  patientGroupIdx = resultsClustering$cluster %in% uniqueGroups[i]
  nPatientsGroup = sum(patientGroupIdx)
  
  data_analysis_subgroup = data_analysis[patientGroupIdx,,drop = FALSE]
  nind = nrow(data_analysis_subgroup)
  
  resultsFeatures = sapply(data.frame(data_analysis_subgroup[,1:ncol(featuresVector), drop = FALSE]),     function(x) pCI(x, alphaci)*100)
  colnames(resultsFeatures) = str_replace_all(colnames(resultsFeatures),"\\.", " ")
  resultsFeaturesErr = resultsFeatures[3,] - resultsFeatures[1,]
  
  resultsComorbidities = sapply(data.frame(data_analysis_subgroup[,-(1:ncol(featuresVector)), drop = FALSE]), function(x) pCI(x, alphaci)*100)
  colnames(resultsComorbidities) = str_replace_all(colnames(resultsComorbidities),"\\.", " ")
  resultsComorbiditiesErr = resultsComorbidities[3,] - resultsComorbidities[1,]
  
  # sex, age, recovered statistics
  data_analysis_metadata <- dataExperiment
  data_analysis_metadata_subgroup = data_analysis_metadata[patientGroupIdx,, drop = FALSE]
  femaleStats = pCI(as.character(data_analysis_metadata_subgroup$DATOS_Sexo) %in% 'M',alphaci)*100
  ageMean = mean(data_analysis_metadata_subgroup$DATOS_Edad)
  ageErr = qnorm(0.975)*sd(data_analysis_metadata_subgroup$DATOS_Edad)/sqrt(nPatientsGroup)
  ageStats = c(ageMean, ageErr)
  recoveredStats = pCI(data_analysis_metadata_subgroup$MORTALITY15_URG == 'False',alphaci)*100
  subgroupResults = list(name = paste("Subgroup", i),features = resultsFeatures, comorbidities = resultsComorbidities, featuresErr = resultsFeaturesErr,  comorbiditiesErr = resultsComorbiditiesErr, nPatientsGroup = nPatientsGroup, femaleStats = femaleStats, recoveredStats = recoveredStats, ageStats = ageStats)
  resultsBySubgroup[[i]] <- subgroupResults
}

resultsStats = lapply(resultsBySubgroup, function(x) x$features)
resultsStatsMean = unlist(lapply(resultsStats, function(x) x[1,]))
resultsStatsErr = unlist(lapply(resultsBySubgroup, function(x) x$featuresErr))

subgroupIds = 1:length(resultsBySubgroup)
groupColors = colorRampPalette(brewer.pal(nSubgroups,"Set3"))(nSubgroups)

pFeaturesBar <- plot_ly(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                        marker = list(color = groupColors[i]),
                        error_y = list(type = "data",
                                       array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                       arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                       color = '#888888'))
for (i in 2:length(uniqueGroups)){
  pFeaturesBar <- pFeaturesBar  %>% add_trace(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                                              marker = list(color = groupColors[i]),
                                              error_y = list(type = "data",
                                                             array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                                             arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                                             color = '#888888'))  
}
# config(displayModeBar = FALSE) %>%
pFeaturesBar <- pFeaturesBar  %>% layout(title = sprintf("Features by subgroup"),
                                         xaxis = list(title = "Medication"),
                                         yaxis = list(title = "% (CI 95%)"),
                                         barmode = 'group'
) %>% 
  config(displaylogo = FALSE)

# pFeaturesBar
pSymptomsBar = pFeaturesBar

i = 1
pFeaturesBarSub <- plot_ly(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                           marker = list(color = groupColors[i]),
                           legendgroup = groupColors[i],
                           # legendgroup = 'group1',
                           # showlegend = T,
                           error_y = list(type = "data",
                                          array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                          arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                          color = '#888888'))
for (i in 2:length(uniqueGroups)){
  pFeaturesBarSub <- pFeaturesBarSub  %>% add_trace(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                                                    marker = list(color = groupColors[i]),
                                                    legendgroup = groupColors[i],
                                                    # legendgroup = 'group1',
                                                    # showlegend = F,
                                                    error_y = list(type = "data",
                                                                   array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                                                   arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$featuresErr),
                                                                   color = '#888888'))  
}
# config(displayModeBar = FALSE) %>%
pFeaturesBarSub <- pFeaturesBarSub  %>% layout(title = sprintf("Features by subgroup"),
                                               xaxis = list(title = "Medication"),
                                               yaxis = list(title = "% (CI 95%)"),
                                               barmode = 'group'
) %>% 
  config(displaylogo = FALSE)
pSymptomsBarSub = pFeaturesBarSub



resultsStats = lapply(resultsBySubgroup, function(x) x$comorbidities)
resultsStatsMean = unlist(lapply(resultsStats, function(x) x[1,]))
resultsStatsErr = unlist(lapply(resultsBySubgroup, function(x) x$comorbiditiesErr))
subgroupIds = 1:length(resultsBySubgroup)
groupColors = colorRampPalette(brewer.pal(nSubgroups,"Set3"))(nSubgroups)
i = 1
pComorbiditiesBar <- plot_ly(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                             marker = list(color = groupColors[i]),
                             error_y = list(symmetric = FALSE,
                                            type = "data",
                                            array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                            arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                            color = '#888888'))
for (i in 2:length(uniqueGroups)){
  pComorbiditiesBar <- pComorbiditiesBar  %>% add_trace(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                                                        marker = list(color = groupColors[i]),
                                                        error_y = list(type = "data",
                                                                       array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                                                       arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                                                       color = '#888888')) 
}
# config(displayModeBar = FALSE) %>%
pComorbiditiesBar <- pComorbiditiesBar  %>% layout(title = sprintf("Comorbidities by subgroup"),
                                                   xaxis = list(title = "Comorbidities"),
                                                   yaxis = list(title = "% (CI 95%)"),
                                                   barmode = 'group'
) %>% 
  config(displaylogo = FALSE)
pComorbiditiesBar
# pComorbiditiesBar

i = 1
pComorbiditiesBarSub <- plot_ly(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                                marker = list(color = groupColors[i]),
                                legendgroup = groupColors[i],
                                ## legendgroup = 'group1',
                                showlegend = F,
                                error_y = list(symmetric = FALSE,
                                               type = "data",
                                               array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                               arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                               color = '#888888'))
for (i in 2:length(uniqueGroups)){
  pComorbiditiesBarSub <- pComorbiditiesBarSub  %>% add_trace(x = colnames(resultsStats[[i]]), y = resultsStats[[i]][1,], type = 'bar', name = paste("Subgroup", i),
                                                              marker = list(color = groupColors[i]),
                                                              legendgroup = groupColors[i],
                                                              ## legendgroup = 'group1',
                                                              showlegend = F,
                                                              error_y = list(type = "data",
                                                                             array = pmin(100-resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                                                             arrayminus = pmin(resultsStats[[i]][1,],resultsBySubgroup[[i]]$comorbiditiesErr),
                                                                             color = '#888888')) 
}
# config(displayModeBar = FALSE) %>%
pComorbiditiesBarSub <- pComorbiditiesBarSub  %>% layout(title = sprintf("Medication and comorbidities by subgroup"),
                                                         xaxis = list(title = "Comorbidities"),
                                                         yaxis = list(title = "% (CI 95%)"),
                                                         barmode = 'group'
) %>% 
  config(displaylogo = FALSE)
# pComorbiditiesBarSub


# pComorbiditiesBarSub

pSubplotSymptomsAndComorbidities = subplot(pSymptomsBarSub, pComorbiditiesBarSub, shareX =TRUE, shareY = TRUE, widths = c(0.4, 0.6))
# pSubplotSymptomsAndComorbidities


