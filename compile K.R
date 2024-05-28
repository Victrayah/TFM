


#TSNE
TSNEresults<- list()
TSNESilhouette <- list()

# Definir un vector para almacenar los resultados de Silhouette
TSNESilhouette <- numeric(12)

# Crear un bucle que va desde bestk=2 hasta 12
for (bestk in 2:12) {
  # Cargar el script a compilar.R
  source("script_compilar_TSNE.R")
  
  # Almacenar los resultados en la lista 'results'
  TSNEresults[[bestk]] <- list(
    pClusters2d = pClusters2d,
    pClusters3d = pClusters3d,
    pOutcome2d = pOutcome2d,
    pOutcome3d = pOutcome3d,
    ITable = ITable,
    OTable = OTable,
    tTable = tTable
  )
  
  # Calcular y almacenar el valor de Silhouette
  TSNESilhouette[bestk] <- Silhouette
}


#PCA
PCAresults <- list()
PCASilhouette <- list()

# Definir un vector para almacenar los resultados de Silhouette
PCASilhouette <- numeric(12)

# Crear un bucle que va desde bestk=2 hasta 12
for (bestk in 2:12) {
  # Cargar el script a compilar.R
  source("script_compilar_PCA.R")
  
  # Almacenar los resultados en la lista 'results'
  PCAresults[[bestk]] <- list(
    Biplot = Biplot,
    pClusters2d = pClusters2d,
    pClusters3d = pClusters3d,
    ITable = ITable,
    OTable = OTable,
    tTable = tTable,
    pOutcome2d = pOutcome2d,
    pOutcome3d = pOutcome3d
  )
  
  # Calcular y almacenar el valor de Silhouette
  PCASilhouette[bestk] <- Silhouette
}

# Guardar los resultados en un archivo 'Allresults.Rdata'
save(TSNEresults, 
     TSNESilhouette,
     PCAresults, 
     PCASilhouette, file='AllresultsPERPvalue91.Rdata')


