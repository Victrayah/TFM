
 # Obtén los tamaños de subgrupo
 subgroup_sizes <- table(dataWithClusters$ClusterGroup)
 original_colnames <- colnames(resultsTable)
 new_colnames <- character(length(original_colnames))
 
 # Recorre las columnas y actualiza los nombres
 for (i in 1:length(original_colnames)) {
   if (i < length(original_colnames)) {
     # Actualiza los nombres de las columnas excepto la última (p-valor)
     new_colnames[i] <- paste("Subgrupo", unique(dataWithClusters$ClusterGroup)[i], "(n = ", subgroup_sizes[i], ")")
   } else {
     # Mantén el nombre de la última columna (p-valor) igual
     new_colnames[i] <- original_colnames[i]
   }
 }

 colnames(resultsTable) <- new_colnames

 # #k=5
 # new_colnames[2] <- "Subgrupo 5 (n = 68)"
 # #k=6
 # new_colnames[2] <- "Subgrupo 4 (n = 137)" 
 # new_colnames[3] <- "Subgrupo 5 (n = 282)"
 # new_colnames[4] <- "Subgrupo 6 (n = 63)"
 

 