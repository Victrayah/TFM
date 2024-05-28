library(irr)
library(ggalluvial)
library(Kendall)

resultados <- list()

# Loop through results_list
for (i in seq_along(results_list)) {
  resultados[[i]] <- as.data.frame(results_list[[i]]$groups)
}

resultados <- data.frame(matrix(ncol = num_experiments, nrow = nrow(datosplot)))

resultados <- as.data.frame(results_list)
rownames(resultados) <- rownames(data_inputs)
names(resultados) <- paste('Experimento-',1:num_experiments)

# Kappa fleiss
kappa_fleiss <- kappam.fleiss(results, exact = FALSE, detail = TRUE)
print(kappa_fleiss)

# Kendall
paciente <- as.numeric(clusters_list[[4]][13, ])
paciente <- paciente[!is.na(paciente)]
valor_estandar <- as.numeric(rep(df_counts[13, 'ResolvedValue'], length(paciente)))
coef_kendall <- Kendall(paciente, valor_estandar)
coef_kendall
coef_kendall_2 <- cor(paciente, valor_estandar, method = 'kendall')
coef_kendall_2

# Chi 2
