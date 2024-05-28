dataWithClusters <- data_inputs
dataWithClusters$ClusterGroup <- as.factor(groups)  

#Función
calcularIntervaloConfianzaNumericas <- function(x) {
  se <- sd(x) / sqrt(length(x))
  t <- qt(0.975, df = length(x) - 1)
  lower <- mean(x) - t * se
  upper <- mean(x) + t * se
  paste0(round(mean(x), 2), " (", round(lower, 2), "-", round(upper, 2), ")")
}

calcularIntervaloConfianzaBinarias <- function(x) {
  n <- sum(!is.na(x))
  prop <- sum(x, na.rm = TRUE) / n
  error <- qnorm(0.975) * sqrt(prop*(1-prop)/n)
  if (is.na(prop) || is.na(error) || prop - error <= 0 || prop + error >= 1) {
    return("NA")
  }
  lower <- prop - error
  upper <- prop + error
  paste0(round(prop, 2), " (", round(lower, 2), "-", round(upper, 2), ")")
}

mpCIfunction <- function(data, clusterVariable) {
  require(dplyr)
  
  calcularIntervalo <- function(x) {
    if (all(x %in% c(0, 1))) {
      calcularIntervaloConfianzaBinarias(x)
    } else {
      calcularIntervaloConfianzaNumericas(x)
    }
  }
  
  mediaIntervaloPorGrupo <- data %>%
    group_by({{ clusterVariable }}) %>%
    summarise(across(everything(), calcularIntervalo)) %>%
    t()
  
  return(mediaIntervaloPorGrupo)
}


#Generar tabla 
resultado <- mpCIfunction(dataWithClusters, ClusterGroup)
colnames(resultado)<-paste0("Subgroup ", resultado[1,])
resultado<-as.data.frame(resultado)
resultado <- resultado[-1, ]
resultsTable<-resultado

tTable = kable(resultsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  pack_rows("Features (%|x, CI 95%)", 1, 3) %>%
  pack_rows("Charlson vars (%|x, CI 95%)", 4, 23) %>%
  pack_rows("Others (%|x, CI 95%)", 24, 28)
