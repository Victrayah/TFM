
# Calcular CI-mean Numericas

CImean <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return("NA")
  }
  se <- sd(x) / sqrt(length(x))
  t <- qt(0.975, df = length(x) - 1)
  lower <- mean(x) - t * se
  upper <- mean(x) + t * se
  paste0(round(mean(x), 2), " (", round(lower, 2), "-", round(upper, 2), ")")
}

# Calcular CI-proportion Binarias

CIproportion <- function(x) {
  n <- sum(!is.na(x))
  prop <- (sum(x, na.rm = TRUE) / n)*100
  error <- (qnorm(0.975) * sqrt((prop/100)*(1-(prop/100))/n))*100
  lower <- prop - error
  upper <- prop + error
  paste0(round(prop, 2), " (", round(lower, 2), "-", round(upper, 2), ")")
}

# Integrar ambos cálculos y fila NA

mpCIfunction <- function(data, clusterVariable) {
  require(dplyr)
  
  # Cálculo según tipo de variable
  
  calcularIntervalo <- function(x) {
    if (all(x %in% c(0, 1))) {
      CIproportion(x)
    } else {
      CImean(x)
    }
  }
  
  # Cálculo NA
  
  calcularNA <- function(x) {
    n <- length(x)
    sum(is.na(x)) / n * 100
  }
  
  # Integrar todo
  
  mediaIntervaloNAporGrupo <- data %>%
    group_by({{ clusterVariable }}) %>%
    summarise(across(everything(), list(Intervalo = calcularIntervalo, NA_Percentage = calcularNA))) %>%
    t()
  
  return(mediaIntervaloNAporGrupo)
}

# Adaptar mpCIdf

convertir_PREmpCIdf <- function(PREmpCIdf) {
  colnames(PREmpCIdf) <- paste0("Subgroup ", PREmpCIdf[1, ])
  PREmpCIdf <- as.data.frame(PREmpCIdf)
  PREmpCIdf <- PREmpCIdf[-1, ]
  mpCIdf <- data.frame()
  num_filas <- nrow(PREmpCIdf)
  
  for (i in seq(1, num_filas, by = 2)) {
    fila_1 <- PREmpCIdf[i, ]
    fila_2 <- PREmpCIdf[i + 1, ]
    
    if (any(fila_2 == 0)) {
      fila_combinada <- fila_1
    } else {
      fila_combinada <- paste(fila_1, "NA:", round(as.numeric(fila_2), 2), "%", sep = " ")
    }
    mpCIdf <- rbind(mpCIdf, fila_combinada)
  }
  rownames(mpCIdf) <- substr(rownames(PREmpCIdf)[seq(1, num_filas, by = 2)], 1, nchar(rownames(PREmpCIdf)[seq(1, num_filas, by = 2)]) - 10)
  colnames(mpCIdf) <- paste("Subgrupo", 1:ncol(mpCIdf))
  
  return(mpCIdf)
}

# Filtrar las variables binarias (0, 1 y NA)
binary_variables <- dataWithClusters %>%
  select_if(function(col) all(col %in% c(0, 1, NA)))

# Realizar Chi-cuadrado en las variables binarias
chi_squared_results <- lapply(binary_variables, function(col) {
  if (length(unique(col)) > 1) {
    chisq.test(table(col, dataWithClusters$ClusterGroup))
  } else {
    NULL
  }
})

# Filtrar las variables no binarias
non_binary_variables <- dataWithClusters %>%
  select_if(function(col) any(!col %in% c(0, 1, NA)))

# Realizar Kruskal-Wallis en las variables no binarias
kruskal_wallis_results <- lapply(non_binary_variables, function(col) {
  kruskal.test(col ~ dataWithClusters$ClusterGroup)
})
