# Jensen-Shannon Distance for Multivariate Gaussians
jsd_multivariate_gaussians <- function(mu1, Sigma1, mu2, Sigma2) {
  # Kullback-Leibler Divergence between two multivariate Gaussians
  kld <- function(mu1, Sigma1, mu2, Sigma2) {
    k <- length(mu1)
    detSigma1 <- det(Sigma1)
    detSigma2 <- det(Sigma2)
    invSigma2 <- solve(Sigma2)
    diffMu <- mu2 - mu1
    term1 <- sum(diag(invSigma2 %*% Sigma1))
    term2 <- t(diffMu) %*% invSigma2 %*% diffMu
    term3 <- log(detSigma2 / detSigma1)
    return(0.5 * (term1 + term2 - k + term3))
  }
  
  # Calculate the average mean and covariance matrix
  M_mu <- (mu1 + mu2) / 2
  M_Sigma <- (Sigma1 + Sigma2) / 2
  
  # Calculate the Jensen-Shannon Divergence
  jsd <- 0.5 * kld(mu1, Sigma1, M_mu, M_Sigma) + 0.5 * kld(mu2, Sigma2, M_mu, M_Sigma)
  
  # Return the square root of the Jensen-Shannon Divergence
  return(sqrt(jsd))
}

# Example usage:
# mu1 <- c(...) # Mean vector of the first Gaussian
# Sigma1 <- matrix(...) # Covariance matrix of the first Gaussian
# mu2 <- c(...) # Mean vector of the second Gaussian
# Sigma2 <- matrix(...) # Covariance matrix of the second Gaussian

# jsd <- jsd_multivariate_gaussians(mu1, Sigma1, mu2, Sigma2)
# print(jsd)
