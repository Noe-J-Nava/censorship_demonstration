# Noé J Nava
# USDA - ERS - APM
# Noe.Nava@usda.gov

# Note: Script is a helper function to calculate the sum of the log-likelihood

aids.logLike <- function(param, s1, s2, lnp1, lnp2, lnp3, lnw) {
  # Defining the parameters with their parameter restrictions
  # alphas
  a1 <- param[1]
  a2 <- param[2]
  a3 <- (1 - a1 - a2)
  
  # betas
  b1 <- param[3]
  b2 <- param[4]
  b3 <- (-b1 - b2)
  
  # gammas
  g11 <- param[5]  
  g12 <- param[6]
  g13 <- (-g11 - g12)
  
  g21 <- g12
  g22 <- param[7]
  g23 <- (-g21 - g22)
  
  g31 <- g13
  g32 <- g23
  g33 <- (-g31 - g32)
  
  # sigmas
  sig11 <- param[8]
  sig12 <- param[9]
  sig21 <- sig12
  sig22 <- param[10]
  
  lnpindex <- 0 + a1*lnp1 + a2*lnp2 + a3*lnp3
  for(i in as.character(1:3)) {
    for(j in as.character(1:3)) {
      
      gij <- get(paste0("g", i, j))
      lnpi  <- get(paste0("lnp", i))
      lnpj  <- get(paste0("lnp", j))
      lnpindex <- lnpindex + 0.5*gij*lnpi*lnpj
      
    }
  }
  
  # Share equations
  s1hat <- a1 + g11*lnp1 + g12*lnp2 + g13*lnp3 + b1*(lnw - lnpindex)
  s2hat <- a2 + g21*lnp1 + g22*lnp2 + g23*lnp3 + b2*(lnw - lnpindex)
  
  shat <- c(s1hat, s2hat)
  s    <- cbind(s1, s2)
  
  # Variance and correlation of variables
  sigma <- c(sig11, sig12, sig21, sig22)
  sigma <- matrix(sigma, ncol = 2, nrow = 2)
  
  logLike <- mvtnorm::dmvnorm(s-shat, sigma = sigma, log = TRUE)
  
  return(sum(logLike))
  
}
