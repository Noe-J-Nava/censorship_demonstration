# Noé J Nava
# USDA - ERS - APM
# Noe.Nava@usda.gov

# Note: Script is a helper function to estimate the elasticities of the system
# using the outcome optimOut#. 
# Therefore, the order of the parameters must not be changed!!!!!!!

etas.estimation <- function(params, s1, s2, s3, lnp1, lnp2, lnp3) {
  
  # Evaluating at their means
  mus1 <- mean(s1)
  mus2 <- mean(s2)
  mus3 <- mean(s3)
  
  mulnp1 <- mean(lnp1)
  mulnp2 <- mean(lnp2)
  mulnp3 <- mean(lnp3)
  
  #params <- optimOut$par
  
  # alphas
  a1 <- params[1]
  a2 <- params[2]
  a3 <- 1 - (a1 + a2)
  
  # betas
  b1 <- params[3]
  b2 <- params[4]
  b3 <- - (b1 + b2)
  
  # gammas
  g11 <- params[5]  
  g12 <- params[6]
  g13 <- - (g11 + g12)
  
  g21 <- g12
  g22 <- params[7]
  g23 <- - (g21 + g22)
  
  g31 <- g13
  g32 <- g23
  g33 <- - (g31 + g32)
  
  # Budget elasticity: 
  for(i in 1:3) {
    bi <- get(paste0("b", i))
    musi  <- get(paste0("mus", i))
    
    etai <- 1 + (bi/musi)
    assign(paste0("eta", i), etai)
  }
  
  # Price elasticity:
  for(i in 1:3) {
    for(j in 1:3) {
      gij <- get(paste0("g", i, j))
      bi  <- get(paste0("b", i))
      aj  <- get(paste0("a", j))
      
      gj1 <- get(paste0("g", j, "1"))
      gj2 <- get(paste0("g", j, "2"))
      gj3 <- get(paste0("g", j, "3"))
      
      muij  <- gij - bi*(aj + gj1*mulnp1 + gj2*mulnp2 + gj3*mulnp3)
      musi  <- get(paste0("mus", i))
      etaij <- (muij/musi)
      
      assign(paste0("eta", i, j), etaij)
    }
  }
  
  eta11 <- eta11 - 1
  eta22 <- eta22 - 1
  eta33 <- eta33 - 1
  
  etas <- c(eta1, eta2, eta3,
            eta11, eta12, eta13,
            eta21, eta22, eta23,
            eta31, eta32, eta33)
  etas <- matrix(etas, nrow = length(etas), ncol = 1)
  
  return(etas)
  
}

