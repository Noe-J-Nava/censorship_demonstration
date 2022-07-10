# Noé J Nava
# USDA - ERS - APM
# Noe.Nava@usda.gov

# Note: Script presents etas into a good presentation
present.etas <- function(etas) {
  col4 <- etas[1:3,1] # budget etas
  row1 <- t(etas[4:6,1]) # price eta wrt. q1
  row2 <- t(etas[7:9,1]) # price eta wrt. q2
  row3 <- t(etas[10:12,1]) # price eta wrt. q3
  
  tab <- matrix(NA, nrow = 3, ncol = 4)
  tab[,4] <- col4
  tab[1,1:3] <- row1
  tab[2,1:3] <- row2
  tab[3,1:3] <- row3
  
  return(tab)
  
}
