# Noé J Nava
# USDA - ERS - APM
# Noe.Nava@usda.gov

# Note: Script uses the latent share to calculate the observed shares.
# Mapping latent shares to observed shares - function
# This is equation (22) in Nava and Dong (2022)
lat <- S
obs.frm.lat.shr <- function(lat) {
  
  lat <- ifelse(lat < 0, 0, lat)
  
  denominator <- rowSums(lat)
  
  lat <- lat/denominator
  
  return(obs)
  
}




S


