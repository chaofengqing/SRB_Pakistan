

GetJackknife <- function(
  data.i, # full sample
  weight.i,
  cluster.i, # identifier for primary sample unit (PSU) to be removed for each
  # round of Jackknife computation.
  observed # observed value that we want to get Jacknife estimate and SE
) {
  ## get estimate and standard error by Jackknife method ##
  # note: for DHS data only!
  
  # total number of primary sample unit for this DHS survey
  cluster.u <- sort(unique(cluster.i))
  U         <- length(cluster.u)
  partial.u <- rep(NA, U)
  
  for (u in 1:U) {
    # identify the u-th cluster and remove data from it
    select.i <- (cluster.i == cluster.u[u])
    
    # the u-th partial prediction SRB(-u)
    malebirth <- sum((data.i[!select.i] == "Male") *
                       weight.i[!select.i], na.rm = TRUE)
    femalebirth <- sum((data.i[!select.i] == "Female") *
                         weight.i[!select.i], na.rm = TRUE)
    partial.u[u] <- GetSRB(malebirth, femalebirth)
    
  }#end of u loop    
  
  # Jackknife estimate and SE
  # original scale
  est <- U * observed - (U - 1) * mean(partial.u)
  se  <- sd(partial.u) * (U - 1) / sqrt(U)
  # natural log scale
  estlog <- U * log(observed) - (U - 1) * mean(log(partial.u))
  selog  <- sd(log(partial.u)) * (U - 1) / sqrt(U)
  
  result <- list(est = est, se = se, estlog = estlog, selog = selog)
  return(result)
  
}#end of GetJackknife() function
