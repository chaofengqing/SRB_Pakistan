###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# Rhat.R
# 
# This script contains function related to computing the Gelman-Rubin R_hat. 
# The functions accept MCMC arrays and return R-hat values to check convergence.
#
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# Rhat1(..)
# Rhat(..,Rhat1(3),..) 
#
###############################################################################

Rhat1 <- function(mat) {
  ## compute R hat for mcmc.array to check convergence ##
  m <- ncol(mat)
  n <- nrow(mat)
  b <- apply(mat, 2, mean)
  B <- sum((b - mean(mat))^2) * n / (m - 1)
  w <- apply(mat, 2, var)
  W <- mean(w)
  
  s2hat <- (n - 1) / n * W + B / n
  Vhat  <- s2hat + B / m / n 
  covWB <- n / m * (cov(w, b^2) - 2 * mean(b) * cov(w, b))
  varV  <- (n - 1)^2 / n^2 * var(w) / m +
    (m + 1)^2 / m^2 / n^2 * 2 * B^2 / (m - 1) +
    2 * (m - 1) * (n - 1) / m / n^2 * covWB
  
  df <- 2 * Vhat^2 / varV
  R  <- sqrt((df + 3) * Vhat / (df + 1) / W)
  
  return(R)
  
} # end of Rhat1 function


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


Rhat <- function(arr) {
  dm <- dim(arr)
  
  if (length(dm) == 2) return(Rhat1(arr))
  if (dm[2] == 1) return(NULL)
  if (dm[3] == 1) return(Rhat1(arr[, , 1]))
  
  return(apply(arr, 3, Rhat1))
  
} # end of Rhat function


