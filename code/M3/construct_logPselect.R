###############################################################################
# Estimation and probabilistic projection of levels and trends 
# in the sex ratio at birth in seven provinces of Nepal
# from 1980 to 2050: a Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 27 Aug 2025
# 
# construct_logPselect.R
# 
# This script is to get trajectories for logP's for each year of the full
# observation period. In JAGS model, we only save those non-missing logP's.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: null
# 
# input data: null
#
# output data: null
#
###############################################################################

# note: here l refers to posterior sample
rho.l    <- rep(rho, L)
sigma.l  <- rep(sigma.eps, L)
logP.jtl <- array(NA, c(C.adj, Tend, L))

for (j in 1:C.adj) {
  set.seed(j*123)
  cat(j, "/", C.adj, "\n")

  t.min <- min.t.j[j]
  t.max <- max.t.j[j]
  
  for (t in 1:t.max) {
    logP.jtl[j, t, ] <- c(mcmc.array[, , paste0("logP.jt[", j, ",", t, "]")])
  } # end of t loop
  
  for (t in (t.max+1):Tend) {
    mean.l <- rho.l * logP.jtl[j, t-1, ]
    logP.jtl[j, t, ] <- rnorm(n = L, mean = mean.l, sd = sigma.l)
  } # end of t loop
} # end of c loop

selectP <- list(logP.jtl = logP.jtl)

## the end ##

