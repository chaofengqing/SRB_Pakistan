

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# construct_logPselect.R
# 
# This script is to get trajectories for logP's for each year of the full
# observation period. In JAGS model, we only save those non-missing logP's.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# InternalGetARTrajectories(1)
# 
# input data: data/output/runname/mcmc.array_runname.rda
#
# output data: data/output/runname/selectP_runname.rda
#
###############################################################################

# note: here l refers to posterior sample
rho.l   <- rep(rho, L)
sigma.l <- rep(sigma.eps, L)
logP.jtl <- array(NA, c(C.adj, Tend, L))

for (j in 1:C.adj) {
  set.seed(j*123)
  cat(j, "/", C.adj, "\n")

  t.min <- min.t.j[j]
  t.max <- max.t.j[j]
  
  for (t in 1:t.max) {
    logP.jtl[j, t, ] <- c(mcmc.array[, , paste0("logP.jt[", j, ",", t, "]")])
  }#end of t loop
  
  for (t in (t.max+1):Tend) {
    mean.l <- rho.l * logP.jtl[j, t-1, ]
    logP.jtl[j, t, ] <- rnorm(n = L, mean = mean.l, sd = sigma.l)
  }#end of t loop
}#end of c loop

selectP <- list(logP.jtl = logP.jtl)

## the end ##

