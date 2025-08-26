##############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan from 
# 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 26 Aug 2025
# construct_ADJselect.R
# 
# This script extracts posterior samples of the adjustment parameters.
#
# used for which run: Main.run
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
adj.jtl <- adj.nodelta.jtl <- array(0, c(C.adj, Tend, L))
dimnames(adj.jtl)[[1]] <- dimnames(adj.nodelta.jtl)[[1]] <- name.c[c.adj]
dimnames(adj.jtl)[[2]] <- dimnames(adj.nodelta.jtl)[[2]] <- years.t

for (j in 1:C.adj) {
  cat(j, "/", C.adj, "\n")
  for (t in 1:Tend) {
    adj.parname <- paste0("alpha.jt[", j, ",", t, "]")
    adj.nodelta.jtl[j, t, ] <- c(mcmc.array[, , adj.parname])
    delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
    adj.jtl[j, t, ] <- adj.nodelta.jtl[j, t, ] * delta.l
  } # end of t loop
} # end of j loop

selectADJ <- list(adj.jtl = adj.jtl,
                  adj.nodelta.jtl = adj.nodelta.jtl)

## the end ##

