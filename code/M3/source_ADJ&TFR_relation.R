
###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
#
# source_ADJ&TFR_relation.R
# 
# This script analyze the relation between starting year of SRB inflation period
# and TFR value. Furthermore, to find the mean of the informative prior for
# start year parameter.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main.R and main_output.R;
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

## This script is to analyze the relation between
## starting year of SRB inflation period and TFR value
## Furthermore, to find the mean of the informative prior for
## start year parameter.

TFR.target <- 2.9 #pnas paper for Pakistan
yr.TFR <- 2030.5 #pnas paper for Pakistan
print(paste("TFR median is", TFR.target))

year.TFRtarget.j <- index.TFRtarget.j <- rep(NA, C.adj)
names(year.TFRtarget.j) <- names(index.TFRtarget.j) <- name.c[c.adj]

for (j in 1:C.adj) {
  c <- c.adj[j]
  year.TFRtarget.j[j] <- yr.TFR#years.t[which.min(abs(tfr - TFR.target))]
  index.TFRtarget.j[j] <- which(years.t == year.TFRtarget.j[j])
} # end of c loop

year.TFRtarget.j

## the end ##

