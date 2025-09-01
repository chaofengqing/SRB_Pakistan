##############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan from 
# 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 26 Aug 2025
# construct_CountryList_PastInflation.R
# 
# This script identifies countries with past SRB inflation 
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

## identify countries with past SRB inflation ##
country.list.past.inflation <- NULL
inflation.prob.j <- rep(NA, C.adj)
names(inflation.prob.j) <- name.c[c.adj]

cutoff <- 0.95

count <- 0
for (j in 1:C.adj) {
  c <- c.adj[j]
  delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
  inflation.prob.j[j] <- mean(delta.l == 1)
  
  if (inflation.prob.j[j] >= cutoff) {
    count <- count + 1
    cat(count, name.c[c], inflation.prob.j[j], "\n")
    country.list.past.inflation <- c(country.list.past.inflation, name.c[c])
  } # end of if(mean(delta.l == 1) == 1)
  
} # end of j loop

cat("Number of areas with past inflation:", length(country.list.past.inflation), "\n")
print(sort(round(inflation.prob.j[!is.element(name.c[c.adj],
                                              country.list.past.inflation)]*100, 1), decreasing = TRUE))
countries <- names(sort(round(inflation.prob.j[!is.element(name.c[c.adj],
                                              country.list.past.inflation)]*100, 1), decreasing = TRUE))
percentages <- sort(round(inflation.prob.j[!is.element(name.c[c.adj],
                                                       country.list.past.inflation)]*100, 1), decreasing = TRUE)

msg <- paste0(countries, " (", percentages, "\\%)", collapse = "; ")
print(msg)
## the end ##

