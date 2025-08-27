###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 27 Aug 2025
#
# construct_onecountry_ADJ_simulation.R
# 
# This script is to simulate SRB inflation for each country to check model performance.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(6)
# 
# 
# input data: mcmc.array_M3.rda
#             selectP_M3.rda
#
# output data: data/output/cis_M1_simulation.rda
#               data/output/PredictCI_M1.rda
#               data/output/Predict80CI_M1.rda
###############################################################################

## w ##
load(paste0(output.dir, "mcmc.array_", runname, ".rda")) #mcmc.array
L <- dim(mcmc.array)[1] * dim(mcmc.array)[2]; L
load(file = paste0(output.dir, "selectP_", runname, ".rda")) #selectP


## read in posterior samples for adj-related parameters from mcmc.array
for (par in hyper.para) {
  par.name <- paste0(par, ".l")
  print(par.name)
  eval(parse(text = paste0(par.name, " <- c(mcmc.array[, , '", par, "'])")))
} # end of par loop

######################################
## simulate adjustment factor first...
library(truncdist)

sim.adj.jtl    <- array(0, c(C.adj, Tend, L))
dimnames(sim.adj.jtl)[[1]] <- name.c[c.adj]
dimnames(sim.adj.jtl)[[2]] <- years.t

sim.T0.j <- sim.T3.j <- rep(NA, C.adj)
names(sim.T0.j) <- names(sim.T3.j) <- name.c[c.adj]

for (j in 1:C.adj) {
  cat(j, "/", C.adj, "\n")
  T0.l <- a.l <- D1.l <- D2.l <- D3.l <- rep(NA, L)
  ## starting year...
  set.seed(123+j*17)
  T0.l <- runif(n = L, pri.T0.c.lower, pri.T0.c.upper)
  
  ## shape parameters...
  for (l in 1:L) {
    set.seed(l*19)
    a.l[l]  <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.a.c.mu, sd = pri.sigma.a.c)
    D1.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D1.c.mu, sd = pri.sigma.D1)
    D2.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D2.c.mu, sd = pri.sigma.D2)
    D3.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D3.c.mu, sd = pri.sigma.D3)
  }#end of l loop
  
  T1.l <- T0.l + D1.l
  T2.l <- T1.l + D2.l
  T3.l <- T2.l + D3.l
  
  ## delta
  set.seed(j*123)
  q.delta.l <- rnorm(L, delta.mu.l, sigma.delta.l)
  p.delta.l <- exp(q.delta.l) / (exp(q.delta.l) + 1)
  delta.l   <- rbinom(n = L, size = 1, prob = p.delta.l)
  
  sim.T0.j[j] <- median(T0.l, na.rm = TRUE)
  sim.T3.j[j] <- median(T3.l, na.rm = TRUE)
  
  for (t in 1:Tend) {
    for (l in 1:L) {
      if (t <= T0.l[l] | t > T3.l[l]) {
        sim.adj.jtl[j, t, l] <- 0
      } else if (t > T0.l[l] & t <= T1.l[l]) {
        sim.adj.jtl[j, t, l] <- (a.l[l] / D1.l[l]) * (t - T0.l[l])
      } else if (t > T1.l[l] & t <= T2.l[l]) {
        sim.adj.jtl[j, t, l] <- a.l[l]
      } else {
        sim.adj.jtl[j, t, l] <- a.l[l] - (a.l[l] / D3.l[l]) * (t - T2.l[l])
      }#end of ifelse
    }#end of l loop
    
    sim.adj.jtl[j, t, ] <- sim.adj.jtl[j, t, ] * delta.l
  } # end of t loop
} # end of j loop

#################################
## simulate logP.ct after 1970...
rho.l   <- rep(rho, L)
sigma.l <- rep(sigma.eps, L)
sim.logP.jtl <- array(NA, c(C.adj, Tend, L))

for (j in 1:C.adj) {
  set.seed(j*123)
  cat(j, "/", C.adj, "\n")
  
  t <- 1
  sim.logP.jtl[j, t, ] <- c(selectP[["logP.jtl"]][j, t, ])
  
  for (t in 2:Tend) {
    mean.l <- rho.l * sim.logP.jtl[j, t-1, ]
    sim.logP.jtl[j, t, ] <- rnorm(n = L, mean = mean.l, sd = sigma.l)
  } # end of t loop
} # end of c loop

###############################################
## get the simulated SRB for these countries...
array.jqt                <- array(NA, c(C.adj, Per, Tend))
dimnames(array.jqt)[[1]] <- name.c
dimnames(array.jqt)[[2]] <- percentiles
dimnames(array.jqt)[[3]] <- floor(years.t)

sim.R.jqt <- sim.P.jqt <-
  sim80.R.jqt <- sim80.P.jqt <- array.jqt

r.e <- t.e <- c.e <- year.e <- iso.e <- source.e <- logSE.e <- rPredict.le <- NULL

for (j in 1:C.adj) {
  cat(j, "/", C.adj, "\n")
  c <- c.adj[j]
  sim.R.lt <- sim.P.lt <- sim.alpha.lt <- matrix(NA, L, Tend)
  
  for (t in 1:Tend) {
    sim.P.lt[, t] <- exp(sim.logP.jtl[j, t, ])    
    ## adjustment factor ##
    sim.alpha.lt[, t] <- sim.adj.jtl[j, t, ]
  }# end of t loop
  
  sim.R.noadj.lt <- sim.P.lt * exp(logNmu)
  
  sim.R.lt <- sim.R.noadj.lt + sim.alpha.lt
  
  ## 95% UI ##
  sim.R.jqt[j, , ] <- SamplesToUI(sim.R.lt)
  sim.P.jqt[j, , ] <- SamplesToUI(sim.P.lt)

  ## 80% UI ##
  sim80.R.jqt[j, , ] <- SamplesToUI(sim.R.lt, percentile.q = c(0.1, 0.5, 0.9))
  sim80.P.jqt[j, , ] <- SamplesToUI(sim.P.lt, percentile.q = c(0.1, 0.5, 0.9))
  
  
  ## add in sampling and non-sampling error ##
  country  <- name.c[c]
  select.i <- which(name.i == country)
  
  r.e <- c(r.e, r.i[select.i])
  t.e <- c(t.e, t.i[select.i])
  c.e <- c(c.e, c.i[select.i])
  year.e <- c(year.e, year.i[select.i])
  logSE.e <- c(logSE.e, logSE.i[select.i])
  
  sigma.d <- logSE.i[select.i]
  
  rPredict.ld <- matrix(NA, nr = L, nc = length(select.i))
  for (d in 1:length(select.i)) {
    rPredict.ld[, d] <- exp(rnorm(n = L,
                                  mean = log(sim.R.lt[, t.i[select.i[d]]]),
                                  sd = sigma.d[d] + sigma.s.l))
  } # end of d loop
  
  rPredict.le <- cbind(rPredict.le, rPredict.ld)
} # end of j loop

## 95% PI ##
Predict.qe <- SamplesToUI(rPredict.le)
save(Predict.qe, file = paste0(output.dir,"PredictCI_",runname,".rda"))
## 80% PI ##
Predict80.qe <- SamplesToUI(rPredict.le, percentile.q = c(0.1, 0.5, 0.9))
save(Predict80.qe, file = paste0(output.dir,"Predict80CI_",runname,".rda"))

res.sim <- list(sim.R.jqt = sim.R.jqt, sim.P.jqt = sim.P.jqt,
                sim80.R.jqt = sim80.R.jqt, sim80.P.jqt = sim80.P.jqt,
                sim.T0.j = sim.T0.j, sim.T3.j = sim.T3.j)
save(res.sim, file = paste0(output.dir, "cis_", runname, "_simulation.rda"))

## the end ##

