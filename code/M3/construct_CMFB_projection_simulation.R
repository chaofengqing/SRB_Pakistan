##############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan from 
# 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 26 Aug 2025
#
# construct_CMFB_projection_simulation.R
# 
# This script simulate SRB inflation for each country to check model performance
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

## simulate SRB inflation for each country to check model performance ##
load(paste0(output.dir, "mcmc.array_", runname, ".rda")) #mcmc.array
L <- dim(mcmc.array)[1] * dim(mcmc.array)[2]; L
load(file = paste0(output.dir, "trajectory_", runname, "_R.rda")) #res.Rtrajectory
load(file = paste0(interim.dir, "birth.full.ct.rda")) #birth.full.ct


## read in posterior samples for adj-related parameters from mcmc.array
for (par in hyper.para) {
  par.name <- paste0(par, ".l")
  print(par.name)
  eval(parse(text = paste0(par.name, " <- c(mcmc.array[, , '", par, "'])")))
}#end of par loop

##########################################################################
## add the simulated SRB imbalance to each province by each starting year
library(truncdist)
N.l <- rep(exp(logNmu), L)

t.start <- which(years.t == 2021.5)

sim.cmfb.cls <- array(0, c(C, L, Tend-t.start+1))
sim.cmfb.cqs <- sim.amfb.cqs <- array(0, c(C, Per, Tend-t.start+1))
sim.misfb.cts <- array(0, c(C, Tend-t.start+1, Tend-t.start+1))
dimnames(sim.cmfb.cls)[[1]] <- dimnames(sim.cmfb.cqs)[[1]] <-
  dimnames(sim.amfb.cqs)[[1]] <- dimnames(sim.misfb.cts)[[1]] <- name.c
dimnames(sim.cmfb.cls)[[3]] <- dimnames(sim.cmfb.cqs)[[3]] <-
  dimnames(sim.amfb.cqs)[[3]] <-
  dimnames(sim.misfb.cts)[[2]] <- dimnames(sim.misfb.cts)[[3]] <- years.t[t.start:Tend]

for (c in c(1:C)[-which(is.element(name.c, c("Balochistan",
                                             "Federally Administered Tribal Areas")) |
                        is.na(birth.ct[, 1]))]) {
  
  cat(c, "/", C, "computing:", name.c[c], "\n")
  j <- c

  R.noadj.lt <- res.Rtrajectory$R.clt[c, , ]
  R.noadj.lt <- cbind(R.noadj.lt, matrix(R.noadj.lt[, Tend], nr = L, nc = 10))
  
  ## assume SRB inflation start in different years since 2021
  for (t1 in t.start:Tend) {
    s <- which(c(t.start:Tend) == t1)
    
    sim.adj.tl <- matrix(0, nr = Tend+10, nc = L)
    T0.l <- a.l <- D1.l <- D2.l <- D3.l <- rep(NA, L)
    ## starting year without uncertainty
    T0.l <- rep(t1, L)
    
    ## shape parameters...
    for (l in 1:L) {
      set.seed(l*19 + s*11)
      a.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.a.c.mu, sd = pri.sigma.a.c)
      D1.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D1.c.mu, sd = pri.sigma.D1)
      D2.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D2.c.mu, sd = pri.sigma.D2)
      D3.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D3.c.mu, sd = pri.sigma.D3)
    }#end of l loop
    
    T1.l <- T0.l + D1.l
    T2.l <- T1.l + D2.l
    T3.l <- T2.l + D3.l
    
    ## delta
    set.seed(j*123 + s*11)
    q.delta.l <- rnorm(L, delta.mu.l, sigma.delta.l)
    p.delta.l <- exp(q.delta.l) / (exp(q.delta.l) + 1)
    delta.l <- rbinom(n = L, size = 1, prob = p.delta.l)
    
    for (t in 1:(Tend+10)) {
      for (l in 1:L) {
        if (t <= T0.l[l] | t > T3.l[l]) {
          sim.adj.tl[t, l] <- 0
        } else if (t > T0.l[l] & t <= T1.l[l]) {
          sim.adj.tl[t, l] <- (a.l[l] / D1.l[l]) * (t - T0.l[l])
        } else if (t > T1.l[l] & t <= T2.l[l]) {
          sim.adj.tl[t, l] <- a.l[l]
        } else {
          sim.adj.tl[t, l] <- a.l[l] - (a.l[l] / D3.l[l]) * (t - T2.l[l])
        }#end of ifelse
      }#end of l loop
      
      sim.adj.tl[t, ] <- sim.adj.tl[t, ] * delta.l
    } # end of t loop
    
    R.lt <- R.noadj.lt + t(sim.adj.tl)
    
    bhatF.lt <-  # estimated female birth - use R
      bhatM.lt <-  # estimated male birth - use R
      bexpF.lt  <- # expected female birth - use R.noadj
      bmisF.lt <- # missing female birth: expected - estimated
      matrix(NA, L, Tend+10)
    
    for (t in 1:(Tend+10)) {
      ## estimated male & female birth - use R ##
      birth.est <- GetSexSpecificBirthfromAllandSRB(
        b.all = birth.full.ct[name.c[c], t], srb = R.lt[, t])
      bhatF.lt[, t] <- bhatF.l <- birth.est[["b.female"]]
      bhatM.lt[, t] <- bhatM.l <- birth.est[["b.male"  ]]
      
      ## expected female birth - use N.l ##
      bexpF.lt[, t] <- bexpF.l <- bhatM.l / N.l
    } # end of t loop
    
    ## missing female birth: expected - estimated ##
    bmisF.lt <- bexpF.lt - bhatF.lt
    bmisF.lt <- ifelse(bmisF.lt < 0, 0, bmisF.lt)
    
    sim.misfb.cts[c, , s] <- apply(bmisF.lt[, t.start:Tend], 2, median)
    
    sim.amfb.l <- rep(0, L)
    for (l in 1:L) {
      sim.cmfb.cls[c, l, s] <- sum(bmisF.lt[l, t.start:Tend])
      sim.amfb.l[l] <- sum(bmisF.lt[l, t.start:(Tend+10)]) / (Tend+10 - t1 + 1)
    } # end of l loop
    
    sim.cmfb.cqs[c, , s] <- SamplesToUI(sim.cmfb.cls[c, , s])
    sim.amfb.cqs[c, , s] <- SamplesToUI(sim.amfb.l)
    
  } # end of s loop
} # end of c loop


# for (c in 1:C) {
#   for (t in t.start:Tend) {
#     s <- which(c(t.start:Tend) == t)
#     sim.amfb.cqs[c, , s] <- SamplesToUI(sim.cmfb.cls[c, , s] / (Tend - t + 1))
#   }#end of t loop  
# }#end of c loop

save(sim.misfb.cts, file = paste0(output.dir,"simulatedMISFB_bystartyear_",runname,".rda"))
save(sim.amfb.cqs, file = paste0(output.dir,"simulatedAMFB_bystartyear_",runname,".rda"))
save(sim.cmfb.cqs, file = paste0(output.dir,"simulatedCMFB_bystartyear_",runname,".rda"))
save(sim.cmfb.cls, file = paste0(output.dir,"simulatedCMFB_bystartyear_cls_",runname,".rda"))

## the end ##

