

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_setupMCMC.R
# 
# This script setup parameters, input data, initial values and JAGS model file
# for JAGS model.
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_*.R
#
# this script calls other scripts: jags_writeJAGSmodel.R
#
# functions called: null
# 
# input data: most constants and vectors are defined in source_BasicSetup
#
# output data:
# 1. ChainIDs
# 2. mort.data
# 3. mort.inits
# 4. mort.parameters
# note: JAGS model file is created in jags_writeJAGSmodel.R
#
# JAGS model setup summary in five parts:
# part 1: assign sequence of ChainIDs based on runID
#
# part 2: specify parameters to be saved for JAGS output, i.e. MCMC array
#
# part 3: specify input data for JAGS model
#
# part 4: write out JAGS model in txt format
# 
###############################################################################


#######################
# MCMC array structure: ChainIDs is specified in jags_setupMCMC.R (need runID).
mcmc.chains    <- 10
ChainIDs       <- seq(1, mcmc.chains)
mcmc.burnin    <- 5000
N.STEPS        <- 50
n.iter.perstep <- 50
mcmc.thin      <- 1


###################
# write JAGS model (jags_writeJAGSmodel.R)
NoTermPerLine <- 2 #shorten the length of long summition to seperate lines

# import posterior info from reduced model run
c.full <- 145
j.adj <- 20

red.info <- read.csv(paste0(input.dir, glb.normal.runname, "_postinfo.csv"), row.names = 1)
Nmu       <- red.info[paste0("a.c[", c.full, "]"), "X50.percentile"]
logNmu    <- log(Nmu) #Pakistan national baseline
rho       <- red.info["rho", "X50.percentile"]
sigma.eps <- red.info["sigma.eps", "X50.percentile"]

## import global adjustment model results
adj.info <- read.csv(paste0(input.dir, glb.adj.runname, "_postinfo_exclude-alpha_jt_post.csv"), row.names = 1)

for (par in c("a", "D1", "D2", "D3")) {
  msg <- paste0("pri.", par, ".c.mu <- adj.info['", par, ".j[", j.adj, "]', 'X50.percentile']")
  print(msg)
  eval(parse(text = msg))
} # end of par loop

cv.parameters <- 0.1
pri.sigma.a.c <- cv.parameters * pri.a.c.mu
pri.sigma.D1 <- cv.parameters * pri.D1.c.mu
pri.sigma.D2 <- cv.parameters * pri.D2.c.mu
pri.sigma.D3 <- cv.parameters * pri.D3.c.mu

## adjustment factor
pri.T0.c.lower <- which(years.t == 1970.5)
pri.T0.c.upper <- which(years.t == 2050.5)

adj.start.range <- 2050.5 - adj.year

pri.sigma.TFRtarget.lower <- 0
pri.sigma.TFRtarget.upper <- 10

pri.sigma.s.lower <- 0
pri.sigma.s.upper <- 0.5

pri.sigma.a.c.lower <- 0
pri.sigma.a.c.upper <- 2

pri.sigma.D1.lower <- pri.sigma.D3.lower <- 0
pri.sigma.D2.lower <- 1
pri.sigma.D1.upper <- 10 #20
pri.sigma.D2.upper <- 10 #20
pri.sigma.D3.upper <- 10 #20

pri.sigma.delta.lower <- 0
pri.sigma.delta.upper <- 2

pri.delta.mu.probscale.lower <- 0
pri.delta.mu.probscale.upper <- 1

##########
## part 1: assign sequence of ChainIDs based on runID

## get the most recent time index with data for each country
max.t.j <- min.t.j <- rep(NA, C.adj)
for (j in 1:C.adj) {
  c <- c.adj[j]
  # the earliest time index with data;
  # or time index for the year 1950, whichever is smaller
  min.t.j[j] <- min(gett.cz[c, 1], which(years.t == 1970.5))
  # the most rescent time index with data
  max.t.j[j] <- gett.cz[c, nt.c[c]]
}#end of c loop


##########
## part 2: specify parameters to be saved for JAGS output, i.e. MCMC array
para.alpha <- NULL
for (j in 1:C.adj) {
  para.alpha <- c(para.alpha,
                  paste0("alpha.jt[", j, ",", 1:Tend, "]")
  )
} # end of j loop

adj.para <- c(paste0("T0.j[", 1:C.adj, "]"),
              paste0("T3.j[", 1:C.adj, "]"),
              para.alpha,
              paste0("a.j[", 1:C.adj, "]"),
              paste0("D1.j[", 1:C.adj, "]"),
              paste0("D2.j[", 1:C.adj, "]"),
              paste0("D3.j[", 1:C.adj, "]"))
delta.para <- c(paste0("delta.j[", 1:C.adj, "]"),
                paste0("p.delta.j[", 1:C.adj, "]"))
hyper.para <- c("sigma.s",#"sigma.TFRtarget",
                "delta.mu", "delta.mu.probscale", "sigma.delta")
paraP <- NULL
for (j in 1:C.adj) {
  ## from 1950 or the earliest year with data (whichever is earlier) to the most recent
  ## year with data
  paraP <- c(paraP, paste0("logP.jt[", j, ",", 1:max.t.j[j], "]"))
}#end of c loop

mort.parameters <- c(adj.para,
                     delta.para,
                     paraP, 
                     hyper.para)



##########
## part 3: specify input data for JAGS model
# adj.timeindex <- which(years.t == adj.year)

mort.data <- c(
  list(
    # prior info
    # adjustment factor
    pri.T0.c.lower = pri.T0.c.lower,
    pri.T0.c.upper = pri.T0.c.upper,
    
    pri.a.c.mu = pri.a.c.mu,
    pri.D1.c.mu = pri.D1.c.mu,
    pri.D2.c.mu = pri.D2.c.mu,
    pri.D3.c.mu = pri.D3.c.mu,
    
    pri.sigma.a.c = pri.sigma.a.c,
    pri.sigma.D1 = pri.sigma.D1,
    pri.sigma.D2 = pri.sigma.D2,
    pri.sigma.D3 = pri.sigma.D3,
    
    pri.sigma.s.lower = pri.sigma.s.lower,
    pri.sigma.s.upper= pri.sigma.s.upper,
    pri.sigma.delta.lower = pri.sigma.delta.lower,
    pri.sigma.delta.upper = pri.sigma.delta.upper,
    
    pri.delta.mu.probscale.lower = pri.delta.mu.probscale.lower,
    pri.delta.mu.probscale.upper = pri.delta.mu.probscale.upper
    
  ),
  
  list(t.i = t.i,
       j.i = j.i,
       c.i = c.i,
       max.t.j = max.t.j,
       I = I, C = C, #R = R,S = S,
       Tend = Tend,
       logNmu = logNmu,
       rho = rho,
       sigma.eps = sigma.eps,
       source.i = source.i,
       logr.i = logr.i,
       logSE.i = logSE.i,
       ## adjustment data
       A = A, C.adj = C.adj,
       c.adj = c.adj,
       indAdj.i = indAdj.i,
       index.TFRtarget.j = index.TFRtarget.j,
       t.a = t.a, j.a = j.a,
       gett.jk = gett.jk, nt.j = nt.j
  )
)


##########
## part 4: write out JAGS model in txt format
JAGSmodel.name <- paste0(runname, ".txt")
if (First.run) source(paste0("code/", runname, "/jags_writeJAGSmodel.R"))

## The End! ##
