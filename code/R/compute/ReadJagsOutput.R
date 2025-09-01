###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# ReadJagsOutput.R
# 
# This script contains function related to reading and combining JAGS simulation outputs 
# saved in multiple steps and chains.The output is returned as a MCMC array.
#
#
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# ReadJagsOutput(..)
# 
###############################################################################

ReadJagsOutput <- function(
  n.steps, n.chains = 3, #n.thin=1,
  maxiter = 7500,  
  # note 1: may be slightly more because of rounding
  # note 2: if you get over maxiter, the thinning can be off at end and start of step
  # note 3: still better than thinning after reading in all the steps because array becomes too large
  ChainNums = NULL,  #note: ChainNums overwrites n.chains if provided
  start.step = 1, #by default start reading in the first step
  runname,
  output.dir = jagsStep.dir #"output/" # directory to save jags objects
){
  ## read in JAGS output ##
  
  if (is.null(ChainNums)) {
    ChainNums <- seq(1, n.chains)
  } else {
    n.chains <- length(ChainNums)
  }
  chain <- ifelse(length(ChainNums) == 1, 1, ChainNums[1])
  
  jags.filename <- paste0("jags_run-", runname, "_chain", chain, "_",
                          "step", start.step, ".Rdata")
  load(paste0(jagsStep.dir, jags.filename))
  n.iter.perstep <- dim(model.update$BUGSoutput$sims.array)[1]
  nsavepersteptemp <- min(n.iter.perstep,
                          ceiling(maxiter * 1 / (n.steps - start.step + 1) * 1 / n.chains))
  N.THIN         <- floor(n.iter.perstep / nsavepersteptemp)
  iter.pick      <- seq(1, n.iter.perstep, N.THIN)
  n.iter.perstep <- length(iter.pick)
  n.sim          <- (n.steps - start.step + 1) * n.iter.perstep
  n.par          <- dim(model.update$BUGSoutput$sims.array)[3]
  print(paste("Additional thinning of:", N.THIN ))
  print(paste("A total of", n.sim * n.chains, "samples will be saved."))
  
  mcmc.array <- array(NA, c(n.sim, n.chains, n.par))
  dimnames(mcmc.array) <- list(NULL, NULL, names(model.update$BUGSoutput$sims.array[1, 1, ]))
  for (chain in 1:n.chains) {
    chain_saved <- ifelse(length(ChainNums) == 1, 1, ChainNums[chain])
    cat(paste0("Reading in chain number ", chain_saved, " (step ", start.step, ")"), "\n")
    jags.filename <- paste0("jags_run-", runname, "_chain", chain_saved, "_",
                            "step", start.step, ".Rdata")
    load(paste0(jagsStep.dir, jags.filename))
    mcmc.array[1:n.iter.perstep,chain, ] <- model.update$BUGSoutput$sims.array[iter.pick, 1, ]
    if (n.steps > 1) {
      for (step in (start.step + 1):n.steps) {
        cat(paste0("Reading in chain number ", chain_saved, " (step ", step, ")"), "\n")
        jags.filename <- paste0("jags_run-", runname, "_chain", chain_saved, "_",
                                "step", step, ".Rdata")
        load(paste0(jagsStep.dir, jags.filename))
        mcmc.array[((step - start.step) * n.iter.perstep + 1):((step - start.step + 1) * n.iter.perstep),
                   chain, ] <- model.update$BUGSoutput$sims.array[iter.pick, 1, ]
      } # end of step loop
    } # end of if (n.steps > 1)
  } # end of chain loop
  
 }
  
  return(mcmc.array)
  
} # end of ReadJagsOutput function
