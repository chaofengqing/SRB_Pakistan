
#################################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 28 Aug 2025
# 
# jags_getMCMC.R
# 
# This script calls JAGS and run the JAGS model on several serves parallely.
#
# used for which run: main.run
#
# this script is called by any other scripts: main.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: most information are from jags_setupMCMC.R.
#
# output data: data/output/runname/temp.JAGSobjects/* - stepwise JAGS output
# note: these output files will be combined in main_output.R to get mcmc.array
#
###############################################################################
## RUN FOR JAGS ##

## for linux server only:
library(MCMCpack)
library(doParallel)
registerDoParallel(cores = mcmc.chains)

## start MCMC

foreach(chain = ChainIDs) %dopar% {
  set.seed(2013 + chain * 1000)
  rnorm(chain)
  library(R2jags) # for JAGS (load this when using Putty to submit on wind server)
  
  mod <- jags(data = mort.data, #inits = mort.inits,
              parameters.to.save = mort.parameters,           
              model.file = paste0(output.dir, JAGSmodel.name),
              jags.seed = chain,
              n.chains = 1, n.iter = n.iter.perstep + mcmc.burnin,
              n.burnin = mcmc.burnin, n.thin = mcmc.thin, DIC = TRUE)
  
  i = 1 # index for which update
  model.update <- mod
  jags.filename <- paste0("jags_run-", runname, "_chain", chain, "_",
                          "step", i, ".Rdata")
  save(model.update, file = paste0(jagsStep.dir, jags.filename))
  cat(paste("MCMC results step", 1, "for chain", chain,
            "written to folder", jagsStep.dir, "at", date(), "\n"))
  
  #--- update MCMC ----------
  if (N.STEPS > 1){
    for (i in 2:(N.STEPS)) {      
      
      model.update <- update(model.update,
                             parameters.to.save = mort.parameters, 
                             n.iter = n.iter.perstep,
                             n.thin = mcmc.thin)
      jags.filename <- paste0("jags_run-", runname, "_chain", chain, "_",
                              "step", i, ".Rdata")
      save(model.update, file = paste0(jagsStep.dir, jags.filename))
      cat(paste("MCMC results step", i, "for chain", chain, 
                "written to folder", jagsStep.dir, "at", date(), "\n"))
      
    } # end of i loop (steps)
  } # end of if(step > 1) 
} # end of chain loop

stopImplicitCluster()
## the end ##


