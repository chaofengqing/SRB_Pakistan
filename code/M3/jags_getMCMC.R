


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
      
    }#end of i loop (steps)
  }#end of if(step > 1) 
}#end of chain loop

stopImplicitCluster()
## the end ##


