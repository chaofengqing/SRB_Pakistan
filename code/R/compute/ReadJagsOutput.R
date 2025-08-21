

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
  #   ntotaltemp <- n.iter.perstep*(n.steps-start.step+1)*n.chains
  #   print(paste("A total of ", n.iter.perstep*(n.steps-start.step+1)*n.chains, "samples were obtained."))
  #   ntotal <- min(maxiter, ntotaltemp)
  #   print(paste("A total of ", ntotal, "samples are saved."))
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
      }#end of step loop
    }#end of if (n.steps > 1)
  }#end of chain loop
  
  #   n.sample.max = 10000000
  #   if (n.sim > n.sample.max){
  #     mcmc.array <- mcmc.array[seq(1, n.sample.max, length.out = n.sample.max), , ]  
  #   }
  
  return(mcmc.array)
  
}#end of ReadJagsOutput function
