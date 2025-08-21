


load(file = paste0(interim.dir, "birth.ct.rda")) #birth.ct

dim(birth.ct) # 29 93
summary(c(birth.ct))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5724   43673  448080  656817  902727 5293693 

#first run
NODES <- seq(1, 7)
getResbysex <- TRUE
#check country result constructed for each node
for (node in NODES) {
  countries <- seq((node - 1) * floor(C / max(NODES)) + 1,
                   ifelse(node < max(NODES), node * floor(C / max(NODES)), C))
  print(countries)
}#end of node loop


library(doParallel)
registerDoParallel(cores = max(NODES))

foreach (node = NODES) %dopar% {
  
  #first run 
  countries <- seq((node - 1) * floor(C / max(NODES)) + 1, 
                   ifelse(node < max(NODES), node * floor(C / max(NODES)), C))
  
  N.l <- rep(exp(logNmu), L)
  
  for (c in 1:C) {
    
    cat(c, "/", C, "computing:", name.c[c], "\n")
    j <- c
    P.lt <- N.lt <- a.lt <- alpha.lt <- matrix(NA, L, Tend)
    
    for (t in 1:Tend) {
      P.lt[, t] <- exp(c(selectP[["logP.jtl"]][j, t, ]))    
      a.lt[, t] <- rep(exp(logNmu), L)
      ## adjustment factor ##
      alpha.lt[, t] <- selectADJ[["adj.nodelta.jtl"]][j, t, ]
    }# end of t loop
    
    delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
    
    R.noadj.lt <- P.lt * a.lt
    
    R2.lt <- R.lt <- R.noadj.lt + alpha.lt * delta.l
    
    

    ##############################
    ## get sex-specific RESULTS ##
    bhatF.lt <-  #estimated female birth - use R
      bhatM.lt <-  #estimated male birth - use R
      bexpF.lt  <- #expected female birth - use R.noadj
      bmisF.lt <- #missing female birth: expected - estimated
      
      matrix(NA, L, Tend)
    
    for (t in 1:Tend) {
      ## estimated male & female birth - use R ##
      birth.est <- GetSexSpecificBirthfromAllandSRB(
        b.all = birth.ct[name.c[c], t], srb = R.lt[, t])
      bhatF.lt[, t] <- bhatF.l <- birth.est[["b.female"]]
      bhatM.lt[, t] <- bhatM.l <- birth.est[["b.male"  ]]
      
      ## expected female birth - use N.l ##
      bexpF.lt[, t] <- bexpF.l <- bhatM.l / N.l
    }#end of t loop
    
    ## missing female birth: expected - estimated ##
    bmisF.lt <- bexpF.lt - bhatF.lt
    bmisF.lt <- ifelse(bmisF.lt < 0, 0, bmisF.lt)
    ## save country specific results ##
    res.c.both <- list(name = name.c[c],
                       R.lt = R.lt, N.lt = N.lt)
    
    res.c.bySex <- list(Bf.lt = bhatF.lt, #estimated female birth
                        Bm.lt = bhatM.lt, #estimated male birth
                        Bexpf.lt = bexpF.lt, #expected female birth
                        Bmisf.lt = bmisF.lt #missing female birth
    )
    
    res.c.full <- c(res.c.both, res.c.bySex)
    save(res.c.full,
         file = paste0(countryTraj.dir, "trajectory_", runname, "_", name.c[c], "_full.rda"))
    
  }#end of c loop
}#end of node loop  

stopImplicitCluster()

## the end ##

