

# This script saves country-specific results: sex ratio (estimated and expected),
# and P multiplier.
# 
###############################################################################

array.jqt                <- array(NA, c(C.adj, Per, Tend))
dimnames(array.jqt)[[1]] <- name.c[c.adj]
dimnames(array.jqt)[[2]] <- percentiles
dimnames(array.jqt)[[3]] <- floor(years.t)

#S1: 1) countries with delta=1, same as S1; 2) countries with delta*alpha=1 and 0 in data period, only keep 0 traj
R1.jqt <-
  #S2: posterior samples of SRB directly from the model
  R2.jqt <-
  #S3: set delta=1 for all countries at risk of SRB inflation
  R3.jqt <-
  P.jqt <- N.jqt <-
  a.jqt <-
  array.jqt

# delta.jl <- matrix(NA, nr = C.adj, nc = L)
S1.l.select.prob.j <- S3.l.select.prob.j <- inflation.prob.j <- rep(NA, C.adj)
names(S1.l.select.prob.j) <-
  names(S3.l.select.prob.j) <- names(inflation.prob.j) <- name.c[c.adj]

cutoff <- k.proj.cutoff <- 0.95
for (j in 1:C.adj) {
  cat(j, "/", C.adj, "\n")
  c <- c.adj[j]
  P.lt <- N.lt <- a.lt <- alpha.lt <- matrix(NA, L, Tend)
  
  for (t in 1:Tend) {
    P.lt[, t] <- exp(c(selectP[["logP.jtl"]][j, t, ]))    
    a.lt[, t] <- rep(exp(logNmu), L)
    ## adjustment factor ##
    alpha.lt[, t] <- selectADJ[["adj.nodelta.jtl"]][j, t, ]
  }# end of t loop
  
  delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
  
  R.noadj.lt <- P.lt * a.lt

  #S1: posterior samples of SRB directly from the model
  R2.lt <- R.noadj.lt + alpha.lt * delta.l
  
  #S2: 1) countries with delta=1, same as S1;
  if (mean(delta.l == 1) >= cutoff) {
    R1.lt <- R2.lt
    S1.l.select.prob.j[j] <- 0
  }#end of if(mean(delta.l == 1) == 1)
  
  #S2: 2) countries with delta*alpha=1 and 0 in data period, only keep 0 traj
  t.datastart <- min(t.i[j.i == j]) #start of data period
  t.dataend <- max(t.i[j.i == j]) #end of data period
  alpha.check <- mean(c(alpha.lt[, t.datastart:t.dataend]) >= 0)
  zero.delta <- delta.l == 0
  zero.alpha.dataperiod <- apply(alpha.lt[, t.datastart:t.dataend] == 0, 1, prod) == 1
  
  if (mean(delta.l == 1) < cutoff & mean(delta.l == 1) > 0 & alpha.check > 0) {
    l.zero.select <- which(zero.delta | zero.alpha.dataperiod)
    
    delta.new.l <- delta.l
    delta.new.l[zero.alpha.dataperiod & delta.l == 1] <- 0
    
    R1.lt <- R.noadj.lt[l.zero.select, ] +
      alpha.lt[l.zero.select, ] * delta.new.l[l.zero.select]
    S1.l.select.prob.j[j] <- length(l.zero.select)/L
    print(paste0(name.c[c], " S1: ", round(S1.l.select.prob.j[j]*100, 2), "%"))
  }#end of if
  
  #S3: set delta=1 for all countries at risk of SRB inflation
  if (mean(delta.l == 1) >= cutoff) {
    R3.lt <- R.noadj.lt + alpha.lt
    S3.l.select.prob.j[j] <- 0
  }#end of if(mean(delta.l == 1) == 1)
  
  if (mean(delta.l == 1) < cutoff & mean(delta.l == 1) > 0 & alpha.check > 0) {
    l.one.select <- which(delta.l == 1)
    R3.lt <- R.noadj.lt[l.one.select, ] +
      alpha.lt[l.one.select, ] * delta.l[l.one.select]
    S3.l.select.prob.j[j] <- length(l.one.select)/L
    print(paste0(name.c[c], " S3: ", round(S3.l.select.prob.j[j]*100, 2), "%"))
    
  }#end of if
  
  inflation.prob.j[j] <- mean(delta.l == 1 & !zero.alpha.dataperiod)
  print(paste0(name.c[c], " inflation prob: ", round(inflation.prob.j[j]*100, 2), "%"))
  
  R1.jqt[j, , ] <- SamplesToUI(R1.lt)
  R2.jqt[j, , ] <- SamplesToUI(R2.lt)
  R3.jqt[j, , ] <- SamplesToUI(R3.lt)
  P.jqt[j, , ] <- SamplesToUI(P.lt)
  a.jqt[j, , ] <- SamplesToUI(a.lt)
  
}#end of j loop

res.proj <- list(R1.jqt = R1.jqt, R2.jqt = R2.jqt, R3.jqt = R3.jqt,
                 P.jqt = P.jqt, a.jqt = a.jqt,
                 S1.l.select.prob.j = S1.l.select.prob.j,
                 S3.l.select.prob.j = S3.l.select.prob.j,
                 inflation.prob.j = inflation.prob.j)



## the end ##
