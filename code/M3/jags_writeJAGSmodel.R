###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 28 Aug 2025
# 
# jags_writeJAGSmodel.R
# 
# This script writes out JAGS model file.
#
# used for which run: main.run
#
# this script is called by any other scripts: jags_setupMCMC.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: most information are from source_BasicSetup and jags_setupMCMC.R.
#
# output data: data/output/M3/runname.txt
#
###############################################################################

# JAGS may not work well on sum with many elements. So we split into several
# sums with fewer element within each sum.

# note: spaces in cat matter!
cat("

model {
    #############################
    ## data model on log-scale ##
    for (i in 1:I) {
    
    logr.i[i] ~ dnorm(logrhat.i[i], tau.i[i])
    
    logrhat.i[i] <- log(r.adj.i[i])
    r.adj.i[i]   <- r.noadj.i[i] + adjustment.a[indAdj.i[i]] * delta.j[j.i[i]]
    r.noadj.i[i] <- exp(logNmu + logP.jt[j.i[i], t.i[i]])

    tau.i[i] <- 1 / (pow(logSE.i[i], 2) + sigma.s^2)
    } # end of i loop
    
    ##################################
    ## alpha.ct - adjustment factor ##
    adjustment.a[1] <- 0 # when indAdj.i = 1, no adjustment
    
    for (a in 2:A) {
    adjustment.a[a] <- alpha.jt[j.a[a], t.a[a]]
    } # end of a loop
    
    for (j in 1:C.adj) {
    for (t in 1:Tend) {
    
    alpha.jt[j, t] <- (beta0.jt[j, t] + beta1.jt[j, t] + beta2.jt[j, t] +
    beta3.jt[j, t])
    
    beta0.jt[j, t] <- (0 * ((t <= T0.j[j]) || (t > T3.j[j])))
    
    beta1.jt[j, t] <- ((a.j[j] / D1.j[j]) * (t - T0.j[j]) *
    (t > T0.j[j] && t <= T1.j[j]))
    
    beta2.jt[j, t] <- (a.j[j] * (t > T1.j[j] && t <= T2.j[j]))
    
    beta3.jt[j, t] <- ((a.j[j] - (a.j[j] / D3.j[j]) *
    (t - T2.j[j])) * (t > T2.j[j] && t <= T3.j[j]))
    
    } # end of k loop
    
    T1.j[j] <- (T0.j[j] + D1.j[j])
    T2.j[j] <- (T0.j[j] + D1.j[j] + D2.j[j])
    T3.j[j] <- (T0.j[j] + D1.j[j] + D2.j[j] + D3.j[j])
    
    ## priors
    a.j[j]  ~ dnorm(pri.a.c.mu, tau.a.c)T(0, )
    D1.j[j] ~ dnorm(pri.D1.c.mu, tau.D1)T(0, )
    D2.j[j] ~ dnorm(pri.D2.c.mu, tau.D2)T(0, )
    D3.j[j] ~ dnorm(pri.D3.c.mu, tau.D3)T(0, )
    
    # T0.j[j] ~ dt(index.TFRtarget.j[j], tau.TFRtarget, 3)
    T0.j[j] ~ dunif(pri.T0.c.lower, pri.T0.c.upper)
    } # end of j loop


    ###############
    ## log(P.ct) ##
    for (j in 1:C.adj) {
    
    logP.jt[j, 1] ~ dnorm(0, tau.eps.stat)
    for (t in 2:max.t.j[j]) {
    logP.jt[j, t] ~ dnorm(rho * logP.jt[j, t-1], tau.eps)
    } # end of t loop
    } # end of j loop
    
    
    ######################################
    ## delta - indicator for adjustment ##
    for (j in 1:C.adj) {
    delta.j[j] ~ dbern(p.delta.j[j])
    p.delta.j[j] <- exp(q.delta.j[j]) / (exp(q.delta.j[j]) + 1)
    q.delta.j[j] ~ dnorm(delta.mu, tau.delta)
    } # end of j loop

    delta.mu <- exp(delta.mu.probscale) / (exp(delta.mu.probscale) + 1)
    

    ############
    ## priors ##
    tau.eps.stat  <- tau.eps * (1 - pow(rho, 2))
    tau.eps       <- pow(sigma.eps, -2)
    tau.a.c <- pow(sigma.a.c, -2)
    tau.D1  <- pow(sigma.D1, -2)
    tau.D2  <- pow(sigma.D2, -2)
    tau.D3  <- pow(sigma.D3, -2)
    tau.delta <- pow(sigma.delta, -2)
    
    sigma.a.c <- pri.sigma.a.c
    sigma.D1 <- pri.sigma.D1
    sigma.D2 <- pri.sigma.D2
    sigma.D3 <- pri.sigma.D3

    delta.mu.probscale ~ dunif(pri.delta.mu.probscale.lower, pri.delta.mu.probscale.upper)
    sigma.delta ~ dunif(pri.sigma.delta.lower, pri.sigma.delta.upper)
    sigma.s ~ dunif(pri.sigma.s.lower, pri.sigma.s.upper)
    } # end of model
    
    ",
    sep    = "",
    append = FALSE,
    file   = file.path(output.dir, JAGSmodel.name),
    fill   = TRUE
)


################
## JAGS NOTES ##
## 1. no empty entries are allowed on the LHS of equation or distribution:
##    not Y[,j], but Y[i,j]; RHS also better to specify.
## 2. cannot use the same expression in loop, i.e. not i <- i+1, must specify
##    to indicate different entries.
## 3. when writing for (i in A:B), B cannot be expression of the number,
##    e.g. given C is an integer, cannot specify B<-C+1 after the loop. need to
##    write as for (i in A:(C+1));
## 4. given T, cannot write sth like for(r in 1:T.c[j,c]) where
##    T.c[j,c]<-T[H[j,c]], should write as for(r in 1:T[cumsum.C[j]+c])
## 5. cannot specify a distribution on V.CI[j,1:C[j]], and then assign values
##    to V.CI[j, 1:C[j]]. Instead, you can include the observed V.CI[j,c]
##    directly as data input.
## 6. inverse() does not work for a single entry, but only for matrix in JAGS.
## 7. no cut() function in JAGS, use dsum()
## 8. cannot use data[ind.a, ind.b] to indicate double index,
##    should use data[ind[ind.a,ind.b]].
## 9. JAGS may not work well on sum with many elements. So we split into several
##    sums with fewer element within each sum.
##10. write for (i in A:B), instead of for (i in C) even if C is a numerical
##    vector.


## The End! ##

