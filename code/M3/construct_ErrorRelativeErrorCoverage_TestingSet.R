

## simulation/validation results ##

## read in data ##
load(file = paste0(output.dir, "cis_", runname, "_simulation.rda")) #res.sim
load(file = paste0(output.dir, "PredictCI_", runname, ".rda")) #Predict.qe
load(file = paste0(output.dir, "Predict80CI_", runname, ".rda")) #Predict80.qe

## create vectors ##
r.e <- t.e <- c.e <- year.e <- name.e <- NULL
for (j in 1:C.adj) {
  c <- c.adj[j]
  country <- name.c[c]
  select.i <- which(name.i == country)
  
  r.e <- c(r.e, r.i[select.i])
  t.e <- c(t.e, t.i[select.i])
  c.e <- c(c.e, c.i[select.i])
  year.e <- c(year.e, year.i[select.i])
  name.e <- c(name.e, name.i[select.i])
}#end of j loop

r.m <- Predict.qe[2, ] # median for predicted left-out data
r.l <- Predict.qe[1, ] # lower bound for predicted left-out data
r.u <- Predict.qe[3, ] # upper bound for predicted left-out data
r.80.l <- Predict80.qe[1, ] # lower bound for predicted left-out data
r.80.u <- Predict80.qe[3, ] # upper bound for predicted left-out data

E <- length(r.e)

#################################################################
## mean/median for error, relative error, and abs of these two ##
Error <- r.e - r.m # error
RelativeError <- Error / r.e * 100 # relative error (%)

## note: the prob for E leftout data to fall outside 95% PI for 95% of the time
quantile(rbinom(Nsim, size = E, prob = 0.05) / E, percentiles)

###############################################################################
## sample one leftout data per country with Nsim repeated draws to get coverage ##
pick.nc <- matrix(NA, nr = Nsim, nc = length(unique(name.e))) #record the chosen leftout for each time
## all regions combined ##
Coverage.nq <- Coverage80.nq <- matrix(NA, nr = Nsim, nc = 2) #FQ: Per - 1 ?
meanError.n <- medianError.n <- medianAbsError.n <- medianAbsRelativeError.n <- rep(NA, Nsim)

for (n in 1:Nsim) { #Nsim repeated draws
  pick <- rep(NA, length(unique(name.e)))
  set.seed(2013 + n * 100)
  
  for (j in 1:length(unique(name.e))) {
    pick[j] <- sample(which(name.e == unique(name.e)[j]), size = 1) #sample one per country
  }#end of j loop
  pick.nc[n, ] <- pick
  
  ## all regions combined (all age groups) ##
  Coverage.nq[n, 1] <- mean(r.e[pick] < r.l[pick]) #Coverage.Below
  Coverage.nq[n, 2] <- mean(r.e[pick] > r.u[pick]) #Coverage.Above
  Coverage80.nq[n, 1] <- mean(r.e[pick] < r.80.l[pick]) #Coverage.Below
  Coverage80.nq[n, 2] <- mean(r.e[pick] > r.80.u[pick]) #Coverage.Above
  meanError.n[n]   <- mean(Error[pick])                           #mean.Error
  medianError.n[n] <- median(Error[pick])                         #median.Error
  medianAbsError.n[n] <- median(abs(Error)[pick])                 #median.AbsError
  medianAbsRelativeError.n[n] <- median(abs(RelativeError)[pick]) #median.AbsRelativeError
  
}#end of n loop

Coverage.OneperCountry.df <- list(
  Coverage.nq = Coverage.nq * 100,
  Coverage80.nq = Coverage80.nq * 100,
  pick.nc = pick.nc,
  meanError.n = meanError.n,
  medianError.n = medianError.n,
  medianAbsError.n = medianAbsError.n,
  medianAbsRelativeError.n = medianAbsRelativeError.n
)


##################
## save to file ##
## all age groups combined ##
result.all <- apply(cbind(Coverage.nq * 100, #3.83 4.77
                          Coverage80.nq * 100,
                          meanError.n, #0.0045
                          medianError.n, #0.0045
                          medianAbsError.n,#0.12
                          medianAbsRelativeError.n),#10.75
                    2, mean, na.rm = TRUE)

n.result <- length(unique(name.e))
N.result <- E
result.tmp <- result.all
result.CoverageOneperCountry <- c(N.result, n.result, result.tmp)
result.CoverageOneperCountry <- as.matrix(result.CoverageOneperCountry)

rownames(result.CoverageOneperCountry) <- c(
  "number of left-out observations within each region",
  "number of countries within each regoin",
  "Left-out observations fall below 95% PI (%)",
  "Left-out observations fall above 95% PI (%)",
  "Left-out observations fall below 80% PI (%)",
  "Left-out observations fall above 80% PI (%)",
  "Mean of Error",
  "Median of Error",
  "Median of absolute Error",
  "Median of absolute Relative Error (%)"
)

colnames(result.CoverageOneperCountry) <- "all regions"
result.CoverageOneperCountry.allAge <- result.CoverageOneperCountry

## save the coverage summary for all left-out observations ##
write.csv(result.CoverageOneperCountry.allAge, row.names = TRUE,
          paste0(output.dir, "Results_CoverageOneperCountrySummary_TestingSet_withError.csv"))


## the end ##


