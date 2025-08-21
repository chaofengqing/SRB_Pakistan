

## construct cummulative number of missing girls for the world and regions ##

#######################
## WITH INFLATION!!! ##
cumsum.missingBf.clt <- array(0, c(C, L, Tend))
cumsum.missingBf.cqt <- array(0, c(C, Per, Tend))

dimnames(cumsum.missingBf.cqt)[[2]] <- percentiles
dimnames(cumsum.missingBf.cqt)[[3]] <- floor(years.t)
dimnames(cumsum.missingBf.cqt)[[1]] <- name.c


for (c in 1:C) {
  cat(c, "in", C, "\n")
  # read in country-specific trajectory
  load(file = paste0(countryTraj.dir, "trajectory_", runname,
                     "_", name.c[c], "_full.rda")) #res.c.full
  
  for (l in 1:L) {
    cumsum.missingBf.clt[c, l, ] <-
      cumsum(res.c.full[["Bmisf.lt"]][l, ])
  }#end of l loop
  cumsum.missingBf.cqt[c, , ] <- SamplesToUI(cumsum.missingBf.clt[c, , ])
}#end of c loop

res.missing.CI <- list(cumsum.missingBf.cqt = cumsum.missingBf.cqt,
                       cumsum.missingBf.clt = cumsum.missingBf.clt)

save(res.missing.CI, file = paste0(output.dir, "cis_", runname, "_CumsumMissing.rda"))

## the end ##

