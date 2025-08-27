#############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 27 Aug 2025
# construct_cumsumMissingFemaleBirth.R
# 
# This script construct cummulative number of missing girls for the world and regions.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(1)
#
# input data: 
# 1. data/output/countryTrajectory/trajectory_M3_Balochistan_full.rda
# 2. data/output/countryTrajectory/trajectory_M3_Khyber Pakhtunkhwa_full.rda
# 3. data/output/countryTrajectory/trajectory_M3_Punjab_full.rda
# 4. data/output/countryTrajectory/trajectory_M3_Sindh_full.rda
# 5. data/output/countryTrajectory/trajectory_M3_Gilgit Baltista_full.rda
# 6. data/output/countryTrajectory/trajectory_M3_Islamabad (ICT)_full.rda
# 7. data/output/countryTrajectory/trajectory_M3_Azad Jammu and Kashmir_full.rda
# 8. data/output/countryTrajectory/trajectory_M3_Federally Administered Tribal Areas_full.rda
#
# output data:
# 1. data/output/cis_M3_CumsumMissing.rda
#
###############################################################################

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
                     "_", name.c[c], "_full.rda")) # res.c.full
  
  for (l in 1:L) {
    cumsum.missingBf.clt[c, l, ] <-
      cumsum(res.c.full[["Bmisf.lt"]][l, ])
  } # end of l loop
  cumsum.missingBf.cqt[c, , ] <- SamplesToUI(cumsum.missingBf.clt[c, , ])
} # end of c loop

res.missing.CI <- list(cumsum.missingBf.cqt = cumsum.missingBf.cqt,
                       cumsum.missingBf.clt = cumsum.missingBf.clt)

save(res.missing.CI, file = paste0(output.dir, "cis_", runname, "_CumsumMissing.rda"))

## the end ##

