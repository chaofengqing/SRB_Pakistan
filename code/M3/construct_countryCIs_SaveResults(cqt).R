#############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 27 Aug 2025
# construct_countryCIs_SaveResults(cqt).R
# 
# This script saves the results.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(11)
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
# 1. data/output/cis_full_M3.rda
# 2. data/output/cis_PAK_M3.rda
# 3. data/output/trajectory_M3_R.rda
# 4. data/output/trajectory_M3 _PAK_R.rda
#
###############################################################################


## construct empty arrays and metrics and assign dimention names ##
array.cqt <- array(0, c(C, Per, Tend))
array.clt <- array(0, c(C, L,   Tend))
matrix.qt <- matrix(0, nr = Per, nc = Tend)
matrix.lt <- matrix(0, nr = L, nc = Tend)

dimnames(array.cqt)[[2]] <- dimnames(matrix.qt)[[1]] <- percentiles
dimnames(array.cqt)[[3]] <- dimnames(array.clt)[[3]] <-
  dimnames(matrix.qt)[[2]] <- dimnames(matrix.lt)[[2]] <-floor(years.t)
dimnames(array.cqt)[[1]] <- dimnames(array.clt)[[1]] <- name.c

## state ##
Bf.cqt <- Bm.cqt <-
  expBf.cqt <-
  misBf.cqt <-
  R.cqt <- N.cqt <- array.cqt

R.clt <- array.clt

## whole India ##
Bf.qt <- Bm.qt <-
  expBf.qt <-
  misBf.qt <-
  R.qt <- matrix.qt

R.lt <- 
  Bf.lt <- Bm.lt <- expBf.lt <-
  misBf.lt <- matrix.lt

#########################
## get country results ##
for (c in 1:C) {
  cat(c, "in", C, "\n")
    
  # read in country-specific trajectory
  load(file = paste0(countryTraj.dir, "trajectory_", runname,
                     "_", name.c[c], "_full.rda")) #res.c.full
  
  ## get country-specific results ##
  R.cqt[c, , ] <- SamplesToUI(res.c.full[["R.lt"]])
  N.cqt[c, , ] <- SamplesToUI(res.c.full[["N.lt"]])
  Bf.cqt[c, , ] <- SamplesToUI(res.c.full[["Bf.lt"]])
  Bm.cqt[c, , ] <- SamplesToUI(res.c.full[["Bm.lt"]])
  expBf.cqt[c, , ] <- SamplesToUI(res.c.full[["Bexpf.lt"]])
  misBf.cqt[c, , ] <- SamplesToUI(res.c.full[["Bmisf.lt"]])
  
  ## whole Pakistan results ##
  Bf.lt.c <- res.c.full[["Bf.lt"]]
  Bm.lt.c <- res.c.full[["Bm.lt"]]
  Bexpf.lt.c <- res.c.full[["Bexpf.lt"]]
  Bmisf.lt.c <- res.c.full[["Bmisf.lt"]]
  
  Bf.lt.c <- ifelse(Bf.lt.c < 0, 0, Bf.lt.c)
  Bm.lt.c <- ifelse(Bm.lt.c < 0, 0, Bm.lt.c)
  Bexpf.lt.c <- ifelse(Bexpf.lt.c < 0, 0, Bexpf.lt.c)
  Bmisf.lt.c <- ifelse(Bmisf.lt.c < 0, 0, Bmisf.lt.c)
  
  Bf.lt <- Bf.lt + Bf.lt.c
  Bm.lt <- Bm.lt + Bm.lt.c
  expBf.lt <- expBf.lt + Bexpf.lt.c
  misBf.lt <- misBf.lt + Bmisf.lt.c
} # end of c loop

R.lt <- Bm.lt / Bf.lt

R.qt <- SamplesToUI(R.lt)
Bf.qt <- SamplesToUI(Bf.lt)
Bm.qt <- SamplesToUI(Bm.lt)
expBf.qt <- SamplesToUI(expBf.lt)
misBf.qt <- SamplesToUI(misBf.lt)


res.full <- list(R.cqt = R.cqt, N.cqt = N.cqt,
                 Bf.cqt = Bf.cqt, Bm.cqt  = Bm.cqt,
                 expBf.cqt = expBf.cqt,
                 misBf.cqt = misBf.cqt)

res.PAK.full <- list(R.qt = R.qt,
                 Bf.qt = Bf.qt, Bm.qt  = Bm.qt,
                 expBf.qt = expBf.qt,
                 misBf.qt = misBf.qt)

res.PAK.Rtrajectory <- list(R.lt = R.lt)

##########################
## save R.ct trajectory ##
for (c in 1:C) {
  print(c)
  # read in country-specific trajectory
  load(file = paste0(countryTraj.dir, "trajectory_", runname,
                     "_", name.c[c], "_full.rda")) #res.c.full
  
  R.clt[c, , ] <- res.c.full[["R.lt"]]
}  # end of c loop

res.Rtrajectory <- list(R.clt = R.clt)


save(res.full, file = paste0(output.dir, "cis_full_", runname, ".rda"))
save(res.PAK.full, file = paste0(output.dir, "cis_PAK_", runname, ".rda"))
save(res.Rtrajectory, file = paste0(output.dir, "trajectory_", runname, "_R.rda"))
save(res.PAK.Rtrajectory, file = paste0(output.dir, "trajectory_", runname, "_PAK_R.rda"))

## the end ##


