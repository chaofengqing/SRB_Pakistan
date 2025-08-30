###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 30 Aug 2025
# 
# main_output.R
# 
# This script is the master file to get all results for Main.run. Run this
# script ONLY AFTER main.R have all been run through.
#
# used for which run: Main.run
#
# this script is called by any other scripts: null
#
# this script calls other scripts:
# 01. source_BasicSetup.R
# 02. source_DirectorySetup.R
# 03. source_adj_DataSetup.R
# 04. source_DataSetup.R
# 05. source_ADJ&TFR_relation.R
# 06. jags_setupMCMC.R
# 07. jags_ConvergenceCheck.R
# 08. construct_logPselect.R
# 09. construct_ADJselect.R
# 10. construct_CountryList_PastInflation.R
# 11. construct_countryCIs.R
# 12. plot_countryCIandData(NOimpute).R
# 13. plot_lineseg_SRB_allstates.R
# 14. plot_countrySRB&TFR.R
# 15. plot_SRB_map_province.R"
# 16. construct_onecountry_ADJ_simulation.R
# 17. construct_cumsumMissingFemaleBirth.R
# 18. construct_ErrorRelativeErrorCoverage_TestingSet.R"
# 19. construct_countryCIs_SaveResults(cqt).R
# 20. construct_CMFB_projection_simulation.R
# 21. table_appendix_supplementary_estimates.R
# 22. plot_newcountry_ADJ_simulation.R
# 23. construct_countryCIs_CountryTrajectories.R
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# ReadJagsOutput(1)
#
# input data in folder data/:
# 1. interim/database_for_modeling_2021-06-01.csv - SR data base;
#                                      created by source_dataCleaning (main.R)
#
# 2. output/M1/temp.JAGSobjects/* - read in stepwise JAGS output
#
# output data in folder data/output/M1/
# 1. selectP_M1.rda  - the estimated probabilities of SRB inflation 
#                      for each country or region based on the Bayesian model outputs.
# 2. cis_M1__senario_proj.rda   - the world/region/country median with 90% CI in order
#                                  to get other tables and plots.
# 3. selectADJ_M1.rda  - country-specific SRB adjustment factors used to 
#                        account for potential data biases.
# 4. mcmc.array_M1.rda  - MCMC array
# 5. traj_M1_R2.jtl.rda  -
# Note: only the main output data are listed here since it is a master script.
# The above output data may be created in other scripts which are called in
# this script.
#################################################################################

## Master code file ##
## This run is to get normal level of SRB only!!!

rm(list = objects())

projname <- "SRB_Pakistan" # name for this project
runname  <- "M3" # name for this run/model; based on M6c_new_full_reg
glb.normal.runname <- "M57_normal"
glb.adj.runname <- "M58_adj_test"

First.run <- FALSE # construct mcmcarray and output?
CleanData <- FALSE # only do once to avoid problems with sorting differences
DoMCMC    <- FALSE  # get step-wise JAGS output?

OneCountry.run <- grepl("_one", runname) # whether this is a one-country run

workdir <- "Your own work dictionary"
setwd(file.path(workdir, projname))

# country name
iso3.code    <- "PAK"
country.name <- "Pakistan"

# setup directories
source("code/source_DirectorySetup.R")

# setup constants, functions, libraries
source("code/source_BasicSetup.R")

# read in cleaned SRB database
dataset <- read.csv(paste0(interim.dir, srb.filename),
                    header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
Inclusion <- dataset$Inclusion
dataset <- dataset[Inclusion, ]

## When model is to get normal SRB, use ALL data from non-Aisian, and 
## data with reference date BEFORE 1970 for Asian countries.
adj.year <- 1970.5 # no adjustment before 1970

source(paste0("code/", runname, "/source_DataSetup.R"))
## data for adjustment factor ##
source(paste0("code/", runname, "/source_adj_DataSetup.R"))
## data related to TFR ##
source(paste0("code/", runname, "/source_ADJ&TFR_relation.R"))
# setup MCMC settings
source(paste0("code/", runname, "/jags_setupMCMC.R"))


## PART 1 - construct MCMC array (primary results) ##
## step 1: read in MCMC step-chains and get MCMC array as a whole
mcmc.array <- ReadJagsOutput(
  n.steps = N.STEPS,
  start.step = 1,
  maxiter = 20000,
  ChainNums = ChainIDs,
  runname = runname, output.dir = output.dir
)
save(mcmc.array, file = paste0(output.dir, "mcmc.array_", runname, ".rda"))

load(paste0(output.dir, "mcmc.array_", runname, ".rda")) #mcmc.array
L <- dim(mcmc.array)[1] * dim(mcmc.array)[2]; L

## convergence check ##
source(paste0("code/", runname, "/jags_ConvergenceCheck.R"))
## reconstruct all logP.ct's ##
source(paste0("code/", runname, "/construct_logPselect.R"))
## reconstruct all alpha.ct's ##
## directly from model
source(paste0("code/", runname, "/construct_ADJselect.R"))
# get country names with past/ongoing SRB inflation
source(paste0("code/", runname, "/construct_CountryList_PastInflation.R"))
save(country.list.past.inflation, file = paste0(output.dir, "country.list.past.inflation.rda")) #country.list.past.inflation
# load(file = paste0(output.dir, "country.list.past.inflation.rda")) #country.list.past.inflation
country.list.remove.adj <- setdiff(name.c, country.list.past.inflation)
## get country-specific R, P first!
source(paste0("code/", runname, "/construct_countryCIs.R"))

save(selectP, file = paste0(output.dir, "selectP_", runname, ".rda"))
save(selectADJ, file = paste0(output.dir, "selectADJ_", runname, ".rda"))
save(res.proj, file = paste0(output.dir, "cis_", runname, "_senario_proj.rda"))

load(file = paste0(output.dir, "selectP_", runname, ".rda")) #selectP
load(file = paste0(output.dir, "selectADJ_", runname, ".rda")) #selectADJ
load(file = paste0(output.dir, "cis_", runname, "_senario_proj.rda")) #res.proj

# save country-specific trajectory
source(paste0("code/", runname, "/construct_countryCIs_CountryTrajectories.R"))
# get World and Regional results
source(paste0("code/", runname, "/construct_countryCIs_SaveResults(cqt).R"))
# get cumulative number of missing female births for world and region
source(paste0("code/", runname, "/construct_cumsumMissingFemaleBirth.R"))

# simulate cumulative number of missing female births after 2020 by province
source(paste0("code/", runname, "/construct_CMFB_projection_simulation.R"))

source(paste0("code/", runname, "/plot_newcountry_ADJ_simulation.R"))
source(paste0("code/", runname, "/plot_countrySRB&TFR.R"))
source(paste0("code/", runname, "/plot_countryCIandData(NOimpute).R"))
source(paste0("code/", runname, "/plot_lineseg_SRB_allstates.R"))
source(paste0("code/", runname, "/plot_SRB_map_region.R"))
source(paste0("code/", runname, "/table_appendix_supplementary_estimates.R"))

## get simulated SRB inflation for each of the 33 countries ##
# read in cleaned SRB database
dataset <- read.csv(paste0(interim.dir, srb.filename),
                    header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
Inclusion <- dataset$Inclusion
dataset <- dataset[Inclusion, ]
source(paste0("code/", runname, "/source_DataSetup.R"))

source(paste0("code/", runname, "/construct_onecountry_ADJ_simulation.R"))
source(paste0("code/", runname, "/construct_ErrorRelativeErrorCoverage_TestingSet.R"))


## The End! ##
