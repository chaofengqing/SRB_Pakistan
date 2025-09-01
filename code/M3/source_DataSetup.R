###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# source_DataSetup.R
# 
# This script setup data and indices for modelling and output.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Functions called in the scripts listed above are not listed.
# PlotCIbandwithDataseries(1) - data series plots for checking purpose.
# 
# input data: null
#
# output data: null
#
# output plot in folder fig/M1/: if Main.run & First.run
# 1. dataSeries_ForModel(noimputeSE)_date.pdf
#
# Data setup summary in several parts:
# part 2b: remove entries in SR data base if there is no IGME estimate for
# its corresponding mortality
#
# part 4a: get source types and make sure VR is the last type
#
# part 4b: get imputed standard error on log-scale
#
# part 6: get indices about country, year, region
#
# part 7: get indices for non-missing P.ct's to avoid sampling for all P.ct's
# instead, only sample the t.i's that have an observation
#
###############################################################################
##########
##########
## part 2b: remove entries in SR data base if there is no IGME estimate for
# its corresponding mortality
data.all <- dataset
I        <- dim(data.all)[1]; I

name.i          <- data.all[, "Domain.Name"       ]
year.i          <- data.all[, "Reference.Date"    ]
method.i        <- data.all[, "Series.Type"       ]
typename.i      <- data.all[, "Series.Category"   ]
surveyyear.i    <- data.all[, "Series.Year"       ]
logSE.i         <- data.all[, "SE.logSRB.Modeling"]
logSEnoimpute.i <- data.all[, "SE.logSRB.Jackknife"] #before imputing for missing SEs
r.i             <- data.all[, "Observed.SRB"      ]
logr.i          <- log(r.i)
surveyplot.i <- paste0(typename.i, " (", surveyyear.i, ")")
surveyplot.i <- gsub("Standard DHS", "DHS", surveyplot.i)
surveyplot.i[typename.i == "SRS"] <- "SRS"

## summary of the database used to fit the model ##
cat("There are", I, "data points in total.\n")
cat("There are", sum(data.all[, "Interval"], na.rm = TRUE), "country-years in total.\n")
cat("There are", length(unique(name.i)), "countries/areas with data.\n")
cat("There are", C, "countries/areas we will estimate SRB.\n")
cat("There are", round(sum(data.all[, "Interval"], na.rm = TRUE)/C, 1), "country-years for each",
    "one of the", C, "countries/areas.\n")
# create a new source type for DHS series excluded in U5MR database
# we still include them in SRB model
unique(typename.i)
table(typename.i)

typename.i <- paste(typename.i, method.i)

typename.i[typename.i == "SRS Direct"] <- "VR"
# VRseries.list <- "VR"

###########
## part 4a: get source types and make sure VR is the last type
# NOTE for later: for 1-country run, you'll have subsets of these 
# need to keep track of what parameters to use for sigma of source type
source.i <- as.numeric(as.factor(typename.i))
S        <- max(source.i); S

###----------------------------------------------------------------
# VR needs to be last one
# print warning here and at the end
if (prod((source.i == S)[typename.i == "SRS"]) != 1) {
  print("Warning: VR is not sourcetype S! Reassignment is carried out...")
} # end of if(prod((source.i == S)[typename.i == "VR"]) != 1)


# for Main.run, the following shows the full set info
# for Validation.run, the following shows the training set info
table(typename.i, source.i)
# source.i (full database info)
# typename.i           1
# Standard DHS Direct 62


source.i <- as.numeric(as.factor(typename.i))
S        <- max(source.i)

table(typename.i, source.i)
# source.i (full database info)
# typename.i           1
# Standard DHS Direct 62
 
###########
## part 4b: get imputed standard error on log-scale
# #0.0882775 full dataset
## Use a minimum for SE at 1% or 2.5%
SE.lower <- 5 / 100
logSE.i[typename.i == "PSLM"] <- SE.lower
##############################################
## plot with data serise and non-imputed SE ##
VRseries.name <- "SRS"

if (First.run) {
  ## plot all countries ##
  pdf(paste0(fig.dir, "dataSeries_ForModel(noimputeSE)_",
             Sys.Date(), ".pdf"), height = 8, width = 18)
  par(cex.lab = 2.5, cex.axis = 2, mgp = c(2.8, 0.8, 0), cex.main = 3)
  layout(t(c(1, 1, 1, 1, 2)), respect = FALSE)
  count <- 0
  
  for (c in 1:C) {
    c.select <- which(name.i == name.c[c])
    # sort by survey date to get nicer legend
    select <- c.select[order(surveyplot.i[c.select])]
    
    if (length(select) == 0) {
      count <- count + 1
      print(paste(count, name.c[c], "has no data."))
    } else {
      par(mar = c(4.5, 5, 4, 0.5))
      PlotCIbandwithDataseries(
        if.SurveyLegend = TRUE, if.sepLegendPage = TRUE, if.LogScale = TRUE,
        dataseries = logr.i, dataseriesSE = logSEnoimpute.i,
        Source = surveyplot.i, baseSeries = VRseries.name,
        x = year.i, select.x = select,
        SElim = c(max(0.7, min(exp(logr.i[select] - 2 * logSEnoimpute.i[select])),
                      na.rm = TRUE),
                  min(1.6, max(exp(logr.i[select] + 2 * logSEnoimpute.i[select])),
                      na.rm = TRUE)),
        datalim = range(data.all[select, "Observed.SRB"], na.rm = TRUE),
        lwd.dataseries = 2.5,  cex.legend = 2, max.legend = 60,
        main = name.c[c],
        alpha.dataseries = 1, alpha.polygon = 0.2, cex.dataseries = 2,
        ylab = "Sex Ratio at Birth", xlab = "Year", cutoff = SRB0)
    } # end of ifelse(length(select) == 0)
  } # end of c loop
  dev.off()
  
}  #end of if(First.run)


##########
## part 6: get indices about country, year, region
min(year.i) #1980
years.t <- c(floor(min(year.i)) : 2050)+0.5
Tend    <- length(years.t)

t.i <- c.i <- rep(NA, I)

for (i in 1:I) {
  # find country-year for each index
  c.i[i] <- which(name.c == name.i[i])
  t.i[i] <- which(years.t == floor(year.i[i]) + 0.5)
} # end of i loop


##########
## part 7: get indices for non-missing P.ct's to avoid sampling for all P.ct's
# instead, only sample the t.i's that have an observation
gett.cz <- matrix(NA, C, I) # reduce dimension later
nt.c <- rep(NA, C)

for (c in 1:C) {
  
  gett <- sort(unique(t.i[name.i == name.c[c]]))
  # make sure all countries have at least two t's 
  # (easier in loop in JAGS, and coding later when reconstructing P)
  if (length(gett) == 0) {
    gett <- c(floor((Tend + 1) / 2), floor((Tend + 1) / 2 + 1))
  }
  if (length(gett) == 1 & max(gett) < Tend) {
    gett <- c(gett, gett + 1)
  }
  if (length(gett) == 1 & max(gett) == Tend) {
    gett <- c(gett - 1, gett)
  }
  nt.c[c] <- length(gett)
  gett.cz[c, 1:nt.c[c]] <- gett
  
} # end of c loop

gett.cz <- gett.cz[, 1:max(nt.c)]

# warning again:
if (prod((source.i == S)[typename.i == "VR"]) != 1) {
  print("STOP: VR is not sourcetype S!")
} else {
  print("Great! VR is indeed sourcetype S!")
} # end of ifelse(prod((source.i == S)[typename.i == "VR"]) != 1)


## The END! ##

