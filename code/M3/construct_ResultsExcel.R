##############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan from 
# 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 26 Aug 2025
#
# construct_ResultsExcel.R
#
# This script saves provincial SRB estimations and projections in excel sheet.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
# functions called: null
# input data: null
# output data: data/output/SRB_by_PakistanProvince_estimates_1980_to_2020.csv
###############################################################################

##################################################
## save provincial SRB estimates in excel sheet ##

col.names <- c("Pakistan.Province.Name",
               "Indicator", "Reference.Year",
               "Quantile", "Model.Estimate")
years.print.t <- 1980:2020

data.out <- NULL
for (c in 1:C) {
  add.data <- matrix(NA, nr = Per * length(years.print.t), nc = length(col.names))
  colnames(add.data) <- col.names
  
  add.data[, "Pakistan.Province.Name"] <- name.c[c]
  add.data[, "Indicator"] <- "Sex Ratio at Birth"
  add.data[, "Reference.Year"] <- rep(years.print.t, each = Per)
  add.data[, "Quantile"] <- rep(c("2.5 percentile", "Median", "97.5 percentile"), length(years.print.t))
  add.data[, "Model.Estimate"] <- c(res.proj$R2.jqt[name.c[c], , paste(years.print.t)])
  
  data.out <- rbind(data.out, add.data)
} # end of c loop

write.csv(data.out, row.names = FALSE, na = "",
          file = paste0(output.dir, "SRB_by_PakistanProvince_estimates_",
                        years.print.t[1], "_to_", rev(years.print.t)[1], ".csv"))



## THE END ##

