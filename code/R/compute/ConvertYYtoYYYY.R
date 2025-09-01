###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# ConvertYYtoYYYY.R
# 
# This script contains a utility function to convert two-digit years 
# into four-digit years.
# 
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# ConvertYYtoYYYY(..)
# 
#################################################


ConvertYYtoYYYY <- function(yy.date) {
  ## convert YY to YYYY format ##
  yy.date <- as.numeric(yy.date)
  
  yyyy.date <- yy.date + 1900
  
  # note: 13 means the year of 2013, will change by time
  if (min(yy.date, na.rm = TRUE) <= 17) {
    yyyy.date <- yy.date + 2000
  }
  
  return(yyyy.date)
} # end of ConvertYYtoYYYY() function
