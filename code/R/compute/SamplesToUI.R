###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# SamplesToUI.R
# 
# This script contains function related to computing specified percentiles 
# from posterior samples.
#
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
#
# SamplesToUI(..)
#
#
###############################################################################
#------------------------------------------------------------------------------

SamplesToUI <- function (
  samples.jt, # inputs, note that it also works for samples.j
  NA.jt = 0, # indicate which entries should be set to NA. If NULL, it's not used.
  percentile.q = percentiles # output percentiles for input
) {
  # output: result.qt
  
  if (is.vector(samples.jt)) {
    result.qt <- quantile(samples.jt + NA.jt, probs = percentile.q, na.rm = TRUE)
  }
  if (is.matrix(samples.jt)) {
    result.qt <- apply(samples.jt + NA.jt, 2, quantile, percentile.q, na.rm = TRUE)
  }
  
  return (result.qt)
  
} # end of SamplesToUI function

