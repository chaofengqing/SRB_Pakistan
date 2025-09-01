###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# StandardizeDecimal.R
# 
# This script contains function related to making number.in to round to n.round. 
#
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
#
# StandardizeDecimal(..)
#
#
###############################################################################
#------------------------------------------------------------------------------


StandardizeDecimal <- function(
  number.in,
  n.round) {
  ## make number.in to round to n.round. If resulting number has decimal places
  # smaller than rounded number, add 0 behind. E.g. round(1.999, 2) to be 2.00
  # instead of 2
  number.out <- format(as.numeric(round(number.in, n.round)), nsmall = n.round)
  
  return(number.out)
} # end of StandardizeDecimal()

