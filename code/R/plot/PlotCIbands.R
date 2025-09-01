
###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# PlotCIbands.R
# 
# This script contains function which plots confidence interval bands 
# and median estimate lines over time from posterior summaries.
#
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
#
# PlotCIbands(..)
#
#
###############################################################################
#------------------------------------------------------------------------------
PlotCIbands <- function(
  if.CI = TRUE, # if plot CI band
  if.line = TRUE, # if plot median or best estimate of CI
  CIs.qt, # q is 3; row 1-lower, row 2-median, row 3-upper
  year.t = years.t,
  col = "red", # color of the CI band
  alpha.CI = 0.35,
  alpha.line = 0.4,
  lty.line = 1, # line style for median estimates
  lwd.CI = 1 #lwd for line of median/best estimates
) {
  
  ## 95% CI plolygon plot over time ##
  if (!if.CI & !if.line) {
    return(invisible) #FQ: need to check if it is ok
  } else {
    order.year <- order(year.t)
    year.t <- year.t[order.year]
    
    if (if.CI) {
      if (is.vector(CIs.qt)) {
        CI.low.t <- CIs.qt[1]
        CI.up.t  <- CIs.qt[3]
      }
      if (is.matrix(CIs.qt)) {
        CIs.qt <- CIs.qt[, order.year]
        CI.low.t <- CIs.qt[1, ]
        CI.up.t  <- CIs.qt[3, ]
      }
      
      x.full <- c(year.t, rev(year.t), year.t[1])
      y.full <- c(CI.low.t, rev(CI.up.t), CI.low.t[1])
      
      #only plot points with non-missing (x,y)
      nonNA <- !is.na(y.full) & !is.na(x.full)
      x.plot <- x.full[nonNA]
      y.plot <- y.full[nonNA]
      
      if (is.matrix(CIs.qt)) {
      polygon(
        x.plot, y.plot,
        border = adjustcolor(col = col,
                             alpha.f = ifelse(length(x.plot) == 2,
                                              alpha.line, alpha.CI / 10)),
        col = adjustcolor(col = col, alpha.f = alpha.CI))
      } # end of if
      
      if (is.vector(CIs.qt)) {
        segments(x0 = year.t, y0 = CI.low.t, y1 = CI.up.t,
                 col = col, lwd = 1.5)
      } # end of if
      
    } # end of if.CI
    
    if (if.line & is.matrix(CIs.qt)) {
      noNA <- !is.na(year.t) #connect all non-NA data
      if (if.CI) {
        estimate.t <- CIs.qt[2, noNA]
      } else {
        estimate.t <- CIs.qt[2, order.year[noNA]]
      } # end of ifelse(if.CI)
      
      lines(estimate.t ~ year.t[noNA],
            lwd = lwd.CI, col = adjustcolor(col = col, alpha.f = alpha.line))
    } # end of if.line
    
  } # end of ifelse (!if.CI & !if.line)
  
} # end of PlotCIbands function

