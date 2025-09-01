###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
#
# plot_lineseg_SRB_allstates.R
# 
# This script compares the estimated SRB and its confidence intervals 
# for multiple regions of Nepal
#
# used for which run: Main.run
#
# this script is called by any other scripts: main_output.R;
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIsegments(1)
# 
# input data: null
#
# output plots in folder fig/: 
# 1.SRB_all_states_2030order.pdf - compares SRB in 2030, 2010, and 1980
# 2.SRB_all_states_2018order.pdf - compares SRB in 2018, 2000, and 1980
#
###############################################################################

name.plot <- name.c
name.plot[name.c == "Northern Central area and Central coastal area"] <-
  "Northern Central area and\nCentral coastal area"
name.plot[name.c == "Northern midlands and mountain areas"] <-
  "Northern midlands and\nmountain areas"

for (k in 1:2) {
  if (k == 1) years.select <- c(2030, 2010) + 0.5
  if (k == 2) years.select <- c(2018, 2000) + 0.5
  t.select <- which(is.element(years.t, years.select))
  
  order.of.year <- max(years.select)
  text.cex <- 0.7
  dot.cex <- 1.5
  name.show.result <- name.c
  pdf(paste0(fig.dir, "SRB_all_states_", floor(order.of.year), "order.pdf"),
      height = 4.5, width = 5)
  par(mar = c(2.5, 7, 0.5, 0.5), cex.lab = text.cex, mgp = c(1.5, 0.5, 0), 
      cex.main = text.cex, cex.axis = text.cex, tcl = -0.5)
  R.cqt <- res.proj$R2.jqt
  
  select.t <- is.element(years.t, years.select)
  select.c <- is.element(name.c, name.show.result)
  PlotCIsegments(
    countryName.c = name.plot,
    cutoff = exp(logNmu), order = 2, data.cqt = R.cqt,
    select.c = select.c, select.t = select.t, yearOrder = order.of.year,
    xlab = "Sex Ratio at Birth", #main = "year 2017",
    colinfo = c("lightgrey", "hotpink4", "hotpink"), vertical.gap = 3,
    lwd.main = 3, lwd.cutoff = 1, cex = dot.cex,
    x.range = c(1.04, 1.16), reset.xlim = TRUE)
  order.c <- order(res.proj$R2.jqt[, 2, paste(years.select[1]-0.5)])
  for (c in 1:C) {
    segments(x0 = res.proj$R2.jqt[name.c[order.c[c]], 1, "1980"],
             x1 = res.proj$R2.jqt[name.c[order.c[c]], 3, "1980"],
             y0 = c+0.25, col = "pink", lwd = 4)
    points(x = res.proj$R2.jqt[name.c[order.c[c]], 2, "1980"], y = c+0.25,
           pch = 19, cex = dot.cex, col = "pink")
    segments(x0 = res.proj$R2.jqt[name.c[order.c[c]], 1, "1980"],
             x1 = res.proj$R2.jqt[name.c[order.c[c]], 3, "1980"],
             y0 = c+0.25, col = 1, lwd = 1)
    points(x = res.proj$R2.jqt[name.c[order.c[c]], 2, "1980"], y = c+0.25,
           pch = 21, bg = "pink", cex = dot.cex - 1/C, col = 1)
  } # end of c loop
  legend("bottomright", paste(c(1980, rev(years.select-0.5))), col = c("pink", "hotpink", "hotpink4"),
         lty = 1, lwd = 4, pch = 19, cex = text.cex, bg = "white")
  box()
  dev.off()
} # end of k loop

## the end ##

