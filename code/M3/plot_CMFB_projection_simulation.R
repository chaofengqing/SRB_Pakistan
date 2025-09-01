
###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# This script uses Bayesian simulation results to generate trend plots and heatmaps
# of missing female births (CMFB, AMFB, MISFB) for selected provinces
# of Pakistan from 2021 to 2050, and outputs them as PDF files.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R;
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# PlotCIband(2)
# 
# input data: 
# 1. data/output/simulatedMISFB_bystartyear_M3.rda
# 2. data/output/simulatedCMFB_bystartyear_M3.rda
# 3. data/output/simulatedAMFB_bystartyear_M3.rda
# 4. data/interim/birth.ct.rda
#
# output plots in folder fig/:
# 1. CMFB_projection_simulation.pdf
# 2. AMFB_projection_simulation.pdf
# 3. AMFB_projection_simulation_heatmap_Khyber Pakhtunkhwa.pdf
# 4. AMFB_projection_simulation_heatmap_Punjab.pdf
# 5. AMFB_projection_simulation_heatmap_Sindh.pdf
# 6. AMFB_projection_simulation_heatmap_Islamabad (ICT).pdf
###############################################################################

load(file = paste0(output.dir,"simulatedMISFB_bystartyear_",runname,".rda")) #sim.misfb.cts
load(file = paste0(output.dir,"simulatedCMFB_bystartyear_",runname,".rda")) #sim.cmfb.cqs
load(file = paste0(output.dir,"simulatedAMFB_bystartyear_",runname,".rda")) #sim.amfb.cqs
load(file = paste0(interim.dir, "birth.ct.rda")) #birth.ct

t.start <- which(years.t == 2021.5)
plot.years <- 2021:2050

pdf(paste0(fig.dir, "CMFB_projection_simulation.pdf"))
par(mar = c(3, 3, 2, 1), mfrow = c(2, 2), mgp = c(1.5, 0.5, 0))
for (c in which(is.element(name.c, c("Khyber Pakhtunkhwa",
                                     "Punjab",
                                     "Sindh",
                                     "Islamabad (ICT)")))) {
  plot(sim.cmfb.cqs[c, 3, ]/1000 ~ plot.years, type = "n",
       ylim = c(0, max(sim.cmfb.cqs[c, 3, ]/1000)),
       xlab = "Start year of SRB inflation process",
       ylab = "CMFB during 2021-2050 (in thousand)",
       main = name.c[c])
  PlotCIbands(CIs.qt = sim.cmfb.cqs[c, , ]/1000, year.t = 2021:2050)
  abline(h = 0)
} # end of c loop
dev.off()

pdf(paste0(fig.dir, "AMFB_projection_simulation.pdf"))
par(mar = c(3, 3, 2, 1.5), mfrow = c(2, 2), mgp = c(1.5, 0.5, 0))
for (c in which(is.element(name.c, c("Khyber Pakhtunkhwa",
                                     "Punjab",
                                     "Sindh",
                                     "Islamabad (ICT)")))) {
  plot(sim.amfb.cqs[c, 3, ]/1000 ~ plot.years, type = "n",
       ylim = c(0, max(sim.amfb.cqs[c, 3, ]/1000)),
       xlab = "Start year of SRB inflation process",
       ylab = "Average AMFB (in thousand)",
       main = name.c[c])
  PlotCIbands(CIs.qt = sim.amfb.cqs[c, , ]/1000, year.t = 2021:2050)
  abline(h = 0)
  
  par(new = TRUE)
  ## Plot the second plot and put axis scale on right
  birth <- as.numeric(birth.ct[name.c[c], paste(plot.years)]) / 1000

  plot(x = plot.years, y = birth, pch = 15,  xlab = "", ylab = "", ylim = range(birth, na.rm = TRUE),
       axes = FALSE, type = "b", col = "navyblue", cex = 1)
  ## a little farther out (line=4) to make room for labels
  mtext("Number of total birth (in thousand)", side = 4, col = "navyblue", line = -1, cex = 0.8, las = 3)
  axis(4, col = "navyblue", col.axis = "navyblue")
  
} # end of c loop
dev.off()


library("lattice")
library(RColorBrewer)
coul <- rev(colorRampPalette(brewer.pal(8, "RdYlBu"))(25))
dimnames(sim.misfb.cts) <- list(name.c, floor(plot.years), floor(plot.years))

# par(mar = c(3, 3, 2, 1.5), mfrow = c(2, 2), mgp = c(1.5, 0.5, 0))
for (c in which(is.element(name.c, c("Khyber Pakhtunkhwa",
                                     "Punjab",
                                     "Sindh",
                                     "Islamabad (ICT)")))) {
  pdf(paste0(fig.dir, "AMFB_projection_simulation_heatmap_", name.c[c], ".pdf"))
  levelplot(sim.misfb.cts[name.c[c], , ]/1000, col.regions = coul, #heat.colors(100),
            scales=list(x=list(rot=45)),
            xlab = "Year", ylab = "Start year of SRB inflation process",
            main = paste(name.c[c], "AMFB (in 000)"))
  dev.off()
} # end of c loop

