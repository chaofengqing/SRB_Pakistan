

############################################
## plot the results for all the countries ##
# read in cleaned database of SRB by Indian state
# dataset <- read.csv(paste0(interim.dir, srb.filename),
#                     header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
# data.all <- dataset[dataset$Inclusion, ]
# 
# name.i          <- data.all[, "Domain.Name"       ]
# # iso.i           <- data.all[, "Domain.Code"       ]
year.i          <- data.all[, "Reference.Date"    ]
# method.i        <- data.all[, "Series.Type"       ]
# typename.i      <- data.all[, "Series.Category"   ]
# surveyyear.i    <- data.all[, "Series.Year"       ]
# logSEnoimpute.i <- data.all[, "SE.logSRB.Modeling"] #before imputing for missing SEs
# r.i             <- data.all[, "Observed.SRB"      ]
# logr.i          <- log(r.i)
surveyplot.i <- paste0(typename.i, " (", surveyyear.i, ")")
surveyplot.i <- gsub("Standard DHS", "DHS", surveyplot.i)
surveyplot.i[typename.i == "SRS"] <- "SRS"
VRseries.name <- "SRS"

pdf(paste0(fig.dir, "CIs_SRB_", country.name, "_region_", runname, "_", Sys.Date(), ".pdf"),
    height = 13, width = 30)
text.cex <- 4.5
par(cex.lab = text.cex, cex.axis = text.cex, mgp = c(11, 3, 0), mar = c(6, 14, 8, 10.7),
    cex.main = 6, las = 1, tcl = -1)
for (country in name.c) {
  c <- which(country == name.c)
  c.select <- which(name.i == name.c[c])
  # sort by survey date to get nicer legend
  select <- c.select[order(surveyplot.i[c.select])]
  
  R.qt <- res.proj[["R2.jqt"]][country, , ]
  
  a.qt <- res.proj$a.jqt[country, , ]
  plot.range <- range(a.qt, R.qt, exp(logr.i[select]),
                      na.rm = TRUE) * c(1.1, 0.9)
  par(las = 1)
  PlotCIbandwithDataseries(
    if.LogScale = TRUE, if.SurveyLegend = TRUE,
    year.t = floor(years.t),
    CI1s = R.qt, nameCI1 = "Estimated SRB",
    CI2s = a.qt, nameCI2 = "National SRB baseline",
    dataseries = logr.i, dataseriesSE = logSEnoimpute.i,
    Source = surveyplot.i, baseSeries = VRseries.name,
    x = year.i, select.x = select, x.lim = range(floor(years.t)),
    SElim = plot.range, datalim = plot.range,
    main = country, alpha.dataseries = 0.8, cex.dataseries = 1.5,
    ylab = "Sex Ratio at Birth", xlab = "Year", cutoff = exp(logNmu),
    lwd.CI1 = 15, lwd.CI2 = 10, lwd.dataseries = 3, colCI = c("red", "darkgreen"),
    cex.legend = 2, legendCI.posi = "topright", legendSurvey.posi = "bottomright")
}#end of c loop
dev.off()


 
## plot all states in one plot
pdf(paste0(fig.dir, "modelFullPeriod_SRB_", country.name, "Region_", runname, ".pdf"), 
    height = 20, width = 15)
layout(matrix(c(1, 1, 1, 4,
                2, 2, 2, 4,
                3, 3, 3, 4), nr = 3, nc = 4, byrow = TRUE))
par(cex.lab = 2.7, cex.axis = 2.5, mgp = c(6.2, 1.4, 0), mar = c(4, 8.2, 4, 1),
    cex.main = 3.2, las = 1, tcl = -1)
yr.start <- 1980
yr.end <- 2050
year.i <- rep(yr.start:yr.end, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, paste(yr.start:yr.end)])
surveyplot.i <- rep(name.c, times = length(yr.start:yr.end))
plot.lim <- range(r.i, na.rm = TRUE)

PlotCIbandwithDataseries(
  if.SurveyLegend = FALSE, if.sepLegendPage = FALSE,
  dataseries = r.i, SElim = plot.lim, datalim = plot.lim,
  baseSeries = "VR", Source = surveyplot.i,
  main = "SRB by Vietnam Region",
  alpha.dataseries = 1,
  alpha.point      = 1,
  alpha.polygon    = 0.5,
  year.t = yr.start:yr.end,
  # CI1s = res.country$R.cqt["India", , paste(yr.start:yr.end)], colCI = 1,
  # nameCI1 = "whole India", legendCI.posi = "topright",
  x = year.i, select.x = TRUE, cex.dataseries = 2,
  x.lim = range(year.i) + c(-1, 1), max.legend = 60,
  ylab = "Sex Ratio at Birth", xlab = "", cutoff = exp(logNmu),
  lwd.dataseries = 2, legendSurvey.posi = "topright", cex.legend = 2.5)
abline(h = c(1.08, 1.10, 1.12, 1.14), col = "grey", lwd = 0.5)
# abline(v = 2016, col = "grey", lwd = 1.5)
# axis(1, at = 2016, labels = 2016, cex = 2.5)
# zoom in on 1980-2016
yr.start <- 1980
yr.end <- 2020
year.i <- rep(yr.start:yr.end, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, paste(yr.start:yr.end)])
surveyplot.i <- rep(name.c, times = length(yr.start:yr.end))
plot.lim <- range(r.i, na.rm = TRUE)
PlotCIbandwithDataseries(
  # if.SurveyLegend = TRUE, if.sepLegendPage = TRUE,
  dataseries = r.i, SElim = plot.lim, datalim = plot.lim,
  baseSeries = "VR", Source = surveyplot.i,
  main = paste0("zoom in ", yr.start, "-", yr.end),
  alpha.dataseries = 1,
  alpha.point      = 1,
  alpha.polygon    = 0.5,
  year.t = yr.start:yr.end,
  x = year.i, select.x = TRUE, cex.dataseries = 2.7,
  x.lim = range(year.i), max.legend = 60,
  ylab = "Sex Ratio at Birth", xlab = "", cutoff = exp(logNmu),
  lwd.dataseries = 3, legendSurvey.posi = "topright", cex.legend = 2.5)
abline(h = c(1.08, 1.10, 1.12, 1.14), col = "grey", lwd = 0.5)
# zoom in on 2016-2050
yr.start <- 2020
yr.end <- 2050
year.i <- rep(yr.start:yr.end, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, paste(yr.start:yr.end)])
surveyplot.i <- rep(name.c, times = length(yr.start:yr.end))
surveyplot.i[surveyplot.i == "Northern Central area and Central coastal area"] <-
  "Northern Central area and\nCentral coastal area"
surveyplot.i[surveyplot.i == "Northern midlands and mountain areas"] <-
  "Northern midlands and\nmountain areas"
plot.lim <- range(r.i, na.rm = TRUE)
PlotCIbandwithDataseries(
  if.SurveyLegend = TRUE, if.sepLegendPage = TRUE,
  dataseries = r.i, SElim = plot.lim, datalim = plot.lim,
  baseSeries = "VR", Source = surveyplot.i,
  main = paste0("zoom in ", yr.start, "-", yr.end),
  alpha.dataseries = 1,
  alpha.point      = 1,
  alpha.polygon    = 0.5,
  year.t = yr.start:yr.end,
  x = year.i, select.x = TRUE, cex.dataseries = 2.5,
  x.lim = range(year.i), max.legend = 60,
  ylab = "Sex Ratio at Birth", xlab = "", cutoff = exp(logNmu),
  lwd.dataseries = 3, legendSurvey.posi = "topright", cex.legend = 1.8)
dev.off()


pdf(paste0(fig.dir, "modelEsti_SRB_", country.name, "Region_", runname, ".pdf"), 
    height = 10, width = 16)
par(cex.lab = 2.7, cex.axis = 2.5, mgp = c(6.2, 1.4, 0), mar = c(4, 8.2, 2, 1),
    cex.main = 3.2, las = 1, tcl = -1)
# layout(t(c(1, 1, 1, 2)))
year.i <- rep(years.t-0.5, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, ])
surveyplot.i <- rep(name.c, times = Tend)
plot.lim <- range(r.i, na.rm = TRUE)

yr.start <- 1980
yr.end <- 2020
year.i <- rep(yr.start:yr.end, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, paste(yr.start:yr.end)])
surveyplot.i <- rep(name.c, times = length(yr.start:yr.end))
plot.lim <- range(r.i, na.rm = TRUE)
PlotCIbandwithDataseries(
  if.SurveyLegend = TRUE, #if.sepLegendPage = TRUE,
  dataseries = r.i, SElim = plot.lim, datalim = plot.lim,
  baseSeries = "VR", Source = surveyplot.i,
  main = "",#paste0("SRB by Pakistan Province ", yr.start, "-", yr.end),
  alpha.dataseries = 1,
  alpha.point      = 1,
  alpha.polygon    = 0.5,
  year.t = yr.start:yr.end,
  x = year.i, select.x = TRUE, cex.dataseries = 2.5,
  x.lim = range(year.i), max.legend = 60,
  ylab = "Sex Ratio at Birth", xlab = "", cutoff = exp(logNmu),
  lwd.dataseries = 3, legendSurvey.posi = "topright", cex.legend = 1.5)
axis(2, at = 1.048)
dev.off()


## projection
pdf(paste0(fig.dir, "modelProj_SRB_", country.name, "Region_", runname, ".pdf"), 
    height = 20/2, width = 13)
par(cex.lab = 2.7, cex.axis = 2.5, mgp = c(6.2, 1.4, 0), mar = c(4, 8.2, 4, 1),
    cex.main = 3.2, las = 1, tcl = -1)#, mfrow = c(2, 1))
yr.start <- 2020
yr.end <- 2050
year.i <- rep(yr.start:yr.end, each = C)
r.i <- c(res.proj[["R2.jqt"]][name.c, 2, paste(yr.start:yr.end)])
surveyplot.i <- rep(name.c, times = length(yr.start:yr.end))
plot.lim <- range(r.i, na.rm = TRUE)
PlotCIbandwithDataseries(
  if.SurveyLegend = TRUE,
  dataseries = r.i, SElim = plot.lim, datalim = plot.lim,
  baseSeries = "VR", Source = surveyplot.i,
  main = paste0("SRB by Vietnam Region ", yr.start, "-", yr.end),
  alpha.dataseries = 1,
  alpha.point      = 1,
  alpha.polygon    = 0.5,
  year.t = yr.start:yr.end,
  x = year.i, select.x = TRUE, cex.dataseries = 2.5,
  x.lim = range(year.i), max.legend = 60,
  ylab = "Sex Ratio at Birth", xlab = "", cutoff = exp(logNmu),
  lwd.dataseries = 3, legendSurvey.posi = "topright", cex.legend = 1.5)
dev.off()

## the end ##

