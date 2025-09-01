
pdf(paste0(fig.dir, "CIs_SRB_Nepal_province_", runname, "_bmj.pdf"),
    height = 50, width = 25)
text.cex <- 4.5
par(cex.lab = text.cex, cex.axis = text.cex, mgp = c(11, 3, 0), mar = c(5, 14, 6, 2),
    cex.main = 6, las = 1, tcl = -1, mfrow = c(7, 1))
plot.range <- range(res.proj$a.jqt, res.proj[["R2.jqt"]], na.rm = TRUE)
for (country in name.c) {
  c <- which(country == name.c)
  # c.select <- which(name.i == name.c[c])
  # # sort by survey date to get nicer legend
  # select <- c.select[order(surveyplot.i[c.select])]
  
  R.qt <- res.proj[["R2.jqt"]][country, , ]
  
  a.qt <- res.proj$a.jqt[country, , ]
  par(las = 1)
  PlotCIbandwithDataseries(
    if.LogScale = TRUE, if.SurveyLegend = TRUE,
    year.t = floor(years.t),
    CI1s = R.qt, #nameCI1 = ifelse(c == 1, "Estimated SRB", NA),
    CI2s = a.qt, #nameCI2 = ifelse(c == 1, "National SRB baseline", NA),
    x = floor(years.t), select.x = TRUE, x.lim = c(2010, 2050),
    SElim = plot.range, datalim = plot.range,
    main = country, #alpha.dataseries = 0.8, cex.dataseries = 1.5,
    ylab = "Sex Ratio at Birth", xlab = "Year", cutoff = exp(logNmu),
    lwd.CI1 = 15, lwd.CI2 = 10, lwd.dataseries = 3, colCI = c("red", "darkgreen"),
    cex.legend = text.cex, legendCI.posi = "topleft", legendSurvey.posi = "bottomright")
  if (c == 1) {
    legend("topright", c("Provincial SRB", "National SRB baseline"),
           col = c("red", "darkgreen"), lwd = 10, cex = text.cex)
  }#end of if(c == 1)
  axis(1, at = 2016, labels = 2016)
  abline(v = 2016)
  
  max.yr <- years.t[which.max(R.qt[2, ])]-0.5
  abline(v = max.yr, lty = 3, col = "red3")
  text(pos = 4, x = max.yr, y = 1.125, labels = max.yr, col = "red3", cex = text.cex)
}#end of c loop
dev.off()

for (country in name.c) {
  R.qt <- res.proj[["R2.jqt"]][country, , ]
  max.yr <- years.t[which.max(R.qt[2, ])]-0.5
  cat(country, max.yr, "\t")
  print(round(R.qt[, paste(max.yr)], 3))
}
