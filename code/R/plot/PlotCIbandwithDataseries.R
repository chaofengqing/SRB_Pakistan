

PlotCIbandwithDataseries <- function(
  plotLegendOnly = FALSE,
  if.NewPlot = TRUE,        # if start a new plot
  if.SurveyLegend = FALSE,  # if plot legend for surveys
  if.sepLegendPage = FALSE, # if plot legend on a seperate page
  if.xlimFix = FALSE,       # if fix xlim for the plot
  if.rescaleXaxis = FALSE,  # if re-scale x-axis
  if.rescaleYaxis = FALSE,  # if re-scale y-axis
  if.LogScale = FALSE,      # if the input data is on log-scale
  unique.sources = NULL,    # need all unique sources to make sure colors match up across plots
  datalim = c(0.5, 1.2),    # ylim for data series
  SElim = c(-0.1, 5),       # ylim for SE of data series
  CI1s = NULL, CI2s = NULL, CI3s = NULL, # CIs for estimates; able to plot three CIs at most
  dataseries = NULL,   # data seris, i.e. observed data
  dataseriesSE = NULL, # standard error for data series, dataseries -/+ 1.96 * dataseriesSE
  year.t = NULL,    # x position for CI
  Source = NULL,       # type of source for data series
  baseSeries = NULL,   # specify the name of base data series
  x,
  x.lim = NULL, # manually assign xlim for plots
  select.x = seq(1, length(x)), # select data from one coutry if x is a vector from multiple countries
  cutoff = c(0, 1),
  
  ## the following arguments are about plotting features:
  nameCI1 = NULL, nameCI2 = NULL, nameCI3 = NULL, # names for CI1/2/3 for the legend
  main = NULL, ylab = NULL, xlab = NULL,
  lwd.dataseries = 1,                    # lwd for dataseries connection line
  lwd.CI1 = 5, lwd.CI2 = 4, lwd.CI3 = 4, # lwd for median/best estimate line of CIs
  cex.legend = 1.5, # font size of both lengend for CIs and data series names
  legendCI.posi = "bottomright",    # position of legend for CIs
  legendSurvey.posi = "bottomleft", # position of legend for data series, i.e. surveys
  max.legend = 30, # the maximum number of survey legends for a single column
  cex.dataseries = 1, # dot size for data series
  col.base = "black",  # col for the base data series
  pch.base = 19,       # dot style for the base data series
  colCI = c("red", "darkgreen", "limegreen"), # col for CI1/2/3 and their estimates
  col.fill = "white", #filled color for data points
  lty.dataseries = 1, #data series line type
  # level of transparency of:
  alpha.estCI      = 0.35, # CI band
  alpha.line       = 0.4,  # median/best estimate line of CIs
  alpha.polygon    = 0.08, # SE band of data series
  alpha.dataseries = 0.5,  # connection line of data series
  alpha.point      = 0.95  # dots of data series
) {
  
  ## plot function for 95% (multiple) CIs
  # with/without (multiple) data series and connection lines ##
  if.CI1s <- ifelse(!is.null(CI1s), TRUE, FALSE)
  if.CI2s <- ifelse(!is.null(CI2s), TRUE, FALSE)
  if.CI3s <- ifelse(!is.null(CI3s), TRUE, FALSE)          
  if.xlimFix      <- ifelse(!is.null(x.lim), TRUE, if.xlimFix)
  if.dataseries   <- ifelse(!is.null(dataseries), TRUE, FALSE)
  if.dataseriesSE <- ifelse(!is.null(dataseriesSE), TRUE, FALSE)
  if.baseSeries   <- ifelse(!is.null(baseSeries), TRUE, FALSE)
  if.CILegend     <- ifelse(prod(is.null(c(nameCI1, nameCI2, nameCI3))) == 1, 
                            FALSE, TRUE)
  
  # 23 unique colors for multiple surveys from data series.
  colchart <- c("darkorange1",
                "darkorchid",
                "yellowgreen",
                "seagreen3",
                "hotpink2",
                "orange", #"slateblue",
                "cornflowerblue", #"goldenrod3",
                "cyan",
                "springgreen4",
                "salmon",
                "lightgoldenrod4",
                "brown1",
                "darkblue",
                "magenta", #"darkgreen",
                "olivedrab3",
                "palegreen3",
                "peru",
                "maroon",
                "cornflowerblue",
                "chartreuse3",
                "hotpink",
                "purple",
                "plum4"
  )
  # FQ: rearrange colors to look nicer
  # number of colors is NOT enough
  colchart <- c(colchart, colchart, colchart)
  
  # point style for data series
  pch.chart <- c(21:25-2, 15, 17, 18) #c(1:19, 18:1, 1:19, 18:1)
  pch.chart <- rep(pch.chart, each = 10)
  
  # set at the beginning in case no input data series for a country when we
  # only want to plot data series.
  collegend <- pchlegend <- legendnames <- NULL
  
  if (plotLegendOnly) {
    #LA: not sure if this is the best idea....
    return() 
  }
  
  dataseries <-  as.numeric(dataseries)
  dataseriesSE <-  as.numeric(dataseriesSE)
  x <- as.numeric(x)
  
  if (
    (if.dataseries & (length(x) == 0 | length(select.x) == 0)) & 
      is.null(CI1s) & is.null(CI2s) & is.null(CI3s)
  ) {
    if (if.NewPlot) {
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = main)
    }#end of if(if.NewPlot)
  } else {
    if (is.null(x.lim)) {
      xLim <- range(x, na.rm = TRUE) # xlim when considering all x across grop
    } else {
      xLim <- x.lim
    }#end of ifelse(is.null(x.lim))
    
    x <- x[select.x]
    
    if (if.xlimFix) {
      xmin <- xLim[1]
      xmax <- xLim[2]
    } else {
      xmin <- min(x, year.t[!is.na(CI1s[1, ])], na.rm = TRUE)
      xmax <- max(x, year.t[!is.na(CI1s[1, ])], na.rm = TRUE)
    }#end of ifelse(if.xlimFix)
    
    Source <- Source[select.x]
    if (if.LogScale) {
      dataseries <- exp(dataseries[select.x])
    } else {
      dataseries <- dataseries[select.x]
    }#end of ifelse(if.LogScale)
    
    # need to construct SE.qy in the standard way no matter we want to
    # plot SE for data series or not. If if.dataseriesSE = FALSE, still
    # need to assign dataseriesSE <- NA since its default value is NULL
    if (!if.dataseriesSE) {
      dataseriesSE <- NA
    }
    
    # *.qy: row in percentiles = c(0.025, 0.5, 0.975)
    # column in observed year for a specific data series (instead of years.t)
    if (if.LogScale) {
      SE.qy <- rbind(exp(log(dataseries) - 2 * dataseriesSE[select.x]),
                     dataseries,
                     exp(log(dataseries) + 2 * dataseriesSE[select.x]))
    } else {
      SE.qy <- rbind(dataseries - 2 * dataseriesSE[select.x],
                     dataseries,
                     dataseries + 2 * dataseriesSE[select.x])
    }#end of ifelse(if.LogScale)
    
    # add max to plotting ylims when SEs are included
    if (if.dataseriesSE & if.dataseries) {
      Ylim = c(max(SElim[1], min(datalim[1], SE.qy, dataseries, cutoff,
                                 CI1s, CI2s, CI3s, na.rm = TRUE)),
               min(SElim[2], max(datalim[2], SE.qy, dataseries, cutoff,
                                 CI1s, CI2s, CI3s, na.rm = TRUE)))      
    }
    if(!(if.dataseriesSE) & if.dataseries) {
      # Ylim = c(max(datalim[1], min(cutoff, dataseries,
      #                              CI1s, CI2s, CI3s, na.rm = TRUE)),
      #          min(datalim[2], max(cutoff, dataseries,
      #                              CI1s, CI2s, CI3s, na.rm = TRUE)))
      Ylim <- datalim
    }
    if(!(if.dataseriesSE) & !(if.dataseries)) {
      Ylim = c(min(datalim[1], cutoff, CI1s, CI2s, CI3s, na.rm = TRUE),
               max(datalim[2], cutoff, CI1s, CI2s, CI3s, na.rm = TRUE))
    }
    
    
    if (if.NewPlot) {
      plot(1, type = "n", xlim = c(xmin, xmax), ylim = Ylim,
           xaxt = ifelse(if.rescaleXaxis, "n", "s"),
           yaxt = ifelse(if.rescaleYaxis, "n", "s"),
           ylab = ylab, xlab = xlab, main = main)
    }#end of if(if.NewPlot)
    
    abline(h = cutoff)
    
    
    sources <- unique(Source)
    
    if (!is.null(unique.sources)) {
      colchart <- colchart[1:length(unique.sources)]
      pch.chart <- pch.chart[1:length(unique.sources)]
      names(colchart) <- names(pch.chart) <- unique.sources
    } else {
      colchart <- colchart[1:length(sources)]
      pch.chart <- pch.chart[1:length(sources)]
      names(colchart) <- names(pch.chart) <- sources
    }#end of ifelse (!is.null(unique.sources))
    
    if (if.dataseries | if.dataseriesSE) {
      sources <- as.character(sources[!is.na(sources)])
      
      for (sur in sources) {
        select.i <- which(Source == sur)
        # setup color for each survey
        col.sur <- ifelse(if.baseSeries & is.element(sur, baseSeries),
                          col.base, colchart[sur])
        
        PlotCIbands(if.CI = if.dataseriesSE, if.line = if.dataseries,
                    CIs.qt = SE.qy[, select.i], year.t = x[select.i],
                    col = col.sur, alpha.CI = alpha.polygon,
                    alpha.line = alpha.dataseries,
                    lty.line = lty.dataseries,
                    lwd.CI = lwd.dataseries)
        # alpha.polygon < alpha.CI, SE is lighter than CIs from CIs1/2/3.        
      }#end of sources loop
      
    }#end of if (if.dataseries | if.dataseriesSE)
    
    ############################
    ## plot CIs and estimates ##
    PlotCIbands(if.CI = if.CI1s, if.line = if.CI1s,
                alpha.line = alpha.point, alpha.CI = alpha.estCI,
                year.t = year.t, CIs.qt = CI1s, col = colCI[1],
                lwd.CI = lwd.CI1)
    
    PlotCIbands(if.CI = if.CI2s, if.line = if.CI2s,
                alpha.line = alpha.point, alpha.CI = alpha.estCI,
                year.t = year.t, CIs.qt = CI2s, col = colCI[2],
                lwd.CI = lwd.CI2)
    
    PlotCIbands(if.CI = if.CI3s, if.line = if.CI3s,
                alpha.line = alpha.point, alpha.CI = alpha.estCI,
                year.t = year.t, CIs.qt = CI3s, col = colCI[3],
                lwd.CI = lwd.CI3)    
    
    #############################
    ## plot data series points ##
    for (sur in sources) {
      col.sur <- adjustcolor(ifelse(if.baseSeries & is.element(sur, baseSeries),
                                    col.base, colchart[sur]),
                             alpha.f = alpha.point)
      pch.sur <- ifelse(if.baseSeries & is.element(sur, baseSeries),
                        pch.base, pch.chart[sur])
      
      select.i <- which(Source == sur)
      order.x <- select.i[order(x[select.i])]
      
      if (if.dataseries) {
        points(dataseries[order.x] ~ x[order.x], lwd = cex.dataseries,
               col = col.sur, bg = col.fill, pch = pch.sur, cex = cex.dataseries)
      }#end of if (if.dataseries)
      
      legendnames <- c(legendnames, sur)
      collegend <- c(collegend, col.sur)
      pchlegend <- c(pchlegend, pch.sur)
    }#end of sur loop
    
  }#end of if (length(x)!=0 & length(select.x)>0) loop
  
  #################
  ## plot legend ##
  if (if.CILegend) {
    legend(legendCI.posi, legend = c(nameCI1, nameCI2, nameCI3),
           col = colCI, lwd = 5, pch = -1, lty = c(1, 1), cex = cex.legend)
  }#end of if (if.CILegend)
  
  if(if.SurveyLegend) {
    if (if.sepLegendPage) {
      par(mar = c(1, 0.5, 1, 0.5))
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
           bty = "n")
    }
    if (length(legendnames) > 0) {
      if (length(legendnames) >= max.legend & if.sepLegendPage) {
        legend("topleft", legend = legendnames[1:max.legend],
               col = collegend[1:max.legend], pch = pchlegend[1:max.legend],
               cex = cex.legend, lwd = 2.3, pt.cex = 1.8, lty = 1, bty = "n")
        legend("topright",
               legend = legendnames[(max.legend + 1):length(legendnames)],
               col = collegend[(max.legend + 1):length(legendnames)],
               cex = cex.legend, lwd = 2.3, pt.cex = 1.8, lty = 1, bty = "n",
               pch = pchlegend[(max.legend + 1):length(legendnames)])
      } else {
        legend(ifelse(if.sepLegendPage, "left", legendSurvey.posi),
               legend = legendnames, col = collegend, cex = cex.legend,
               lwd = 2.3, pt.cex = 1.8, lty = 1, pch = pchlegend, bty = "o")
      }#end of ifelse(length(legendnames) >= 20 & if.sepLegendPage)
    }#end of if (length(legendnames) > 0)    
  }#end of if.SurveyLegend  
  
}#end of PlotCIbandwithDataseries() function

