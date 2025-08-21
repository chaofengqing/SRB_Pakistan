

text.cex <- 0.9
boundary.lwd <- 0.5
NoDataColor <- "gray" # color for polygons with no data
boundary.color <- "slategray4"#"black" # color for country boundaries
background.color <- "white" # color for oceans, seas and lakes


library("sp")
library("RColorBrewer")
library("ggplot2")
library("rgdal")
library("scales")
require("rgeos",character.only=TRUE)

country.name <- "Pakistan"

# Read in the state-level polygon shapefile (no antarctica)
map1 <- readOGR("data/gadm36_PAK_shp/", "gadm36_PAK_1")
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(map1) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
map1.robin <- spTransform(map1, CRS("+proj=robin")) # (requires rgdal package)

map1.name <- as.character(map1.robin$NAME_1)


pdf(file = paste0(fig.dir, "map_", country.name, "_region_SRB.pdf"), width = 5)
par(mar = c(0, 0, 2, 0))
for (yr in seq(1980, 2050, 5)) {
  print(yr)
  print(round(quantile(res.proj$R2.jqt[, 2, paste(yr)], seq(0, 1, length.out = 9), na.rm = TRUE), 4))
  start <- round(floor(min(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
  end <- round(ceiling(max(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
  cutoff <- seq(start, end, length.out = 5)
  myPalette <- brewer.pal(4, "Reds")
  
  if (end > start) {
    # Read in the state-level polygon shapefile (no antarctica)
    map1 <- readOGR("data/gadm36_PAK_shp/", "gadm36_PAK_1")
    # convert to Robinson projection (the projection preferred by UN Cartography)
    proj4string(map1) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
    map1.robin <- spTransform(map1, CRS("+proj=robin")) # (requires rgdal package)
    
    srb.s <- rep(NA, length(map1.name))
    names(srb.s) <- map1.name
    srb.s[map1.name] <- res.proj$R2.jqt[ExternalFullPakistanProvinceName(map1.name), 2, paste(yr)]
    map1.robin$SRB <- srb.s[map1.name]
    
    # Find the center of each region and label lat and lon of centers
    centroids <- gCentroid(map1.robin, byid=TRUE)
    # select.names <- is.element(map1.robin$NAME_1, name.show.result)
    centroidLons <- coordinates(centroids)[, 1]
    centroidLats <- coordinates(centroids)[, 2]
    names(centroidLons) <- names(centroidLats) <- map1.name
    
    code.plot <- NULL
    for (j in 1:C.adj) {
      add.code <- map1.robin$NAME_1[j]
      code.plot <- c(code.plot, add.code)
    }#end of j loop
    
    cutoff.labs <- as.character(cut(cutoff, cutoff, include.lowest = TRUE))[-1]
    cutoff.labs <-
      ifelse(substr(cutoff.labs, 6, 6) == ",",
             paste0(substr(cutoff.labs, 1, 5), "0", substr(cutoff.labs, 6, nchar(cutoff.labs))), cutoff.labs)
    cutoff.labs <-
      ifelse(substr(cutoff.labs, 12, 12) == "]",
             paste0(substr(cutoff.labs, 1, 11), "0", substr(cutoff.labs, 12, nchar(cutoff.labs))), cutoff.labs)
    cutoff.labs <- gsub(",", "; ", cutoff.labs, fixed = TRUE)
    
    
    # colouring the districts with range of colours
    col_no <- as.numeric(cut(map1.robin$SRB, cutoff, include.lowest = TRUE))
    names(col_no) <- map1.robin$NAME_1
    col_no[is.na(col_no)] <- length(cutoff)
    map1.robin$col_no <- col_no
    
    ## plot the map ##
    plot(map1.robin,border=1,bg=background.color, col = myPalette[map1.robin$col_no],
         lwd = boundary.lwd, main = paste0("Sex Ratio at Birth Projection (", yr, ")"))#, ylim = c(10^6*2.967999, 10^6*2.968)
    # title(paste0("Sex Ratio at Birth Projection (", yr, ")"), cex.main = 1, outer = TRUE)
    text(centroidLons, centroidLats, labels = code.plot, cex = 1)
    legend(legend = cutoff.labs, bg = "white", box.col = "white",
           fill = myPalette, "bottomleft", cex = 1)
    library(prettymapr)
    addscalebar(label.cex = 1, pos = "bottomright")
    addnortharrow(scale = 0.9)
    
    legend("topleft", map1.name, pch = paste(1:length(map1.name)), lty = 0)
  }#end of if(end > start)
}#end of yr loop
dev.off()



####################
## only estimates ##
plot.yr <- c(2000, 2015)
print(round(quantile(res.proj$R2.jqt[, 2, paste(plot.yr)], seq(0, 1, length.out = 9), na.rm = TRUE), 4))
start <- 1.045#round(floor(min(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
end <- 1.08#round(ceiling(max(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
cutoff <- c(1.045, 1.050, 1.055, 1.060, 1.065, 1.12)#seq(start, end, length.out = out.number)
out.number <- length(cutoff)
myPalette <- brewer.pal(out.number-1, "Reds")

cutoff.labs <- as.character(cut(cutoff, cutoff, include.lowest = TRUE))[-1]
cutoff.labs <-
  ifelse(substr(cutoff.labs, 6, 6) == ",",
         paste0(substr(cutoff.labs, 1, 5), "0", substr(cutoff.labs, 6, nchar(cutoff.labs))), cutoff.labs)
cutoff.labs <-
  ifelse(substr(cutoff.labs, 12, 12) == "]",
         paste0(substr(cutoff.labs, 1, 11), "0", substr(cutoff.labs, 12, nchar(cutoff.labs))), cutoff.labs)
cutoff.labs <- gsub(",", "; ", cutoff.labs, fixed = TRUE); cutoff.labs

pdf(file = paste0(fig.dir, "map_", country.name, "_region_SRB_esti.pdf"), width = 8)
par(mar = c(0, 0, 2, 0), mfrow = c(1, 2), oma = c(0, 0, 0, 0))
for (yr in plot.yr) {
  print(yr)
  
  # Read in the state-level polygon shapefile (no antarctica)
  map1 <- readOGR("data/gadm36_PAK_shp/", "gadm36_PAK_1")
  # convert to Robinson projection (the projection preferred by UN Cartography)
  proj4string(map1) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
  map1.robin <- spTransform(map1, CRS("+proj=robin")) # (requires rgdal package)
  
  srb.s <- rep(NA, length(map1.name))
  names(srb.s) <- map1.name
  srb.s[map1.name] <- res.proj$R2.jqt[ExternalFullPakistanProvinceName(map1.name), 2, paste(yr)]
  map1.robin$SRB <- srb.s[map1.name]
  
  # Find the center of each region and label lat and lon of centers
  centroids <- gCentroid(map1.robin, byid=TRUE)
  # select.names <- is.element(map1.robin$NAME_1, name.show.result)
  centroidLons <- coordinates(centroids)[, 1]
  centroidLats <- coordinates(centroids)[, 2]
  names(centroidLons) <- names(centroidLats) <- map1.name
  
  code.plot <- NULL
  for (j in 1:C.adj) {
    add.code <- which(map1.robin$NAME_1[j] == map1.name)
    code.plot <- c(code.plot, add.code)
  }#end of j loop
  
  
  # colouring the districts with range of colours
  col_no <- as.numeric(cut(map1.robin$SRB, cutoff, include.lowest = TRUE))
  names(col_no) <- map1.robin$NAME_1
  col_no[is.na(col_no)] <- length(cutoff)
  col_no["F.A.T.A."] <- NA ## DO NOT PLOT FATA RESULTS!
  map1.robin$col_no <- col_no
  
  
  ## plot the map ##
  plot(map1.robin,border=1,bg=background.color, col = myPalette[map1.robin$col_no],
       lwd = boundary.lwd, main = paste0("Sex Ratio at Birth, Pakistan (", yr, ")"))#, ylim = c(10^6*2.967999, 10^6*2.968)
  # title(paste0("Sex Ratio at Birth Projection (", yr, ")"), cex.main = 1, outer = TRUE)
  text(centroidLons, centroidLats, labels = code.plot, cex = 1)
  if (yr == 2000) {
    legend(legend = c(cutoff.labs, "Results not shown"), bg = "white", box.col = "white",
           fill = c(myPalette, "white"), "bottomleft", cex = 1)
    legend("topleft", ExternalFullPakistanProvinceName(map1.name),
           pch = paste(1:length(map1.name)), lty = 0, bty = "n")
  }#end of if(yr == 2000)
  if (yr == 2015) {
    library(prettymapr)
    addscalebar(label.cex = 1, pos = "bottomright")
    addnortharrow(scale = 0.7)
  }#end of if(yr == 2015)
}#end of yr loop
dev.off()



######################
## only projections ##
plot.yr <- c(2020, 2030, 2040)
print(round(quantile(res.proj$R2.jqt[, 2, paste(plot.yr)], seq(0, 1, length.out = 9), na.rm = TRUE), 4))
start <- 1.05#round(floor(min(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
end <- 1.08#round(ceiling(max(res.proj$R2.jqt[, 2, paste(yr)], na.rm = TRUE)*1000)/1000, 3)
cutoff <- c(1.050, 1.055, 1.060, 1.065, 1.070, 1.075, 1.080)#seq(start, end, length.out = out.number)
out.number <- length(cutoff)
myPalette <- brewer.pal(out.number-1, "Reds")

cutoff.labs <- as.character(cut(cutoff, cutoff, include.lowest = TRUE))[-1]
cutoff.labs <-
  ifelse(substr(cutoff.labs, 5, 5) == ",",
         paste0(substr(cutoff.labs, 1, 4), "0", substr(cutoff.labs, 5, nchar(cutoff.labs))), cutoff.labs)
cutoff.labs <-
  ifelse(substr(cutoff.labs, 10, 10) == "]",
         paste0(substr(cutoff.labs, 1, 9), "0", substr(cutoff.labs, 10, nchar(cutoff.labs))), cutoff.labs)
cutoff.labs <- gsub(",", "; ", cutoff.labs, fixed = TRUE); cutoff.labs

pdf(file = paste0(fig.dir, "map_", country.name, "_region_SRB_proj.pdf"), width = 10)
par(mar = c(0, 0, 2, 0), mfrow = c(1, 3), oma = c(0, 0, 0, 0))
for (yr in plot.yr) {
  print(yr)
  
  # Read in the state-level polygon shapefile (no antarctica)
  map1 <- readOGR("data/gadm36_PAK_shp/", "gadm36_PAK_1")
  # convert to Robinson projection (the projection preferred by UN Cartography)
  proj4string(map1) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
  map1.robin <- spTransform(map1, CRS("+proj=robin")) # (requires rgdal package)
  
  srb.s <- rep(NA, length(map1.name))
  names(srb.s) <- map1.name
  srb.s[map1.name] <- res.proj$R2.jqt[ExternalFullPakistanProvinceName(map1.name), 2, paste(yr)]
  map1.robin$SRB <- srb.s[map1.name]
  
  # Find the center of each region and label lat and lon of centers
  centroids <- gCentroid(map1.robin, byid=TRUE)
  # select.names <- is.element(map1.robin$NAME_1, name.show.result)
  centroidLons <- coordinates(centroids)[, 1]
  centroidLats <- coordinates(centroids)[, 2]
  names(centroidLons) <- names(centroidLats) <- map1.name
  
  code.plot <- NULL
  for (j in 1:C.adj) {
    add.code <- map1.name[j]
    code.plot <- c(code.plot, add.code)
  }#end of j loop
  
  
  # colouring the districts with range of colours
  col_no <- as.numeric(cut(map1.robin$SRB, cutoff, include.lowest = TRUE))
  names(col_no) <- map1.robin$NAME_1
  col_no[is.na(col_no)] <- length(cutoff)
  map1.robin$col_no <- col_no
  
  ## plot the map ##
  plot(map1.robin,border=1,bg=background.color, col = myPalette[map1.robin$col_no],
       lwd = boundary.lwd, main = paste0("Sex Ratio at Birth Projection (", yr, ")"))#, ylim = c(10^6*2.967999, 10^6*2.968)
  # title(paste0("Sex Ratio at Birth Projection (", yr, ")"), cex.main = 1, outer = TRUE)
  text(centroidLons, centroidLats, labels = code.plot, cex = 1)
  legend(legend = cutoff.labs, bg = "white", box.col = "white",
         fill = myPalette, "bottomleft", cex = 1.3)
  if (yr == 2040) {
    library(prettymapr)
    addscalebar(label.cex = 1.3, pos = "bottomright")
    addnortharrow(scale = 0.7)
  }#end of if
}#end of yr loop
dev.off()

## the end ##


