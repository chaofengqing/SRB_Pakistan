

## This script is to analyze the relation between
## starting year of SRB inflation period and TFR value
## Furthermore, to find the mean of the informative prior for
## start year parameter.

TFR.target <- 2.9 #pnas paper for Pakistan
yr.TFR <- 2030.5 #pnas paper for Pakistan
print(paste("TFR median is", TFR.target))

year.TFRtarget.j <- index.TFRtarget.j <- rep(NA, C.adj)
names(year.TFRtarget.j) <- names(index.TFRtarget.j) <- name.c[c.adj]

for (j in 1:C.adj) {
  c <- c.adj[j]
  # tfr <- as.numeric(tfr.ct[name.c[c], ])
  year.TFRtarget.j[j] <- yr.TFR#years.t[which.min(abs(tfr - TFR.target))]
  index.TFRtarget.j[j] <- which(years.t == year.TFRtarget.j[j])
}#end of c loop

year.TFRtarget.j

## the end ##

