

## paper results ##
yr <- 2016
sort(res.proj$R2.jqt[, 2, paste(yr)])
round(res.proj$R2.jqt["Province 5", , paste(yr)], 3)
round(res.proj$R2.jqt["Province 2", , paste(yr)], 3)

yr <- 1980
sort(res.proj$R2.jqt[, 2, paste(yr)])
round(res.proj$R2.jqt["Province 7", , paste(yr)], 3)
round(res.proj$R2.jqt["Province 4", , paste(yr)], 3)

## read in prince and district info
pro.info <- read.csv(paste0(aux.data.dir, "district_DHScode.csv"),
                     header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
for (p in 1:7) {
  print(paste("Province", p))
  msg <- paste(pro.info[pro.info$Province.Name == p, "District.Name"], collapse = "; ")
  print(paste(sum(pro.info$Province.Name == p), "Districts"))
  print(msg)
}#end of p loop
range(pro.info$District.Code)


adj.info <- read.csv(paste0(output.dir, runname,
                            "_postinfo_exclude-alpha_jt.csv"), row.names = 1)

for (j in 1:C.adj) {
  t.index <- as.numeric(floor(adj.info[paste0("T0.j[", j, "]"), c("X50.percentile",
                                                         "X2.5.percentile",
                                                         "X97.5.percentile")]))
  t.index[t.index < 1] <- 1
  start.yr <- years.t[t.index]
  cat("Province", c.adj[j], floor(start.yr), "\n")
}
