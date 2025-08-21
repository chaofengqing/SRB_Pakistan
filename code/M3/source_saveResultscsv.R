

R.cqt <- res.proj$R2.jqt
dimnames(R.cqt)

col.name <- c("Province.Name", "Indicator", "Reference.Year", "Quantile", "Model.Estimate")

## SRB estimates
year.start <- 1980
year.end <- 2018

dataout <- NULL
for (c in 1:C) {
  for (yr in c(year.start:year.end)) {
    data.add <- data.frame(
      Province.Name = name.c[c],
      Indicator = "Sex ratio at birth",
      Reference.Year = yr,
      Quantile = c("2.5 percentile", "Median", "97.5 percentile"),
      Model.Estimate = round(R.cqt[name.c[c], , paste(yr)], 4)
    )
    dataout <- rbind(dataout, data.add)
  }#end of yr loop
}#end of c loop

write.csv(dataout, file = paste0(output.dir, "SRB_", year.start, "-", year.end, "_Pakistan_Province.csv"), row.names = FALSE)


## 20240221 save SRB projections
year.start <- 1980
year.end <- 2050

dataout <- NULL
for (c in seq(C)) {
  for (yr in c(year.start:year.end)) {
    data.add <- data.frame(
      Province.Name = name.c[c],
      Indicator = "Sex ratio at birth",
      Reference.Year = yr,
      Quantile = c("2.5 percentile", "Median", "97.5 percentile"),
      Model.Estimate = round(R.cqt[name.c[c], , paste(yr)], 4)
    )
    dataout <- rbind(dataout, data.add)
  }#end of yr loop
}#end of c loop

write.csv(dataout, file = paste0(output.dir, "SRB_", year.start, "-", year.end, "_Pakistan_Province.csv"), row.names = FALSE)

## the end ##

