
##############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# table_appendix_supplementary_estimates.R
# 
# This script extracts posterior samples of the adjustment parameters.
#
# used for which run: Main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: null
# 
# input data: null
#
# output data: null
#
###############################################################################

## print out results in latex table ##

## table for appendix ##
load(file = paste0(output.dir, "country.list.past.inflation.rda"))#country.list.past.inflation
load(file = paste0(output.dir, "cis_full_", runname, ".rda")) #res.full
load(file = paste0(output.dir, "cis_", runname, "_CumsumMissing.rda")) #res.missing.CI

start.year <- c(1980, 1991, 2001, 2011, 1980)
end.year   <- c(1990, 2000, 2010, 2020, 2020)
t.start <- t.end <- rep(NA, length(start.year))
for (j in 1:length(start.year)) {
  t.start[j] <- which(floor(years.t) == start.year[j])
  t.end[j] <- which(floor(years.t) == end.year[j])
}#end of j loop

K <- length(start.year)

table.colnames <- c(
  "variable",
  paste0(start.year, "-", end.year)
)

table.rownames <- paste(rep(c("Estimated female births",
                              "Expected female births",
                              "AMFB", "CMFB"), each = 2),
                        rep(c(1, 2), times = length(c("Estimated female births",
                                                      "Expected female births",
                                                      "AMFB", "CMFB"))))

table.df <- matrix(NA, nr = length(table.rownames), nc = length(table.colnames))
rownames(table.df) <- table.rownames
colnames(table.df) <- table.colnames
table.df[, "variable"] <- rep(c("Estimated female births",
                                "Expected female births",
                                "AMFB", "CMFB"), each = 2)
for (c in which(is.element(name.c, country.list.past.inflation))) {
  print(c)
  country <- name.c[c]
  
  # read in country-specific trajectory
  load(file = paste0(countryTraj.dir, "trajectory_", runname,
                     "_", name.c[c], "_full.rda")) #res.c.full
  
  # Estimated and expected female births
  for (par in c("Bf", "expBf")) {
    parB <- ifelse(par == "expBf", "Bexpf", par)
    for (k in 1:K) {
      B.l <- apply(res.c.full[[paste0(parB, ".lt")]][, t.start[k]:t.end[k]], 1, sum, na.rm = TRUE) /
        (t.end[k] - t.start[k] + 1)
      B.q <- round(SamplesToUI(B.l) / 1000, 1)
      # use medians to compute medians!!!
      B.q[2] <- round((sum(res.full[[paste0(par, ".cqt")]][c, 2, t.start[k]:t.end[k]], na.rm = TRUE) /
        (t.end[k] - t.start[k] + 1)) / 1000, 1)
      
      # B.q <- round(B.q)
      # for (q in 1:Per) {
      #   B.q[q] <- prettyNum(B.q[q], width = 3, big.mark = ",")
      # }#end of q loop
      
      parname <- ifelse(par == "Bf", "Estimated female births", "Expected female births")
      
      table.df[paste(parname, 1), paste0(start.year[k], "-", end.year[k])] <-
        paste0(B.q[2])
      table.df[paste(parname, 2), paste0(start.year[k], "-", end.year[k])] <-
        paste0("[", B.q[1], "; ", B.q[3], "]")
    }#end of k loop
  }#end of par loop
  
  
  ## AMFB & CMFB
  for (par in c("CMFB", "AMFB")) {
    for (k in 1:K) {
      B.l <- (res.missing.CI[["cumsum.missingBf.clt"]][c, , t.end[k]] -
                res.missing.CI[["cumsum.missingBf.clt"]][c, , t.start[k]-1]) /
        ifelse(par == "CMFB", 1, t.end[k] - t.start[k] + 1)
      B.q <- round(SamplesToUI(B.l) / 1000, 1)
      # use medians to compute medians!!!
      B.q[2] <- round(((median(res.missing.CI[["cumsum.missingBf.clt"]][c, , t.end[k]]) -
                   median(res.missing.CI[["cumsum.missingBf.clt"]][c, , t.start[k]-1])) /
        ifelse(par == "CMFB", 1, t.end[k] - t.start[k] + 1)) / 1000, 1)
      
      # B.q <- round(B.q)
      # for (q in 1:Per) {
      #   B.q[q] <- prettyNum(B.q[q], width = 3, big.mark = ",")
      # }#end of q loop
      
      table.df[paste(par, 1), paste0(start.year[k], "-", end.year[k])] <-
        paste0(B.q[2])
      table.df[paste(par, 2), paste0(start.year[k], "-", end.year[k])] <-
        paste0("[", B.q[1], "; ", B.q[3], "]")
    }#end of k loop
  }#end of par loop
  
}#end of c loop


table.df.save <- table.df

## create multirow column for country names
table.df <- table.df.save
table.df[, 1] <- as.character(table.df[, 1])
rle.lengths <- rle(table.df[, 1])$lengths
last.duplicate <- which(1:length(table.df[, 1]) %% 2 == 0)#the last repeated country name
table.df[-last.duplicate, 1] <- ""
table.df[last.duplicate, 1] <-
  paste0("\\multirow{", (-1)*rle.lengths, "}{3cm}{", table.df[last.duplicate, 1], "}")
table.df[, 1] <-
  paste0(c(rep(rep(c("", "\\rowcolor{lightblue1}"), each = 2),
             times = length(country.list.past.inflation) / 2), rep("", 2)), table.df[, 1])

library(xtable)
table <- xtable(table.df, 
                label = "tab_missingbirth_result",
                align = paste0("|l|p{3cm}|", paste(rep("c", dim(table.df)[2]-1), collapse = ""), "|") # the 1st entry is for rownames
)
cat("\014")
print(table, tabular.environment = 'tabular', floating = FALSE,
      include.colnames = TRUE, include.rownames = FALSE,
      hline.after = c(-1, last.duplicate), caption.placement = "top",# size = "small",
      sanitize.text.function=function(x){x} #display formula!!
)

## the end ##


