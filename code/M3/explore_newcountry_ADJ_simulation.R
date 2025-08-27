###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 28 Aug 2025
#
# explore_newcountry_ADJ_simulation.R
# 
# This script is to simulate SRB inflation for new country to check model performance.
#
# used for which run: main.run
#
# this script is called by any other scripts: main_output.R
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(6)
# 
# 
# input data: null
#
# output data: fig/explore_newcountry_ADJ_simulation.pdf
###############################################################################

######################################
## simulate adjustment factor first...
library(truncdist)

sim.adj.tl    <- matrix(0, nr = Tend, nc = L)
dimnames(sim.adj.tl)[[1]] <- years.t

T0.l <- a.l <- D1.l <- D2.l <- D3.l <- rep(NA, L)
## starting year without uncertainty
T0.l <- rep(36, L) #runif(n = L, pri.T0.c.lower, pri.T0.c.upper)

## shape parameters...
for (l in 1:L) {
  set.seed(l*19)
  a.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.a.c.mu, sd = pri.sigma.a.c)
  D1.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D1.c.mu, sd = pri.sigma.D1)
  D2.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D2.c.mu, sd = pri.sigma.D2)
  D3.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D3.c.mu, sd = pri.sigma.D3)
}#end of l loop

T1.l <- T0.l + D1.l
T2.l <- T1.l + D2.l
T3.l <- T2.l + D3.l

## delta
set.seed(j*123)
q.delta.l <- rnorm(L, delta.mu.l, sigma.delta.l)
p.delta.l <- exp(q.delta.l) / (exp(q.delta.l) + 1)
delta.l <- rbinom(n = L, size = 1, prob = p.delta.l)

for (t in 1:Tend) {
  for (l in 1:L) {
    if (t <= T0.l[l] | t > T3.l[l]) {
      sim.adj.tl[t, l] <- 0
    } else if (t > T0.l[l] & t <= T1.l[l]) {
      sim.adj.tl[t, l] <- (a.l[l] / D1.l[l]) * (t - T0.l[l])
    } else if (t > T1.l[l] & t <= T2.l[l]) {
      sim.adj.tl[t, l] <- a.l[l]
    } else {
      sim.adj.tl[t, l] <- a.l[l] - (a.l[l] / D3.l[l]) * (t - T2.l[l])
    } # end of ifelse
  } # end of l loop
  
  sim.adj.tl[t, ] <- sim.adj.tl[t, ] * delta.l
} # end of t loop

sim.adj.qt <- SamplesToUI(t(sim.adj.tl))

## plot the sim adj
pdf(paste0(fig.dir, "explore_newcountry_ADJ_simulation.pdf"), height = 4, width = 8.5)
par(las = 1, mar = c(4, 4, 1, 1), cex.axis = 1.3, cex.lab = 1.3, mgp = c(2.9, 0.6, 0))
plot(1, xlim = c(2000, 2050),
     ylim = c(0, 0.15), #xaxt = "n",
     ylab = "SRB inflation", xlab = "Year", main = "")
PlotCIbands(CIs.qt = sim.adj.qt, year.t = years.t, alpha.line = 1,
            lwd.CI = 6)
m1 <- years.t[min(which(sim.adj.qt[3, ] > 0))]
m2 <- years.t[200]
m3 <- years.t[which.max(sim.adj.qt[2, ])]
m4 <- years.t[max(which(sim.adj.qt[2, ] > 0))] + 1
m5 <- years.t[max(which(sim.adj.qt[3, ] > 0))] + 1

mark <- c(m1, m2, m3, m4, m5) - m2
posi <- floor(m2) + mark
labe <- paste0("t*", ifelse(mark < 0, paste(mark),
                            ifelse(mark == 0, "", paste0("+", mark))))
labe <- expression(x[c] - 26)
axis(1, at = posi, labels = c(expression(x[c] - 26),
                              expression(x[c]),
                              expression(x[c] + 17),
                              expression(x[c] + 38),
                              expression(x[c] + 77)))
axis(2, at = max(sim.adj.qt), labels = round(max(sim.adj.qt), 2))
axis(2, at = max(sim.adj.qt[2, ]), labels = round(max(sim.adj.qt[2, ]), 2))
abline(v = posi, lty = 2, col = "darkgray")
abline(h = max(sim.adj.qt), lty = 2, col = "darkgray")
abline(h = max(sim.adj.qt[2, ]), lty = 2, col = "darkgray")
dev.off()

