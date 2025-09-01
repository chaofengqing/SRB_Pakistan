###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
#
# plot_newcountry_ADJ_simulation.R
# 
# This script plot simulation of SRB inflation for a new country
#
# used for which run: Main.run
#
# this script is called by any other scripts: main_output.R;
#
# this script calls other scripts: null
#
# functions called: function(2) means the function is called twice in this
# script. Those functions called in the scripts listed above are not listed.
# SamplesToUI(2)
# PlotCIbands(1)
#
# input data: null
#
# output plots in folder fig/: explore_newPAKprovince_ADJ_simulation.pdf
#
###############################################################################

## plot simulation of SRB inflation for a new country ##
L <- 25000
library(truncdist)
N.l <- rep(exp(logNmu), L)

t1 <- 1

sim.adj.tl <- matrix(0, nr = Tend+10, nc = L)
T0.l <- a.l <- D1.l <- D2.l <- D3.l <- rep(NA, L)
## starting year without uncertainty
T0.l <- rep(t1, L)

## shape parameters...
for (l in 1:L) {
  set.seed(l*19)
  a.l[l]  <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.a.c.mu, sd = pri.sigma.a.c)
  D1.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D1.c.mu, sd = pri.sigma.D1)
  D2.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D2.c.mu, sd = pri.sigma.D2)
  D3.l[l] <- rtrunc(n = 1, spec = "norm", a = 0, mean = pri.D3.c.mu, sd = pri.sigma.D3)
} # end of l loop

T1.l <- T0.l + D1.l
T2.l <- T1.l + D2.l
T3.l <- T2.l + D3.l

## delta
set.seed(j*123)
q.delta.l <- rnorm(L, delta.mu.l, sigma.delta.l)
p.delta.l <- exp(q.delta.l) / (exp(q.delta.l) + 1)
delta.l <- rbinom(n = L, size = 1, prob = p.delta.l)

for (t in 1:(Tend+10)) {
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


sim.adj.qt <- SamplesToUI(t(sim.adj.tl)) #res.sim.adj$sim.adj.qt

## simulated inflation probability
sigma.delta.l <- c(mcmc.adj[, , "sigma.delta"])
delta.mu.probscale.l <- c(mcmc.adj[, , "delta.mu.probscale"])
delta.mu.l <- exp(delta.mu.probscale.l) / (exp(delta.mu.probscale.l) + 1)
q.delta.l <- rnorm(L, delta.mu.l, sigma.delta.l)
p.delta.l <- exp(q.delta.l) / (exp(q.delta.l) + 1)
round(SamplesToUI(p.delta.l)*100, 2)

## plot the sim adj
pdf(paste0(fig.dir, "explore_newPAKprovince_ADJ_simulation.pdf"), height = 6, width = 10)
par(las = 1, mar = c(3, 4.7, 2, 1), cex.axis = 1.3, cex.lab = 1.3, mgp = c(3.5, 0.6, 0))
# par(mar = c(2, 4.6, 0.2, 1), las = 1, mgp = c(3.4, 0.7, 0), cex.axis = 1.5, cex.lab = 1.5)
plot(1, xlim = c(1965, 2010), ylim = c(0, 0.08), xaxt = "n",
     ylab = "SRB inflation", xlab = "", main = "simulated SRB inflation for a Pakistan province")
axis(1, at = mean(c(1965, 2010)), padj = 1.7, labels = "Year", tick = FALSE)
PlotCIbands(CIs.qt = sim.adj.qt, year.t = floor(years.t), alpha.line = 1,
            lwd.CI = 6)
m1 <- years.t[min(which(sim.adj.qt[3, ] > 0))]
max.inf <- round(max(sim.adj.qt[2, ]), 2)
m2 <- years.t[min(which(round(sim.adj.qt[2, ], 2) == max.inf))]
m3 <- years.t[max(which(round(sim.adj.qt[2, ], 2) == max.inf))]+1
m4 <- years.t[max(which(sim.adj.qt[2, ] > 0))] + 1
m5 <- years.t[max(which(sim.adj.qt[3, ] > 0))] + 1

mark <- c(m1, m2, m3, m4, m5) - m1
posi <- floor(m1) + mark-1
labe <- paste0("t*", ifelse(mark < 0, paste(mark),
                            ifelse(mark == 0, "", paste0("+", mark))))
labe <- expression(x[c] - 26)
axis(1, at = posi, labels = c(expression(t[0]),
                              bquote(t[0] + .(m2-m1)),
                              bquote(t[0] + .(m3-m1)),
                              bquote(t[0] + .(m4-m1)),
                              bquote(t[0] + .(m5-m1))))
axis(2, at = max(sim.adj.qt), labels = round(max(sim.adj.qt), 3))
axis(2, at = max(sim.adj.qt[2, ]), labels = round(max(sim.adj.qt[2, ]), 3))
abline(v = posi, lty = 2, col = "darkgray")
abline(h = max(sim.adj.qt), lty = 2, col = "darkgray")
abline(h = max(sim.adj.qt[2, ]), lty = 2, col = "darkgray")
legend("topright", legend = bquote(t[0] ~ ": start year of SRB inflation"),
       lty = 0, col = NA, bg = "white", cex = 1.5)
dev.off()

## the end ##
