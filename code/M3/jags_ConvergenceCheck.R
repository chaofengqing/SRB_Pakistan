

###############################################################################
# A systematic assessment of national, regional and global sex ratios of
# infant, child and under-five mortality and identification of countries with
# outlying levels
#
# Code constructed by: Leontine ALKEMA and Fengqing CHAO
# Code last revised by: Fengqing CHAO on 25 Feb 2014
# 
# jags_ConvergenceCheck.R
# 
# This script checks the convergence of JAGS model output, i.e. mcmc.array. It
# produces trace plots for (selected) parameters, and compute median, CI, R hat
#
# used for which run: Main.run; Validation.run; Excl.run
#
# this script is called by any other scripts: main*_output.R
#
# this script calls other scripts: null
# functions called:                null
# 
# input data: data/output/runname/mcmc.array_runname.rda
#
# output data: data/output/runname/*_postinfo.csv - R hat; post sample median, CI
# output plot: fig/runname/convergence/*
#
###############################################################################


################
## trace plot ##

## hyper para ##
pdf(paste0(convergeplot.dir, "trace_", runname, ".pdf"))
for (par in hyper.para) {
  PlotTrace(parname = par, mcmc.array = mcmc.array)
}#end of par loop
dev.off()

## starting year of adjustment period ##
pdf(paste0(convergeplot.dir, "trace_ADJstartyear_", runname, ".pdf"))
for (j in 1:C.adj) {
  par <- paste0("T0.j[", j, "]")
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste(par, name.c[c.adj[j]]))
}#end of par loop
dev.off()

## ending year of adjustment period ##
pdf(paste0(convergeplot.dir, "trace_ADJendyear_", runname, ".pdf"))
for (j in 1:C.adj) {
  par <- paste0("T3.j[", j, "]")
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste(par, name.c[c.adj[j]]))
}#end of par loop
dev.off()

## inflation probability ##
pdf(paste0(convergeplot.dir, "trace_prob_delta_", runname, ".pdf"))
for (j in 1:C.adj) {
  par <- paste0("p.delta.j[", j, "]")
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste(par, name.c[c.adj[j]]))
}#end of par loop
dev.off()

## inflation probability ##
pdf(paste0(convergeplot.dir, "trace_delta_", runname, ".pdf"))
for (j in 1:C.adj) {
  par <- paste0("delta.j[", j, "]")
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste(par, name.c[c.adj[j]]))
}#end of par loop
dev.off()


###############################################
## compute R hat and post sample information ##
select.para <- setdiff(dimnames(mcmc.array)[[3]], para.alpha)
post.full <- getPostInfo(mcmc.array = mcmc.array[, , select.para])
write.csv(post.full, paste0(output.dir, runname, "_postinfo_exclude-alpha_jt.csv")) #checking only

inflated.alpha <- NULL
for (j in 1:C.adj) {
  yr.start <- ceiling(post.full[paste0("T0.j[", j, "]"), "50 percentile"]) + 1
  yr.end <- floor(post.full[paste0("T3.j[", j, "]"), "50 percentile"]) - 1
  inflated.alpha <- c(inflated.alpha, paste0("alpha.jt[", j, ",", yr.start:yr.end, "]"))
}#end of j loop
inflated.alpha <- intersect(inflated.alpha, para.alpha)
post.alpha <- getPostInfo(mcmc.array = mcmc.array[, , inflated.alpha])
write.csv(post.alpha, paste0(output.dir, runname, "_postinfo_alpha_jt.csv")) #checking only


## trace plot for para with big Rhat values ##
par.select <- rownames(post.full)[1:25]
pdf(paste0(convergeplot.dir, "trace_problematic_", runname, ".pdf"))
for (par in par.select) {
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste0(par, " Rhat=", round(post.full[par, "Rhat (descending)"], 2)))
}#end of par loop
dev.off()

par.select <- rownames(post.alpha)[1:25]#[post.alpha[, "Rhat (descending)"] > 1.1]
pdf(paste0(convergeplot.dir, "trace_problematic_alpha_", runname, ".pdf"))
for (par in par.select) {
  PlotTrace(parname = par, mcmc.array = mcmc.array,
            main = paste0(par, " Rhat=", round(post.alpha[par, "Rhat (descending)"], 2)))
}#end of par loop
dev.off()


#################################################################
## further checking of alpha.ct convergence when delta is zero ##
pdf(paste0(convergeplot.dir, "alpha_density_byDelta.pdf"))
par(mfrow = c(3, 3), mar = c(2, 2, 2, 1), mgp = c(2, 0.5, 0), cex.main = 1)
for (j in 1:C.adj) {
  delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
  l.one.select <- which(delta.l == 1); length(l.one.select)
  l.zero.select <- which(delta.l == 0); length(l.zero.select)
  
  yr.start <- ceiling(post.full[paste0("T0.j[", j, "]"), "50 percentile"]) + 1
  yr.end <- floor(post.full[paste0("T3.j[", j, "]"), "50 percentile"]) - 1
  yr.end <- ifelse(yr.end > Tend, Tend, yr.end)
  if (length(l.one.select) < L) {
    for (t in yr.start:yr.end) {
      adj.nodelta <- c(mcmc.array[, , paste0("alpha.jt[", j, ",", t, "]")])
      
      adj0 <- adj.nodelta[l.zero.select]
      adj1 <- adj.nodelta[l.one.select]
      
      plot(density(adj.nodelta), type = "l", xlab = "", lwd = 2,
           ylim = range(density(adj0)$y, density(adj1)$y, density(adj.nodelta)$y),
           main = paste("alpha", name.c[c.adj[j]], "yr", years.t[t]-0.5))
      lines(x = density(adj0)$x, y = density(adj0)$y, col = 4)
      lines(x = density(adj1)$x, y = density(adj1)$y, col = 2)
      abline(v = c(median(adj.nodelta), median(adj0), median(adj1)), col = c(1, 4, 2))
      legend("topright", lwd = 3, col = c(1, 4, 2),
             c("all (100%)",
               paste0("delta=0 (", round(mean(delta.l == 0)*100), "%)"),
               paste0("delta=1 (", round(mean(delta.l == 1)*100), "%)")))
    }#end of t loop
  }#end of if(l.one.select < L)
}#end of j loop
dev.off()

## The End! ##
