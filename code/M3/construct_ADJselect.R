


# note: here l refers to posterior sample
adj.jtl <- adj.nodelta.jtl <- array(0, c(C.adj, Tend, L))
dimnames(adj.jtl)[[1]] <- dimnames(adj.nodelta.jtl)[[1]] <- name.c[c.adj]
dimnames(adj.jtl)[[2]] <- dimnames(adj.nodelta.jtl)[[2]] <- years.t

for (j in 1:C.adj) {
  cat(j, "/", C.adj, "\n")
  for (t in 1:Tend) {
    adj.parname <- paste0("alpha.jt[", j, ",", t, "]")
    adj.nodelta.jtl[j, t, ] <- c(mcmc.array[, , adj.parname])
    delta.l <- c(mcmc.array[, , paste0("delta.j[", j, "]")])
    adj.jtl[j, t, ] <- adj.nodelta.jtl[j, t, ] * delta.l
  }#end of t loop
}#end of j loop

selectADJ <- list(adj.jtl = adj.jtl,
                  adj.nodelta.jtl = adj.nodelta.jtl)

## the end ##

