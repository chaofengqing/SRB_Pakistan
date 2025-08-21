

## extra data for adjustment factor ##

adj.year # no adjustment before adj.year

# adjustment for selected countries with reference year after adj.year
adj.name.list <- name.c
# check if country/area names are in name.c format
adj.name.list[!is.element(adj.name.list, name.c)]

select.noadj <- (!is.element(name.i, adj.name.list) |
                   (is.element(name.i, adj.name.list) & year.i < adj.year))
mean(select.noadj) # 0.21

# create a vector of index, indAdj.i, to identify:
# 1) unique country-year with adjustment;
# 2) all the rest country-years w/o adjustment.
# assign index to be 1 for those data without adjustment
indAdj.i <- rep(NA, I)
indAdj.i[select.noadj] <- 1

# get unique combination for country-years with adjustment
country.year.df <- unique(cbind(c.i, t.i)[!select.noadj, ])
# the first row is dummy, just to incorporate when indAdj.i=1, no adjustment.
country.year.df <- rbind(c(0, 0), country.year.df)
head(country.year.df)

# total number of unique adjustment types
A <- dim(country.year.df)[1]; A
c.a <- country.year.df[, "c.i"]
t.a <- country.year.df[, "t.i"]

for (i in 1:I) {
  if (is.na(indAdj.i[i])) {
    indAdj.i[i] <- which(c.a == c.i[i] & t.a == t.i[i])
  }#end of if(is.na(indAdj.i[i]))
}#end of i loop

sum(is.na(indAdj.i)) #0

# identify all the adjustment countries
C.adj <- length(adj.name.list); C.adj # number of countries with adjustment
c.adj <- NULL
for (c in 1:C) {
  if (is.element(name.c[c], adj.name.list)) {
    c.adj <- c(c.adj, c)
  }#end of if
}#end of c loop
# double check...
identical(sort(adj.name.list), sort(name.c[c.adj]))

j.a <- rep(NA, A)
for (a in 1:A) {
  j.a[a] <- ifelse(c.a[a] == 0, 0, which(c.adj == c.a[a]))
}#end of a loop

j.i <- rep(NA, I)
for (i in 1:I) {
  j.i[i] <- which(c.adj == c.i[i])
}#end of a loop


## get indices for non-missing alpha.ct's to avoid sampling for all alpha.ct's
# instead, only sample the t.a's that have an observation
gett.jk <- matrix(NA, C.adj, A) # reduce dimension later
nt.j <- rep(NA, C.adj)

for (j in 1:C.adj) {
  
  gett <- sort(unique(t.a[c.a == c.adj[j]]))
  
  # make sure all countries have at least two time points
  if (length(gett) == 0) {
    gett <- c(which(years.t == adj.year),
              which(years.t == adj.year) + 1)
  }#end of if
  if (length(gett) == 1 & max(gett) < Tend) {
    gett <- c(gett, gett + 1)
  }#end of if
  if (length(gett) == 1 & max(gett) == Tend) {
    gett <- c(gett - 1, gett)
  }#end of if
  
  nt.j[j] <- length(gett)
  gett.jk[j, 1:nt.j[j]] <- gett
  
}#end of c loop

gett.jk <- gett.jk[, 1:max(nt.j)]

## the end ##



## the end ##

