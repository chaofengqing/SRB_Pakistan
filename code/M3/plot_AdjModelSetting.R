

## illustration plot for adjustment model setting ##


# workdir <- "D:/Dropbox/SRB/fig/" #laptop
# workdir <- "/Users/FQ/Dropbox/SRB/fig/" #iMac

plot.dir <- "/Users/chaof/work/SRB_Pakistan/fig/M3/"
# install.packages("shape", dependencies = TRUE)
library(shape)

t0 <- 0
t1 <- 30
t2 <- 70
t3 <- 130
t4 <- 170
t.max <- 200

a1 <- 20
a2 <- 80
a.max <- 90
a.arrow1 <- 15
a.arrow2 <- 85
a.text1 <- a.arrow1 - 5
a.text2 <- a.arrow2 + 6

lwd.adj <- 10
lwd.arrow <- 2

col.adj <- "slategray4"
col.arrow <- "darkgray"

cex.symbol <- 2.2

arrow.length <- 0.5
arrow.width <- 0.3

########################################################################
########################################################################
pdf(paste0(plot.dir, "AdjModelSetting_tech_PAK.pdf"))
par(mar = c(4, 5, 3, 1), las = 1, cex.main = 1.5, cex.lab = 1.5, cex.axis = cex.symbol,
    font.lab = 2)
plot(y = 1:a.max, x = seq(2, t.max, t.max / a.max),
     type = "n", xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = expression(alpha[paste(p,',',t)]), main = "Sex ratio transition for a Pakistan province")
axis(1, at = c(t1, t2, t3, t4), cex = 2.2,
     labels = c(expression(t[paste(0,',',p)]),
                expression(t[paste(1,',',p)]),
                expression(t[paste(2,',',p)]),
                expression(t[paste(3,',',p)])),
     col = 'black', col.axis = 1, col.ticks = 'black',
     font = 4, family = 'serif')
axis(2, at = c(a1, a2), cex = cex.symbol,
     labels = c(0, expression(xi[p])),
     col = 'black', col.axis = 1, col.ticks = 'black',
     font = 4, family = 'serif')

abline(h = a1)
abline(v = c(t1, t2, t3, t4), lty = 2)
abline(h = a2, lty = 2)
segments(x0 = t0, y0 = a1, x1 = t1, y1 = a1, lwd = lwd.adj, col = col.adj)
segments(x0 = t1, y0 = a1, x1 = t2, y1 = a2, lwd = lwd.adj, col = col.adj)
segments(x0 = t2, y0 = a2, x1 = t3, y1 = a2, lwd = lwd.adj, col = col.adj)
segments(x0 = t3, y0 = a2, x1 = t4, y1 = a1, lwd = lwd.adj, col = col.adj)
segments(x0 = t4, y0 = a1, x1 = t.max, y1 = a1, lwd = lwd.adj, col = col.adj)

segments(x0 = t1, y0 = a.arrow1, x1 = t4, y1 = a.arrow1,
         lwd = lwd.arrow, col = col.arrow)

Arrowhead(x0 = t1, y0 = a.arrow1, angle = 180, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)
Arrowhead(x0 = t2, y0 = a.arrow1, angle = 0, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)

Arrowhead(x0 = t2, y0 = a.arrow1, angle = 180, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)
Arrowhead(x0 = t3, y0 = a.arrow1, angle = 0, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)

Arrowhead(x0 = t3, y0 = a.arrow1, angle = 180, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)
Arrowhead(x0 = t4, y0 = a.arrow1, angle = 0, arr.length = arrow.length,
          arr.width = arrow.width, arr.adj = 1, arr.type = "triangle",
          lcol = col.arrow, arr.lwd = 1)

text(x = (t1+t2)/2, y = a.text1, labels = expression(lambda[paste(1,',',p)]),
     col = 1, font = 4, family = 'serif', cex = cex.symbol)
text(x = (t2+t3)/2, y = a.text1, labels = expression(lambda[paste(2,',',p)]),
     col = 1, font = 4, family = 'serif', cex = cex.symbol)
text(x = (t3+t4)/2, y = a.text1, labels = expression(lambda[paste(3,',',p)]),
     col = 1, font = 4, family = 'serif', cex = cex.symbol)
dev.off()

## the end ##


