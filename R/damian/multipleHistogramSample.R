a <- rnorm(100)
b <- rnorm(100, 1.5)

n.breaks <- 10
breaks <- seq(min(c(a, b)), max(c(a, b)), length=n.breaks)
a.breaks <- cut(a, breaks)
b.breaks <- cut(b, breaks)

a.bars <- table(a.breaks)
b.bars <- table(b.breaks)

all.bars <- c()
for (i in 1:(n.breaks + 1)) {
  all.bars <- c(all.bars, a.bars[i], b.bars[i])
}

barplot(all.bars, col=c(2, 4))



a <- rpois(100, 3)
b <- rpois(100, 5)
#breaks <- 0:6
a.bars <- table(a)
b.bars <- table(b)

all.bars <- c()
for (i in 1:10) {
  all.bars <- c(all.bars, a.bars[i], b.bars[i])
}
mn <- min(all.bars, na.rm=T)
mx <- max(all.bars, na.rm=T)

barplot(all.bars, col=c(2, 4), space=0, xaxt='n')
axis(1, at=seq(1, 2*(mx - mn) + 1, 2), labels=mn:mx, tick=F)
