a <- rnorm(100)
b <- rnorm(100, 1.5)

n.breaks <- 10
breaks <- seq(min(c(a, b)), max(c(a, b)), length=n.breaks)
a.breaks <- cut(a, breaks)
b.breaks <- cut(b, breaks)

a.bars <- table(a.breaks)
b.bars <- table(b.breaks)

all.bars <- c()
for (i in 1:(n.breaks - 1)) {
  all.bars <- c(all.bars, a.bars[i], b.bars[i])
  }

barplot(all.bars, col=c(2, 4))
