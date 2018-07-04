get.a.nought <- function(Y) {
  sum(Y) / 12
}


get.a.j <- function(Y, j) {
  T <- 1:12
  ((pi*j/12) / sin(pi*j/12)) * sum(Y * cos(2*pi*j*T / 12) / 6)
}


get.b.j <- function(Y, j) {
  T <- 1:12
  ((pi*j/12) / sin(pi*j/12)) * sum(Y * sin(2*pi*j*T / 12) / 6)
}


interpolate.daily <- function(a0, A, B, n.days) {
  y <- numeric(n.days)
  i <- 1
  for (t in seq(1, 12, length=n.days)) {
    harmonic <- 0
    for (j in c(1:6)) {
      harmonic <- (harmonic 
                   + A[j] * cos(2*pi*j*t / 12) 
                   + B[j] * sin(2*pi*j*t / 12))
    }  
    y[i] <- a0 + harmonic
    i <- i + 1
  }
  y
}

get.daily.from.monthly <- function(monthly.means, n.days) {
  a0 <- get.a.nought(monthly.means)
  A <- numeric(6)
  B <- numeric(6)

  for (j in c(1:6)) {
    A[j] <- get.a.j(monthly.means, j)
    B[j] <- get.b.j(monthly.means, j)
  }  
  interpolate.daily(a0, A, B, n.days)
}