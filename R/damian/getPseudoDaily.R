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


interpolate.daily <- function(a0, A, B, days.in.year=365) {
  y <- numeric(days.in.year)
  i <- 1
  for (t in seq(1, 12, length=days.in.year)) {
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

get.daily.from.monthly <- function(monthly.means, days.in.year=365) {
  a0 <- get.a.nought(monthly.means)
  A <- numeric(6)
  B <- numeric(6)

  for (j in c(1:6)) {
    A[j] <- get.a.j(monthly.means, j)
    B[j] <- get.b.j(monthly.means, j)
  }  
  interpolate.daily(a0, A, B, days.in.year)
}