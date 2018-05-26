library(tseries)

get.best.arima <- function(x.ts, 
                           start.ord=c(0, 0, 0, 0, 0, 0), 
                           max.ord=c(1, 1, 1, 1, 1, 1), 
                           best.aic=Inf) {
  n <- length(x.ts)
  for (p in start.ord[1]:max.ord[1]) {
    for (d in start.ord[2]:max.ord[2]) {
      for (q in start.ord[3]:max.ord[3]) {
        for (P in start.ord[4]:max.ord[4]) {
          for (D in start.ord[5]:max.ord[5]) {
            for (Q in start.ord[6]:max.ord[6]) {	
              fit <- arima(x.ts, 
                           order=c(p, d, q), 
                           seas=list(order=c(P, D, Q), frequency(x.ts)), 
                           method='CSS')
              fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
              if (fit.aic < best.aic) {
              	 best.aic <- fit.aic
              	 best.fit <- fit
              	 best.params <- c(p, d, q, P, D, Q)
              	 cat('\n\nNew Best ARIMA found with parameters', 
              	     p, d, q, P, D, Q)
              	 cat('\nAIC:', fit.aic)
              }
            }
          }
        }
        
        cat('\nFinished testing:', p, d, q, P, D, Q)
      }
    }
  }
  cat('\n')
  list(aic=best.aic, mod=best.fit, params=best.params)
}


get.best.arima <- function(x.ts, 
                           start.ord=c(0, 0, 0), 
                           max.ord=c(1, 1, 1), 
                           best.aic=Inf) {
  n <- length(x.ts)
  for (p in start.ord[1]:max.ord[1]) {
    for (d in start.ord[2]:max.ord[2]) {
      for (q in start.ord[3]:max.ord[3]) {
        fit <- arima(x.ts, order=c(p, d, q), method='CSS')
        fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
        if (fit.aic < best.aic) {
          best.aic <- fit.aic
          best.fit <- fit
          best.params <- c(p, d, q)
          cat('\n\nNew Best ARIMA found with parameters', p, d, q)
          cat('\nAIC:', fit.aic)
        }
        cat('\nFinished testing:', p, d, q)
      }
    }
  }
  cat('\n')
  list(aic=best.aic, mod=best.fit, params=best.params)
}


#beetle <- c( 7317, 7441, 7557, 8087, 7735, 8750, 9728, 10392, 11395, 11595, 11792, 
#            10370, 9514, 8586, 8019)
temp <- seq(50, 60, length=15)
temp <- temp + rnorm(15, sd=1)
plot(temp, type='l')
beetle <- c( 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0)
beetle <- rev(beetle)            
Beetle <- data.frame(
  t=beetle, 
  t_1=c(NA, beetle[1:((length(beetle) - 1))]),
  t_2=c(rep(NA, 2), beetle[1:((length(beetle) - 2))]),
  t_3=c(rep(NA, 3), beetle[1:((length(beetle) - 3))]),
  temp=temp)
Beetle

glmod <- glm(t ~ t_1 + temp, data=Beetle, family=binomial)
glmod <- step(glmod)
summary(glmod)
preds <- predict(glmod, type='response')

years <- 2000:2014
all.years <- 1900:2014
plot(years, rev(beetle), type='l', xlim=c(1900, 2014), ylim=c(0, 1))
lines(years, rev(c(NA, preds)), col=2)
error <- preds - beetle[-c(1:3)]
(err.mean <- mean(error))
(err.sd <- sd(error))

ITERS <- 1000

for (i in 1:ITERS) {
  preds <- numeric(115)
  preds[1:15] <- beetle
  for (t in 16:115) {
    e <- rnorm(1, sd=err.sd)
    t_1 <- preds[t - 1]
    t_2 <- preds[t - 2]
    t_3 <- preds[t - 3]
    temp <- 
    pred <- predict(
      glmod, newdata=data.frame(t_1=t_1, t_2=t_2, t_3=t_3), type='response') + e
    pred <- max(min(pred, 1), 0)
    preds[t] <- pred
  }
  lines(all.years, rev(preds), col=rgb(1, 0, 0, 0.05))	
}
