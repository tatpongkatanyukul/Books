x <- seq(1,30, len=50)

par(mfrow=c(2,1))
plot(x, x^2, type='l', ylab='y', ylim=c(0,1000),
  main='x^2 and (x^2 - x^2)')
lines(x, x^2 - x^2, col=2, lty=2)

plot(x, log(exp(x^2)), type='l', ylab='y', ylim=c(0,1000),
  main='log(exp(x^2)) and log(exp(x^2)/exp(x^2))')
lines(x, log(exp(x^2)/exp(x^2)), col=2, lty=2)
