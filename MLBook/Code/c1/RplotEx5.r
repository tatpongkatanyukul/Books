x <- seq(-0.5, 0.5, len=50)

plot(x, 10*exp(-x^2)-8.8, type='l', ylab='y',
  main='10*exp(-x^2)-8.8 v.s. sin(x)')
lines(x, sin(x), col='red', lty=2)
legend(0, -0.5, c('10*exp(-x^2)-8.8', 'sin(x)'),
   lty=c(1,2), col=c('black', 'red'))


###############################
## Machine Precision Exercise
###############################

> .Machine$double.eps
[1] 2.220446e-16

###############################
## round off error
###############################

x <- seq(1,30, len=50)

par(mfrow=c(2,1))
plot(x, x^2, type='l', ylab='y', ylim=c(0,1000),
  main='x^2 and (x^2 - x^2)')
lines(x, x^2 - x^2, col=2, lty=2)

plot(x, log(exp(x^2)), type='l', ylab='y', ylim=c(0,1000),
  main='log(exp(x^2)) and log(exp(x^2)/exp(x^2))')
lines(x, log(exp(x^2)/exp(x^2)), col=2, lty=2)


###############################
## round off error 2
###############################

x <- seq(1,10, len=20)
y <- x/7
x == 7*y