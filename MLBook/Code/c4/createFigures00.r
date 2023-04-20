a <- as.matrix(seq(-10, 10, len=1000))

hardlim <- function(x){
   if( x > 0 ) return (1)
return(0)
}

plot(a, apply(a, 1, hardlim), type='l', ylab='f(a)', main='hard limit')

##dev.copy2eps(file='hardlim.eps')


plot(a, apply(a, 1, hardlim), type='l', lty=1, col=1, ylab='f(a)',
   main='activation function')
lines(a, 1/(1+exp(-a)), lty=2, col=2)
legend(3, 0.6, c('hard limit', 'sigmoid'), lty=c(1,2), col=c(1,2))
dev.copy2eps(file='activation.eps')