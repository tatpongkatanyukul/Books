p=par(mfrow=c(1,2))

plot(x, sin(x), type='l', main='sine')

plot(x, sin(x), type='b', main='sine v.s. cosine', pch='x', ylab='y', xlab='theta', ylim=c(-2,2))
lines(x, cos(x), col='red')
points(x, cos(x), pch='*', col='red')

legend(5,2,c('sine','cosine'), pch=c('x','*'), col=c('black', 'red'))
par(p)


