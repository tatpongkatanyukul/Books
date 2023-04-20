f <- function(x){ x[1]^2 + x[2]^2 }

x1s = seq(-2,2, len=20)
x2s = seq(-2,2, len=20)

x1x2 <- expand.grid( x=x1s , y=x2s )

z <- matrix(apply(x1x2, 1, f),20,20)

persp(x1s, x2s, z,phi=-20,theta=30, xlab='x1', ylab='x2', zlab='f(x)',
 col='green') -> res
points( trans3d(0,0,0,pmat=res), col='red', pch=16)

dev.copy2eps(file='SOSC.eps')
