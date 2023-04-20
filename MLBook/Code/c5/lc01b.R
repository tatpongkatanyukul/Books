source('scgwrap09a.r')

N <- 50
r <- normalize(train.X)
Ni <- c(1:10,seq(15,50,by=5))
NN <- length(Ni)
xs <- matrix(seq(0,4*pi,len=4*N),1,4*N)
xsn <- normalize(xs,xmin=r$min,xmax=r$max)$norm

load('LCm5vars.RData')
yr2 <- c(0,20)
yr3 <- c(0,20)

load('LCm10vars.RData')
yr2 <- c(0,10)
yr3 <- c(0,20)

load('LCm20vars.RData')
yr2 <- c(0,10)
yr3 <- c(0,20)

load('LCm200vars.RData')
yr2 <- c(0,5)
yr3 <- c(0,5e8)


p=par(mfrow=c(1,4))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('LC M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', ylim=yr2,
  main=paste('Zoom-in LC M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
xr <- 11:18
plot(Ni[xr], perfs[xr,4]/perfs[xr,3], type='l', ylim=yr3,
  main='test E/train E', xlab='Training Size', ylab='E2/E1')

## Fig 4 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main=paste('Output M', perfs[1,1]), xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps', sep=''))


