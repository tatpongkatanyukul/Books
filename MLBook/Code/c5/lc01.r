source('scgwrap09a.r')

## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  r <- normalize(train.X)
  train.Xn <- r$norm

  test.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  test.T <- f(test.X)
  test.Xn <- normalize(test.X,xmin=r$min,xmax=r$max)$norm

##############
## new code ##
##############

ids <- sample(N)
Ni <- c(1:10,seq(15,50,by=5))
NN <- length(Ni)

#################
  M = 2
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 2', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm2.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm2vars.RData')


#################
  M = 5
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 5', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm5.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm5vars.RData')

#################
  M = 10
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 10', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm10vars.RData')


#################
  M = 20
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 20', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm20.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm20vars.RData')

#################
  M = 40
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 40', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm40.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm40vars.RData')


#################
  M = 200
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 200', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm200.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm200vars.RData')


#################
  M = 2000
#################
recs <- vector('list', NN)
perfs <- matrix(0, NN, 4) ## M, Ni, train.E, test.E

for(i in 1:NN){

  res <- nnTrain.scg(train.Xn[,ids[1:Ni[i]],drop=F],train.T[,ids[1:Ni[i]],drop=F], 
           nHiddens=M, nEpoch=500, tol=1e-8)
  train.y <- nnOutput(res$net, train.Xn[,ids[1:Ni[i]],drop=F])
  test.y <- nnOutput(res$net, test.Xn)

  train.mse <- mean( (train.y - train.T[ids[1:Ni[i]],drop=F])^2 )
  test.mse <- mean( (test.y - test.T)^2 )

recs[[i]] <- list(M=M, net=res)
perfs[i,] <- c(M, Ni[i], train.mse, test.mse)
}##end for Ni

ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )

plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main='M = 2000', xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

##dev.copy2eps(file='LCm2000.eps')
##save(train.X, train.T, test.X, test.T, ids, recs, perfs, file='LCm2000vars.RData')

###################
## Redo the plots
###################

xs <- matrix(seq(0,4*pi,len=4*N),1,4*N)
xsn <- normalize(xs,xmin=r$min,xmax=r$max)$norm

load('LCm2vars.RData')

p=par(mfrow=c(1,3))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', 
  main=paste('Training MSE: M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main='M 2', xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps'))

load('LCm5vars.RData')

p=par(mfrow=c(1,4))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('LC M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', 
  main=paste('Zoom-in LC M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
xr <- 11:18
plot(Ni[xr], perfs[xr,4]/perfs[xr,3], type='l',
  main='test E/train E', xlab='Training Size', ylab='E2/E1')

## Fig 4 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main=paste('Output M', perfs[1,1]), xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps', sep=''))

load('LCm10vars.RData')

p=par(mfrow=c(1,4))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('LC M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', 
  main=paste('Zoom-in LC M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
xr <- 11:18
plot(Ni[xr], perfs[xr,4]/perfs[xr,3], type='l',
  main='test E/train E', xlab='Training Size', ylab='E2/E1')

## Fig 4 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main=paste('Output M', perfs[1,1]), xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps', sep=''))

load('LCm20vars.RData')

p=par(mfrow=c(1,3))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('LC M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', ylim=c(0,3),
  main=paste('Zoom-in LC M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main=paste('Output M', perfs[1,1]), xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)


dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps'))

load('LCm40vars.RData')

p=par(mfrow=c(1,3))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', 
  main=paste('Training MSE: M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps'))

load('LCm200vars.RData')

p=par(mfrow=c(1,3))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('LC M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', ylim=c(0,6e-8),
  main=paste('Zoom-in LC M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', 
  main=paste('Output M', perfs[1,1]), xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)


dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps'))

load('LCm2000vars.RData')

p=par(mfrow=c(1,3))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
plot(Ni, perfs[,3], type='b', ylim=c(0,20e-9),
  main=paste('Training MSE: M', perfs[1,1]), 
  xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 3 ##
ys <- nnOutput(recs[[NN]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', col='red', xlab='x', ylab='y')
points(train.X, train.T, pch='o', col='blue')
lines(xs, ys, col='black')

par(p)

dev.copy2eps(file=paste('LCM',perfs[1,1],'.eps'))
