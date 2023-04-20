source('scgwrap09a.r')

N = 50
###################
## Redo the plots
###################

load('LCm2vars.RData')
  r <- normalize(train.X)
  train.Xn = r$norm
  test.Xn <- normalize(test.X,xmin=r$min,xmax=r$max)$norm

xs <- matrix(seq(0,4*pi,len=4*N),1,4*N)
xsn <- normalize(xs,xmin=r$min,xmax=r$max)$norm

Ni <- c(1:10,seq(15,50,by=5))
NN <- length(Ni)

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

#################
  M = 1
#################
Ni = c(1,seq(5,50,by=5))
NN = length(Ni)

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

##save(Ni, perfs, recs, train.X, train.T,
##     test.X, test.T, file='lc02M1.RData')

load('lc02M1.RData')

p=par(mfrow=c(4,2))
par(mar=c(4,4,2,0))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3]*2) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig ##
i = 1
ys <- nnOutput(recs[[i]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', 
  main=paste(Ni[i], 'dp(s)'), col='gray', xlab='x', ylab='y')
points(train.X[,ids[1:Ni[i]]], train.T[,ids[1:Ni[[i]]]], pch='o', col='blue')
lines(xs, ys, col='black')

## Figs ##
for(i in c(2,3,4,5,length(Ni))){
  ys <- nnOutput(recs[[i]]$net$net, xsn)
  plot(test.X, test.T, type='p', pch='x', 
    main=paste(Ni[i], 'dp(s)'), col='gray', xlab='x', ylab='y')
  points(train.X[,ids[1:Ni[i]]], train.T[,ids[1:Ni[[i]]]], pch='o', col='blue')
  lines(xs, ys, col='black')
}

par(p)

##dev.copy2eps(file='highBiasM1.eps')

#################
  M = 2000
#################
Ni = c(1,seq(5,50,by=5))
NN = length(Ni)

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

##save(Ni, perfs, recs, train.X, train.T,
##     test.X, test.T, file='lc02M2000.RData')
load('lc02M2000.RData')

p=par(mfrow=c(4,2))
par(mar=c(4,4,2,0))
## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3]*2) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig ##
i = 1
ys <- nnOutput(recs[[i]]$net$net, xsn)
plot(test.X, test.T, type='p', pch='x', 
  main=paste(Ni[i], 'dp(s)'), col='gray', xlab='x', ylab='y')
points(train.X[,ids[1:Ni[i]]], train.T[,ids[1:Ni[[i]]]], pch='o', col='blue')
lines(xs, ys, col='black')

## Figs ##
for(i in c(2,3,4,5,length(Ni))){
  ys <- nnOutput(recs[[i]]$net$net, xsn)
  plot(test.X, test.T, type='p', pch='x', 
    main=paste(Ni[i], 'dp(s)'), col='gray', xlab='x', ylab='y')
  points(train.X[,ids[1:Ni[i]]], train.T[,ids[1:Ni[[i]]]], pch='o', col='blue')
  lines(xs, ys, col='black')
}

par(p)

##dev.copy2eps(file='highVarianceM2000.eps')