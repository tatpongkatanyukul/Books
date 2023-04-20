

load('lc02M1.RData')

##p=par(mfrow=c(5,2))
p <- par(no.readonly = TRUE)
#layout(matrix(c(1,2,3,3,4:9), 5, 2, byrow = TRUE))
layout(matrix(c(1,2,3:8), 4, 2, byrow = TRUE))

par(mar=c(4,4,2,0.5))

## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
#xr <- 3:11
#ymnx <-c( 0, min(perfs[xr,4]) )
#plot(Ni[xr], perfs[xr,3], type='b', ylim=ymnx,
#  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
#lines(Ni[xr], perfs[xr,4], lty=2, col=2)
#points(Ni[xr], perfs[xr,4], pch='*', col=2)

## Fig 3 ##
#plot(Ni, log(perfs[,4]/perfs[,3]),type='l',
#  main='log(test E/train E) v.s. N',
#  xlab='Train Size', ylab='log(E2/E1)')

xr <- 3:11
plot(Ni[xr], perfs[xr,4]/perfs[xr,3],type='l',
  main='test E/train E',
  xlab='Train Size', ylab='E2/E1')

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

dev.copy2eps(file='LChiBiasM1.eps')

##########


load('lc02M2000.RData')

##p=par(mfrow=c(5,2))
p <- par(no.readonly = TRUE)
#layout(matrix(c(1,2,3,3,4:9), 5, 2, byrow = TRUE))
layout(matrix(c(1,2,3:8), 4, 2, byrow = TRUE))

par(mar=c(4,4,2,0.5))

## Fig 1 ##
ymnx <-c( min(perfs[,3:4]), max(perfs[,3:4]) )
plot(Ni, perfs[,3], type='b', ylim=ymnx,
  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
lines(Ni, perfs[,4], lty=2, col=2)
points(Ni, perfs[,4], pch='*', col=2)

## Fig 2 ##
#xr <- 8:11
#ymnx <-c( 0, min(perfs[xr,4]) )
#plot(Ni[xr], perfs[xr,3], type='b', ylim=ymnx,
#  main=paste('M', perfs[1,1]), xlab='Training Size', ylab='MSE')
#lines(Ni[xr], perfs[xr,4], lty=2, col=2)
#points(Ni[xr], perfs[xr,4], pch='*', col=2)

## Fig 3 ##
#plot(Ni, log(perfs[,4]/perfs[,3]),type='l',
#  main='log(test E/train E) v.s. N',
#  xlab='Train Size', ylab='log(E2/E1)')

plot(Ni, perfs[,4]/perfs[,3],type='l',
  main='test E/train E',
  xlab='Train Size', ylab='E2/E1')

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

dev.copy2eps(file='LChiVarianceM2000.eps')