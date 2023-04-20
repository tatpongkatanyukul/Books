## read data

y.dat <- read.csv("fixed_yacht.data", sep = " ")

##
##source('nnOutputCode.r')
##source('nnTrainCode.r')
##source('normalizeCode.r')
source('nnRegressionSigmoid02.r')

## Separate data
## 70% for training; 15% for validation; 15% for test

N <- nrow(y.dat)
N.train <- round(0.7 * N)
N.val <- round((N - N.train)/2)
N.test <- N - N.train - N.val

train.ids <- sample(N, N.train)
val.ids <- sample(setdiff(1:N, train.ids), N.val)
test.ids <- setdiff(1:N, c(train.ids, val.ids))

train.X <- t(y.dat[train.ids,1:6])
train.T <- t(y.dat[train.ids,7])
r <- normalize(train.X, returnParms=TRUE)
train.Xn <- r$data

val.X <- t(y.dat[val.ids,1:6])
val.T <- t(y.dat[val.ids,7])
val.Xn <- normalize(val.X, means=r$means, stdevs=r$stdevs)

test.X <- t(y.dat[test.ids,1:6])
test.T <- t(y.dat[test.ids,7])
test.Xn <- normalize(test.X, means=r$means, stdevs=r$stdevs)

## Train

##net <- nnTrain(train.Xn, train.T, nHiddens=10, rhoh=0.01, rhoo=0.001, wmax=0.5, nEpochs=5000)
## 1.79

##net <- nnTrain(train.Xn, train.T, nHiddens=10, rhoh=0.01, rhoo=0.0003, wmax=0.5, nEpochs=5000)
## 2.26

net <- nnTrain(train.Xn, train.T, nHiddens=10, rhoh=0.003, rhoo=0.0003, wmax=0.5, nEpochs=5000)
## 1.198

net <- nnTrain(train.Xn, train.T, nHiddens=10, rhoh=0.001, rhoo=0.0003, wmax=0.5, nEpochs=10000)
##save(net, train.X, train.T, val.X, val.T, test.X, test.T, file='yachtNet.RData')

load('yachtNet.RData')

## evaluation: I haven't done validation yet.

eval.Xn <- cbind(val.Xn, test.Xn)
eval.T <- cbind(val.T, test.T)
eval.y <- nnOutput(net, eval.Xn)
eval.rmse <- sqrt(mean((eval.y - eval.T)^2))


order.eval <- order(eval.T)
par(mfrow=c(1,2))
plot(1:10000, net$errors, main='Training error', xlab='epoch', ylab='rmse', type='l')
plot(eval.T[order.eval], eval.y[order.eval], xlab='target', ylab='nn\'s output',
  main=paste('Test: RMSE = ',round(eval.rmse,3)))
lines(c(min(eval.T), max(eval.T)), c(min(eval.T), max(eval.T)), col='red')

##dev.copy2eps(file='yachtExample.eps')


plot(eval.T, eval.y, xlab='target', ylab='nn\'s output',
  main=paste('Test: RMSE = ',round(eval.rmse,3)))
lines(c(min(eval.T), max(eval.T)), c(min(eval.T), max(eval.T)), col='red')
