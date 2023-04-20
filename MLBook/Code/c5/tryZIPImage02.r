## Explore Data

train.zip <- read.table('./imageZIP/zip.train')

testimg <- train.zip[1,-1]

image(seq(1,16), seq(1,16), matrix(rnorm(256),16,16))

image(seq(1,16), seq(1,16), matrix(as.numeric(testimg),16,16))
## see what number it is: train.zip[1,1]

numzip <- apply(train.zip,c(1,2),as.numeric)

image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))
# O.K. I can show the image

# Rearrage it to the correct orientation?
image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))
# This is original

# try
image(seq(1,16), seq(1,16), t(matrix(numzip[2,-1],16,16)))
# equivalent to mirror and rotate pi/2  ccw or rotate pi/2 cw then mirror

# try 
image(seq(1,16), seq(1,16), apply(matrix(numzip[2,-1],16,16),1,rev))
# equivalent to rotate pi/2 ccw

# try
image(seq(1,16), seq(1,16), t(apply(matrix(numzip[2,-1],16,16),1,rev)))
# bingo!

x11()
image(seq(1,16), seq(1,16), t(apply(matrix(numzip[2,-1],16,16),1,rev)))
# displaying image with correct orientation

disp.img <- function(imgseq,...){
   xs <- seq(1,16)
   ys <- xs
   z <- matrix(imgseq, 16, 16)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr),...)
}

disp.img(numzip[2,-1], main=paste(numzip[2,1]))
# O.K. I can show the image

  
## Prepare Data

  X <- t(numzip[,-1])  # X is D x N
  T <- t(numzip[,1])   # T is 1 x N
  N <- ncol(X)

## Separate Data into Training and Validation Sets

  id.rand <- sample(N)

  marker <- round(0.75*N)

  train.X <- X[,id.rand[1:marker]]
  train.T <- T[,id.rand[1:marker]]

  validate.X <- X[,id.rand[-1:-marker]]
  validate.T <- T[,id.rand[-1:-marker]]


  test.zip <- read.table('./imageZIP/zip.test')
  test.num <- apply(test.zip,c(1,2),as.numeric)
  #image(seq(1,16), seq(1,16), 
  # t(apply(matrix(test.num[2,-1],16,16),1,rev)))

  test.X <- t(test.zip[,-1])  # X is D x N
  test.T <- t(test.zip[,1])   # T is 1 x N
  N.test <- ncol(test.X)


## Train Network

  source('nnMultiClass02.r') # or source('nnMultiClass.r')
  train.T.K <- makeIndicators(train.T)
  validate.T.K <- makeIndicators(validate.T)

  nets <- vector('list', 10)
  records <- vector('list', 10)

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=40, rhoh=0.0002, rhoo=0.002, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m40.RData')


p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)
## dev.copy2eps(file='m40trainCost.eps')

#################
## m 40, slower
#################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=40, rhoh=0.0001, rhoo=0.001, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m40slower.RData')

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

##dev.copy2eps(file='m40trainCostLowRho.eps')

#################
## m 40, longer
#################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=40, rhoh=0.0001, rhoo=0.001, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m40longer.RData')

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

##dev.copy2eps(file='m40trainCostlonger.eps')


################
## m 20
################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=20, rhoh=0.0001, rhoo=0.001, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m20.RData')

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

################
## m 10
################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=10, rhoh=0.0001, rhoo=0.001, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m10.RData')

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

dev.copy2eps(file='m10train.eps')

################
## m 5
################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=5, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=5, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m5.RData')

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)



dev.copy2eps(file='m5train.eps')


###############################
## m 3
###############################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=3, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)


  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=3, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)


##dev.copy2eps(file='m3train.eps')
##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m3.RData')


###############################
## m 2
###############################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=2, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=2, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=2, rhoh=0.00001, rhoo=0.0001, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)

dev.copy2eps(file='m2train.eps')
save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
  file='tryZIPimage02m2.RData')

###############################
## m 1
###############################

  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=1, rhoh=0.00003, rhoo=0.0003, wmax=0.1, 
	nEpochs=500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)


  for(i in 1:10){

    log1 <- Sys.time()
    nets[[i]] <- nnTrain(train.X,train.T.K, 
      nHiddens=1, rhoh=0.00001, rhoo=0.0001, wmax=0.1, 
	nEpochs=500, net=nets[[i]],
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
      val.X=validate.X, val.T=validate.T.K)
    log2 <- Sys.time()

    ## Test Network

    test.y <- nnOutput(nets[[i]],test.X)
    CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

    records[[i]] = c(log2 - log1, CorrectRatio)
  }

p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)


dev.copy2eps(file='m1train.eps')
save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
  file='tryZIPimage02m1.RData')


#################

plot(c(1, 2, 3, 5, 10, 20, 40), c(0.39, 0.69, 0.8, 0.89, 0.92, 0.93, 0.93),
  main='Correct Ratio v.s. M', xlab='M', ylab='Correct Ratio', type='b')

dev.copy2eps(file='zipComporingMs.eps')