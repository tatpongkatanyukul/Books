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

image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))
  
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

  log1 <- Sys.time()
  net <- nnTrain(train.X,train.T.K, 
    nHiddens=40, rhoh=0.0001, rhoo=0.002, wmax=0.1, 
	nEpochs=1500, net=NULL,
	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
    val.X=validate.X, val.T=validate.T.K)
  log2 <- Sys.time()

  log2 - log1
## Time difference of 13.0381 mins rhoh=0.0001, rhoo=0.00001, wmax=0.05, 0.9257598
## Time difference of 12.49068 mins rhoh=0.0001, rhoo=0.001, wmax=0.1, 0.9207773
## Time difference of 1.038962 mins nEpochs=1500, 0.8624813
##  37.05007 secs, rhoh=0.0001, rhoo=0.003, wmax=0.1, 0.8898854

## Test Network

  test.y <- nnOutput(net,test.X)

  CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

##save(CorrectRatio, train.X, train.T.K, validate.X, validate.T.K, net,
##  file='ZIPimage01a.RData')
##> CorrectRatio
##[1] 0.9257598
