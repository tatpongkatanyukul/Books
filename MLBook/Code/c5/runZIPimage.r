## Explore Data

train.zip <- read.table('zip.train')

testimg <- train.zip[1,-1]

image(seq(1,16), seq(1,16), matrix(rnorm(256),16,16))

image(seq(1,16), seq(1,16), matrix(as.numeric(testimg),16,16))

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

displayzipimage <- function(ImageData,N=sqrt(length(ImageData))){
   xs <- seq(1,N)
   ys <- xs
   z <- matrix(ImageData, N, N)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr))
}

displayzipimage(numzip[2,-1])
# O.K. I can show the image

image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))

## Recap How Data is imported

  train.zip <- read.table('zip.train')
  ## check data type
  class(train.zip)

  numzip <- apply(train.zip,c(1,2),as.numeric)
  ## check data type
  class(numzip)

  ## check how data looks like
  image(seq(1,16), seq(1,16), t(apply(matrix(numzip[2,-1],16,16),1,rev)))
  
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

## Train Network

  source('nnMultiClassNaNStop.r') # or source('nnMultiClass.r')
  train.T.K <- makeIndicators(train.T)

  net <- nnTrain(train.X,train.T.K, 
    nHiddens=40, rhoh=0.01, rhoo=0.001, wmax=0.05, nEpochs=3000, net=NULL)

## Validate Network (to choose model: nHiddens, but I have done that and found M=40 is O.K.)

  validate.y <- nnOutput(net,validate.X)

  CorrectRatio <- sum(which.class(validate.y)==validate.T)/length(T)

## Test Network

# I'll let student test it themselves. I'm tired, man!

