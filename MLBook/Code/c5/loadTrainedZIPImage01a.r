## previous version: Week09Aug5n7/tryZIPImage01a.r
## this version: simplify and provide trained net for freeDraw.r
## created Aug 3rd, 2013.

displayzipimage <- function(ImageData,N=sqrt(length(ImageData)), p.title='zip'){
   xs <- seq(1,N)
   ys <- xs
   z <- matrix(ImageData, N, N)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr), main=p.title)
}

##############
## Load data
##############

  test.zip <- read.table('./imageZIP/zip.test')
  test.num <- apply(test.zip,c(1,2),as.numeric)

  test.X <- t(test.zip[,-1])  # X is D x N
  test.T <- t(test.zip[,1])   # T is 1 x N
  N.test <- ncol(test.X)

#####################
## Train Network
#####################

  source('nnMultiClass02.r')

  ##log1 <- Sys.time()
  ##net <- nnTrain(train.X,train.T.K, 
  ##  nHiddens=40, rhoh=0.0001, rhoo=0.00001, wmax=0.05, 
  ##	nEpochs=3000, net=NULL,
  ##	earlystopping=TRUE, early.tol=1, ## val.E > 2*best.val.E
  ##  val.X=validate.X, val.T=validate.T.K)
  ##log2 <- Sys.time()

  load(file='ZIPimage01a.RData')

  ## Test Network

  test.y <- nnOutput(net,test.X)

  CorrectRatio <- sum(which.class(test.y)==test.T)/N.test

##> CorrectRatio
##[1] 0.9257598


