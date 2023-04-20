## Explore Data

train.zip <- read.table('./imageZIP/zip.train')

numzip <- apply(train.zip,c(1,2),as.numeric) ## normalized to [-1,1]

##save(records, train.X, train.T.K, validate.X, validate.T.K, nets,
##  file='tryZIPimage02m40longer.RData')

disp.img <- function(imgseq,...){
   xs <- seq(1,16)
   ys <- xs
   z <- matrix(imgseq, 16, 16)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr),...)
}

gray10 = gray(seq(1,0,len=10))
gray256 = gray(seq(1,0,len=256))



disp.img(numzip[2,-1], main=paste(numzip[2,1]), col=gray10)


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

load('tryZIPimage02m20.RData')


p=par(mfrow=c(2,5))
for(i in 1:10){
   N = max(which(nets[[i]]$errors > 0))
   plot(1:N, nets[[i]]$errors[1:N], 
     main= paste('Train ', i, 
     ';\nTest\'s correct ', round(records[[i]][2],2)),
     xlab='epoch', ylab='cost', type='l')
}
par(p)


##########################
## make confusion matrix
##########################
i = 2
    test.y <- nnOutput(nets[[i]],test.X)


(which.class(test.y)==0)[1:20]
(test.T ==j)[1:20]
(which.class(test.y)==0)[1:20] & (test.T ==j)[1:20]

intersect( which(which.class(test.y)==2), which(test.T==0))


   for(i in 0:9){
   for(j in 0:9){
     cat( paste(sum( (which.class(test.y)==i) & (test.T ==j)), ' & ') )
      

   }##end for j
     cat('\n')
   }##end for i


#############################
## see wrong classification
#############################

wrong.ids <- which( which.class(test.y) != test.T )


p=par(mfrow=c(2,4))
par(mar=c(0,0,4,0))
for(j in 1:8){

   i = wrong.ids[j]
   disp.img(test.X[,i], main=paste('real', test.T[i], '\npredict', which.class(test.y[,i,drop=F])), col=gray256, axes=F)
}
par(p)
dev.copy2eps(file='sampleWrongClass.eps')

##############################
## see internal representation
##############################

p=par(mfrow=c(4,5))
par(mar=c(1,1,4,1))
for(i in 1:20){
  
  disp.img(nets[[2]]$W1[i,-1], col=gray256, axes=F)
}
par(p)

## This does not help much. So, better leave it out.