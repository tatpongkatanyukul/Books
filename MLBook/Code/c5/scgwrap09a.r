#####################
## Wrapper Function
#####################
rm(list=ls())

##source('nnscg15.r')

source('dEwCode.r')
source('mseWCode.r')
source('initWeightsNugyenWidrowCode.r')
source('nnOutputMoreCode.r')
source('scgCode1.r')
source('nnTerminationCode.r')
source('scgOthersCode.r')
source('normCode.r')




##########################################
## Global variable: for debugging purpose
##########################################
debug.msg <<- "INIT"

###########################################################################
# nnTrain.scg (wrapper)
###########################################################################


nnTrain.scg <- function(X, T, nHiddens, net=NULL, nEpochs=500, nntype='regression',
                        val.X=NULL, val.T=NULL, tol=0, min.grad=1e-12, max.val.fail=8, internal.norm=F, ...){
##
## val.X=NULL or val.T=NULL infers no early stopping.
##
cat('\n* train ANN with scg on ', nntype, '\n')

  ########################
  ## redefine termination
  ########################

  wrap.term <- function(xk, term.data=NULL, term.info=NULL,
      doplot=FALSE, E.logs=NULL){

      nnTermination(xk, term.data, term.info,
         tol=tol, min.grad=min.grad, max.val.fail=max.val.fail,
         doplot, E.logs, nntype)

  }##end scg.termfn


  #######################
  ## initialize weights
  #######################

   D <- nrow(X)
   N <- ncol(X)
   K <- nrow(T)
   M <- nHiddens

   if (is.null(net)) {
      ## Initialize weights
      net <- init.weights.nugyenwidrow(D, M, K, activeregion=c(-4, 4))

   }#if
      
   ws0 <- unpack.w(net)


   prob.info <- list(D=D, M=M, K=K, X=X, T=T,
     nntype=nntype)

   term.info <- list(prob.info=prob.info, val.X=val.X, val.T=val.T)

   wsk.log <- scg(dE.w, mse.w, ws0, prob.info=prob.info, 
    term.fn=wrap.term, term.info=term.info, 
    MaxN=nEpochs, log=TRUE)

##cat('Here I am\n')
##browser()
   net <- pack.w(wsk.log$w, D, M, K)

   if(!is.null(val.X) & !is.null(val.T)) {
        net <- wsk.log$term.results$val.best.net;
   }
   
return(c(wsk.log, list(net=net)))
}##end nnTrain.scg


######################################
## regression
######################################
regression.example <- function(){

  ## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  r <- normalize(train.X)
  train.Xn <- r$norm

  r.s <- normalize.s(train.X,returnParms=TRUE)
  train.Xns <- r.s$data
  
  test.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  test.T <- f(test.X)
  test.Xn <- normalize(test.X,xmin=r$min,xmax=r$max)$norm

  test.Xns <- normalize.s(test.X,r.s$means,r.s$stdevs)

  val.X <- matrix(seq(0,4*pi,len=round(N/10)),nrow=1)
  val.T <- f(val.X)  

  val.Xn <- normalize(val.X,xmin=r$min,xmax=r$max)$norm

  ################
  ## train: SCG
  ################

  resSCGn <- nnTrain.scg(train.Xn,train.T, nHiddens=20, nEpoch=500)

  resSCGnv <- nnTrain.scg(train.Xn,train.T, nHiddens=20, nEpoch=500,
               val.X=val.Xn, val.T=val.T)


  resSCGns <- nnTrain.scg(train.Xns,train.T, nHiddens=20, nEpoch=500)

  #############
  ## test
  #############
  yn <- nnOutput(resSCGn$net, test.Xn)

  ynv <- nnOutput(resSCGnv$net, test.Xn)

  yns <- nnOutput(resSCGns$net, test.Xns)

  
  pmn <- min( c(yn, yns, test.T) )
  pmx <- max( c(yn, yns, test.T) )
  plot(test.X, test.T, ylim=c(pmn,pmx), type='b', main='scg',
  xlab='x', ylab='y')
  points(test.X, yn, pch='x', col='red')
  points(test.X, ynv, pch='*', col='green')
  points(test.X, yns, pch='+', col='blue')

  legend(7,3, c('real', 'n', 'nv', 'ns'), pch=c('o','x','*','+'), 
     col=c('black', 'red', 'green','blue'))
  
  sse=c(sum( (yn - test.T)^2 ), sum( (ynv - test.T)^2 ), sum( (yns - test.T)^2 ))

  mse=c(mean( (yn - test.T)^2 ), mean( (ynv - test.T)^2 ), mean( (yns - test.T)^2 ))

  cat('\ntotal error:(normalize:min/max, early stopping, normalize:mean.stdev\n')
  print(sse)
  cat('\nmse:\n')
  print(mse)
  
}##end regression.example  


######################################
## biclass
######################################

biclass.example <- function(){

  cat('classify either a given record is whether Iris virginica.\n')

  N = nrow(iris)

  X <- t(iris[,-5])  ## D x N
  Y <- matrix((iris[,5] == 'virginica'), nrow=1) ## 1 x N

  train.ids <- sample(1:N, round(N*0.7))

  train.X <- X[, train.ids[-(1:10)]]
  train.Y <- Y[, train.ids[-(1:10)], drop=FALSE]

  val.X <- X[, train.ids[1:10]]
  val.Y <- Y[, train.ids[1:10], drop=FALSE]

  test.X <- X[, -train.ids]
  test.Y <- Y[, -train.ids, drop=FALSE]

  r <- normalize(train.X)
  train.Xn <- r$norm

  test.Xn <- normalize(test.X,xmin=r$min,xmax=r$max)$norm

  val.Xn <- normalize(val.X,xmin=r$min,xmax=r$max)$norm

  ############
  ## scg
  ############

  resSCGn <- nnTrain.scg(train.Xn,train.Y, nHiddens=20, nEpoch=1000, nntype='biclass')

  resSCGnv <- nnTrain.scg(train.Xn,train.Y, nHiddens=20, nEpoch=1000, nntype='biclass',
               val.X=val.Xn, val.T=val.Y)

  ############
  ## test
  ############
  
  yn <- nnOutput(resSCGn$net, test.Xn, nntype='biclass')  
  ynv <- nnOutput(resSCGnv$net, test.Xn, nntype='biclass')  

  yn.class <- (yn > 0.5)
  ynv.class <- (ynv > 0.5)

  correct.n <- sum( yn.class == test.Y )/length(test.Y) * 100
  correct.nv <- sum( ynv.class == test.Y )/length(test.Y) * 100

  cat('\ncorrect ratios:\n')
  print(c(correct.n, correct.nv))
##[1] 88.88889 95.55556
}##end biclass.example  


multiclass.example <- function(zippath='../04ANNAppImg/ZIPImage/imageZIP'){

  cat('This example requires ZIP image data.\n')
  cat('Dataset can be obtained from\n')
  cat('* description: http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/zip.info.txt;\n')
  cat('* training set: http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/zip.train.gz;\n')
  cat('* test set: http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/zip.test.gz.\n')


  D <- 256
  M <- 40
  K <- 10

  train.path <- paste(zippath, '/zip.train', sep='')
  if( !file.exists(train.path) ){
    cat(paste('No file: ', train.path, '\n'))
    return()
  }

  train.zip <- read.table(train.path)
  numzip <- apply(train.zip,c(1,2),as.numeric)

  X <- t(numzip[,-1])  # X is D x N
  T <- t(numzip[,1])   # T is 1 x N
  N <- ncol(X)

  numzip <- apply(train.zip,c(1,2),as.numeric)

  X <- t(numzip[,-1])  # X is D x N
  T <- t(numzip[,1])   # T is 1 x N
  N <- ncol(X)

  id.rand <- sample(N)

  marker <- round(0.75*N)

  train.X <- X[,id.rand[1:marker]]
  train.T <- T[,id.rand[1:marker]]

  val.X <- X[,id.rand[-1:-marker]]
  val.T <- T[,id.rand[-1:-marker]]

  test.path <- paste(zippath, '/zip.test', sep='')
  if( !file.exists(test.path) ){
    cat(paste('No file: ', test.path, '\n'))
    return()
  }

  test.zip <- read.table(test.path)
  test.num <- apply(test.zip,c(1,2),as.numeric)
  test.X <- t(test.zip[,-1])  # X is D x N
  test.T <- encode.OK(t(test.zip[,1]))   # T is 10 x N

  train.T.K <- encode.OK(train.T)
  val.T.K <- encode.OK(val.T)

  N.test <- ncol(test.X)

  ###################
  ## Train
  ###################

log1 <- Sys.time()
print(log1)

  res1 <- nnTrain.scg(train.X,train.T.K, nHiddens=40, 
               nEpoch=3000, nntype='multiclass',
               val.X=val.X, val.T=val.T.K)

log2 <- Sys.time()
print(log2)

cat('\nElaspse Time\n')
print(log2-log1)

  ############
  ## Test
  ############
    
  ## best.val.net
  y1 <- nnOutput(res1$net, test.X, 
         nntype='multiclass')

  y1.class <- decode.OK(y1, c('0','1','2','3','4','5','6','7','8','9'))
  correct.perc <- sum(y1.class == decode.OK(test.T))/N.test * 100

  cat(paste('Correct %', round(correct.perc,2), '\n'))

}##end multiclass.example