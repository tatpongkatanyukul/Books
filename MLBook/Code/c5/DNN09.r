## Deep Neural Network
## this version: do stochastic GD (shuffle training data)
## previous version: DNN08.r (working)
## created Mar 30th, 2015.

#########################################################
### Encode & Decode functions
#########################################################

encode1ofK <- function(T, classes=sort(unique(as.character(T)))){
  K <- length(classes)
  N <- length(T)

  T.K <- (matrix(T,K,N, byrow=TRUE) == matrix(classes,K,N,byrow=FALSE))*1

  rownames(T.K) <- classes
  return(T.K)
}

## Y is K x N, each k corresponds to each class indicator

decode1ofK <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

#########################################################
### Activation functions
#########################################################

sigmoid = function (A){
  return(  1 / (1 + exp(-A)) )
}##end sigmoid

dsigmoid = function (Z) {
  return( Z*(1 - Z) )
}##end dsigmoid

sreclin = function (A){
  return(  log(1 + exp(A)) )
}##end sigmoid

dsreclin = function (Z) {
  return( exp(Z)/(1 + exp(Z)) )
}##end dsigmoid

##xs = seq(-10,10,len=500)
##plot(xs, sreclin(xs), type='l')
##lines(xs, dsreclin(xs), col='red')

identity = function(A){ return(A) }

logistic = function(A){
  return( 1/(1+exp(-A)) )
}

softmax = function(A){
  return( exp(A)/matrix(colSums(exp(A)), nrow(A), ncol(A), byrow=TRUE) )
}

##########################################################
### Neural net for deep learning
##########################################################

nnTrain = function (X,T, nHiddens, rho, nEpochs,
                     acf=NULL, dacf=NULL, pdropout=0.0,
                     net=NULL, wmax=0.1, batchsize=Inf,
                    doPlot=TRUE, plotTitle='Training',
                    ValX=NULL, ValT=NULL, maxValFail=8, 
                    tol=1e-3) {
## acf: activation functions
## dacf: derivatives of acf's
## pdropout: probabilities of droping out (default 0 = no drop-out)
## batchsize (size of minibatch): Inf= batch mode; 1 = online mode
## net: initial network
## ValX, ValT, maxValFail: early stopping validation data
## tol: if training error < tol, stop training.
## 
##
## status: 0 = NaN, 1 = OK (reach nEpochs), 2 = convergence (|Eold - Enew| < tol), 3 = early stoppoing

##browser()
   ## Initialization ##
 
   rdpo = 1 - pdropout  ##  prob. of staying
   D = nrow(X)          ##, e.g., D = 256
   N = ncol(X)          ##, e.g., N = 2200
   K = nrow(T)          ##, e.g., K = 3
   M = c(nHiddens,K)    ##, e.g., M =c(10,5,3)

   NL = length(M)            ## Number of layers (>= 2), e.g., NL = 3
   W = vector('list', NL)    ##
   Z = vector('list', NL)    ##

   DELTA = vector('list', NL)
   dE = vector('list', NL)
  
   msg = 'OK'
   status = 1

   if (is.null(net)) {
      ## Initialize weights

      W[[1]] = matrix( runif(M[1]*(1+D), -wmax, wmax), M[1], 1+D)
      for( i in 2:NL ){
        W[[i]] = matrix(runif(M[i]*(1+M[i-1]), -wmax, wmax), M[i], 1+M[i-1])
      }      

      ## history of errors
      errors = matrix(0,1,nEpochs)
      firstEpoch = 1  # this will be used for indexing of errors      
   } else {
      W = net$W
      if( !is.null(net$acf) ){
        acf = net$acf
      }
      if( !is.null(net$dacf) ){
        dacf = net$dacf
      }

      errors = matrix(c(net$errors, rep(0,nEpochs)),nrow=1)
      firstEpoch = length(net$errors)+1
   }#if

  if(is.null(acf)){
  ## Default to regression network
    for(i in 1:(NL-1)){
       acf = c(acf, list(sigmoid))
    }    
    acf = c(acf, list(identity))
  }

  if(is.null(dacf)){
  ## Default to regression network
    for(i in 1:(NL-1)){
       dacf = c(dacf, list(dsigmoid))
    }    
  }


   doVal = !(is.null(ValT) & is.null(ValX))   
   MinE  = Inf
   BestW = NULL
   BestZ = NULL
   ValFailCount = 0

   NB = 1               ## number of minibatches
   bIDs = list(1:N)     ## id of each minibatch; default = batch mode (all in one batch)
   if(is.finite(batchsize)){
     NB = ceiling(N/batchsize)
     bIDs = vector('list', NB)
     for(bi in 1:NB){
       bIDs[[bi]] = (bi - 1)*batchsize + 1:batchsize
     }

     ## fix last minibatch
     bIDs[[NB]] = bIDs[[NB]][ bIDs[[NB]] <= N ]
   }##if

   Eold = Inf
   ## Training ##
   for (epoch in 1:nEpochs){
   SID = sample(N)  ## shuffled IDs

     erri = 0
     for (bi in 1:NB){
      
      ############################
      ## (1) Forward propagation
      ############################
      Z0 = X[,SID[bIDs[[bi]]],drop=F]
      Nbi = ncol(Z0)

      ## dropout mask
      DM = matrix(rbinom( D*Nbi, 1, rdpo ),D,Nbi)    
      BZ0 = rbind(1, Z0*DM )
      A1 = W[[1]] %*% BZ0
      Z[[1]] = acf[[1]](A1)

      ## Calculate output unit outputs, Y, which is K x N.
      for(m in 2:NL){
        ## dropout mask
        DM = matrix(rbinom( M[m-1]*Nbi, 1, rdpo ),M[m-1],Nbi)    
        BZ = rbind(1,Z[[m-1]]*DM)
        A = W[[m]] %*% BZ
        Z[[m]] =acf[[m]](A)
      }
      
      ##############################
      ## (2) Evaluate output delta
      ##############################

      Y = Z[[NL]]
      DELTA[[NL]] = Y - T[,SID[bIDs[[bi]]],drop=F]


      ## Check errors ##
      errbi = sum(DELTA[[NL]]^2) # Sum Squared Error
      erri = erri + errbi

      #############################
      ## (3) Backpropagate errors
      #############################

      for(j in NL:2){
        Sj = t(W[[j]][,-1,drop=FALSE]) %*% DELTA[[j]]
        DELTA[[j-1]] = dacf[[j-1]](Z[[j-1]]) * Sj
      }

      #############################
      ## (4) Evaluate derivatives
      #############################

      dE[[1]] = DELTA[[1]] %*% t(BZ0)
      for(i in 2:NL){
        dE[[i]] = DELTA[[i]] %*% t(rbind(1,Z[[i-1]]))
      }

      ## Update weights with Gradient Descent
      for(i in 1:NL){
        W[[i]] = W[[i]] - rho[i] * dE[[i]]
      }

 }#for bi
 
 erri = erri/N              ## MSE
 errors[epoch+firstEpoch-1] = erri

 #######################
 ## Do early stopping ##
 #######################
 if( doVal ){

   valNet = list(W=W,acf=acf,dacf=dacf,pdropout=pdropout)
   valY = nnOutput(valNet,ValX)

   valE = sqrt(mean((valY-ValT)^2))

   if(valE < MinE){
     MinE = valE
     BestW = W
     BestZ = Z
   } else {
     if( valE > MinE * 2 ) ValFailCount = ValFailCount + 1;
   }
   if(ValFailCount > maxValFail){

     msg='Reach max val fail'
     status = 3 ## early stopping
     break;
   }
 }##if doVal

 #################
 ## Do plot
 #################

 ne = epoch + firstEpoch - 1

 if (doPlot && (ne %% 50 == 0) ) {
   plot(errors[1:(firstEpoch+epoch-1)],
      xlab="Epoch",ylab="MSE",type="l",
      main=plotTitle)
 }

 ######################
 ## Check termination
 ######################

 if(is.nan(erri) ){
   print('\nReach NaN\n')
   msg = paste('epoch ', epoch, ': NaN')
   status = 0   ## NaN
   break;
 }##if
 DiffE = abs(Eold - erri)
 if( DiffE < tol ){
   msg = paste('epoch ', epoch, 
               ': |Eold - Enew| = ', DiffE, ' < tol ', tol)
   status = 2   ## Convergence
   break;
 }##if

 Eold = erri

}#for epoch


if(doVal){
  W = BestW
  Z = BestZ
}

return( list(W=W, Z=Z, 
     errors=errors[1:(firstEpoch+epoch-1)], N=firstEpoch+epoch-1,
     acf=acf, dacf=dacf, pdropout=pdropout, msg=msg, status=status) )
}##end nnTrain

######################################################################
### Calculate output of neural network.
######################################################################

nnOutput <- function (net,X, details=FALSE) {

  r = 1     
  if(!is.null(net$pdropout)){
    r = 1 - net$pdropout    ## r: prob. of staying
  }

  ## X is D x N
  BX = rbind(1,X)
  A = net$W[[1]] %*% BX

  NL = length(net$W)
  Z = vector('list',NL - 1)

  for(i in 1:(NL-1)){
    Z[[i]] = net$acf[[i]](A)
    BZ = rbind(1,Z[[i]])
    A = net$W[[i+1]] %*% BZ
  }
  Y = net$acf[[NL]](A)

  if(details){
    return(list(y=Y/r, Z=Z))
  }


  return(Y/r)
}

######################################################################
### Function to normalize inputs.
######################################################################

normalize <- function(X,means=apply(X,1,mean),stdevs=apply(X,1,sd),
                      returnParms=FALSE) {
  stdevs[stdevs==0] <- 1

  D <- nrow(X)
  N <- ncol(X)
  X <- (X - matrix(rep(means,N),D,N))/
	  matrix(rep(stdevs,N),D,N)
  if (returnParms)
    list(data=X,means=means,stdevs=stdevs)
  else
    X
}

################################################
## Pre-train
################################################

tmp <- function(){
X=train.X
p.noise=0.5
nHiddens=Ms 
rho=c(1e-4,1e-4,1e-4)
nEpochs=NRUN
acf= c(sigmoid, sigmoid, sigmoid)
dacf= c(dsigmoid, dsigmoid, dsigmoid)
aeAcf=logistic
initNet=dnNet
wmax=0.4
minRho=1e-12
etol=1e-4
minibatch=50
doPlot=TRUE
}

denoising = function(X, p.noise=0.5, nHiddens=c(20,10,10), 
         rho=c(1e-4,1e-4,1e-4), nEpochs=c(500,500,500),
         acf= c(sigmoid, sigmoid, sigmoid), 
         dacf= c(dsigmoid, dsigmoid, dsigmoid),
         aeAcf=logistic,
         wmax=0.4, minRho=1e-12, ##minRho=.Machine$double.eps,
         initNet=NULL, etol=1e-4,
         minibatch=100, 
         doPlot=TRUE){
## nEpochs: minimum is c(1,1,1)
## set any of nEpochs to keep pre-train on that layer.

##browser()

  ## X in [D x N]
  D = nrow(X);
  N = ncol(X);
  Mh = length(nHiddens);
  Ms = c(D, nHiddens)

  W = vector('list', Mh)
  Z = vector('list', Mh)
  costs = vector('list', Mh)

  status = matrix(0, 1, Mh)

  L2net0=vector('list', Mh)

  if(!is.null(initNet)){
    for(i in 1:Mh){
      L2net0[[i]] = list(W=list(initNet$W[[i]],
                              matrix( runif((1+Ms[i+1])*Ms[i], -wmax, wmax), 
                              Ms[i], (1+Ms[i+1]) )),
                         acf= c(acf[i], aeAcf), 
                         dacf= dacf[i], 
                       Z=list(initNet$Z[[i]]), 
                       errors=initNet$errors[[i]], 
                       status=initNet$status[i])
    }
  }
  
  iX = X

  for(m in 1:Mh){

##browser()

    neti = L2net0[[m]]
    DM = matrix(rbinom( Ms[m]*N, 1, 1 - p.noise ),Ms[m],N)    
    noisyX = iX*DM

    lr = rho[m]

    if(nEpochs[m] != 0){ ## if nEpochs[m] == 0, skip layer's pretrain

      while(lr > minRho){
        neti = nnTrain(noisyX,iX, nHiddens=Ms[1+m], 
          rho=c(lr,lr), nEpochs=nEpochs[m],
          acf= c(acf[m], aeAcf), 
          dacf= dacf[m], 
          net=L2net0[[m]], tol=etol, batchsize=minibatch, 
          plotTitle=paste('Autoencoding L',m))

        ## status: 0 = NaN, 1 = OK (reach nEpochs), 2 = convergence (|Eold - Enew| < tol), 3 = early stoppoing
        if(neti$status > 0) break else {
          lr = lr*0.1
          cat(m, ': reduce learning rate to ', lr, '\n')
        }##if
      }##while
    }##end if(nEpochs[m] != 0)
   
    status[m] = neti$status[1]  

    if( is.nan( sum( W[[m]] ) ) ) return()

     W[[m]] = neti$W[[1]]
     Z[[m]] = nnOutput(neti, iX, details=TRUE)$Z[[1]]
     costs[[m]] = neti$errors

  iX = Z[[m]]

  }##end for
  
list(W=W, Z=Z, errors=costs, status=status)
}##end function denoising

######################################
## Test
######################################

test1 = function(){
  load('do01.RData')
  datX = train.X
  train.TK = encode1ofK(train.T)
  datTK = train.TK

   net1 = nnTrain(datX,datTK, nHiddens=c(20,10,10), 
         rho=c(1e-5,1e-5,1e-5,1e-4), nEpochs=500,
         acf= c(sreclin, sreclin, sreclin, softmax), 
         dacf= c(dsreclin, dsreclin, dsreclin),
         wmax=0.4)

   y = nnOutput(net1, train.X, details=TRUE)

   train.y = nnOutput(net1, train.X)
   test.y = nnOutput(net1, test.X)

   trainY = decode1ofK(train.y,classes=c('0','1','2','3','4','5','6','7','8','9'))
   testY = decode1ofK(test.y,classes=c('0','1','2','3','4','5','6','7','8','9'))
   
   Ntrain = ncol(train.T)
   Ntest = ncol(test.T)

   trainCorrectRatio = sum( (trainY == train.T) )/Ntrain
   testCR = sum( (testY == test.T) )/Ntest
   testCR
}##end test1