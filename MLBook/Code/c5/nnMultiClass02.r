## previous version: nnMultiClassNaNStop01.r
## this version: add early stopping
## Created Aug 3rd, 2013.

######################################################################
### Neural net for multiple classification
######################################################################

## X: D x N
## W1: M x (1+D)
## W2: K x (1+M)
## Y: K x N

## dotX: (1+D) x N
## A: M x N
## Z: M x N
## dotZ: (1+M) x N

## DELTA2: K x N
## dotS: (1+M) x N
## DELTA1: M x N

## dE1: M x (1+D)
## dE2: K x (1+M)

nnTrain <- function (X,T,nHiddens,rhoh,rhoo,wmax=0.1,nEpochs,
   graph=TRUE,net=NULL,NaNStop=TRUE,
   earlystopping=FALSE, early.tol=0.3, 
   val.X=NULL, val.T=NULL) {

   D <- nrow(X)
   N <- ncol(X)
   K <- nrow(T)
   M <- nHiddens

   if (is.null(net)) {
      ## Initialize weights
      W1 <- matrix(runif(M*(1+D),-wmax,wmax),M,1+D)
      W2 <- matrix(runif(K*(1+M),-wmax,wmax),K,1+M)

      ## history of error
      errors <- matrix(0,1,nEpochs)
      firstEpoch <- 1  # this will be used in indexing of errors
      
   } else {
      W1 <- net$W1
      W2 <- net$W2
      errors <- matrix(c(net$errors, rep(0,nEpochs)),nrow=1)
      firstEpoch <- length(net$errors)+1

   }#if

   dotX <- rbind(1,X)

   best.net <- net;
   best.val.E <- Inf;
   
   for (epoch in 1:nEpochs){
     errsum <- matrix(0,K,1)

     ## (1) Forward propagation
     ## Calculate hidden unit outputs, Z, which is M x N
     Z <- sigmoid(W1 %*% dotX)

     ## Calculate output unit outputs, Y, which is K x N.
     dotZ <- rbind(1,Z)
     A <- W2 %*% dotZ
     Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)

     ## (2) Evaluate output delta
     DELTA2 <- Y - T

     ## (3) Backpropagate errors
     S <- t(W2[,-1,drop=FALSE]) %*% DELTA2
     DELTA1 <- dsigmoid(Z)*S

     ## (4) Evaluate derivatives
     dE2 <- DELTA2 %*% t(dotZ)
     dE1 <- DELTA1 %*% t(dotX)

     ## Update weights with Gradient Descent
     W2 <- W2 - rhoo * dE2
     W1 <- W1 - rhoh * dE1

     errors[epoch+firstEpoch-1] <- -sum( T * log(Y) )

     ne <- epoch + firstEpoch - 1

     if (graph && ne > 9 && (ne %% round(ne/10) == 0)) {
       plot(errors[1:(firstEpoch+epoch-1)],xlab="Epoch",ylab="Error",type="l",main="Error")
     }

     if (NaNStop) {
        if( sum(is.nan(W1)) + sum(is.nan(W2)) > 0 ){
          return(list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors, epoch=epoch, dE2=dE2, dE1=dE1))
        }
     }## end if (NaNStop)

     if (earlystopping) {
		 val.Y <- nnOutput(list(W1=W1, W2=W2), val.X)
		 val.E <- sum( (val.Y - val.T)^2 )

		 if (val.E < best.val.E) {
			best.net <- list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors)
			best.val.E <- val.E

		 } else { ## val.E >= best.val.E
			
			if( val.E > (1 + early.tol) * best.val.E ){
				## val.E is getting larger: sign of overfitting.
				break;
			}##end if

		 }##end if-else

     }##end if (earlystopping)

   }#for

if(earlystopping) return(best.net)

list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors)
}#nnTrain

######################################################################
### Calculate output of neural network.
######################################################################

nnOutput <- function (net,X) {

cat('nnOuput:\n')
print(dim(net$W1))
print(dim(X))

  K <- nrow(net$W2)
  N <- ncol(X)

  ## X is D x N
  dotX <- rbind(1,X)
  Z <- sigmoid( net$W1 %*% dotX )
  dotZ <- rbind(1,Z)
  A <- net$W2 %*% dotZ
  Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)

  return(Y)
}

######################################################################
### Unit output function and its derivative.
######################################################################

sigmoid <- function (a)  1 / (1 + exp(-a))

dsigmoid <- function (z)  z * (1 - z)

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

######################################################################
### Function to discretize the output
######################################################################

hardlimit <- function(x, cutoff=0.5){
  y <- apply(x > cutoff, c(1,2), as.numeric)
}

######################################################################
### Function to make indicators for every class
######################################################################

## Example, T may take any value from "Aikido", "Boxing", "Capoeira"
##          T.K <- makeIndicator(T) will transform T to take either 
##          c(1, 0, 0) for Aikido
##          c(0, 1, 0) for Boxing
##          c(0, 0, 1) for Capoeira
##
## Try
## T <- matrix(c("A","A","B","C","B"),nrow=1)
## T.K <- makeIndicators(matrix(c("A","A","B","C","B"),nrow=1))

makeIndicators <- function(T, classes=sort(unique(as.character(T)))){
  K <- length(classes)
  N <- length(T)

  T.K <- (matrix(T,K,N, byrow=TRUE) == matrix(classes,K,N,byrow=FALSE))*1

  rownames(T.K) <- classes
  return(T.K)
}

######################################################################
### Function to determine class from probabilities
######################################################################

## Y is K x N, each k corresponds to each class indicator

which.class <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

######################################################################
### XOR example
######################################################################

classifyexample <- function(rhoh=0.1, rhoo=0.01, nEpochs=5000) {
 inputs <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1),nrow=2)
 targets <- matrix(c("lo", "hi", "hi", "lo"), nrow=1)

 T.K <- makeIndicators(targets)
 net <- nnTrain(inputs, T.K, nHiddens=4, 
                rhoh=rhoh, rhoo=rhoo, wmax=0.1, nEpochs=nEpochs)

 y <- nnOutput(net,inputs)
 print(y)
 print(which.class(y))

}

######################################################################
######################################################################

cat("For examples, run\n\n")
cat("  classifyexample()\n")