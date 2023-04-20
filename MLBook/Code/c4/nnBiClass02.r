## previous version: nnBinaryClass01.r
## this version: add early stopping
## created Aug 3rd, 2013

######################################################################
### Neural net for binary classification
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

nnTrain <- function (X,T,nHiddens,rhoh,rhoo,wmax=0.1,nEpochs,net=NULL,
                     graph=TRUE, plottitle='Error', NaNStop=TRUE,
					   	earlystopping=FALSE, early.tol=0.1, 
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
      Y <- 1/(1 + exp(-A))

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

      errors[epoch+firstEpoch-1] <- sum( -( T * log(Y) + (1 - T) * log(1-Y) ) )

      ne <- epoch + firstEpoch - 1

      if (graph && ne > 9 && (ne %% round(ne/10) == 0)) {
         plot(errors[1:(firstEpoch+epoch-1)],
			xlab="Epoch",ylab="Cost",type="l",main=plottitle)
      }

     if (NaNStop) {
        if( sum(is.nan(W1)) + sum(is.nan(W2)) > 0 ){
          return(list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors[1:epoch], epoch=epoch, dE2=dE2, dE1=dE1))
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

      }##end if

   }#for

if(earlystopping) return(best.net)

list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors[1:epoch])
}#nnTrain

######################################################################
### Calculate output of neural network.
######################################################################

nnOutput <- function (net,X) {

#cat('nnOuput:\n')
#print(dim(net$W1))
#print(dim(X))

  ## X is D x N
  dotX <- rbind(1,X)
  Z <- sigmoid( net$W1 %*% dotX )
  dotZ <- rbind(1,Z)
  A <- net$W2 %*% dotZ
  Y <- 1/(1 + exp(-A))

  return(Y)
}

######################################################################
### Unit output function and its derivative.
######################################################################

sigmoid <- function (z)  1 / (1 + exp(-z))

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
### XOR example
######################################################################

xorexample <- function() {
 inputs <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1),nrow=2)
 targets <- matrix(c(0, 1, 1, 0, 0, 0, 0, 1),nrow=2,byrow=TRUE)

 graphics.off() ### to close all graphics windows
 
 net <- nnTrain(inputs, targets, nHiddens=4, 
                rhoh=0.3, rhoo=0.03, wmax=0.3, nEpochs=5000)

 print("Inputs:")
 print(t(inputs))
 print("Targets:")
 print(t(targets))

 outs.train <- nnOutput(net,inputs)
 print("Outputs:")
 print(t(outs.train))

 print("Train RMSE")
 print(sqrt(mean(matrix(targets-outs.train)^2)))

 ##Plot first 4 hidden unit outputs.
 xs <- seq(0,1,length=20)
 ys <- seq(0,1,length=20)
 gridpoints <- t(as.matrix(expand.grid(xs,ys)))
 Z <- sigmoid(net$W1 %*% rbind(1,gridpoints))
 x11()
 p <- par(mfrow=c(2,2),mar=c(3,2,2,1))
 for (i in 1:4) {
   persp(xs,ys,matrix(Z[i,],20,20),xlab="x1",ylab="x2",
         zlab=paste("Hidden Unit",i),ticktype="detailed",
         theta=40,phi=30,d=10,font=2,lab=c(3,3,7))
 }# end for
 par(p)
 
 ## Plot two outputs of network
 A <- net$W2 %*% rbind(1,Z)
 y <- 1/(1 + exp(-A))
 x11()
 p <- par(mfrow=c(1,2),mar=c(3,2,2,1))
 for (i in 1:2) {
   persp(xs,ys,matrix(y[i,],20,20),xlab="x1",ylab="x2",
         zlab=paste("Output Unit",i),ticktype="detailed",
         theta=40,phi=30,d=10,font=2,lab=c(3,3,7))
 }
 par(p)
 
}

######################################################################
######################################################################

cat("For examples, run\n\n")
cat("  net <- xorexample()\n")