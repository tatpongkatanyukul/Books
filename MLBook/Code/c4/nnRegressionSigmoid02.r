## previous version: nnRegression01.r
## this version: add early stopping feature.
## Created Aug 3rd, 2013.
## Last modified by Tatpong Katanyukul (COE, KKU), Aug 3rd, 2013
## status: it is working (Aug 17th, 2013)
##   

######################################################################
### Unit output function and its derivative.
######################################################################

sigmoid <- function (a){
  return(  1 / (1 + exp(-a)) )
  ##return( tanh(a) )
}##end sigmoid

dsigmoid <- function (z) {

  return( (1 - z)*z )
  ##return( 1 - z*z )
}##end dsigmoid

plot.sigmoid <- function(){

xs <- seq(-10, 10, len=100)

p=par(mfrow=c(2,2))
  plot(xs, tanh(xs), type='l', main='tanh')
  lines(xs, (1- xs*xs), col='red')

  plot(xs, 1 / (1 + exp(-xs)), type='l', main='sigmoid')
  lines(xs, xs * (1 - xs), col='red')

  plot(xs, tanh(xs), type='l', main='tanh/tanh\'')
  lines(xs, (1- tanh(xs)*tanh(xs)), col='red')

  plot(xs, 1 / (1 + exp(-xs)), type='l', main='sigmoid/sigmoid\'')
  lines(xs, (1-1 / (1 + exp(-xs)))*(1 / (1 + exp(-xs))), col='red')

  par(p)
}##end plot.sigmoid

######################################################################
### Neural net for nonlinear regression
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
                     graph=TRUE,net=NULL, 
					   	earlystopping=FALSE, early.tol=0.1, 
						val.X=NULL, val.T=NULL) {
#browser()

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

      ## (1) Forward propagation
      ## Calculate hidden unit outputs, Z, which is M x N
      Z <- sigmoid(W1 %*% dotX)

      ## Calculate output unit outputs, Y, which is K x N.
      dotZ <- rbind(1,Z)
      Y <- W2 %*% dotZ

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

      errors[epoch+firstEpoch-1] <- sqrt(mean(DELTA2^2)) # RMSE

      ne <- epoch + firstEpoch - 1

      if (graph && ne > 9 && (ne %% round(ne/10) == 0)) {
         plot(errors[1:(firstEpoch+epoch-1)],xlab="Epoch",ylab="RMSE",type="l",main="RMSE")
      }

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

list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors)
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
  Y <- net$W2 %*% dotZ
  return(Y)
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

######################################################################
### Examples
######################################################################

### sineexample(TRUE) to show changes in hidden outputs
sineexample <- function(details=FALSE) {
  
  graphics.off() # to close all graphical displays
  
  ## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  r <- normalize(train.X,returnParms=TRUE)
  train.Xn <- r$data
  
  test.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  test.T <- f(test.X)
  test.Xn <- normalize(test.X,r$means,r$stdevs)

  x11() # prepare one display for error plot in nnTrain
  nEpochs <- 10000

  if (details) {
    x11() # display for plotting hidden outputs

    log1 <- Sys.time()
    net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.03,rhoo=0.001, wmax=0.3, nEpochs=10)
    for (ep in 1:nEpochs) {
      net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.3, nEpochs=1, net=net)

      z <- sigmoid( net$W1 %*% rbind(1,test.Xn) )
      y <- nnOutput(net,test.Xn)
      dev.set(dev.next()) # to manage display device
      p <- par(mfcol=c(2,1)) # "set plot [par]ameters" to organize multiple plots
      matplot(t(test.X),t(z),type="l",lwd=2,
        main="Hidden Outputs", xlab='x', ylab='y')
      matplot(t(test.X),cbind(t(test.T),t(y)),type="l",lwd=2,main="Network Outputs v.s. Targets",
        xlab='x', ylab='y')
      dev.set(dev.next())
      #system("sleep 0.01")
    }

    log2 <- Sys.time()

    cat("Time spent:\n")
    print(log2 - log1)
  } else {
    net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.3, nEpochs=nEpochs)
  }
  
  outs.train <- nnOutput(net,train.Xn)
  outs.test <- nnOutput(net,test.Xn)

  RMSE.train <- sqrt(mean((train.T - outs.train)^2))
  RMSE.test <- sqrt(mean((test.T - outs.test)^2))

  x11()
  p<-par(mfrow=c(2,1))
  matplot(cbind(t(test.T),t(outs.test)),xlab="x",ylab="y",type="b",
          pch=c("o","x"), main="Test Data")
  matplot(cbind(t(train.T),t(outs.train)),xlab="x",ylab="y",type="b",
          pch=c("o","x"), main="Train Data")

  legend(x=5,y=5,legend=c('T','Y'), lty=c("solid","dashed"), 
          pch=c("o","x"), col=c('black','red'))

  par(p) # restore graphical parameters
  
  cat('RMSE train =',RMSE.train,', test =',RMSE.test,'\n')

  net
}

######################################################################
### XOR example
######################################################################

xorexample <- function() {
 inputs <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1),nrow=2)
 targets <- matrix(c(0, 1, 1, 0, 0, 0, 0, 1),nrow=2,byrow=TRUE)

 graphics.off() ### to close all graphics windows
 
 net <- nnTrain(inputs, targets, nHiddens=4, 
                rhoh=0.1, rhoo=0.01, wmax=0.1, nEpochs=4000)

 print("Inputs:")
 print(t(inputs))
 print("Targets:")
 print(t(targets))

 outs.train <- nnOutput(net,inputs)
 print("Outputs:")
 print(t(outs.train))

 print("Rounded outputs:")
 print(round(t(outs.train)))

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
 y <- net$W2 %*% rbind(1,Z)
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
cat("  net <- sineexample(TRUE)\n")
cat("  net <- sineexample()\n")
cat("  net <- xorexample()\n")