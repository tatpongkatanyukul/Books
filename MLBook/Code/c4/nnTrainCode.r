nnTrain <- function (X,T,nHiddens,rhoh,rhoo,
  wmax=0.1,nEpochs=2000,graph=TRUE,net=NULL) {

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

   }#for

list(W1=W1, W2=W2, Z=Z, Y=Y, errors=errors)
}#nnTrain