nnTrain.optim <- function(X, T, nHiddens, net=NULL, wmax=0.1, ...){

   D <- nrow(X)
   N <- ncol(X)
   K <- nrow(T)
   M <- nHiddens

  #######################
  ## initialize weights
  #######################
   if (is.null(net)) {
      ## Initialize weights
      W1 <- matrix(runif(M*(1+D),-wmax,wmax),M,1+D)
      W2 <- matrix(runif(K*(1+M),-wmax,wmax),K,1+M)
      net <- list(W1=W1, W2=W2)      
   } else {
      W1 <- net$W1
      W2 <- net$W2
   }#if

   ##############################
   ## Define objective function   
   ##############################
   ob.fn <- function(ws){

      cost = 0.5 * sum( (nnOutput(pack.w(ws), X) - T)^2 )

   return(cost)
   }##end ob.fn

   ##############################
   ## Define gradient function   
   ##############################
   gr.fn <- function(ws){
  
      net = pack.w(ws)
      W1 = net$W1
      W2 = net$W2

      dotX <- rbind(1,X)

      ## (1) Forward propagation
      Z <- sigmoid(W1 %*% dotX)
      dotZ <- rbind(1,Z)
      A <- W2 %*% dotZ
      Y <- A    ## regression output
   
      ## (2) Evaluate output delta
      DELTA2 <- Y - T

      ## (3) Backpropagate errors
      S <- t(W2[,-1,drop=FALSE]) %*% DELTA2
      DELTA1 <- dsigmoid(Z)*S

      ## (4) Evaluate derivatives
      dE2 <- DELTA2 %*% t(dotZ)
      dE1 <- DELTA1 %*% t(dotX)
  
      dws <- c(dE1, dE2)

   return(dws)
   }##end gr.fn

   ####################
   ## arg min ob.fn
   ####################

   ws <- unpack.w(net)
   res <- optim(ws, ob.fn, gr.fn, ...)
   
return(c(res, list(net=pack.w(res$par, D, M, K))))
}##end nnTrain.optim
