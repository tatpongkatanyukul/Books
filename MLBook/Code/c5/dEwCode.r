dE.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T, nntype)

  X <- prob.info$X
  T <- prob.info$T
  N <- ncol(X)  
  K <- prob.info$K
  
  nntype <- 'regression'
  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }
    
  netW <- pack.w(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  ## Forward pass
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);
  A <- W2 %*% dotZ

  if(nntype == 'regression'){
    Y <- A
  }##end if
  if(nntype == 'biclass'){
    Y <- 1/(1+exp(-A))
  }##end if
  if(nntype == 'multiclass'){
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
  }##end if
  
  ## Backward pass  
  DELTA2 <- ( Y - T ) /N  
  S <- t(W2[,-1,drop=FALSE]) %*% DELTA2      # M x N
  DELTA1 <- dsigmoid(Z)*S                    # M x N  
  dE2 <- DELTA2 %*% t(dotZ)                  # K x (1+M)
  dE1 <- DELTA1 %*% t(dotX)                  # M x (1+D)
  
  return(c(dE1, dE2))
}## end dE.w
