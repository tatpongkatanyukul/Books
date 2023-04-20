mse.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T, ...
  ## nntype ='regression'/'biclass'/'multiclass')
  ## Note: biclass, K must be 1. 
  
  X <- prob.info$X
  T <- prob.info$T
  N <- ncol(X)  
  K <- prob.info$K
    
  nntype <- 'regression'  ## default
  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }

  netW <- pack.w(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);
  A <- W2 %*% dotZ

  if(nntype == 'regression'){
    Y <- A;  
    mse <- mean((Y - T)^2)
  }##end if
  if(nntype == 'biclass'){
    Y <- 1/(1+exp(-A))
    costn <- T * log(Y) + (1-T) * log(1-Y)
    perfect.ids <- which(is.nan(costn)) 
    costn[perfect.ids] <- 0   ## a perfect result has 0 cost.
    mse <- -sum(costn)        
  }##end if
  if(nntype == 'multiclass'){
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
    mse <- -sum( T * log(Y) )
  }##end if
  
  return(mse)
}## end mse.w
