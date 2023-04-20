sigmoid <- function (a){
  return(  1 / (1 + exp(-a)) )
}##end sigmoid

dsigmoid <- function (z) {
  return( (1 - z)*z )
}##end dsigmoid

nnOutput <- function (net, X, nntype='regression') {
  X <- as.matrix(X)
  
  ## Forward Pass  
  dotX <- rbind(1, X)
  Z <- sigmoid( net$W1 %*% dotX )
  dotZ <- rbind(1,Z)
  N <- ncol(X)
  K <- nrow(net$W2)
  A <- net$W2 %*% dotZ;
  if(nntype == 'regression'){
    Y <- A
  }##end if
  if(nntype == 'biclass'){
    Y <- 1/(1+exp(-A))
  }##end if
  if(nntype == 'multiclass'){
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
  }##end if
  
  return(Y)
}
