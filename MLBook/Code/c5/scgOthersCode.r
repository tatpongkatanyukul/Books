encode.OK <- function(T, classes=sort(unique(as.character(T)))){
  K <- length(classes)
  N <- length(T)

  T.K <- (matrix(T,K,N, byrow=TRUE) == matrix(classes,K,N,byrow=FALSE))*1

  rownames(T.K) <- classes
  return(T.K)
}

decode.OK <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

hard.limit <- function(y, threshold=0.5){
  return(y > threshold)
}

sigmoid <- function (a)  1 / (1 + exp(-a)) 
dsigmoid <- function (z)  z * (1 - z) 

pack.w <- function(ws, D, M, K){
  ## W1: M x (1+D)
  ## W2: K x (1+M)
  
  W1 <- matrix(ws[1:(M*(1+D))], M, 1+D)
  W2 <- matrix(ws[-1:-(M*(1+D))], K, 1+M)
  
  list(W1=W1, W2=W2)
}## end pack.w

unpack.w <- function(net){
  ws <- c(net$W1, net$W2)
}## end unpack.w

init.weights <- function(D, M, K, wmax=0.1){
  W1 <- matrix(runif(M*(1+D),-wmax,wmax),M,1+D)
  W2 <- matrix(runif(K*(1+M),-wmax,wmax),K,1+M)
  
  list(W1=W1, W2=W2)
}## end init.weights



