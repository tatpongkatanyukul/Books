init.weights.nugyenwidrow <- function(D, M, K, activeregion=c(-4, 4)){
  
  Wmax <- 0.7*M^(1/D);
  Wh.rand <- matrix(runif(M*D,-1,1),M,D)
  norm.factor <- sqrt(matrix(rowSums(Wh.rand^2),M,1)) %*% matrix(1, 1, D)
  Wh <- Wmax * Wh.rand/norm.factor  
  bh <- Wmax*seq(-1,1,len=M)*sign(runif(M, -1, 1))
    
  a.scale <- (activeregion[2] - activeregion[1])/2;
  a.offset <- (activeregion[2] + activeregion[1])/2;
  
  Wh <- Wh * a.scale
  bh <- bh * a.scale + a.offset
    
  W1 <- cbind(bh,Wh)    
  W2 <- matrix(runif(K*(1+M),-1,1),K,1+M)
  
  list(W1=W1, W2=W2)
}## end init.weights
