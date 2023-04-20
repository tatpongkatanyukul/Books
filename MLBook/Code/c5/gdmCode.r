#######################################
## Gradient Descent with Momentum
#######################################

gdm <- function(grad, f, x0, alpha=0.01, beta=0.2, tol=0.00001, MaxN=1000, log=FALSE){
## Example
##   f  <- function(x){(x[1] - 4)^4 + (x[2] - 3)^2 + 4*(x[3] + 5)^4}
##   df <- function(x){ matrix(c( 4*(x[1] - 4)^3, 2*(x[2] - 3), 16*(x[3] + 5)^3 ) ,3,1)}
##   gdm(df, f, alpha=0.008, beta=0.2, x0=matrix(c(4,2,-1),3,1), log=TRUE, tol=1e-5)

  D <- length(x0)

  p <- matrix(0, D, 1)  
  logs <- matrix(c(0,p,x0),1+2*D,1)

  diff <- tol*2
  for (i in 1:MaxN) {

    gradF <- grad(x0)
    p <- - gradF + beta*p
    x <- x0 + alpha*p

    x0 <- x    
    logs <- cbind(logs, c(i,p,x))
    diff <- sqrt( mean((gradF)^2) )
    if (diff < tol) break
  }## for

  if(log){ return(logs) }
  else { return(x) }
}## end gdm