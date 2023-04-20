steepestdescent <- function(grad, f, x0, tol=1e-5, 
   MaxN=500, log=FALSE){

  D <- length(x0)

  logs <- matrix(c(0,0,x0),2+D,1)

  diff <- tol*2
  for (i in 1:MaxN) {

    g <- function(a){ f(x0 - a*grad(x0)) }
    alpha <- goldensearch(g, a0=0, b0=1, 
       tol=tol, log=FALSE)

    gradF <- grad(x0)
    x <- x0 - alpha*gradF

    diff <- sqrt( mean((gradF)^2) )
    x0 <- x    
    logs <- cbind(logs, c(i,alpha,x0))

    if (diff < tol) break
  }## for

  if(log){ return(logs) }
  else { return(x) }
}## end steepestdescent

