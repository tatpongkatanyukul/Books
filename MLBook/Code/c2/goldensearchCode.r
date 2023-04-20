goldensearch <- function(f, a0, b0, tol=0.1, 
   rho=0.382, log=FALSE){

   diff <- b0 - a0
   a1 <- a0 + rho*diff
   b1 <- b0 - rho*diff

   fa0 <- f(a0)
   fb0 <- f(b0)
   fa1 <- f(a1)
   fb1 <- f(b1)

   logs <- matrix(c(a0,fa0,b0,fb0),4,1)   

   while (diff > tol) {
      if( fa1 < fb1 ) {
        b0 <- b1
        fb0 <- fb1
        b1 <- a1
        fb1 <- fa1

        diff <- b0 - a0
        a1 <- a0 + rho*diff
        fa1 <- f(a1)

      } else { ## fa1 >= fb1
        a0 <- a1
        fa0 <- fa1
        a1 <- b1
        fa1 <- fb1

        diff <- b0 - a0
        b1 <- b0 - rho*diff
        fb1 <- f(b1)     

      } ## end if

      logs <- cbind(logs, matrix(c(a0,fa0,b0,fb0),4,1))
   }## end while

  rownames(logs) <- c("a0", "f(a0)","b0", "f(b0)")
  if(log) { return(logs) }
  else { return((a0+b0)/2) }
}## goldensearch