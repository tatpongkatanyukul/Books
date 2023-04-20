gd <- function(x, df, alpha=0.1, tol=1e-5, 
  MaxN=200, log=TRUE){

  D <- length(x)
  logs <- matrix(0, D, MaxN)

  for(i in 1:MaxN){

     new.x <- x - alpha * df(x)

     logs[,i] <- new.x

     if(is.infinite(new.x)) break;
     if(is.nan(new.x)) break;

     if( abs(new.x - x) < tol ){
        break;
     }
     x <- new.x

  }
  
  if(log){ return(logs[,1:i]) }
return(new.x)
}