normalize <- function(X,means=apply(X,1,mean),stdevs=apply(X,1,sd),
                      returnParms=FALSE) {
  stdevs[stdevs==0] <- 1

  D <- nrow(X)
  N <- ncol(X)
  X <- (X - matrix(rep(means,N),D,N))/
	  matrix(rep(stdevs,N),D,N)
  if (returnParms)
    list(data=X,means=means,stdevs=stdevs)
  else
    X
}
