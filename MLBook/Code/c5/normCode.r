###########################################################################
# Normalization
###########################################################################

normalize <- function(X, 
                      xmin=matrix(apply(X, 1, min), nrow(X), 1),
                      xmax=matrix(apply(X, 1, max), nrow(X), 1),
                      norm.min=matrix(-1, nrow(X), 1),
                      norm.max=matrix(1, nrow(X), 1)
                      ){
## roughly tested: Sep 4th, 2012  
## X : matrix of D x N
## xmin : matrix of D x 1
## xmax : matrix of D x 1
  
  # x: D x N
  D <- nrow(X);
  N <- ncol(X);
  
  Mmin <- matrix(xmin, D, N, byrow=FALSE);
  Mmax <- matrix(xmax, D, N, byrow=FALSE);
  normMmin <- matrix(norm.min, D, N, byrow=FALSE);
  normMmax <- matrix(norm.max, D, N, byrow=FALSE);

  xnorm <- (X - Mmin) * (normMmax - normMmin)/(Mmax - Mmin) + normMmin;
  
  return(list(norm=xnorm, min=xmin, max=xmax, norm.min=norm.min, norm.max=norm.max))
}#end normalize


normalize.s <- function(X,means=apply(X,1,mean),stdevs=apply(X,1,sd),
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