## previous version: myscg08multiclassB.r (classification), myscg08.r (regression)
## this version: clean up myscg08multiclassB.r's mess (Aug 13th, 2013).
## In dE.w(.), try using DELTA2 <- ( Y - T ) * error.scaled.by.norm/N,
##   it works. But, DELTA2 <- ( Y - T ) * 2/N * error.scaled.by.norm seems to work a little bit better.
##   Confer runZIPscg01.r (later case) and runZIPscg02.r (first case).
## In mse.w(.) try using mse <- -sum( T * log(Y) ), it works.
##   But, mse <- -sum( T * log(Y) + (1-T) * log(1-Y) ) seems to be clearly better.
##   Confer runZIPscg02.r (later case) and runZIPscg03.r (first case).
##
## testing
##   nntype='regression' on sinexample() and XORexample(), checked Aug 13th, 2013.
##   nntype='biclass' on biclassexample(), checked Aug 13th, 2013.
##   nntype='multiclass' on runZIPscg04.r (Aug 13th,2013) {Also, multiclassexample(), based on the same code.}


######################################################################
### Function to make indicators for every class
######################################################################

## Example, T may take any value from "Aikido", "Boxing", "Capoeira"
##          T.K <- makeIndicator(T) will transform T to take either 
##          c(1, 0, 0) for Aikido
##          c(0, 1, 0) for Boxing
##          c(0, 0, 1) for Capoeira
##
## Try
## T <- matrix(c("A","A","B","C","B"),nrow=1)
## T.K <- makeIndicators(matrix(c("A","A","B","C","B"),nrow=1))

makeIndicators <- function(T, classes=sort(unique(as.character(T)))){
  K <- length(classes)
  N <- length(T)

  T.K <- (matrix(T,K,N, byrow=TRUE) == matrix(classes,K,N,byrow=FALSE))*1

  rownames(T.K) <- classes
  return(T.K)
}

######################################################################
### Function to determine class from probabilities
######################################################################

## Y is K x N, each k corresponds to each class indicator

which.class <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

####################################
## sigmoid function
####################################

sigmoid <- function (a)  tanh(a)     ## 1 / (1 + exp(-a))
dsigmoid <- function (a)  (1 - a*a)  ## a * (1 - a)

###########################################################################
# weight-packing function
###########################################################################

## tested: Feb 4th, 2012
pack.weights <- function(ws, D, M, K){
  ## W1: M x (1+D)
  ## W2: K x (1+M)
  
  W1 <- matrix(ws[1:(M*(1+D))], M, 1+D)
  W2 <- matrix(ws[-1:-(M*(1+D))], K, 1+M)
  
  list(W1=W1, W2=W2)
}## end pack.weights

###########################################################################
# weight-unpacking function
###########################################################################

## tested: Feb 4th, 2012
unpack.weights <- function(net){
  ws <- c(net$W1, net$W2)
}## end unpack.weights

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

###########################################################################
# Denormalization
###########################################################################

denormalize <- function(y, Tmin, Tmax, 
                        norm.min=matrix(-1, nrow(y), 1), 
                        norm.max=matrix(1, nrow(y), 1)){
## roughly tested: Sep 4th, 2012
  
  # y: K x N
  K <- nrow(y);
  N <- ncol(y);  

  Tmin <- matrix(Tmin, K, N, byrow=FALSE);
  Tmax <- matrix(Tmax, K, N, byrow=FALSE);
  norm.min <- matrix(norm.min, K, N, byrow=FALSE);
  norm.max <- matrix(norm.max, K, N, byrow=FALSE);
  
  Y.denorm <- (y - norm.min) * (Tmax - Tmin)/(norm.max - norm.min) + Tmin;

return(Y.denorm)
}#end denormalize

###########################################################################
# error function
###########################################################################

mse.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T, 
  ##  do.norm=list(X.min, X.max, T.min, T.max),
  ##  nntype)
  ##    If do.norm=NULL, it means no normalization.
  ## nntype ='regression'/'biclass'/'multiclass'

  do.norm <- FALSE
  
  X <- prob.info$X
  T <- prob.info$T

  N <- ncol(X)  
  K <- prob.info$K
  
  if(!is.null(prob.info$do.norm)){
    
    # Do normalization
    do.norm <- TRUE
    
    Xnorm <- normalize(prob.info$X, 
                       xmin=prob.info$do.norm$X.min, 
                       xmax=prob.info$do.norm$X.max)
    X <- Xnorm$norm;

  }# end if
  
  nntype <- 'regression'

  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }

  netW <- pack.weights(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);

  if(nntype == 'regression'){
    Y <- W2 %*% dotZ;

    if(do.norm){
      Y <- denormalize(Y, 
        Tmin=prob.info$do.norm$T.min,
        Tmax=prob.info$do.norm$T.max);
    }# end if
  
    mse <- mean((Y - T)^2)
  }##end if
  if(nntype == 'biclass'){
    A <- W2 %*% dotZ
    Y <- 1/(1+exp(-A))

    mse <- -sum( T * log(Y) + (1-T) * log(1-Y) )
  }##end if
  if(nntype == 'multiclass'){
    A <- W2 %*% dotZ
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
    mse <- -sum( T * log(Y) + (1-T) * log(1-Y) )
  }##end if
  
  return(mse)
}## end mse.w

###########################################################################
# derivative error function
###########################################################################

dE.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T, 
  ## do.norm=list(X.min, X.max, T.min, T.max))
  ##    If do.norm=NULL, it means no normalization.

  do.norm <- FALSE
  
  X <- prob.info$X
  T <- prob.info$T

  N <- ncol(X)  
  K <- prob.info$K
  
  if(!is.null(prob.info$do.norm)){
    
    # Do normalization
    do.norm <- TRUE

    Xnorm <- normalize(prob.info$X, 
                       xmin=prob.info$do.norm$X.min, 
                       xmax=prob.info$do.norm$X.max)
    X <- Xnorm$norm;
    
  }# end if

  nntype <- 'regression'

  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }
    
  netW <- pack.weights(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  ## Forward pass
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);

  error.scaled.by.norm <- 1;

  if(nntype == 'regression'){
    Y <- W2 %*% dotZ;

    if(do.norm){
      Y <- denormalize(Y, 
        Tmin=prob.info$do.norm$T.min,
        Tmax=prob.info$do.norm$T.max);

      error.scaled.by.norm <- matrix((prob.info$do.norm$T.max - prob.info$do.norm$T.min)/2, nrow=K, ncol=N);
    }# end if
  }##end if
  if(nntype == 'biclass'){
    A <- W2 %*% dotZ
    Y <- 1/(1+exp(-A))

  }##end if
  if(nntype == 'multiclass'){
    A <- W2 %*% dotZ
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)

  }##end if
  
  ## Backward pass
  
  DELTA2 <- ( Y - T ) * error.scaled.by.norm/N
  ##DELTA2 <- ( Y - T ) * 2/N * error.scaled.by.norm;  # K x N
  
  S <- t(W2[,-1,drop=FALSE]) %*% DELTA2      # M x N
  DELTA1 <- dsigmoid(Z)*S                    # M x N
  
  dE2 <- DELTA2 %*% t(dotZ)                  # K x (1+M)
  dE1 <- DELTA1 %*% t(dotX)                  # M x (1+D)
  
  return(c(dE1, dE2))
}## end dE.w

###########################################################################
# Uniform distribution Weight initialization
###########################################################################

init.weights <- function(D, M, K, wmax=0.1){
  W1 <- matrix(runif(M*(1+D),-wmax,wmax),M,1+D)
  W2 <- matrix(runif(K*(1+M),-wmax,wmax),K,1+M)
  
  list(W1=W1, W2=W2)
}## end init.weights

###########################################################################
# Nugyen-Widrow weight initialization
###########################################################################

init.weights.nugyenwidrow <- function(D, M, K, activeregion=c(-2, 2)){
## tanh: activeregion=c(-2,2), per Matlab
## logsig: 1 / (1 + exp(-a)): activeregion=c(-4,4), per Matlab
  
  Wmax <- 0.7*M^(1/D);
  Wh.rand <- matrix(runif(M*D,-1,1),M,D)
  norm.factor <- sqrt(matrix(rowSums(Wh.rand^2),M,1)) %*% matrix(1, 1, D)
  Wh <- Wmax * Wh.rand/norm.factor
  
  bh <- Wmax*seq(-1,1,len=M)*sign(runif(M, -1, 1))
  ## other options
  ## (a) bh <- Wmax*seq(-1,1,len=M)*sign(Wh[,1])   # per Matlab
  ## (b) bh <- runif(M, -Wmax, Wmax)   # per Nugyen-Widrow paper
  
  
  ## Scale W1 so that a = W1 %*% dotX would fall within the active region of the transfer function.
  ## This is per Matlab.
  ## x of range -1 to 1 is mapping to y of range ymin to ymax
  ##
  ## (y - ymin)/(ymax - ymin) = (x - -1)/2
  ## y = (x + 1) * (ymax - ymin)/2 + ymin
  ## y = (ymax - ymin)/2 x + (ymax + ymin)/2
  ## y = (ymax - ymin)/2 (w xi + b) + (ymax + ymin)/2
  ## w' = (ymax - ymin)/2 * w
  ## b' = (ymax - ymin)/2 * b + (ymax + ymin)/2
  
  a.scale <- (activeregion[2] - activeregion[1])/2;
  a.offset <- (activeregion[2] + activeregion[1])/2;
  
  Wh <- Wh * a.scale
  bh <- bh * a.scale + a.offset
  
  
  W1 <- cbind(bh,Wh)
  
  
  W2 <- matrix(runif(K*(1+M),-1,1),K,1+M)
  
  list(W1=W1, W2=W2)
}## end init.weights

###########################################################################
# Neural Network Output
###########################################################################

nnOutput <- function (net, X, do.norm=NULL, nntype='regression') {

  X <- as.matrix(X)

  boolean.do.norm <- FALSE
  
  if(!is.null(do.norm)){
    
    boolean.do.norm <- TRUE

    Xnorm <- normalize(X, 
                       xmin=do.norm$X.min, 
                       xmax=do.norm$X.max)
    
    X <- Xnorm$norm;
    
  } 
  
  ## Forward Pass
  
  dotX <- rbind(1, X)
  Z <- sigmoid( net$W1 %*% dotX )
  dotZ <- rbind(1,Z)

  N <- ncol(X)
  K <- nrow(net$W2)

  if(nntype == 'regression'){
    Y <- net$W2 %*% dotZ;

    if(boolean.do.norm){
      Y <- denormalize(Y, 
        Tmin=do.norm$T.min,
        Tmax=do.norm$T.max);

    }# end if
  }##end if
  if(nntype == 'biclass'){
    A <- net$W2 %*% dotZ
    Y <- 1/(1+exp(-A))

  }##end if
  if(nntype == 'multiclass'){
    A <- net$W2 %*% dotZ
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)

  }##end if
  
  return(Y)
}

###########################################################################
# Scaled Conjugate Gradient Algorithm
###########################################################################

scg <- function(df, f, x1, prob.info=NULL, term.fn=nnTermination, term.info=NULL,
                MaxN=1000, sigma=5.0e-5, lambda1=5.0e-7,
                log=FALSE, doplot=TRUE, nntype='regression'){
                # default values of MaxN, sigma, and lambda1 per Matlab's trainscg.m

  term.results <- NULL
  
  x1 <- as.matrix(x1)
  
  ## Martin F. Moller,
  ## A Scaled Conjugate Gradient Algorithm 
  ## for Fast Supervised Learning, Neural Networks
  ## Vol. 6, 1993, pp. 525--533.
  
  ##################################### 
  ## Algorithm from pp. 530--531.
  ## 1. Initialize.
  #####################################
  
  xk <- x1
  lambdak <- lambda1       ## used to regulate indefiniteness of the Hessian
  
  lambdabar <- 0           ## used to regulate indefiniteness of the Hessian
  rk <- -df(xk, prob.info)
  pk <- rk
  success <- TRUE
  
  NR <- nrow(xk)  ## size of xk: NR x 1
  
  logs <- rbind(0, success, 0, 
                0, lambdabar, lambdak, 
                0, 0, 0, 0)
  
  rownames(logs) <- c("k", "success", "sigmak", 
                      "deltak", "lambdabar", "lambdak",
                      "muk", "alphak", "DELTA", "Ew")
  
  xtrace <- rbind(0, xk)

  #####################################
  ## 2. If success = true, calculate derivatives.
  #####################################
  
  for(k in 1:MaxN){
    
    pkSq <- t(pk) %*% pk
    norm.pk <- sqrt(pkSq)
    
    if(pkSq < .Machine$double.eps){## per Chuck's practice
      ## Terminate
      if(log){ return( list(logs=logs, xtrace=xtrace, x=xk, note="reach machine precision") ) } else { return(xk) }  
    }##end if(pkSq ...)
    
    
    if(success){
      
      sigmak <- as.numeric(sigma/norm.pk)
      
      dEplus <- df(xk + sigmak*pk, prob.info)
      dE <- df(xk, prob.info)
      
      sk <- (dEplus - dE)/sigmak
      deltak <- as.numeric(t(pk) %*% sk)
      
    }## if(success)
    
    #####################################
    ## 3. Scale deltak.
    #####################################
    
    deltak <- deltak + (lambdak - lambdabar) * pkSq
    
    if(is.nan(deltak)) browser()  ## per Chuck's practice
    
    #####################################
    ## 4. If deltak <= 0, make H positive definite.
    #####################################
    
    if(deltak <= 0){
      
      cat("\ndebug: step 4: deltak = ", deltak)     
      lambdabar <- 2*(lambdak - deltak/pkSq)
      
      deltak <- - deltak + lambdak*pkSq
      
      lambdak <- lambdabar
      
    }## if(deltak <= 0)
    
    #####################################
    ## 5. Calculate step size.
    #####################################
    
    muk <- t(pk) %*% rk
    alphak <- as.numeric(muk/deltak)
        
    #####################################
    ## 6. Calculate the comparison parameter.
    #####################################
    xnew <- xk + alphak*pk
    Eplus <- f(xnew, prob.info)
    E <- f(xk, prob.info)
    
    DELTA <- 2*deltak*(E - Eplus)/(muk^2)
    
	if(is.nan(DELTA)) browser()  

    #####################################
    ## 7. If DELTA >= 0, reduce error.
    #####################################
    
    if(DELTA >= 0){
      
      ## xnew has been in step 6
      rnew <- -df(xnew, prob.info)   ## gradient descent direction
      
      lambdabar <- 0
      success <- TRUE
      
      if(k %% NR == 0){
        ## restart algorithm
        pnew <- rnew
      }else{
        beta <- as.numeric( (t(rnew) %*% rnew - t(rnew) %*% rk) /muk )
        pnew <- rnew + beta*pk
      }## end if(k %% NR == 0)
      
      if(DELTA >=0.75){
        lambdak <- lambdak/4
      }## end if(DELTA >=0.75)
      
      ## only DELTA >= 0 (good approximation) that we take this update.
      xk <- xnew
      rk <- rnew
      pk <- pnew
      
    }else{## DELTA < 0
      
      lambdabar <- lambdak
      success <- FALSE
      
      ## when DELTA < 0 (bad approximation), 
      ## we adjust our variables to correct positive definiteness.
      ## And, redo it again.
      
    }## end if(DELTA >= 0)
    
    #####################################
    ## 8. If DELTA < 0.25, increase scale.
    #####################################
    
    if(DELTA < 0.25){
      
      lambdak <- lambdak + deltak*(1 - DELTA)/pkSq
      
    }## end if(DELTA < 0.25)
    
    #####################################
    ## 9. If rk != 0, go for the next round.
    ##    Otherwise, stop.
    #####################################
    
    logs <- cbind(logs, rbind(k, success, sigmak,
                              deltak, lambdabar, lambdak,
                              muk, alphak, DELTA, E))
    
    xtrace <- cbind(xtrace, rbind(k, xk))
    
    term.results <- term.fn(xk, term.results, term.info, 
		doplot=doplot, E.logs=logs[10,-1], nntype=nntype)
    
    if (term.results$boolean){
      
      ## Terminate
      if(log){ return( list(logs=logs, xtrace=xtrace, x=xk, term.results=term.results) ) } else { return(xk) }  
      
    }## end if

  ##cat(paste(k, ': E = ', E, '\n'))
  }## end for(k in 1:MaxN)
  
  print("Reach maximum iterations")
  if(log){ return( list(logs=logs, xtrace=xtrace, x=xk, term.results=term.results, note="reach max iterations") ) }
  else { return(xk) }  
  
  
}## end scg

#####################################################################################
# Termination function
#####################################################################################
# First created: Aug 20th, 2012
# tested: ?

nnTermination <- function(xk, term.data=NULL, 
                          term.info=NULL,
                          tol=0, min.grad=1e-12, max.val.fail=8,
                          doplot=FALSE, E.logs=NULL	, nntype='regression'
                          ){

  if(is.null(term.data)){
    term.count <- 0
    
    val.fail <- 0
    val.old.perf <- Inf    
    
    val.best.perf <- Inf
    val.best.net <- NULL
    val.best.term.count <- 0    
  } else {
    term.count <- term.data$term.count
    
    val.fail <- term.data$val.fail
    val.old.perf <- term.data$val.old.perf
    
    val.best.perf <- term.data$val.best.perf
    val.best.net <- term.data$val.best.net
    val.best.term.count <- term.data$val.best.term.count
    
  }#end if
  
  term.count <- term.count + 1
  
  grad.mse <- mean( dE.w(xk, prob.info=term.info$prob.info)^2 )
  perf <- mse.w(xk, prob.info=term.info$prob.info)
  
  term.boolean <- FALSE
  
  if(perf <= tol){ term.boolean <- TRUE }
  if(grad.mse <= min.grad){ term.boolean <- TRUE }
  
  if( !is.null(term.info$val.X) && !is.null(term.info$val.T) ){
    val.X <- term.info$val.X
    val.T <- term.info$val.T
    D <- term.info$prob.info$D
    M <- term.info$prob.info$M
    K <- term.info$prob.info$K
    
    net <- pack.weights(xk, D, M, K)
    
    val.y <- nnOutput(net, val.X, 
       do.norm=term.info$prob.info$do.norm, nntype=nntype)
    
	if(nntype == 'regression'){
	    val.mse <- mean( (val.y - val.T)^2 )
    } else { ## 'biclass' or 'multiclass'
	    val.mse <- -sum( val.T * log(val.y) + (1-val.T) * log(1-val.y) )
    }

    if(val.mse > val.old.perf){ val.fail <- val.fail + 1 }
    if(val.fail >= max.val.fail){ term.boolean <- TRUE }
  
    if(val.mse < val.best.perf){
      val.best.perf <- val.mse
      val.best.net <- net
      val.best.term.count <- term.count  
    }#end if
    val.old.perf <- val.mse

  }#end if
  
  if(doplot){

	k <- length(E.logs)
    if(k > 9 && (k %% round(k/10) == 0))
		plot(1:length(E.logs), E.logs, xlab='epoch', ylab='E', type='l')
  }
  
return(list(boolean=term.boolean, term.count=term.count, 
         val.fail=val.fail, val.old.perf=val.old.perf,
         val.best.perf=val.best.perf, val.best.net=val.best.net, 
         val.best.term.count=val.best.term.count))
}# end function nnTermination


###########################################################################
# Sine Example
###########################################################################

sinexample <- function(){
  ## sine function to fit
  fsin <- function (x)  8 * sin(x) + rnorm(length(x))
  
  N <- 80
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- fsin(train.X)

  Xn <- normalize(train.X)
  Tn <- normalize(train.T)
  
  val.X <- matrix(seq(0,4*pi,len=round(N/10)),nrow=1)
  val.T <- fsin(val.X)  
  
  test.X <- matrix(seq(0,4*pi,len=round(N/10)),nrow=1)
  test.T <- fsin(test.X)
  
  D <- 1
  M <- 10
  K <- 1
    
  ws <- init.weights.nugyenwidrow(D, M, K)
  
  ws0 = c(ws$W1, ws$W2)
  
  prob.info <- list(D=D, M=M, K=K, X=train.X, T=train.T, 
    do.norm=list(X.min=Xn$min, X.max=Xn$max, 
                 T.min=Tn$min, T.max=Tn$max),
    nntype='regression')
  
  term.info <- list(prob.info=prob.info, val.X=val.X, val.T=val.T) 

  wsk.log <- scg(dE.w, mse.w, ws0, prob.info=prob.info, 
    term.fn=nnTermination, term.info=term.info, log=TRUE,
    nntype='regression')

  net <- wsk.log$term.results$val.best.net;
  
  train.y <- nnOutput(net, train.X, 
    do.norm=prob.info$do.norm, nntype='regression')
  test.y <- nnOutput(net, test.X, 
    do.norm=prob.info$do.norm, nntype='regression')
  
  X11()
  p=par(mfrow=c(2,1))
  plot(train.X, train.T, type='l', col='red')
  points(train.X, train.y, col='blue')
  
  
  plot(test.X, test.T, type='l', col='red')
  points(test.X, test.y, col='blue')
  
  par(p)
  
  ## Aug 13th, 2013: checked.
}## end sinexample()

###########################################################################
# Exclusive-OR Example
###########################################################################

XORexample <- function(){
  D <- 2
  M <- 4
  K <- 2
  inputs <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1),nrow=2)
  targets <- matrix(c(0, 1, 1, 0, 0, 0, 0, 1),nrow=2,byrow=TRUE)
  Xn <- normalize(inputs)
  Tn <- normalize(targets)
  
  ws <- init.weights.nugyenwidrow(D, M, K)  
  #ws <- init.weights(D, M, K)
  
  ws0 = c(ws$W1, ws$W2)  
  prob.info <- list(D=D, M=M, K=K, X=inputs, T=targets, 
    do.norm=list(X.min=Xn$min, X.max=Xn$max, 
                 T.min=Tn$min, T.max=Tn$max),
    nntype='regression')  
  term.info <- list(prob.info=prob.info, val.X=NULL, val.T=NULL) 
  
  wsk.log <- scg(dE.w, mse.w, ws0, prob.info=prob.info, 
    term.fn=nnTermination, term.info=term.info, log=TRUE, 
    nntype='regression')
  
  net <- pack.weights(wsk.log$x, D, M, K)
  
  y <- nnOutput(net, inputs, do.norm=prob.info$do.norm, nntype='regression')

  X11()
  res <- 10;  
  xs <- seq(0,1,length=res)
  ys <- seq(0,1,length=res)
  gridpoints <- t(as.matrix(expand.grid(xs,ys)))
  
  zs <- nnOutput(net, gridpoints, do.norm=prob.info$do.norm)   
    
  p=par(mfrow=c(2,1))
  persp(xs,ys,matrix(zs[1,],res,res),xlab="x1",ylab="x2",
        zlab=paste("Output Unit"),
        theta=40,phi=30,d=10,font=2,lab=c(3,3,7), main='XOR')
  persp(xs,ys,matrix(zs[2,],res,res),xlab="x1",ylab="x2",
        zlab=paste("Output Unit"),ticktype="detailed",
        theta=40,phi=30,d=10,font=2,lab=c(3,3,7), main='AND')
  par(p)
}##end XORexample()

###########################################################################
# biclass Example
###########################################################################

biclassexample <- function(){
  cat('classify either a given record is whether Iris virginica.\n')

  N = nrow(iris)

  X <- t(iris[,-5])  ## D x N
  Y <- matrix((iris[,5] == 'virginica'), nrow=1) ## 1 x N

  train.ids <- sample(1:N, round(N*0.7))

  train.X <- X[, train.ids[-(1:10)]]
  train.Y <- Y[, train.ids[-(1:10)], drop=FALSE]

  val.X <- X[, train.ids[1:10]]
  val.Y <- Y[, train.ids[1:10], drop=FALSE]

  test.X <- X[, -train.ids]
  test.Y <- Y[, -train.ids, drop=FALSE]

  #####################
  ## prepare for scg
  #####################
  Xn <- normalize(train.X)
  Tn <- normalize(train.Y)

  ###################
  ## init w
  ###################
  D <- 4
  M <- 4
  K <- 1
  ws <- init.weights.nugyenwidrow(D, M, K)  
  
  ws0 = c(ws$W1, ws$W2)  
  prob.info <- list(D=D, M=M, K=K, 
     X=train.X, T=train.Y, 
     do.norm=list(X.min=Xn$min, X.max=Xn$max, 
                  T.min=Tn$min, T.max=Tn$max),
     nntype='biclass'
  )  
  term.info <- list(prob.info=prob.info, 
    val.X=val.X, val.T=val.Y) 

  ############
  ## scg
  ############

  log1 <- Sys.time()
  wsk.log <- scg(dE.w, mse.w, ws0, MaxN=3000,
     prob.info=prob.info, term.fn=nnTermination, 
     term.info=term.info, log=TRUE, nntype='biclass')
  log2 <- Sys.time()

  print(log2 - log1)

  ############
  ## test
  ############

  net <- pack.weights(wsk.log$x, D, M, K)
  
  y <- nnOutput(net, test.X, do.norm=prob.info$do.norm,
         nntype='biclass')  

  y.class <- (y > 0.5)

  correct.perc <- sum( y.class == test.Y )/length(test.Y) * 100
  cat('Correction % =', round(correct.perc,1), '\n')

  X11()
  plot(test.X[3,test.Y==1], test.X[4,test.Y==1],
    xlim=c(1,7), ylim=c(0.1,2.5), xlab='p.L', ylab='p.W',
    pch='*', col='blue', , main=paste('Test set (correct ',
    round(correct.perc,1), '%)'))
  points(test.X[3,test.Y==0], test.X[4,test.Y==0],
    pch='+', col='black')

  points(test.X[3,y.class==1], test.X[4,y.class==1],
   pch='O',col='red')

  legend(5,1, c('real virginica','others','nn virginica'),
    pch=c('*', '+', 'O'), col=c('blue', 'black', 'red'))
}##end biclassexample

###########################################################################
# multiclass Example
###########################################################################

multiclassexample <- function(zippath='./imageZIP'){

  cat('This example requires ZIP image data.\n')

  D <- 256
  M <- 40
  K <- 10

  train.path <- paste(zippath, '/zip.train', sep='')
  if( !file.exists(train.path) ){
    cat(paste('No file: ', train.path, '\n'))
    return()
  }


  train.zip <- read.table('./imageZIP/zip.train')
  numzip <- apply(train.zip,c(1,2),as.numeric)

  X <- t(numzip[,-1])  # X is D x N
  T <- t(numzip[,1])   # T is 1 x N
  N <- ncol(X)

  numzip <- apply(train.zip,c(1,2),as.numeric)

  X <- t(numzip[,-1])  # X is D x N
  T <- t(numzip[,1])   # T is 1 x N
  N <- ncol(X)

  id.rand <- sample(N)

  marker <- round(0.75*N)

  train.X <- X[,id.rand[1:marker]]
  train.T <- T[,id.rand[1:marker]]

  val.X <- X[,id.rand[-1:-marker]]
  val.T <- T[,id.rand[-1:-marker]]
  val.Tk <- makeIndicators(val.T)

  inputs <- train.X
  targets <- makeIndicators(train.T)   # T is 10 x N

  test.path <- paste(zippath, '/zip.test', sep='')
  if( !file.exists(test.path) ){
    cat(paste('No file: ', test.path, '\n'))
    return()
  }


  test.zip <- read.table(test.path)
  test.num <- apply(test.zip,c(1,2),as.numeric)
  test.X <- t(test.zip[,-1])  # X is D x N
  test.T <- makeIndicators(t(test.zip[,1]))   # T is 10 x N
  N.test <- ncol(test.X)

  #####################################
  ## to get normalize parameters,
  ## e.g., X.min, X.max, T.min, T.max
  #####################################
  Xn <- normalize(inputs)
  Tn <- normalize(targets)
  
  ###################
  ## init w
  ###################
  ws <- init.weights.nugyenwidrow(D, M, K)  
  
  ws0 = c(ws$W1, ws$W2)  
  prob.info <- list(D=D, M=M, K=K, 
     X=inputs, T=targets, 
     do.norm=list(X.min=Xn$min, X.max=Xn$max, 
                  T.min=Tn$min, T.max=Tn$max),
     nntype='multiclass'
  )  
  term.info <- list(prob.info=prob.info, val.X=val.X, val.T=val.Tk) 
   
  ############
  ## scg
  ############

  cat('Training process may take up to a few minutes.\n')

  log1 <- Sys.time()
  wsk.log <- scg(dE.w, mse.w, ws0, MaxN=3000,
     prob.info=prob.info, term.fn=nnTermination, 
     term.info=term.info, log=TRUE, nntype='multiclass')
  log2 <- Sys.time()

  print(log2 - log1)

  ########################################
  ## test
  ########################################

  net <- pack.weights(wsk.log$x, D, M, K)
  
  y <- nnOutput(net, test.X, do.norm=prob.info$do.norm,
         nntype='multiclass')

  y.class <- which.class(y, c('0','1','2','3','4','5','6','7','8','9'))
  correct.perc <- sum(y.class == which.class(test.T))/N.test * 100

  cat(paste('Correct %', round(correct.perc,2), '\n'))

  ########################################
  ## plot
  ########################################

  ###########################
  ## display image function
  ###########################

  disp.img <- function(x, p.title='image'){

   xs <- seq(1,16)
   ys <- xs
   z <- matrix(x, 16, 16)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr),main=p.title)
  }##end disp.img

  X11()
  
  rids <- sample(1:ncol(test.X), 10)
  p=par(mfrow=c(2,5))
  for(i in 1:10){
    disp.img(test.X[,rids[i]], 
    paste('image of ', which.class(test.T)[rids[i]],
    '\nnn y = ', y.class[rids[i]]))
  }
  par(p)

return(net)
}##end multiclassexample

############################
## interaction
############################

cat('See sinexample()\n')
cat(' or XORexample()\n')
cat(' or biclassexample()\n')
cat(' or multiclassexample()\n')
