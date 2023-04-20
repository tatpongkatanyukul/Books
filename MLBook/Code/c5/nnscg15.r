## previous version: myscg12 (working fine w/ issues of NaN, May 9th, 2014)
## this version: fix NaN issues in myscg12.
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

encode.OK <- function(T, classes=sort(unique(as.character(T)))){
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

decode.OK <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

######################################################################
### Function to determine biclass from probabilities
######################################################################

hard.limit <- function(y, threshold=0.5){
  return(y > threshold)
}

####################################
## sigmoid function
####################################

sigmoid <- function (a)  1 / (1 + exp(-a)) 
dsigmoid <- function (z)  z * (1 - z) 

###########################################################################
# weight-packing function
###########################################################################

## tested: Feb 4th, 2012
pack.w <- function(ws, D, M, K){
  ## W1: M x (1+D)
  ## W2: K x (1+M)
  
  W1 <- matrix(ws[1:(M*(1+D))], M, 1+D)
  W2 <- matrix(ws[-1:-(M*(1+D))], K, 1+M)
  
  list(W1=W1, W2=W2)
}## end pack.w

###########################################################################
# weight-unpacking function
###########################################################################

## tested: Feb 4th, 2012
unpack.w <- function(net){
  ws <- c(net$W1, net$W2)
}## end unpack.w

###########################################################################
# error function
###########################################################################

mse.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T, ...
  ## nntype ='regression'/'biclass'/'multiclass')
  ## Note: biclass, K must be 1. 
  ## Otherwise, fix code (,i.e., perfect.ids...). May 9th, 2014.
  
  X <- prob.info$X
  T <- prob.info$T

  N <- ncol(X)  
  K <- prob.info$K
    
  nntype <- 'regression'

  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }

  netW <- pack.w(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);
  A <- W2 %*% dotZ

  if(nntype == 'regression'){
    Y <- A;  
    mse <- mean((Y - T)^2)
  }##end if
  if(nntype == 'biclass'){
    Y <- 1/(1+exp(-A))

##    mse <- -sum(T * log(Y) + (1-T) * log(1-Y))
    ## May 9th, 2014. 
    ## Numerical problem occurs when we reach perfect output
    ## Y = 0, T = 0 and Y = 1, T = 1 leads to 0*log(0), NaN.
    ## Therefore, if we found NaN, it is either
    ## 0*log(0) + 1*log(1)
    ## or 1*log(1) + 0*log(0).
    ## Either of them, we want 0.

    costn <- T * log(Y) + (1-T) * log(1-Y)
    perfect.ids <- which(is.nan(costn)) 
    costn[perfect.ids] <- 0   ## perfect results has 0 costs.
    mse <- -sum(costn)        

  }##end if
  if(nntype == 'multiclass'){
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
    mse <- -sum( T * log(Y) )
  }##end if
  
  return(mse)
}## end mse.w

###########################################################################
# derivative error function
###########################################################################

dE.w <- function(ws, prob.info){
  ## prob.info=list(D, M, K, X, T)

  X <- prob.info$X
  T <- prob.info$T

  N <- ncol(X)  
  K <- prob.info$K
  
  nntype <- 'regression'

  if(!is.null(prob.info$nntype)){
    nntype <- prob.info$nntype
  }
    
  netW <- pack.w(ws, prob.info$D, prob.info$M, prob.info$K);
  W1 <- netW$W1;
  W2 <- netW$W2;
  
  ## Forward pass
  dotX <- rbind(1,X);
  Z <- sigmoid(W1 %*% dotX);
  dotZ <- rbind(1,Z);
  A <- W2 %*% dotZ

  if(nntype == 'regression'){
    Y <- A
  }##end if
  if(nntype == 'biclass'){
    Y <- 1/(1+exp(-A))
  }##end if
  if(nntype == 'multiclass'){
    Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)
  }##end if
  
  ## Backward pass
  
  DELTA2 <- ( Y - T ) /N
  
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

init.weights.nugyenwidrow <- function(D, M, K, activeregion=c(-4, 4)){
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
                0, 0, 0, 0, 0)
  
  rownames(logs) <- c("k", "success", "sigmak", 
                      "deltak", "lambdabar", "lambdak",
                      "muk", "alphak", "DELTA", "Ew", "Ewplus")
  
  xtrace <- rbind(0, xk)

  #####################################
  ## 2. If success = true, calculate derivatives.
  #####################################
  
  for(k in 1:MaxN){
    
    pkSq <- t(pk) %*% pk
    norm.pk <- sqrt(pkSq)
    
    if(pkSq < .Machine$double.eps){## per Chuck's practice
      ## Terminate
      if(log){ return( list(logs=logs, xtrace=xtrace, x=xk, note="pkSq: reach machine precision") ) } else { return(xk) }  
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
    
    if(is.nan(deltak) | is.infinite(deltak)){
      ## browser()   ## uncomment this to catch the incident

      cat('deltak issue: deltak = ', deltak, '\n') 
      errFileName = paste('ErrorDeltak',
                      format(Sys.time(), "%Y%b%dT%H%M%S"),
                      '.RData', 
                      sep='')
      cat('* deltak issue: see ', errFileName,'\n')

      save(logs, xtrace, file=errFileName)

      ## Reset scg parameters to the beginning.
      ## This is a temporary patch. May 10th, 2014.

      lambdak <- lambda1       ## used to regulate indefiniteness of the Hessian
  
      lambdabar <- 0           ## used to regulate indefiniteness of the Hessian
      rk <- -df(xk, prob.info)
      pk <- rk
      success <- TRUE

      next()  
    }
    

    #####################################
    ## 4. If deltak <= 0, make H positive definite.
    #####################################
    
    if(deltak <= 0){
      
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
    
    if(is.infinite(Eplus)){
    ## my fix Eplus Inf, May 14, 2014.
    ## See InvestigateNaNscg12.r.

    cat('Eplus Inf: alphak =', alphak, '\n') ## debug

      alphak <- 1

      repeat{
        
        xnew <- xk + alphak*pk
        Eplus <- f(xnew, prob.info)

        if(E - Eplus > 0) break;
        alphak <- alphak * 0.1

        if(alphak < .Machine$double.eps){
          ## Terminate
          if(log){ return( list(logs=logs, xtrace=xtrace, x=xk, note="fix alphak: reach machine precision") ) } else { return(xk) }  
        }##end if(alphak ...)

      }##end repeat
      

    }##end if


    
    DELTA <- (E - Eplus)*deltak*2/(muk^2)    

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
                              muk, alphak, DELTA, E, Eplus))
    
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

## interface to scg: (xk, term.data, term.info,doplot, E.logs, nntype)

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
    
    net <- pack.w(xk, D, M, K)
    
    val.y <- nnOutput(net, val.X, nntype=nntype)
    
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



##############
cat('\nload scg library\n')

