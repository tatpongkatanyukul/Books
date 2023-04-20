nnTermination <- function(xk, term.data=NULL, term.info=NULL,
                          tol=0, min.grad=1e-12, max.val.fail=8,
                          doplot=FALSE, E.logs=NULL, nntype='regression'){

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
    } 
    if(nntype == 'biclass'){
	   val.cost <- val.T * log(val.y) + (1-val.T) * log(1-val.y)
         perfect.ids <- which(is.nan(val.cost))
         val.cost[perfect.ids] <- 0  ## perfect results has 0 costs.
         val.mse <- -sum(val.cost)
    }
    if(nntype == 'multiclass'){
         val.mse <- -sum( val.T * log(val.y) )
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
