scg <- function(df, f, w1, prob.info=NULL, term.fn=nnTermination, 
                term.info=NULL, MaxN=1000, sigma=5.0e-5, 
                lambda1=5.0e-7, log=FALSE, doplot=TRUE){

  term.results <- NULL  
  w1 <- as.matrix(w1)

  ## Step 1. ##
  
  wk <- w1
  lambdak <- lambda1       
  lambdabar <- 0           
  rk <- -df(wk, prob.info)
  pk <- rk
  success <- TRUE
  
  NR <- nrow(wk)  ## size of wk: NR x 1
  
  logs <- rbind(0, success, 0, 0, lambdabar, lambdak, 0, 0, 0, 0, 0)  
  rownames(logs) <- c("k", "success", "sigmak", "deltak", "lambdabar", "lambdak","muk", "alphak", "DELTA", "Ew", "Ewplus")
  
  wtrace <- rbind(0, wk)

  ## Step 2. ##
  
  for(k in 1:MaxN){
    
    pkSq <- t(pk) %*% pk
    norm.pk <- sqrt(pkSq)
    
    if(success){      
      sigmak <- as.numeric(sigma/norm.pk)      
      dEplus <- df(wk + sigmak*pk, prob.info)
      dE <- df(wk, prob.info)      
      sk <- (dEplus - dE)/sigmak
      deltak <- as.numeric(t(pk) %*% sk)      
    }## if(success)
    
    ## Step 3. ##
    
    deltak <- deltak + (lambdak - lambdabar) * pkSq    

    ## Step 4. ##
    
    if(deltak <= 0){      
      lambdabar <- 2*(lambdak - deltak/pkSq)      
      deltak <- - deltak + lambdak*pkSq      
      lambdak <- lambdabar      
    }## if(deltak <= 0)
    
    ## Step 5. ##
    
    muk <- t(pk) %*% rk
    alphak <- as.numeric(muk/deltak)
        
    ## Step 6. ##

    wnew <- wk + alphak*pk
    Eplus <- f(wnew, prob.info)
    E <- f(wk, prob.info)
    
    if(is.infinite(Eplus)){
      cat('\nEplus is Inf; alphak =', alphak, '; muk =', muk,'\n')    
      browser()
      ## My suggestion (May 29th, 2014): 
      ## Downsize alphak, but keep the sign, 
      ## e.g., alphak <- 1 (for alphak > 0) and 
      ## alphak <- -1 (for alphak < 0).
      ## After downsizing, check if Eplus < E.
    }##end if

    DELTA <- (E - Eplus)*deltak*2/(muk^2)    

    ## Step 7. ##
    
    if(DELTA >= 0){      
      rnew <- -df(wnew, prob.info)   ## gradient descent direction      
      lambdabar <- 0
      success <- TRUE
      
      if(k %% NR == 0){
        pnew <- rnew
      }else{
        beta <- as.numeric( (t(rnew) %*% rnew - t(rnew) %*% rk) /muk )
        pnew <- rnew + beta*pk
      }## end if(k %% NR == 0)
      
      if(DELTA >=0.75){
        lambdak <- lambdak/4
      }## end if(DELTA >=0.75)
      
      wk <- wnew
      rk <- rnew
      pk <- pnew
      
    }else{## DELTA < 0      
      lambdabar <- lambdak
      success <- FALSE      
    }## end if(DELTA >= 0)
    
    ## Step 8. ##
    
    if(DELTA < 0.25){      
      lambdak <- lambdak + deltak*(1 - DELTA)/pkSq      
    }## end if(DELTA < 0.25)
    
    ## Step 9. ##
    
    logs <- cbind(logs, rbind(k, success, sigmak, deltak, lambdabar, lambdak, muk, alphak, DELTA, E, Eplus))    
    wtrace <- cbind(wtrace, rbind(k, wk))    
    term.results <- term.fn(wk, term.results, term.info, doplot=doplot, E.logs=logs[10,-1])
    
    if (term.results$boolean){      
      ## Terminate
      if(log){ return( list(logs=logs, wtrace=wtrace, w=wk, term.results=term.results) ) } else { return(wk) }        
    }## end if

  }## end for(k in 1:MaxN)
  
  print("Reach maximum iterations")
  if(log){ return( list(logs=logs, wtrace=wtrace, w=wk, term.results=term.results, note="reach max iterations") ) }

return(wk)    
}## end scg
