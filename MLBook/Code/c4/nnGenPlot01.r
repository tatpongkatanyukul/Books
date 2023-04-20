source('nnRegressionSigmoid01.r')

  graphics.off() # to close all graphical displays
  
  ## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  r <- normalize(train.X,returnParms=TRUE)
  train.Xn <- r$data
  
  test.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  test.T <- f(test.X)
  test.Xn <- normalize(test.X,r$means,r$stdevs)

##  save(train.X, train.T, test.X, test.T, file='simpleData.RData')


  ##########
  ## plot 1
  ##########


  xs <- seq(0,4*pi, len=N)
  ys <- xs + 8 * sin(xs)

  plot(xs, ys, type='l', col='red', xlab='x', ylab='y')
  points(train.X, train.T)
  points(test.X, test.T, pch='x')

##  dev.copy2eps(file='simpleExampleData.eps')  

  ##########
  ## plot 2
  ##########

  
  x11() # prepare one display for error plot in nnTrain
  nEpochs <- 10000

    x11() # display for plotting hidden outputs

    net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.3, nEpochs=1)
      z <- sigmoid( net$W1 %*% rbind(1,test.Xn) )
      y <- nnOutput(net,test.Xn)

      p <- par(mfcol=c(2,1)) # "set plot [par]ameters" to organize multiple plots
      matplot(t(test.X),t(z),type="l",lwd=2,
        main=paste("Hidden Outputs (after ", ncol(net$errors), "epochs)"),
        xlab='x', ylab='y')
      matplot(t(test.X),cbind(t(test.T),t(y)),type="l",lwd=2,main="Network Outputs v.s. Targets",
        xlab='x', ylab='y')

      dev.copy2eps(file=paste('simpleN', 1,'.eps'))
      dev.set(dev.next())

    for (ep in 1:5000) {
      net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.5, nEpochs=1, net=net)

      z <- sigmoid( net$W1 %*% rbind(1,test.Xn) )
      y <- nnOutput(net,test.Xn)
      dev.set(dev.next()) # to manage display device
      p <- par(mfcol=c(2,1)) # "set plot [par]ameters" to organize multiple plots
      matplot(t(test.X),t(z),type="l",lwd=2,
        main=paste("Hidden Outputs (after ", ncol(net$errors), "epochs)"),
        xlab='x', ylab='y')
      matplot(t(test.X),cbind(t(test.T),t(y)),type="l",lwd=2,main="Network Outputs v.s. Targets",
        xlab='x', ylab='y')

      if(sum(ep == c(100, 300, 1000, 2000, 3000, 5000))>0){
         dev.copy2eps(file=paste('simpleN', ncol(net$errors),'.eps'))
      }
      dev.set(dev.next())
      #system("sleep 0.01")
    }
#dev.set(dev.prev())


#dev.set(dev.next())
