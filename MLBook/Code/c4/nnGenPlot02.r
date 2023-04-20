source('nnRegressionSigmoid02.r')

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
load('simpleData.RData')

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
                   rhoh=0.01,rhoo=0.0003, wmax=1, nEpochs=1)
      z <- sigmoid( net$W1 %*% rbind(1,train.Xn) )
      y <- nnOutput(net,train.Xn)

      p <- par(mfcol=c(2,1)) # "set plot [par]ameters" to organize multiple plots
      par(mar=c(3,3,2,1))
      matplot(t(train.X),t(z),type="l",lwd=2,
        main=paste("Hidden Outputs (after ", ncol(net$errors), "epochs)"),
        xlab='x', ylab='y')
      matplot(t(train.X),cbind(t(train.T),t(y)),type=c('p','l'),pch='o', lty=1, lwd=2,
        main=paste("Network Outputs v.s. Targets (",ncol(net$errors),"epochs)"),
        xlab='x', ylab='y')

      dev.copy2eps(file=paste('simpleN', 1,'.eps', sep=''))
      dev.set(dev.next())

    for (ep in 1:9000) {
      net <- nnTrain(train.Xn,train.T, nHiddens=20, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.5, nEpochs=1, net=net)

      z <- sigmoid( net$W1 %*% rbind(1,train.Xn) )
      y <- nnOutput(net,train.Xn)
      dev.set(dev.next()) # to manage display device
      p <- par(mfcol=c(2,1)) # "set plot [par]ameters" to organize multiple plots
      matplot(t(train.X),t(z),type="l",lwd=2,
        main=paste("Hidden Outputs (after ", ncol(net$errors), "epochs)"),
        xlab='x', ylab='y')
      matplot(t(train.X),cbind(t(train.T),t(y)),type=c('p','l'),pch='o', lty=1, lwd=2,
        main=paste("Network Outputs v.s. Targets (",ncol(net$errors),"epochs)"),
        xlab='x', ylab='y')

      if(sum(ep == c(100, 300, 1000, 2000, 3000, 5000, 6000, 7000, 8000, 9000))>0){
         dev.copy2eps(file=paste('simpleN', ncol(net$errors),'.eps', sep=''))
      }
      dev.set(dev.next())
      #system("sleep 0.01")
    }
#dev.set(dev.prev())


#dev.set(dev.next())

save(net, file='finalNet.RData')
p <- par(mfcol=c(1,1))
plot(1:9001, net$errors, type='l', xlab='epoch', ylab='Error',
  main='Training error')
##abline(v=c(101, 301, 1001, 3001, 6001, 9001), lty=2, col='red')
dev.copy2eps(file='error9001.eps')
