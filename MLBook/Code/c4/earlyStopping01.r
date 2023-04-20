source('nnRegressionSigmoid03.r')

  graphics.off() # to close all graphical displays
  
  ## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  
  test.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  test.T <- f(test.X)

##  save(train.X, train.T, test.X, test.T, file='simpleData.RData')

##write.table(t(rbind(train.X, train.T)), file = "simpleTrain.csv", sep=' ')
##write.table(t(rbind(test.X, test.T)), file = "simpleTest.csv", sep=' ')

load('simpleData.RData')

  r <- normalize(train.X,returnParms=TRUE)
  train.Xn <- r$data
  test.Xn <- normalize(test.X,r$means,r$stdevs)



################
## early stopping
################
  val.X <- matrix(seq(0,4*pi,len=round(N/3)),nrow=1)
  val.T <- f(test.X)

  val.Xn <- normalize(val.X,r$means,r$stdevs)

#################################################
  M = 1
  NEP = 5000
    net1 = NULL
    net1 <- nnTrain(train.Xn,train.T, nHiddens=M, 
                   rhoh=0.01,rhoo=0.0003, wmax=0.5, nEpochs=NEP,
                   net=net1,
                   earlystopping=TRUE, 
                   early.tol=0.1, val.X=val.Xn, val.T=val.T)
    
    save(net1, file=paste('net1N',net1$N,'.RData',sep=''))

    test.y1 <- nnOutput(net1,test.Xn)
    test.rmse1 <- sqrt(mean((test.y1 - test.T)^2))
    par(mar=c(2,2,3,1))
    plot(test.X, test.T, pch='o', xlab='x', ylab='y', 
       main=paste('M=',M,'; epochs=',net1$N,
      '\n train e=', round(net1$errors[net1$N],2), '; test e=', round(test.rmse1,2)))
    xs <- matrix(seq(0, 4*pi, len=N), nrow=1);

    xs.n <- normalize(xs,r$means,r$stdevs)
    ys1 <- nnOutput(net1,xs.n)

    lines(xs, ys1, col='red')
    points(train.X, train.T, pch='x')

    dev.copy2eps(file=paste('simpleNet1','E',net1$N,'.eps', sep=''))


