 source('dEwCode02b.r')
 source('mseWCode02b.r')
 source('numericalGrad02.r')
 source('normCode.r')
 source('initWeightsNugyenWidrowCode.r')
 source('scgOthersCode.r')

  ## sine function to fit
  f <- function (x)  x + 8 * sin(x) + rnorm(length(x))

  N <- 50
  train.X <- matrix(seq(0,4*pi,len=N),1,N)
  train.T <- f(train.X)
  r <- normalize(train.X)
  train.Xn <- r$norm

  D = 1
  M = 20
  K = 1
  

  prob.info <- list(D=D, M=M, K=K, X=train.Xn, T=train.T,
     nntype='regression')


  net <- init.weights.nugyenwidrow(D, M, K, activeregion=c(-4, 4))
  w <- unpack.w(net)

ngrad1 = numericalGrad(function(x){mse.w(x,prob.info)}, w, epsilon=1)
ngrad2 = numericalGrad(function(x){mse.w(x,prob.info)}, w, epsilon=1e-2)
ngrad3 = numericalGrad(function(x){mse.w(x,prob.info)}, w, epsilon=1e-4)


cgrad = dE.w(w, prob.info)

#################
## compare them
#################

cbind(ngrad1, cgrad)
plot(1:61, ngrad1 - cgrad)

par(mfrow=c(1,2))
boxplot( cbind(ngrad1-cgrad, ngrad2-cgrad, ngrad3-cgrad),
  names=c('1','1e-2','1e-4'), main='Difference between n. grad. and b. grad.',
  xlab='Epsilon', ylab='Error')

dev.copy2eps(file='testNumGrad02b.eps')

> mean(ngrad1-cgrad)
[1] 0.003742474
> mean(ngrad2-cgrad)
[1] 4.256114e-07
> mean(ngrad3-cgrad)
[1] 1.759771e-11
