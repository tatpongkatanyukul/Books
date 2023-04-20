## Previous version: expDL01a.r (working)
## * this version: use DNN09.r (stochastic GD)
##
## Created Mar 30th, 2015.

rm(list=ls())
source('DNN09.r')

#########################
## Experiment factor
#########################

fsuffix = 'expDL01'

K = 10; 
Ms = c(20,20,20)
M3 = Ms[3]; 

dn.actFuncs  = c(sigmoid, sigmoid, sigmoid)
dn.dactFuncs = c(dsigmoid, dsigmoid, dsigmoid)
dn.aeAct = logistic

ft.actFuncs  = c(sigmoid, sigmoid, sigmoid, softmax)
ft.dactFuncs = c(dsigmoid, dsigmoid, dsigmoid)

dnEpochs1 = c(2000,2000,2000)
ftEpochs=4000
ftEpochs2=1000
ftTol = 1e-8

DropOutProb = 0.0

mbsize = 100

##########################
# Prepare data
##########################

load('do01.RData')
##train.X, train.T, test.X, test.T

Nall = ncol(train.X)

##N =100 ## training data size:  100 200 500 1000 1500 3000 4000 5000 5468
##allowIDs = sample(Nall, N)
N = Nall
allowIDs = sample(Nall) ## use all training data

allowX = train.X[,allowIDs,drop=F]
allowT = train.T[,allowIDs,drop=F]

trainIDs = sample(N, round(0.7*N))
datX = allowX[,trainIDs]
allowTK = encode1ofK(allowT)
datTK = allowTK[,trainIDs]

valX = allowX[,-trainIDs]
valTK = allowTK[,-trainIDs]

##########################
## Pre-train: denoising
##########################

lr = 1e-4
 log1 = Sys.time()
 dnNet = denoising(datX, p.noise=0.5, nHiddens=Ms, 
         rho=c(lr,lr,lr), nEpochs=dnEpochs1,
         acf=dn.actFuncs, 
         dacf=dn.dactFuncs,
         aeAcf=dn.aeAct,
         etol=1e-5,
         minibatch=mbsize, doPlot=TRUE)
 log2 = Sys.time()

par(mfrow=c(1,3))
for(i in 1:3){
  plot(1:length(dnNet$errors[[i]]),
    dnNet$errors[[i]], xlab='epoch', ylab='SSE',
    main=paste('Pretrain L', i, sep=''), type='l')
}
##dev.copy2eps(file='ExpDL01bPretrain.eps')

##save(dnNet, file='expDL01bDnNet.RData')

#########################
## Check learned weights
#########################

disp.pix = function(pmat, N=16, ...){
  xs = seq(1,N); ys = xs;
  z = matrix(pmat, N, N)
  zr = apply(z, 1, rev)
  image(xs, ys, t(zr), 
     col=gray(1 - (1:256/256)), ...)
}

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 disp.pix(dnNet$W[[1]][i,-1], xaxt='n', yaxt='n')
}

##dev.copy2eps(file='expDL01bPreW1.eps')

par(mfrow=c(4,5))
par(mar=c(2,2,3,0))
for(i in 1:20){
 hist(dnNet$W[[1]][i,-1], xlab='w', 
 main=paste('W',i,sep=''))
}

##########################
## Fine Tune
##########################

  wmax =0.1
  W4 = matrix(runif(K*(1+M3), -wmax, wmax), K, 1+M3)
  netW = c(dnNet$W, list(W4))
  net0 = list(W=netW)

  #################
  ## Fine-tune
  #################

  log1 <- Sys.time()
  print(log1)

  net1 = nnTrain(datX,datTK, nHiddens=Ms, 
         rho=c(1e-5,1e-5,1e-5,1e-4), nEpochs=ftEpochs,
         acf= ft.actFuncs, 
         dacf= ft.dactFuncs,
         pdropout=DropOutProb,
         ValX=valX, ValT=valTK, 
         net=net0, batchsize=mbsize, tol=ftTol)

  log2 <- Sys.time()
  print(log2)

##save(net1, log1, log2, file='expDL01bNet1.RData')

########################
## Test
########################

test.yk = nnOutput(net1, test.X)
test.y = decode1ofK(test.yk,classes=c('0','1','2','3','4','5','6','7','8','9'))

accuracy = sum(test.y == test.T)/length(test.T)

########################
## View Weights
########################


############
## W1
############

####################
## After Denoising
####################


######################
## After Fine Tuning
######################

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 disp.pix(neti$W[[1]][i,-1], xaxt='n', yaxt='n')
}

############
## W2
############

####################
## After Denoising
####################

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 iFeat = dnNet$W[[2]][i,-1] %*% dnNet$W[[1]][,-1]
 disp.pix(iFeat, xaxt='n', yaxt='n')
}

######################
## After Fine Tuning
######################

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 iFeat = neti$W[[2]][i,-1] %*% neti$W[[1]][,-1]
 disp.pix(iFeat, xaxt='n', yaxt='n')
}


############
## W3
############

####################
## After Denoising
####################

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 iFeat = dnNet$W[[3]][i,-1] %*% dnNet$W[[2]][,-1] %*% dnNet$W[[1]][,-1]
 disp.pix(iFeat, xaxt='n', yaxt='n')
}

######################
## After Fine Tuning
######################

par(mfrow=c(4,5))
par(mar=c(0,1,1,0))
for(i in 1:20){
 iFeat = neti$W[[3]][i,-1] %*% neti$W[[2]][,-1] %*% neti$W[[1]][,-1]
 disp.pix(iFeat, xaxt='n', yaxt='n')
}

############
## W4
############

######################
## After Fine Tuning
######################

par(mfrow=c(2,5))
par(mar=c(0,1,1,0))
for(i in 1:10){
 iFeat = neti$W[[4]][i,-1] %*% neti$W[[3]][,-1] %*% neti$W[[2]][,-1] %*% neti$W[[1]][,-1]
 disp.pix(iFeat, xaxt='n', yaxt='n')
}

###############################

Tids = vector('list',10)
for(i in 1:10){
  Tids[[i]] = which(test.T == (i-1))
}

par(mfrow=c(10,2))
for(i in 1:10){
  ri = sample(length(Tids[[i]]),1)
  id = Tids[[i]][ri]

  par(mar=c(0,1,1,0))
  disp.pix(test.X[,id], xaxt='n', yaxt='n')

  nout = nnOutput(neti, test.X[,id,drop=F], details=TRUE)
  yk = nout$y
  y = decode1ofK(yk,
   classes=c('0','1','2','3','4','5','6','7','8','9'))

  par(mar=c(0.1,1,3,0.1))
  plot(0:9, yk, main=paste('y = ', y), xaxt='n', yaxt='n')
}



##################################

par(mfrow=c(1,3))
for(i in 1:3){
  plot(1:length(goodNet$errors[[i]]),
    goodNet$errors[[i]], xlab='epoch', ylab='SSE',
    main='Pretrain L1', type='l')
}

par(mfrow=c(1,3))
for(i in 1:3){
  plot(1:length(dnNet$errors[[i]]),
    dnNet$errors[[i]], xlab='epoch', ylab='SSE',
    main='Pretrain L1', type='l')
}

