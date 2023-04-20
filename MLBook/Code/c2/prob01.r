Ns = c(10, 100, 1000, 10000, 100000, 1e6, 1e7)

records = matrix(0, 2, 7)

##

N = 10
picked = matrix(0, 1, N)
for(i in 1:N){
  picked[i] = sample(12,1)
}

gCountIDs = which(picked < 10)
oCountIDs = which(picked >= 10)

PA = length(gCountIDs)/N

plot(gCountIDs, picked[gCountIDs], col='green', pch=19,
 ylim=c(0,13), xlab='Experiment i', ylab='Outcome',
 main=paste('N(A)/N = ', PA))
points(oCountIDs, picked[oCountIDs], col='orange', pch=19)

##dev.copy2eps(file='probDemo01.eps')


##

records[,1] = c(10, 0.8)

for(j in 2:7){
  N = Ns[j]
  picked = matrix(0, 1, N)

  for(i in 1:N){
    picked[i] = sample(12,1)
  }

  gCountIDs = which(picked < 10)
  oCountIDs = which(picked >= 10)

  PA = length(gCountIDs)/N

  records[,j] = c(N, PA)
}

> records
     [,1]   [,2]     [,3]      [,4]       [,5]        [,6]         [,7]
[1,] 10.0 100.00 1000.000 1.000e+04 1.0000e+05 1.00000e+06 1.000000e+07
[2,]  0.8   0.68    0.754 7.564e-01 7.4917e-01 7.49291e-01 7.499472e-01

plot(1:7, records[2,], type='b', xlab='N in Power of 10', ylab='N(A)/N',
 main='The ratio is converging to 0.75')
lines(c(0,8), c(0.75, 0.75), lty=2, col='red')
##dev.copy2eps(file='demoProb02.eps')