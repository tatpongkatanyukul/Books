###################################################
## Taylor's approximation example
###################################################

  rm(list=ls())
  N <- 80
  x <- matrix(seq(0, 4*pi, len=N), ncol=1)

  ## put x0, the function and its derivatives here
  x0 <- 2*pi
  f0s <- matrix(c(sin(x0), cos(x0), -sin(x0), -cos(x0)),ncol=1)	

  h <- x - x0

  ## Taylor series approximation

  M <- length(f0s) - 1

  ## Hpoly is an N x (M+1) matrix
  Hpoly <- matrix(h, nrow=N, ncol=M+1, byrow=FALSE)^matrix(0:M, N, M+1, byrow=TRUE)   

  coeffs <- Hpoly / matrix(factorial(0:M), N, M+1, byrow=TRUE)

  y <- coeffs %*% f0s


  p <- par(mfrow=c(2,2))

  plot(x, sin(x), type='l', main=c("Taylor's approx. of sin(x) w/ x0=", round(x0,1), ", M =",M), col="red")  
  lines(x, y, col='blue') 
  points(x, y, col='blue', pch='o')
  abline(v=x0, col='green')


  ## y2
  x0 <- pi
  f0s <- matrix(c(sin(x0), cos(x0), -sin(x0), -cos(x0)),ncol=1)
  h <- x - x0
  M <- length(f0s) - 1
  Hpoly <- matrix(h, nrow=N, ncol=M+1, byrow=FALSE)^matrix(0:M, N, M+1, byrow=TRUE)   
  coeffs <- Hpoly / matrix(factorial(0:M), N, M+1, byrow=TRUE)
  y2 <- coeffs %*% f0s
  plot(x, sin(x), type='l', main=c("Taylor's approx. of sin(x) w/ x0=", round(x0,1), ", M =",M), col="red")
  lines(x, y2, col='blue') 
  points(x, y2, col='blue', pch='o')
  abline(v=x0, col='green')

  ## y3
  x0 <- 2*pi
  f0s <- matrix(c(sin(x0), cos(x0), -sin(x0), -cos(x0), sin(x0), cos(x0), -sin(x0), -cos(x0)),ncol=1)
  h <- x - x0
  M <- length(f0s) - 1
  Hpoly <- matrix(h, nrow=N, ncol=M+1, byrow=FALSE)^matrix(0:M, N, M+1, byrow=TRUE)   
  coeffs <- Hpoly / matrix(factorial(0:M), N, M+1, byrow=TRUE)
  y3 <- coeffs %*% f0s
  plot(x, sin(x), type='l', main=c("Taylor's approx. of sin(x) w/ x0=", round(x0,1), ", M =",M), col="red")
  lines(x, y3, col='blue') 
  points(x, y3, col='blue', pch='o')
  abline(v=x0, col='green')



  ## y4
  x0 <- 2*pi
  f0s <- matrix(c(sin(x0), cos(x0), -sin(x0), -cos(x0), sin(x0), cos(x0), -sin(x0), -cos(x0),sin(x0), cos(x0), -sin(x0), -cos(x0), sin(x0), cos(x0), -sin(x0), -cos(x0)),ncol=1)
  h <- x - x0
  M <- length(f0s) - 1
  Hpoly <- matrix(h, nrow=N, ncol=M+1, byrow=FALSE)^matrix(0:M, N, M+1, byrow=TRUE)   
  coeffs <- Hpoly / matrix(factorial(0:M), N, M+1, byrow=TRUE)
  y4 <- coeffs %*% f0s
  plot(x, sin(x), type='l', main=c("Taylor's approx. of sin(x) w/ x0=", round(x0,1), ", M =",M), col="red")
  lines(x, y4, col='blue') 
  points(x, y4, col='blue', pch='o')
  abline(v=x0, col='green')


  ## restore plot parameter values
  par(p)



## Golden section search
## Created:       Jan 4th, 2012
## Last modified: Jan 4th, 2012
## Example
##   qf <- function(x){x^4 -14*x^3 + 60*x^2 -70*x}
##   logs <- goldensearch(qf, 0, 2, log=TRUE)

goldensearch <- function(f, a0, b0, tol=0.1, rho=0.382, log=FALSE){

#f <- qf

#a0 <- 0
#b0 <- 2
#tol <- 0.1
#rho <- 0.382

   diff <- b0 - a0
   a1 <- a0 + rho*diff
   b1 <- b0 - rho*diff

   fa0 <- f(a0)
   fb0 <- f(b0)
   fa1 <- f(a1)
   fb1 <- f(b1)

   logs <- matrix(c(a0,fa0,b0,fb0),4,1)   

   while (diff > tol) {
      if( fa1 < fb1 ) {
        b0 <- b1
        fb0 <- fb1
        b1 <- a1
        fb1 <- fa1

        diff <- b0 - a0
        a1 <- a0 + rho*diff
        fa1 <- f(a1)

      } else { ## fa1 >= fb1
        a0 <- a1
        fa0 <- fa1
        a1 <- b1
        fa1 <- fb1

        diff <- b0 - a0
        b1 <- b0 - rho*diff
        fb1 <- f(b1)     

      } ## end if

      logs <- cbind(logs, matrix(c(a0,fa0,b0,fb0),4,1))
   }## end while

  rownames(logs) <- c("a0", "f(a0)","b0", "f(b0)")
  if(log) { return(logs) }
  else { return((a0+b0)/2) }
}## goldensearch

###########################################################################

## Newton Method
## Created:       Jan 5th, 2012
## Last modified: Jan 5th, 2012
## Example
##   f  <- function(x){x^4 -14*x^3 + 60*x^2 -70*x}
##   df <- function(x){4*x^3 -42*x^2 + 120*x -70}
##   df2 <- function(x){12*x^2 -84*x + 120}
##   logs <- newtonsearch(df, df2, x0=0)

newtonsearch <- function(df, df2, x0, tol=0.01, MaxN=100){
  logs <- matrix(0, 2, MaxN)
  i <- 1
  diff <- 2*tol
  while(diff > tol){

    x <- x0 - df(x0)/df2(x0)

    diff <- abs(x - x0) 
    logs[,i] <- c(i,x)
    x0 <- x
    
    i <- i+1
    if (i > MaxN) break;
  }## while

  return(logs)
}## end newtonsearch

#######################################################

## Steepest Descend Method
## Created:       Jan 6th, 2012
## Last modified: Jan 6th, 2012
## Example
##   f  <- function(x){(x[1] - 4)^4 + (x[2] - 3)^2 + 4*(x[3] + 5)^4}
##   df <- function(x){ matrix(c( 4*(x[1] - 4)^3, 2*(x[2] - 3), 16*(x[3] + 5)^3 ) ,3,1)}
##   logs <- steepestdescend(df, f, x0=matrix(c(4,2,-1),3,1), log=TRUE)
##   

steepestdescend <- function(grad, f, x0, tol=0.00001, MaxN=1000, log=FALSE){

#x0 <- matrix(c(4,2,-1),3,1)
#grad <- df
#tol <- 0.0001
#MaxN <- 1000

  D <- length(x0)

  logs <- matrix(c(0,0,x0),2+D,1)

  diff <- tol*2
  for (i in 1:MaxN) {

    g <- function(a){ f(x0 - a*grad(x0)) }
    alpha <- goldensearch(g, a0=tol/10, b0=1, tol=tol, log=FALSE)

    gradF <- grad(x0)
    x <- x0 - alpha*gradF

    diff <- sqrt( mean((gradF)^2) )
    x0 <- x    
    logs <- cbind(logs, c(i,alpha,x0))

    if (diff < tol) break
  }## for

  if(log){ return(logs) }
  else { return(x) }
}## end steepestdescend

#######################################################

## Newton Method
## Created:       Jan 10th, 2012
## Last modified: 
## Example
##   f  <- function(x){ (x[1] + 10*x[2])^2 + 5*(x[3] - x[4])^2 + (x[2] - 2*x[3])^4 + 10*(x_1 - x_4)^4 }
##   df <- function(x){ matrix(c( 2*(x[1] + 10*x[2]) + 40*(x[1] - x[4])^3, 
##                                20*(x[1] + 10*x[2]) + 4*(x[2] - 2*x[3])^3,
##                                10*(x[3] - x[4]) - 8*(x[2] - 2*x[3])^3,
##                                  -10*(x[3] - x[4]) - 40*(x[1] - x[4])^3      ) ,4,1)}
##   Hf <- function(x){ matrix(c( 2 + 120*(x[1] - x[4])^2, 20, 0, -120*(x[1]-x[4])^2,
##                                20, 200 + 12*(x[2] - 2*x[3])^2, -24*(x[2] - 2*x[3])^2, 0,
##                                0, -24*(x[2] - 2*x[3])^2, 10 + 48*(x[2] - 2*x[3])^2, -10,
##                                -120*(x[1] - x[4])^2, 0, -10, 10 + 120*(x[1] - x[4])^2    ), 4,4, byrow=TRUE)}
##
##   logs <- newtonmethod(Hf, df, f, x0=matrix(c(3, -1, 0, 1),4,1), log=TRUE)

newtonmethod <- function(Hf, df, f, x0, tol=0.00001, MaxN=1000, log=FALSE){

  D <- length(x0)

  logs <- matrix(c(0,f(x0),x0),2+D,1)
  rownames(logs) <- c('i', 'f(x)', 1:D) 

  diff <- tol*2
  for (i in 1:MaxN) {

    Hk <- Hf(x0)
    invHk <- solve(Hk)
    gk <- df(x0)
    
    x <- x0 - invHk %*% gk

    diff <- sqrt( mean((x - x0)^2) )
    x0 <- x    
    logs <- cbind(logs, c(i,f(x0),x0))

    if (diff < tol) break
  }## for
  
  if(log){ return(logs) }
  else { return(x) }
}## end newtonmethod

#######################################################

cat('try goldensearch:\n')
cat('qf <- function(x){x^4 -14*x^3 + 60*x^2 -70*x}\n')
cat('logs <- goldensearch(qf, 0, 2, log=TRUE)\n')

cat('try steepestdescend:\n')
cat('f  <- function(x){(x[1] - 4)^2 + 2*(x[2] - 3)^2}\n')
cat('df <- function(x){ matrix(c(2*(x[1] - 4), 4*(x[2] - 3) ), 2, 1) }\n')
cat('logs <- steepestdescend(df, f, x0=matrix(c(8,6),2,1), log=TRUE)\n')

###################################################
## Contour Plot
###################################################

f  <- function(x){(x[1] - 4)^2 + 2*(x[2] - 3)^2}
df <- function(x){ matrix(c(2*(x[1] - 4), 4*(x[2] - 3) ), 2, 1) }
logs <- steepestdescend(df, f, x0=matrix(c(8,6),2,1), log=TRUE)

N <- 80
x1 <- seq(2,8,len=N)
x2 <- seq(0,6,len=N)
x1x2 <- expand.grid(x=x1, y=x2)

z <- matrix(apply(t(x1x2),2,f),N,N)
contour(x1, x2, z, xlim=c(2,8), ylim=c(0,6))
lines(logs[2,], logs[3,], col='red')
points(logs[2,], logs[3,], pch=20)


###################################################
## Contour Plot of Positive Definite Matrix
###################################################

xQx <- function(x, Q){ t(x) %*% Q %*% x }

Q1 <- matrix(c(3,0,0,4), 2, 2, byrow=TRUE)
Q2 <- matrix(c(3,8,8,4), 2, 2, byrow=TRUE)

xQx1 <- function(x){ xQx(x,Q1) }
xQx2 <- function(x){ xQx(x,Q2) }

x1 <- seq(-8,8,len=40)
x2 <- seq(-8,8,len=40)
x1x2 <- expand.grid(x=x1, y=x2)

z1 <- matrix(apply(t(x1x2),2,xQx1),40,40)
z2 <- matrix(apply(t(x1x2),2,xQx2),40,40)

p <- par(mfrow=c(2,2),mar=c(3,2,2,1))

contour(x1, x2, z1)

persp(x1,x2,z1)

contour(x1, x2, z2)

persp(x1,x2,z2)

par(p)

########################
## plot CZ's fig 8.2
########################

fobj <- function(x, v=matrix(c(2,3),2,1), 
  Z=matrix(c(4,-1.5,-1.5,5),2,2,byrow=T)){ 
  - exp( -(t(x-v) %*% Z %*% (x-v) ) )
}

N <- 200
xs <- seq(0,4,len=N)
ys <- seq(0,6,len=N)
z <- matrix(0, N, N)

for(i in 1:N){
for(j in 1:N){
   z[i,j] <- fobj(matrix(c(xs[i], ys[j]),2,1))
}}

#par(mfrow=c(2,2))

#par(mar=c(3,3,3,1))
#persp(xs, ys, z, theta=-20, phi=30, 
#  shade=0.5, col='green', border=NA)

#par(mar=c(4,4,3,1))
#contour(xs, ys, z, xlab='x', ylab='y')

par(mar=c(4,4,3,1))
contour(xs, ys, z, xlab='x', ylab='y', 
  xlim=c(1.5,2.1), ylim=c(2.7,3.3))
points(2, 3, pch='*', col='red')


###
###
v0 <- matrix(c(2,3),2,1)
Z0 <- matrix(c(4,-1.5,-1.5,5),2,2,byrow=T)

fobj <- function(x, v=matrix(c(2,3),2,1), 
  Z=matrix(c(4,-1.5,-1.5,5),2,2,byrow=T)){ 
  - exp( -(t(x-v) %*% Z %*% (x-v) ) )
}

df <- function(x, v=v0, Z=Z0){ as.numeric(fobj(x, v, Z)) * 
  matrix(
  c(-2*Z[1,1]*x[1] -(Z[1,2]+Z[2,1])*x[2] +
    2*Z[1,1]*v[1] +(Z[1,2]+Z[2,1])*v[2],
    -(Z[1,2]+Z[2,1])*x[1] -2*Z[2,2]*x[2] +
    (Z[1,2]+Z[2,1])*v[1] +2*Z[2,2]*v[2]
   ), 2, 1) 
}
logs <- steepestdescend(df, fobj, x0=matrix(c(2.5,3),2,1), log=TRUE, tol=1e-5)

N <- 80
d1n <- 1
d1x <- 3
d2n <- 2
d2x <- 4
x1 <- seq(d1n,d1x,len=N)
x2 <- seq(0,6,len=N)
x1x2 <- expand.grid(x=x1, y=x2)

z <- matrix(apply(t(x1x2),2,fobj),N,N)

#############################
## Plot Momentum Motivation
#############################

par(mfrow=c(2,2))
par(mar=c(3,3,3,1))

x1 <- seq(1.98,2.08,len=N)
x2 <- seq(2.98,3.03,len=N)
x1x2 <- expand.grid(x=x1, y=x2)

z <- matrix(apply(t(x1x2),2,fobj),N,N)

fm = function(a){ a }
xs = fm(x1)
ys = fm(x2)
path.x = fm(logs[3,])
path.y = fm(logs[4,])
range.y = c(2.98,3.03) ##c(min(path.y), max(path.y))
range.x = c(1.98,2.08) ##c(min(path.x), max(path.x))


contour(xs, ys, z, ylim=range.y, 
                   xlim=range.x,
        main='A: Zig-zag behavior')
lines(path.x, path.y, col='black', lty=1)
points(logs[3,], logs[4,], pch=20)

contour(xs, ys, z, ylim=range.y, 
                   xlim=range.x,
        main='B: Momentum')
lines(path.x, path.y, col='gray', lty=2)
points(logs[3,], logs[4,], pch=20)

m.vec = rbind(logs[3,-1] - logs[3,-14], 
              logs[4,-1] - logs[4,-14])

m.vec2 = m.vec^2
u.vec = m.vec
u.vec[1,] = m.vec[1,]/sqrt(m.vec2[1,]+m.vec2[2,])
u.vec[2,] = m.vec[2,]/sqrt(m.vec2[1,]+m.vec2[2,])

c.vec = u.vec[,-12] + 0.35*u.vec[,-1] 

lines( c(path.x[3], path.x[3]+0.02*u.vec[1,3]),
       c(path.y[3], path.y[3]+0.02*u.vec[2,3]), 
       col='red', lty=1)

lines( c(path.x[3], path.x[3]+0.02*u.vec[1,2]),
       c(path.y[3], path.y[3]+0.02*u.vec[2,2]), 
       col='red', lty=1)

lines( c(path.x[3], path.x[3]+c.vec[1,3]),
       c(path.y[3], path.y[3]+c.vec[2,3]), 
       col='blue', lty=2)

text(path.x[3]-0.01, path.y[3]+0.008, 'g')
text(path.x[3], path.y[3]-0.008, 'p')

## plot C ##

contour(xs, ys, z, ylim=range.y, 
                   xlim=range.x,
        main='C: At another point')
lines(path.x, path.y, col='gray', lty=2)
points(logs[3,], logs[4,], pch=20)

m.vec = rbind(logs[3,-1] - logs[3,-14], 
              logs[4,-1] - logs[4,-14])

m.vec2 = m.vec^2
u.vec = m.vec
u.vec[1,] = m.vec[1,]/sqrt(m.vec2[1,]+m.vec2[2,])
u.vec[2,] = m.vec[2,]/sqrt(m.vec2[1,]+m.vec2[2,])

c.vec = u.vec[,-12] + 0.35*u.vec[,-1] 

lines( c(path.x[4], path.x[4]+0.02*u.vec[1,4]),
       c(path.y[4], path.y[4]+0.02*u.vec[2,4]), 
       col='red', lty=1)

lines( c(path.x[4], path.x[4]+0.02*u.vec[1,3]),
       c(path.y[4], path.y[4]+0.02*u.vec[2,3]), 
       col='red', lty=1)

lines( c(path.x[4], path.x[4]+c.vec[1,4]),
       c(path.y[4], path.y[4]+c.vec[2,4]), 
       col='blue', lty=2)

text(path.x[4], path.y[4]-0.008, 'g')
text(path.x[4]-0.01, path.y[4]+0.008, 'p')

## plot D ##

contour(xs, ys, z, ylim=range.y, 
                   xlim=range.x,
        main='D: And another point')
lines(path.x, path.y, col='gray', lty=2)
points(logs[3,], logs[4,], pch=20)

m.vec = rbind(logs[3,-1] - logs[3,-14], 
              logs[4,-1] - logs[4,-14])

m.vec2 = m.vec^2
u.vec = m.vec
u.vec[1,] = m.vec[1,]/sqrt(m.vec2[1,]+m.vec2[2,])
u.vec[2,] = m.vec[2,]/sqrt(m.vec2[1,]+m.vec2[2,])

c.vec = u.vec[,-12] + 0.35*u.vec[,-1] 

lines( c(path.x[5], path.x[5]+0.02*u.vec[1,5]),
       c(path.y[5], path.y[5]+0.02*u.vec[2,5]), 
       col='red', lty=1)

lines( c(path.x[5], path.x[4]+0.03*u.vec[1,4]),
       c(path.y[5], path.y[4]+0.03*u.vec[2,4]), 
       col='red', lty=1)

lines( c(path.x[5], path.x[5]+c.vec[1,5]),
       c(path.y[5], path.y[5]+c.vec[2,5]), 
       col='blue', lty=2)

text(path.x[5]-0.01, path.y[5]+0.008, 'g')
text(path.x[5], path.y[5]-0.008, 'p')


##dev.copy2eps(file='MomentumMotivation01.eps')

######################
## gradient descent
######################

gd <- function(x, df, alpha=0.1, tol=1e-5, MaxN=200, log=TRUE){

  logs <- matrix(0, 1, MaxN)

  for(i in 1:MaxN){

     new.x <- x - alpha * df(x)

     logs[1,i] <- new.x

     if(is.infinite(new.x)) break;
     if(is.nan(new.x)) break;

     if( abs(new.x - x) < tol ){
        break;
     }
     x <- new.x


  }
  
  if(log){ return(logs[,1:i]) }

return(new.x)
}


######################
## 1-D illustration
######################


f <- function(x){ - exp( - (x-5)^2 ) }
df <- function(x){ f(x) * (-2*x + 10) }

xs <- seq(3.5, 6.8, len=100)

par(mfrow=c(3,2))
par(mar=c(4,4,1,1))

##
alpha <- 0.01

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 6.5
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 4
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

##

alpha <- 0.1

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 6.5

points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 4
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

##

alpha <- 1

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 6.5

points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 4
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(5, -0.2, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

##dev.copy2eps(file='gdFixedStepSize.eps')


########
##xs <- c(-1.5, 0, 1, 2, 3)
##ys <- c(10, 0, 3, 4, 9)

##w <- w.polyfitM(xs, ys, M=4)
##w
#              [,1]
#[1,]  1.827579e-14
#[2,]  2.812698e+00
#[3,]  1.843386e+00
#[4,] -2.187302e+00
#[5,]  5.312169e-01

w <- c(1.827579e-14, 2.812698e+00, 1.843386e+00,
       -2.187302e+00, 5.312169e-01)

f <- function(x){ 
   w[1] + w[2]*x + w[3]*x^2 + w[4]*x^3 + w[5]*x^4
}
df <- function(x){ 
   w[2] + 2*w[3]*x + 3*w[4]*x^2 + 4*w[5]*x^3
}

xs <- seq(-2, 6, len=200)

par(mfrow=c(2,2))
par(mar=c(4,4,1,1))

#############
## upper row
#############
alpha <- 0.1
plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 2
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha) #, MaxN=8)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(2, 250, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

######################
## search x upper row
######################
x.path <- c(x0,logs)

plot(x.path, f(x.path), type='b', 
  xlab='x', ylab='f(x)',
  col='green', pch=20)

points(x0, f(x0), pch='o', col='blue')
points(found.x, f(found.x), 
  pch='X', col='red')

lines(xs, f(xs), col='gray', lty=2)


#############
## lower row
#############
alpha <- 1
plot(xs, f(xs), type='l', xlab='x', ylab='f(x)')

x0 <- 2
points(x0, f(x0), pch='o', col='blue')

logs <- gd(x0, df, alpha=alpha) #, MaxN=8)
N.logs <- length(logs)
points(logs, f(logs), pch=20)
found.x <- logs[N.logs]
points(found.x, f(found.x), 
  pch='X', col='red')
text(2, 250, 
  paste('alpha = ', alpha, '\nN = ', N.logs,
  '\nfinal x = ', round(found.x,5)))

######################
## search x lower row
######################
x.path <- c(x0,logs)

plot(x.path, f(x.path), type='b', 
  xlab='x', ylab='f(x)',
  col='green', pch=20, xlim=c(-2,6), ylim=c(0,300))

points(x0, f(x0), pch='o', col='blue')
points(found.x, f(found.x), 
  pch='X', col='red')

lines(xs, f(xs), col='gray', lty=2)

##dev.copy2eps(file='gdStepSizeDiverge.eps')