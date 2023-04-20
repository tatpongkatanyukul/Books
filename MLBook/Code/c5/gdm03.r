#######################################
## Gradient Descent with Momentum
#######################################

gdm <- function(grad, f, x0, alpha=0.01, beta=0.2, tol=0.00001, MaxN=1000, log=FALSE){

  D <- length(x0)

  p <- matrix(0, D, 1)  
  logs <- matrix(c(0,p,x0),1+2*D,1)

  diff <- tol*2
  for (i in 1:MaxN) {

    gradF <- grad(x0)

    p <- - gradF + beta*p

    x <- x0 + alpha*p

    x0 <- x    
    logs <- cbind(logs, c(i,p,x))

    diff <- sqrt( mean((gradF)^2) )
    if (diff < tol) break
  }## for

  if(log){ return(logs) }
  else { return(x) }
}## end gdm




#######################################################

grad.desc <- function(grad, f, x0, alpha=0.01, tol=0.00001, MaxN=1000, log=FALSE){

  D <- length(x0)

  logs <- matrix(c(0,0,x0),2+D,1)

  diff <- tol*2
  for (i in 1:MaxN) {

    gradF <- grad(x0)
    x <- x0 - alpha*gradF

    diff <- sqrt( mean((gradF)^2) )
    x0 <- x    
    logs <- cbind(logs, c(i,alpha,x0))

    if (diff < tol) break
  }## for

  if(log){ return(logs) }
  else { return(x) }
}## end grad.desc


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


##############################
## main 
##############################

fobj <- function(x){ 
  - exp( -(4*x[1]^2 - 7*x[1] - 3*x[1]*x[2]
           -24*x[2] + 5*x[2]^2 + 43) )
}

df <- function(x){ 
  fobj(x)*matrix(c(-8*x[1] + 7 + 3*x[2],
                    3*x[1] +24 - 10*x[2]), 2, 1)
}

logs <- steepestdescend(df, fobj, x0=matrix(c(2.5,3),2,1), log=TRUE, tol=1e-5)

N <- 80

#############################
## Plot Momentum Motivation
#############################

x1 <- seq(1,3,len=N)
x2 <- seq(2,4,len=N)
x1x2 <- expand.grid(x=x1, y=x2)

z <- matrix(apply(t(x1x2),2,fobj),N,N)

par(mfrow=c(2,2))
par(mar=c(3,3,3,1))

## P1 ##
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") ) 
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]

# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x1, x2, z, phi=30, theta=-10, 
        col=color[facetcol],border=NA,
        main='3D Plot\nobjective function')

## P2 ##

contour(x1, x2, z,
        main='Contour Plot\nobjective function')


## P3 ##

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
        main='Zig-zag behavior')
lines(path.x, path.y, col='black', lty=1)
points(logs[3,], logs[4,], pch=20)

## P4 ##

contour(xs, ys, z, ylim=range.y, 
                   xlim=range.x,
        main='Directions')
lines(path.x, path.y, col='gray', lty=1)
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
text(path.x[3]-0.03, path.y[3]+0.004, 'd')


##dev.copy2eps(file='MomentumMotivation02.eps')

#############################
## Plot Momentum Motivation
#############################

par(mfrow=c(1,3))
par(mar=c(3,3,3,1))

## P1 ##
sz = 0.00004
xa = 2-sz
xb = 2+sz
ya = 3-sz
yb = 3+sz

x1 <- seq(xa,xb,len=N)
x2 <- seq(ya,yb,len=N)
x1x2 <- expand.grid(x=x1, y=x2)
#range.y = c(2.98,3.03) 
#range.x = c(1.98,2.08) 

range.y = c(ya,yb) 
range.x = c(xa,xb) 

z <- matrix(apply(t(x1x2),2,fobj),N,N)
contour(x1, x2, z, ylim=range.y, 
                   xlim=range.x,
        main='Steepest descent')
lines(logs[3,], logs[4,], col='blue', lty=2)
##points(logs[3,], logs[4,], pch=paste(1:ncol(gd.logs)))
points(logs[3,], logs[4,], pch=1)


## P2 ##

z <- matrix(apply(t(x1x2),2,fobj),N,N)
contour(x1, x2, z, ylim=range.y, 
                   xlim=range.x,
        main='Gradient descent')
gd.logs <- grad.desc(df, fobj, alpha=0.125, x0=matrix(c(2.5,3),2,1), log=TRUE, tol=1e-5)


lines(gd.logs[3,], gd.logs[4,], col='blue', lty=2)
#points(gd.logs[3,], gd.logs[4,], pch=paste(1:ncol(gd.logs)))
points(gd.logs[3,], gd.logs[4,], pch=1)

## P3 ##

z <- matrix(apply(t(x1x2),2,fobj),N,N)
contour(x1, x2, z, ylim=range.y, 
                   xlim=range.x,
        main='Gradient descent\nw/ momentum')
gdm.logs <- gdm(df, fobj, alpha=0.125, beta=0.1, x0=matrix(c(2.5,3),2,1), log=TRUE, tol=1e-5)

lines(gdm.logs[4,], gdm.logs[5,], col='blue', lty=2)
#points(gdm.logs[4,], gdm.logs[5,], col='blue', pch=paste(1:ncol(gd.logs)))
points(gdm.logs[4,], gdm.logs[5,], col='blue', pch=1)

##dev.copy2eps(file='gdm02.eps')

