## Created Aug 3rd, 2013
## Modified from help(getGraphicsEvent)

library(grid)

####################
## load trained net
####################

load('chosenNet.RData')

###################
## functions
###################

gray2 = seq(0,1, len=2)

disp.img <- function(ImageData,...){
   xs <- seq(1,16)
   ys <- xs
   z <- matrix(ImageData, 16, 16)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr), ...)
}

nnOutput <- function (net,X) {

  K <- nrow(net$W2)
  N <- ncol(X)

  dotX <- rbind(1,X)
  Z <- sigmoid( net$W1 %*% dotX )
  dotZ <- rbind(1,Z)
  A <- net$W2 %*% dotZ
  Y <- exp(A)/matrix(colSums(exp(A)), K, N, byrow=TRUE)

  return(Y)
}

sigmoid <- function (a)  1 / (1 + exp(-a))

decode.OK <- function(Y.K, classes=rownames(Y.K)){
  if (is.null(classes)) {
    classes <- as.character(seq(1,nrow(Y.K)))
  }# end if

  Y.class <- classes[apply(Y.K, 2, which.max)]
  return(matrix(Y.class,nrow=1))
}

####################
## Global variable
####################

zs <<- matrix(0, 16, 16)

####################
## draw.off
####################

draw.off <- function(){
	X11()
	p=par(mfrow=c(2,1))
		
		zs.zip <- matrix(0, 256, 1)

		zs.zip <- 2*c(t(apply(zs,1, rev))) - 1
		disp.img(zs.zip, axes=F, col=gray2, main='Draw')
          
	    draw.y <- nnOutput(net, matrix(zs.zip, 256, 1))

		plot(0:9, log(draw.y),
			main=paste('Output', decode.OK(draw.y)),
                  xlab='digit', ylab='y')

	par(p)

}##end draw.off


####################
## draw.on
####################

draw.on <- function() {

	xs <- (1:16 - 0.5)/16
	ys <- (1:16 - 0.5)/16

	##################
	## Draw template
	##################
	
	grid.polygon(c(0,1,1,0), c(0,0,1,1), 
      gp = gpar(col='blue', fill='yellow'))

	x.grid <- rep(xs, each=16)
	y.grid <- rep(ys, times=16)

	grid.rect(x = x.grid, 
		y = y.grid,
		width = 1/16, 
		height = 1/16,
		gp = gpar(col="red"))

	#################################
	## Define functions and handlers
	#################################

	setZ <- function(x, y){
		ix <- which.min((xs - x)^2)
		iy <- which.min((ys - y)^2)

		zs[ix, iy] <<- 1

	return(list(i=ix, j=iy))
	}##end setZ

	drawZ.all <- function(Z){

		for(i in 1:16){
		for(j in 1:16){
			if(zs[i,j] == 1){
				grid.rect(x = xs[i], y = ys[j],
				width = 1/16,	height = 1/16,
				gp = gpar(col='blue', fill='red'))
			}else{
				grid.rect(x = xs[i], y = ys[j],
				width = 1/16,	height = 1/16,
				gp = gpar(col='blue', fill='yellow'))
			}

		}}##end for ij

	}##end drawZ.all

	drawZ <- function(ix, iy){

			if(zs[ix,iy] == 1){
				grid.rect(x = xs[ix], y = ys[iy],
				width = 1/16,	height = 1/16,
				gp = gpar(col='blue', fill='red'))
			}else{
				grid.rect(x = xs[ix], y = ys[iy],
				width = 1/16,	height = 1/16,
				gp = gpar(col='blue', fill='yellow'))
			}

	}#end drawZ

	##############
	## Handlers
	##############

    startx <- NULL
    starty <- NULL
    usr <- NULL
	write.on <- FALSE

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
        
    dragmousedown <- function(buttons, x, y) {
        startx <<- x
        starty <<- y
        devset()
        usr <<- par("usr")
        write.on <<- !(write.on)

        if(write.on){
           eventEnv$onMouseMove <- dragmousemove

			loc.Z <- setZ(x, y)
			drawZ(loc.Z$i, loc.Z$j)

        } else {
	    	eventEnv$onMouseMove <- NULL
        }##end if(write.on)

        NULL
    }
    
    dragmousemove <- function(buttons, x, y) {
        devset()

			loc.Z <- setZ(x, y)
			drawZ(loc.Z$i, loc.Z$j)        
        NULL
    }
    
    mouseup <- function(buttons, x, y) {    
    	eventEnv$onMouseMove <- NULL
    }	
        
    keydown <- function(key) {

        draw.off()

        eventEnv$onMouseMove <- NULL
        NULL
    }
    
    setGraphicsEventHandlers(prompt="Click and drag, hit q to quit",
                     onMouseDown = dragmousedown,
                     onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()

}## end draw.on()

##################
## main
##################


savepar <- par(ask=FALSE)

draw.on()
# This currently only works on the Windows
# and X11(type = "Xlib") screen devices...

getGraphicsEvent()

par(savepar)

