## Created Aug 3rd, 2013
## Modified from help(getGraphicsEvent)

##rm(list=ls())

##source('loadTrainedZIPImage01a.r')
source('loadTrainedZIPImage01a.r')
library(grid)

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
##		displayzipimage(test.X[,1], p.title='zip')
		
		zs.zip <- matrix(0, 256, 1)

		zs.zip <- 2*c(t(apply(zs,1, rev))) - 1
		displayzipimage(zs.zip, p.title='draw')

	    draw.y <- nnOutput(net, matrix(zs.zip, 256, 1))

		plot(0:9, log(draw.y),
			main=paste('ANN output = ', which.class(draw.y)))

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
	
##    X11()
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

cat(paste('x: ', x, ': ix = ', ix,
			'y: ', y, ': iy = ', iy, '\n'))


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

##		 cat(paste('m.down: x=', x, ',y=',y,'\n'))
        NULL
    }
    
    dragmousemove <- function(buttons, x, y) {
        devset()

			loc.Z <- setZ(x, y)
			drawZ(loc.Z$i, loc.Z$j)
        
##		 cat(paste('m.move: x=', x, ',y=',y,'\n'))

        NULL
    }
    
    mouseup <- function(buttons, x, y) {    
    	eventEnv$onMouseMove <- NULL
    }	
        
    keydown <- function(key) {

		draw.off()

        if (key == "q")	return(invisible(1))
        eventEnv$onMouseMove <- NULL
        NULL

    }
    
    setGraphicsEventHandlers(prompt="Click and drag, hit q to quit",
                     onMouseDown = dragmousedown,
##                     onMouseUp = mouseup,
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

