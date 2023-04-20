## Explore Data

train.zip <- read.table('./imageZIP/zip.train')

testimg <- train.zip[1,-1]

image(seq(1,16), seq(1,16), matrix(rnorm(256),16,16))

image(seq(1,16), seq(1,16), matrix(as.numeric(testimg),16,16))
## see what number it is: train.zip[1,1]

numzip <- apply(train.zip,c(1,2),as.numeric)

image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))
# O.K. I can show the image

# Rearrage it to the correct orientation?
image(seq(1,16), seq(1,16), matrix(numzip[2,-1],16,16))
# This is original

# try
image(seq(1,16), seq(1,16), t(matrix(numzip[2,-1],16,16)))
# equivalent to mirror and rotate pi/2  ccw or rotate pi/2 cw then mirror

# try 
image(seq(1,16), seq(1,16), apply(matrix(numzip[2,-1],16,16),1,rev))
# equivalent to rotate pi/2 ccw

# try
image(seq(1,16), seq(1,16), t(apply(matrix(numzip[2,-1],16,16),1,rev)))
# bingo!

x11()
image(seq(1,16), seq(1,16), t(apply(matrix(numzip[2,-1],16,16),1,rev)))
# displaying image with correct orientation

displayzipimage <- function(ImageData,N=sqrt(length(ImageData))){
   xs <- seq(1,N)
   ys <- xs
   z <- matrix(ImageData, N, N)
   zr <- apply(z,1,rev)
   image(xs,ys,t(zr), col=gray.colors(12))
}

which(numzip[1:100,1] == 9)


layout(matrix(1:10, 2,5,byrow=T))
par(mar=c(1,1,1,1))
displayzipimage(-numzip[9,-1]) ## 0
par(mar=c(1,1,1,1))
displayzipimage(-numzip[8,-1]) ## 1
par(mar=c(1,1,1,1))
displayzipimage(-numzip[42,-1]) ## 2
par(mar=c(1,1,1,1))
displayzipimage(-numzip[5,-1]) ## 3
par(mar=c(1,1,1,1))
displayzipimage(-numzip[3,-1]) ## 4

par(mar=c(1,1,1,1))
displayzipimage(-numzip[2,-1]) ## 5
par(mar=c(1,1,1,1))
displayzipimage(-numzip[1,-1]) ## 6
par(mar=c(1,1,1,1))
displayzipimage(-numzip[4,-1]) ## 7
par(mar=c(1,1,1,1))
displayzipimage(-numzip[18,-1]) ## 8
par(mar=c(1,1,1,1))
displayzipimage(-numzip[87,-1]) ## 9

dev.copy2eps(file='zipdigits.eps')
