theta1 <- 6*pi/10
vq1 <- matrix(c(cos(theta1), sin(theta1)), 2,1)

par(mfrow=c(2,5))
par(mar=c(2,2,2,1))

for(i in 1:20){

 plot(0, 0, ylim=c(-1.5,1.5), xlim=c(-1.5,1.5), 
   xlab='x', ylab='y',
   main=paste('v2 of ', round(i/10,2), ' pi'))

 lines(c(cos(theta1-pi/2), cos(theta1+pi/2)),
   c(sin(theta1-pi/2), sin(theta1+pi/2)), 
   col='gray', lty=2)

 v2 <- matrix(c(cos(i/10*pi), sin(i/10*pi)), 2,1)

 vs <- t(vq1) %*% v2

 lines(c(0, vq1[1]), c(0, vq1[2]), col='red')
 text(vq1[1]+0.2, vq1[2]+0.2, 
   paste(round(theta1/pi,2), 'pi'))
 lines(c(0, v2[1]), c(0, v2[2]), col='blue')
 text(0,-0.5, paste('v1*v2=', round(vs,2)))

 if(i == 10){
    x11()
    par(mfrow=c(2,5))
    par(mar=c(2,2,2,1))
 }
}
