mywave <- function(x1, x2){

return(exp(-(x1^2 + x2^2)/10))
}

x1s <- seq(-10, 10, len=50)
x2s <- seq(-10, 10, len=50)
z <- matrix(0, 50, 50)

for(i in 1:50){
for(j in 1:50){
  z[i,j] <- mywave(x1s[i], x2s[j])
}}

persp(x1s, x2s, z, col='ivory', shade=0.7, theta = 135, phi = 30)