##

mammo <- read.csv("mammographic_masses.data", sep = ",", header = FALSE)

dim(mammo) ## 961 x 6

####################################
## Doing data type for convenience
####################################

mam1 <- mammo
mam1[mam1 == '?'] <- NA
mam2 <- apply(mam1, 2, as.numeric)
rownames(mam2) <- rownames(mam1)

summary(mam2)

############################### 
## dealing with missing data
###############################

###################################################
##  Option 1: ignore records with missing values
###################################################

mam3 <- na.omit(mam2)

###################################################
##  Option 2: replace any record of missing values 
##            with a set of records of all possible 
##            attribute values
###################################################

  for(i in 1:6){  ## go through each field

    if( sum(is.na(mam2[,i])) > 0 ){

        ## Identify missing records
        missing.ids <- which(is.na(mam2[,i]))
        N.miss <- length(missing.ids)

        ## Find possible attribute values
        unique.vals <- setdiff(unique(mam2[,i]), NA)
        N.vals <- length(unique.vals)

        for(j in 1:N.miss){
           new.rec <- matrix(mam2[missing.ids[j],], N.vals, 6, byrow=T)
           for(k in 1:N.vals){
              new.rec[k,i] <- unique.vals[k]
           }
           mam2 <- rbind(mam2, new.rec)
        }##end for j


        mam2 <- mam2[-missing.ids,]
    }##end if

  }##end for i

dim(mam2)

###############################
## Prepare data
###############################

source('nnBiClass02.r')

N <- nrow(mam2)
ids <- sample(N)

N1 <- round(N*0.8)   ## number of datapoints for X-val/Train
N2 <- N - N1         ## number of datapoints for final testing

set1.X <- t(mam2[ids[1:N1],1:5])
set1.T <- t(mam2[ids[1:N1],6])
r <- normalize(set1.X, returnParms=TRUE)
set1.Xn <- r$data

set2.X <- t(mam2[ids[-1:-N1],1:5])
set2.T <- t(mam2[ids[-1:-N1],6])
set2.Xn <- normalize(set2.X, means=r$means, stdevs=r$stdevs)


###############################
## Prepare validation folds
## 5 folds
###############################

fold.Ns <- c(370, 369, 369, 369, 369)
fold.ids <- list()

id1s <- sample(N1)

id.marks <- c(0, cumsum(fold.Ns))
for(i in 1:5){
   
   fold.ids[[i]] <- id1s[ (id.marks[i]+1):id.marks[i+1] ]

}



####################################
## Finding hyperparameters
####################################

Ms <- c(5, 10, 30)

## triplet of rhoh, rhoo, and # epochs
rho.Ns <- list(c(0.01, 0.001, 5000),
               c(0.01, 0.01, 5000),
               c(0.001, 0.01, 5000))

nets <- list()

## variables to save [m, rhoh, rhoo, Nepochs, fold, error]
records <- matrix(0, 60, 6) 

i.rec <- 1

cat('Start at \n')
print(Sys.time())

for(m in Ms){
for(j in 1:3){

   rho1=rho.Ns[[j]][1]
   rho2=rho.Ns[[j]][2]
   Nepoch=rho.Ns[[j]][3]

   for( i in 1:5 ){

       cat('** Running :', i.rec, ' of 60\n')

       train.Xn <- set1.Xn[,-fold.ids[[i]]]
       train.T <- set1.T[,-fold.ids[[i]],drop=F]

       xval.Xn <- set1.Xn[,fold.ids[[i]]]
       xval.T <- set1.T[,fold.ids[[i]],drop=F]
       xval.N <- ncol(xval.T)

       net <- nnTrain(train.Xn, train.T, 
         nHiddens=m, rhoh=rho1, rhoo=rho2, wmax=0.5, 
         nEpochs=Nepoch, plottitle=paste('Cost at run ', i.rec))
   
       nets[[i.rec]] <- net
       xval.y <- hardlimit(nnOutput(net, xval.Xn))
       correct.rate <- sum(xval.y == xval.T)/xval.N

       records[i.rec,] <- c(m, rho1, rho2, Nepoch, i, correct.rate)     
       cat('Done:', i.rec, ' of 60; correct = ', round(correct.rate,2), '\n')

       i.rec <- i.rec + 1

   }##end for i

}##end j
}##end for m

cat('Done at \n')
print(Sys.time())

## ~25 min. run.

##save(records, nets, set2.Xn, set2.T, file='xvalRecords03.RData')
##write(t(records[1:45,]), file = "xvalRecords03.txt", sep=',', ncolumns=6)

##load('xvalRecords03.RData')


par(mfrow=c(5,9))
par(mar=c(2,2,2,1))
for(i in 1:45){
   plot(1:5000, nets[[i]]$errors, type='l', xlab='epochs', ylab='cost',
      main=paste('Train: ', i))


}


#############

par(mfrow=c(4,5))
par(mar=c(2,2,2,1))
for(i in 1:20){
   plot(1:5000, nets[[i]]$errors, type='l', xlab='epochs', ylab='cost',
      main=paste('Train: ', i))
}

##dev.copy2eps(file='trainMammoSample.eps')

########################################
## averaging 5 fold results
########################################
avg.recs <- matrix(0, 9, 5)

for(i in 1:9){
   j <- (i-1)*5+1

   avg.recs[i,] <- c(records[j, 1:4], mean(records[j:(j+4),6]))

}



###################################################
## Found hyperparameters: $30$, $0.001$, $0.01$
###################################################

fnets <- list()

for(i in 1:10){

       net <- nnTrain(set1.Xn, set1.T, 
         nHiddens=30, rhoh=0.001, rhoo=0.01, wmax=0.5, 
         nEpochs=5000, plottitle=paste('Cost at run ', i))

       fnets[[i]] <- net

}## end for i

##save(fnets, file='mammo03fnets.RData')
load('mammo03fnets.RData')



par(mfrow=c(5,2))
par(mar=c(2,2,2,1))

for(i in 1:10){

   set2.y <- hardlimit(nnOutput(fnets[[i]], set2.Xn))
   correct.rate <- sum(set2.y == set2.T)/N2

   plot(1:5000, fnets[[i]]$errors, type='l', xlab='epochs', ylab='cost',
      main=paste('net ', i, ', correct ', round(correct.rate,3)))

}

##dev.copy2eps(file='mammo3final1.eps')


####################
## Precision/Recall
####################


   set2.y <- hardlimit(nnOutput(fnets[[1]], set2.Xn))
   correct.rate <- sum(set2.y == set2.T)/N2

true.pos <- sum( (set2.y == 1) & (set2.T == 1) )
true.neg <- sum( (set2.y == 0) & (set2.T == 0) )
false.pos <- sum( (set2.y == 1) & (set2.T == 0) )
false.neg <- sum( (set2.y == 0) & (set2.T == 1) )

r.precis <- true.pos/(true.pos + false.pos)
r.recall <- true.pos/(true.pos + false.neg)


####################
## F Score
####################

F.score <- 2*r.precis*r.recall/(r.precis + r.recall)

####################
## ROC
####################
