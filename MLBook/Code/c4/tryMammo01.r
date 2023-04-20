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

source('nnBiClass01.r')

N <- nrow(mam2)
N.train <- round(0.7 * N)
N.val <- round((N - N.train)/2)
N.test <- N - N.train - N.val

train.ids <- sample(N, N.train)
val.ids <- sample(setdiff(1:N, train.ids), N.val)
test.ids <- setdiff(1:N, c(train.ids, val.ids))

train.X <- t(mam2[train.ids,1:5])
train.T <- t(mam2[train.ids,6])
r <- normalize(train.X, returnParms=TRUE)
train.Xn <- r$data

val.X <- t(mam2[val.ids,1:5])
val.T <- t(mam2[val.ids,6])
val.Xn <- normalize(val.X, means=r$means, stdevs=r$stdevs)

test.X <- t(mam2[test.ids,1:5])
test.T <- t(mam2[test.ids,6])
test.Xn <- normalize(test.X, means=r$means, stdevs=r$stdevs)


####################################
## Train
####################################

net <- nnTrain(train.Xn, train.T, 
   nHiddens=10, rhoh=0.001, rhoo=0.0003, wmax=0.5, 
   nEpochs=5000, net=net)

####################################
## Evaluation
####################################

eval.Xn <- cbind(val.Xn, test.Xn)
eval.T <- cbind(val.T, test.T)
eval.y <- hardlimit(nnOutput(net, eval.Xn))
eval.correct <- sum(eval.y == eval.T)/length(eval.T) * 100

eval.correct

##save(train.X, train.T, val.X, val.T, test.X, test.T, 
   net, file='mammo2e01.RData')


