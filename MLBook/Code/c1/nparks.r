list.parks <- function(i=NULL){
   parks.to.see <- c('���˭�', '�觡�Шҹ', '�����')

   N <- length(parks.to.see)

   if(is.null(i)){
      i <- 1:N
   }
   for(p in i){
      cat(paste('park: ', parks.to.see[p], '\n'))
   }

}

list.parks()