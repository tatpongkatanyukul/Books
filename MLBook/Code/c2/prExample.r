##

PCred = 0.4
PCblue = 0.6

PgR = 9/12
PoR = 3/12

PgB = 2/10
PoB = 8/10

N = 1000

recs = matrix(0, 2, N)

for(i in 1:N){

  r = runif(1)
  if(r < PCred){
  ## Crate is red
    recs[1,i] = 'r'

    r = runif(1)
    if(r < PgR){
    ## Ball is green
      recs[2,i] = 'g'
    } else {
    ## Ball is orange
      recs[2,i] = 'o'
    }

  } else {
  ## Crate is blue
    recs[1,i] = 'b'

    r = runif(1)
    if(r < PgB){
    ## Ball is green
      recs[2,i] = 'g'
    } else {
    ## Ball is orange
      recs[2,i] = 'o'
    }

  }

}##end N

recs

Cr = sum(recs[1,] == 'r')
Cb = sum(recs[1,] == 'b')

Cg = sum(recs[2,] == 'g')
Co = sum(recs[2,] == 'o')

