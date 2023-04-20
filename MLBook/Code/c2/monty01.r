##

N = 100
doors = c(1,2,3)

recs = matrix(0, 3, N)
for(i in 1:N){
  ## which door has a car
  car = sample(doors,1)

  ## which door has been chosen
  chosen = sample(doors,1)

  ## which door has the host opened
  open = sample( rep(doors[-c(car, chosen)],2), 1)

  recs[,i] = c(car, chosen, open)
}

## check if open == car
sum(recs[3,] == recs[1,])

## check if open == chosen
sum(recs[3,] == recs[2,])

## count how many times I'll get the car without switching
sum(recs[2,] == recs[1,])

## count how many times I'll get the car with switching
sum(recs[2,] != recs[1,])


