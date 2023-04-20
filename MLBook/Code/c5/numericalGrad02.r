## modified from Andrew Ng's computeNumericalGradient.m (Aug 1st, 2014)

numericalGrad <- function(cost.fn, w, epsilon=1e-4){
## cost.fn: r function returning cost function.
## w: a vector of parameters
## cost.fn(w) returns a single value of cost function.

  Nw = length(w)

  ## Initialize numgrad with zeros
  numgrad = matrix(0, Nw, 1);

  for( i in 1:Nw){
    theta1 = w;
    theta2 = w;
    theta1[i] = w[i] + epsilon;
    theta2[i] = w[i] - epsilon;

    c1 = cost.fn(theta1)
    c2 = cost.fn(theta2)

    numgrad[i] = ( c1 - c2 )/(2*epsilon);    
  }##end for

return(numgrad)
}##end numericalGrad(..)

