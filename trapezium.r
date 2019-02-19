#Approximate an integral using the trapezium rule with equally spaced x values

trapezium <- function(v,a,b) {
  #v is the vector containing all the y values
  #a is the lower limit of integration
  #b is the upper limit of integration
  n <- length(v)-1
  h <-(b-a)/n
    intv <- (h/2) * (2*sum(v) - v[1] - v[n+1]) ## intv is the calculated integral. You may
    ## need a couple of lines for this, but
    ## DO NOT USE A LOOP!!! You might want to
    ## know that the sum() command can be
    ## used to sum the elements of a vector.
    # and return intv
    intv
}

