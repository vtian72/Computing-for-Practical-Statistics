#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
QuarticMin <- function(x0,tol=1e-6,MaxIter=100,Verbose=TRUE) {
#######################################################################
##
##    Function to illustrate the Newton-Raphson method for 
##    minimising the expression h(x) = x^4 - 2x^3 + 3x^2 - 4x + 5.
##    The arguments are:
##
##    x0      An initial guess at the location of the minimum.
##    tol     The "tolerance": the algorithm is judged to 
##            have converged when either the gradient or the
##            relative change in the estimate is less than 
##            this. The default value is 10^-6.
##    MaxIter The maximum number of iterations allowed.
##    Verbose A logical value, indicating whether or not to 
##            print the iteration history to the console as
##            the algorithm progresses.
##
##    The function returns a list object, with named components
##    "x" (the value of x at the minimum), "hx" (the value of
##    h(x) at the minimum), "gradient" (the value of h'(x) at
##    the minimum) and "N.iter" (the number of iterations)
##
#######################################################################
  #
  # Start by initialising all the quantities that we need, 
  # as follows:
  #
  # x         Current value of the estimate (initialise to x0)
  # Iter      Number of iterations (initialise to 0)
  # dh.dx     Current gradient: this is 4x^3 - 6x^2 + 6x - 4.
  # RelChange The relative change in the estimate. Since we're
  #           just starting, we don't yet know what this is:
  #           we should initialise it to something large, like
  #           infinity (you might be tempted to initialise it 
  #           to zero, but then the algorithm will think that
  #           it's converged as soon as it starts because the
  #           "while" condition below will be FALSE!).
  #
  x <- x0
  Iter <- 0
  dh.dx <- (4*x^3) - (6*x^2) + (6*x) - 4
  RelChange <- Inf
  #
  # Now loop. The "while" condition will become FALSE as soon 
  # as EITHER the absolute value of the gradient becomes 
  # less than tol, OR the absolute value of the relative 
  # change becomes less than tol, OR the iteration count
  # reaches MaxIter
  #
  while (abs(dh.dx) > tol & abs(RelChange) > tol & Iter < MaxIter) {
    if (Verbose) {
      cat(paste("Iteration",Iter,":   Estimate =",signif(x,4),
                "    Gradient = ",signif(dh.dx,4),"\n"))
    }
    x.old <- x   # Remember the current estimate
    d2h.dx2 <- (12*x^2) - (12*x) + 6       # Second derivative
    x <- x - (dh.dx / d2h.dx2)             # Update the estimate ...
    dh.dx <- (4*x^3) - (6*x^2) + (6*x) - 4 # ... and the gradient ...
    RelChange <- (x-x.old) / x.old         # ... and the relative change ...
    Iter <- Iter + 1                       # ... and the iteration count.
  }
  if (Verbose) cat("---\n") # Insert separator if writing progress to screen
  hmin <- x^4 - (2*x^3) + (3*x^2) - (4*x) + 5  # Minimised value of h(x)
  list(x=x, hx=hmin, gradient=dh.dx, N.iter=Iter)
}