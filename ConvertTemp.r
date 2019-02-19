convert.temp <- function(degrees.F, plot.wanted=TRUE, ...) {
  #
  #   To convert a vector of temperatures from degrees Fahrenheit
  #   to degrees Celsius, and optionally to plot the results. 
  #   Arguments:
  #
  #   degrees.F   A vector of temperatures in Fahrenheit
  #   plot.wanted A logical value indicating whether or not
  #               a plot should be produced
  #   ...         Other arguments to the plot() command
  #               (see help(par) for details - and good luck!)
  #
  #   The value of the function is a vector of temperatures
  #   in degrees Celsius, corresponding to the input 
  #   vector degrees.F. If a plot is requested, a side
  #   effect is to produce a plot on the current graphics
  #   device.
  #
  degrees.C <- (degrees.F - 32) * 5/9
  if (plot.wanted) plot(degrees.F, degrees.C, ...)
  degrees.C
}
