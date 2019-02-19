Mandelbrot1 <- function(nx,ny,xlim=c(-2,1),ylim=c(-1,1),max.iter=1000,
                        ColourScale=NULL,...) {
  #############################################################################
  #
  #   This is a function to compute (approximately) the rate of 
  #   divergence of the complex-valued sequence 0, z_1, z_2, z_3, ... 
  #   where z_i = (z_{i-1})^2 + c and c is a complex number. The 
  #   rate of divergence is evaluated over a grid of values of c
  #   in the complex plane, and then plotted. The algorithm 
  #   used is a direct implementation of that given at
  #   https://en.wikipedia.org/wiki/Mandelbrot_set, looping
  #   over all the grid nodes and computing the value separately
  #   for each one. The arguments are:
  #
  #     nx,ny         The numbers of grid nodes in the x- and y-directions
  #     xlim,ylim     The plotting limits in the x- and y-directions. The 
  #                   default values will display the entire Mandelbrot set; 
  #                   changing these will enable you to "zoom in" and 
  #                   see the fractal structure.
  #     max.iter      The maximum number of iterations to use at each
  #                   grid node
  #     ColourScale   A vector of colours to use for the image, of length
  #                   max.iter. If NULL (the default), the function uses
  #                   an elegant blue-purple scale which shows the structures 
  #                   quite nicely. 
  #     ...           Other arguments to the image command.
  #
  #   The function returns a matrix with nx rows and ny columns, containing
  #   the numbers of iterations at each grid node. As a side effect, an
  #   image of the Mandelbrot set is created on the current graphics device.
  #
  #   NOTE that this function is SLOW because it uses loops: therefore it 
  #   should not be used for nx and ny bigger than about 100. If you want 
  #   a higher-resolution image, use the function Mandelbrot2 instead.
  #
  #############################################################################
  #
  #   First step: define a colour scale if the user hasn't supplied one.
  #   This modifies one of the examples given in the help for
  #   colorRampPalette, to produce something fairly classy :-)
  #   It goes from "deep purple" (10% red, 10% green and 20% blue) to
  #   completely transparent blue: the effect being that "low" values
  #   look deep purple and "high" values look almost white. The
  #   intermediate points on the scale are included to ensure that 
  #   it doesn't go white too quickly. 
  #
  if (is.null(ColourScale)) {
    colscl <- 
      colorRampPalette(c(rgb(0.1,0.1,0.2,1.0),      #
                         rgb(0.1,0.0,0.3,1.0),      #
                         rgb(0.1,0.0,0.5,1.0),      #
                         rgb(0.0,0.0,1.0,0.0)), alpha=TRUE)(1000)
  } else { # If a colour scale was provided, use it.
    colscl <- ColourScale
  }
  #
  #   Next: define grids of x and y values, with nx and ny elements
  #   respectively and spanning the requested ranges of x and y
  #
  x.grid <- seq(xlim[1],xlim[2],length.out=nx)
  y.grid <- seq(ylim[1],ylim[2],length.out=ny)
  #
  #   Set up a matrix for storing the results
  #
  mandelbrot <- matrix(nrow=nx,ncol=ny)
  #
  #   And loop over each pixel. The algorithm is taken directly from 
  #   the Wikipedia page given above, so no further comments EXCEPT
  #   to note the condition "iter < max.iter" in the while() loop:
  #   this is an "escape clause" so that you can get out if it looks
  #   as though x^2 + y^2 is never going to be bigger than 4.
  #
  for (i in 1:nx) {
    for (j in 1:ny) {
      x <- 0
      y <- 0
      iter <- 0
      while (x^2 + y^2 < 4 & iter < max.iter) {  # NB escape clause!
        xtemp <- x^2 - y^2 + x.grid[i]
        y <- 2*x*y + y.grid[j]
        x <- xtemp
        iter <- iter + 1
      }
      mandelbrot[i,j] <- iter
    }
  }
  #
  #   Now plot the image. The log(log()) is simply because the image() 
  #   command divides the range of the object to be plotted into equal 
  #   intervals when assigning colours, and the distribution of values 
  #   in the "mandelbrot" is very highly skewed with a huge majority 
  #   of values at the top end: if this is mapped directly in the image
  #   command therefore, almost everything comes out in the last 
  #   few colours. By taking logs twice, a much more uniform distribution
  #   of values is obtained so that all the colours are used. The 
  #   zlim argument is provided simply in order to ensure that exactly
  #   the same colour mapping is used every time, regardless of the 
  #   choices of xlim and ylim (which might lead to differences in the 
  #   ranges of values stored in the "mandelbrot" matrix)
  #
  image(x.grid,y.grid,log(log(mandelbrot)+1),col=colscl,
        zlim=c(0,log(log(max.iter)+1)),...)
  #
  #   And return the matrix, so that the user can work with it if
  #   required. Note the "invisible" command: the matrix will usually
  #   be very large (e.g. 100*100 at least, for "accurate" representations
  #   of the Mandelbrot set) so it doesn't make much sense to write it
  #   to the console.
  #
  invisible(mandelbrot)
}

Mandelbrot2 <- function(nx,ny,xlim=c(-2,1),ylim=c(-1,1),max.iter=1000,
                        ColourScale=NULL,...) {
  #############################################################################
  #
  #   This function is a fast alternative to Mandelbrot1, to compute 
  #   (approximately) the rate of divergence of the complex-valued 
  #   sequence 0, z_1, z_2, z_3, ... where z_i = (z_{i-1})^2 + c and 
  #   c is a complex number. It uses basically the same algorithm
  #   as Mandelbrot1, but without using loops. It is MUCH, MUCH
  #   faster and can be used to produce images over a grid of size
  #   (e.g.) 1000*1000 in under a minute on a reasonable laptop.
  #   The arguments and result are exactly the same as for 
  #   Mandelbrot1: see the header to that function for details.
  #
  #############################################################################
  #
  #   Define a colour scale. See comments in Mandelbrot1
  #
  if (is.null(ColourScale)) {
    colscl <- 
      colorRampPalette(c(rgb(0.1,0.1,0.2,1.0),      #
                         rgb(0.1,0.0,0.3,1.0),      #
                         rgb(0.1,0.0,0.5,1.0),      #
                         rgb(0.0,0.0,1.0,0.0)), alpha=TRUE)(1000)
  } else { 
    colscl <- ColourScale
  }
  #
  #   This time, to avoid loops the x.grid and y.grid objects aren't
  #   just vectors, they are *matrices* of dimension nx*ny: each 
  #   row of x.grid contains a single x value repeated ny times, and
  #   each column of y.grid contains a single y value repeated nx 
  #   times. Essentially we're just creating multiple copies of the 
  #   grid vectors, so that the (i,j)th entry of each matrix
  #   contains the value for the corresponding grid node.
  #   
  x.grid <- matrix(rep(seq(xlim[1],xlim[2],length.out=nx),ny),ncol=ny)
  y.grid <- matrix(rep(seq(ylim[1],ylim[2],length.out=ny),nx),ncol=ny,byrow=TRUE)
  mandelbrot <- matrix(nrow=nx,ncol=ny)
  #
  #   All other objects are matrices as well, including a *logical*
  #   matrix called NotDone, which will contain TRUE values for 
  #   grid nodes that aren't "done" yet (i.e. that still need to 
  #   be updated further) and FALSE values for grid nodes that 
  #   *are* done. At the outset, obviously none of the grid
  #   nodes are "done", so NotDone should be a matrix of TRUE
  #   values.
  #
  x <- y <- xtemp <- matrix(0,nrow=nx,ncol=ny)
  NotDone <- (x==0)
  iter <- 0 # Keep track of the iteration number
  #
  #   In the next line, the "while" condition is to continue
  #   so long as there is *any* grid node that is not yet
  #   "done" (and to terminate in any case if the maximum
  #   number of iterations is exceeded). The expressions in 
  #   the while loop then operate directly and simultaneously
  #   on *all* of the relevant elements of the matrices. 
  #   Notice that you're used to referencing a matrix element
  #   as (for example) x[i,j] for row i and column j. Here 
  #   however, because NotDone is itself a matrix of the 
  #   same dimensions as everything else, it is perfectly
  #   legitimate to write x[NotDone] with no comma in the
  #   middle: this just selects the elements of x 
  #   corresponding to the TRUE elements of NotDone. In
  #   this way, as the while() loop progresses and increasing
  #   numbers of grid nodes get "done", we end up working
  #   with smaller and smaller subsets of the matrices.
  #   The use of a logical object to identify subsets in 
  #   this way is sometimes called "masking" (because it
  #   masks out the bits that we're not interested in at
  #   each iteration).
  #
  while (any(NotDone) & iter < max.iter) {
   xtemp[NotDone] <- (x[NotDone])^2 - (y[NotDone])^2 + x.grid[NotDone]
   y[NotDone] <- 2*x[NotDone]*y[NotDone] + y.grid[NotDone]
   x[NotDone] <- xtemp[NotDone]
   iter <- iter + 1
   mandelbrot[NotDone] <- iter
   NotDone <- (x^2 + y^2 < 4)
  }
  #
  #   Everything else is the same as in Mandelbrot1
  #
  image(x.grid[,1],y.grid[1,],log(log(mandelbrot)+1),col=colscl,
        zlim=c(0,log(log(max.iter)+1)),...)
invisible(mandelbrot)
}
