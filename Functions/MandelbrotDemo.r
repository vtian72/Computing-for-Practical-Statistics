#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######       To demonstrate the use of the functions in          ######
######    MandelbrotFunctions.r, for calculating and mapping     ######
######                  the Mandelbrot set                       ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
#
#   Load the functions
#
source("MandelbrotFunctions.r")
#
#   A very coarse-resolution version using the slow version with loops
#
Mandelbrot1(50,50)
#
#   The same with the fast version
#
Mandelbrot2(50,50)

#
#   Make a high-quality image, writing directly to a PDF file. Don't
#   even try to use the Mandelbrot1 function for this! Uncomment it
#   if you want to try it: it's commented out because it will 
#   probably take a few minutes on a UCL Desktop computer (it
#   takes under a minute on my laptop ...)
#
# pdf("MandelbrotSet_Full.pdf",width=8,height=5)
# par(mar=c(1,1,1,1))
# Mandelbrot2(1000,1000,xlab="",ylab="",axes=FALSE)
# box(lwd=2)
# dev.off()
#
#   Now an interactive series of zooms. Do this on a large-ish x11() 
#   window so that locator() can be used to select zoom areas
#
x11(width=10,height=7)
par(mfrow=c(2,2),mar=c(1,1,2,1)) # 2*2 plot array, narrow margins except at top
Mandelbrot2(500,500,max.iter=1000,
            xlab="",ylab="",axes=FALSE,main="Complete Mandelbrot set")
box(lwd=2)
for (i in 1:3) {
  cat("Click 2 points on plot to select zoom region ...\n")
  ZoomRegion <- locator(2)
  rect(ZoomRegion$x[1],ZoomRegion$y[1],ZoomRegion$x[2],ZoomRegion$y[2],border="yellow")
  cat(paste("Working on region (",ZoomRegion$x[1],",",ZoomRegion$x[2],
            ") x (",ZoomRegion$y[1],",",ZoomRegion$y[2],") ...\n",sep=""))
  Mandelbrot2(500,500,max.iter=1000,
              xlim=sort(ZoomRegion$x),ylim=sort(ZoomRegion$y),
                      xlab="",ylab="",axes=FALSE,main=paste("Zoom",i))
  box(lwd=2)
}
dev.copy(pdf,"MandelbrotSet_Zooms.pdf",width=10,height=7)
dev.off()
