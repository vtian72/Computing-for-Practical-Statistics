#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######  STAT0023 Workshop 1: graphics for exploratory analysis   ######
######                                                           ######
######        Visualising the Fisher/Anderson iris data          ######
######                                                           ######
######  This script illustrates how different graphical          ######
######  displays can be used to explore the structure in         ######
######  a dataset. The data are measurements (in cm) of petal    ######
######  length and width, and sepal length and width, for three  ######
######  species of iris (a type of flower), and they are         ######
######  provided as part of the standard R distribution          ######
######  so there's no need to read them from a file.             ######
######                                                           ######
######  However, this script *does* use some additional          ######
######  libraries, so it also gives an opportunity to            ######
######  practice loading libraries and installing them           ######
######  if they're not already installed.                        ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##	First step: load the required libraries. The ggplot2 library
##  provides some convenient commands for displaying graphics
##  for different groups of observations. The rgl library contains 
##  routines that allow interactive graphics - we'll use the 
##  plot3d() command from this library below. Finally, the 
##  RColorBrewer library provides some easy ways of generating
##  colour scales that are good for specific purposes.
##
##  If you get an error when running the line below, you will need
##  to install the relevant library / libraries. For the 2019
##  session, you may find that the ggplot2, rgl and RColorBrewer 
##  libraries are not installed on the computers in UCL cluster rooms
##  (or in the Desktop@UCLAnywhere system used in the Birkbeck room).
##  Complete instructions for package installation can be found 
##  in the "Introduction to R" document provided on the Moodle 
##  page for Week 1 of the course - but, from a machine in a UCL 
##  cluster room, the short version is
##
##  1.  On the "Tools" menu in RStudio, select "Install Packages"
##  2.  In the "Packages" dialogue, type the name(s) of the package(s)
##      you want to install e.g. rgl, RColorBrewer. Make sure that
##      the "Install dependencies" box is checked. 
##  3.  Click "Install", and wait for the packages to be downloaded
##      and installed (this might take a minute or two).
##
##  If you're using Desktop@UCLAnywhere (e.g. from a cluster room in
##  Birkbeck), you should install the packages ONE AT A TIME. If you
##  get a question "Do you want to install from sources the package
##  which needs compilation?", select "No". The effect of this will
##  be to install some slightly older versions of some packages - 
##  this doesn't matter. If you were to answer "Yes" to this question,
##  R would try to install the most recent versions of the packages
##  which use software that is not available on Desktop@UCLAnywhere. 
##
##  The instructions above will install a *personal* copy of the 
##  packages on your N: drive (for obvious reasons, students aren't 
##  able to add packages to UCL's central R system!). The packages 
##  will then be available to you the next time you start up R, so
##  you only need to do the installation once.
##
library(ggplot2); library(rgl); library(RColorBrewer)
##
##  Next: load the data, look at their structure so that you know
##  what variables are available, and read the documentation so that 
##  you know what the variables represent.
##
data(iris)
str(iris)  # You should know what this does, after the Galapagos example
?iris      # This shows the help page for the iris dataset
##
##  Where would you start with an analysis of these data? Possibly
##  a summary of the dataset:
##
summary(iris)
##
##  Notice that the "Species" variable is not numeric: it's a 
##  "factor". The "summary" command handles this differently 
##  from the other, numeric variables. Notice also that 
##  the data are *balanced* i.e. the same number of observations
##  (how many are there?) for each species. Other than that, 
##  there's not much to be said about these summary statistics. 
##  Nonetheless, you might want to look at summary statistics 
##  separately for each species. One way to do this is to 
##  use the [] construction, for example:
##
mean(iris$Sepal.Length[iris$Species=="setosa"])
mean(iris$Sepal.Length[iris$Species=="versicolor"])
mean(iris$Sepal.Length[iris$Species=="virginica"])
##
##  That gets a bit tedious however. Here's a quicker way:
##
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=mean)
##
##  You can use the same thing to calculate variances, 
##  medians, quartiles etc. 
##
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=var)
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=median)
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=sd)
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=quantile, prob=0.25)
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=quantile, prob=0.75)
##
##  You already know what the next command will give you. Think 
##  about it, then type the command to see if you were right!
##
tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=length)
help(tapply)
##  
##  None of this really gives us much of a feel for the data. 
##  Plotting the data is never a bad idea ... here, start by
##  opening a new graphics window with a specific size, to
##  facilitate copying to a file later on if required. But
##  ONLY do this if we haven't done it already! Note that
##  the dev.cur() command tells you what (if any) is the
##  current graphics device. Also, the x11() command produces a 
##  "windows" device on a Windows computer, and an "X11cairo"
##  device on a Linux machine. The expression 
##
##    names(dev.cur()) %in% c("windows","X11cairo")
##
##  is TRUE if the name of the current device is either "windows"
##  or "X11cairo", and FALSE otherwise. The "!" symbol negates 
##  this: so the next line translates as 'if the current graphics
##  device is *not* a "windows" or "X11cairo" device, open a new
##  one with the specified width and height'. For the x11() 
##  command, the dimensions are given in inches. 
##
if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
plot(iris)
##
##  Note: the "Species" variable has been coded as numeric in 
##  the plots: this is not very appropriate. Also, petal 
##  length and width seem almost perfectly linearly related
##  so perhaps there's no need to consider them both in 
##  the remaining analysis. There seems to be some strange
##  structure in some of the remaining plots however. 
##  Do you have any idea of what might be causing this? 
##
##  Since "Species" is a factor (i.e. a grouping variable),
##  it may be more useful to produce some plots in which 
##  the distributions of other variables are shown separately
##  for each species, to aid comparison. A boxplot is an
##  obvious way to do this. 
##
boxplot(Sepal.Length ~ Species, data=iris,xlab="Species",
        ylab="Sepal length (cm)",
        main="Distributions of sepal lengths by species",
        col="salmon")
##
##  That suggests clear differences between the species:
##  also that the variances look roughly equal for 
##  versicolor and virginica at any rate, and that the 
##  distributions seem roughly symmetric so that assumptions
##  of normality within groups might be reasonable (remember
##  the purposes of an exploratory analysis, from the lecture
##  slides). This gives us an opportunity to remind ourselves
##  how to carry out a t-test for equality of means in 
##  two groups - but first we should check the variances. 
##
var.test(iris$Sepal.Length[iris$Species=="versicolor"],
         iris$Sepal.Length[iris$Species=="virginica"])
##
##  The p-value is greater than 0.05 so we're justified in
##  proceeding with a t-test as though the variances are equal.
##  Notice also that the output gives a confidence interval
##  for the ratio of variances between the two groups. 
##
##  Here's the t-test - draw your own conclusions (but note
##  the use of var.equal=TRUE in the command)
##
t.test(iris$Sepal.Length[iris$Species=="versicolor"],
       iris$Sepal.Length[iris$Species=="virginica"],
       var.equal=TRUE)
##
##  OK: so we've looked at the distributions of one individual
##  variable for the three species - but what about how they
##  vary together? One way to do this is to do separate
##  scatterplots of pairs of variables for each species, with 
##  everything on a common scale. This can be done using the
##  ggplot() command from the ggplot2 library. The way this
##  works is slightly unintuitive, particularly if you're 
##  not a native English speaker. However, it is becoming 
##  very popular in the "data science" community so you 
##  should know about it (indeed, many of you may have
##  encountered it already). The basic idea is to build
##  up a plot in "layers", following a structured approach
##  that is supposed to help you think clearly about what
##  you're trying to do (the "gg" in "ggplot" stands for 
##  "grammar of graphics"). In the command below, the first
##  line defines the basic plot structure, and the remaining
##  lines add layers (notice the use of the "+" symbol, 
##  which is used by the ggplot2 library to denote "adding
##  a layer to the plot"). We'll run the command first to see
##  what it does, and then decconstruct its components. 
##
ggplot(data=iris, mapping=aes(x=Petal.Length, y=Sepal.Length) )+
  geom_point() +
  facet_wrap(~ Species) +
  labs(x="Petal length (cm)", y="Sepal length (cm)",  
       title="Sepal length versus petal length for specimens of three iris species") +
  theme(plot.title=element_text(hjust=0.5))
##
##  OK, here's the explanation:
##
##  - The first line defines the *data* to be used in the plot
##    (which must be a data frame - ggplot expects *all* data
##    to be in data frames), as well as the mappings from 
##    the columns of the data frame to the characteristics of 
##    the plot. In this example, the Petal.Length variable is 
##    mapped to the x-axis of the plot and the Sepal.Length
##    variable is mapped to the y-axis. For some reason, 
##    ggplot() refers to these mappings (as well as other
##    associations between variables and plot characteristics)
##    as "aesthetics" and defines them using the aes() command 
##    within the "mapping" argument to ggplot(), as in the 
##    example above.
##  - The second line adds a new layer to the plot. It is a layer
##    of points, so that the result is a scatterplot. In ggplot2,
##    points are regarded as a type of "geometric object" (others
##    are lines, smooth curves and so on). Notice that ggplot
##    already knows what the x- and y- co-ordinates of the points
##    should be, because they were defined by the data and mapping
##    of the main ggplot() command in the first line. 
##  - The third line splits the layer into three panels, each 
##    corresponding to a different value of the "Species" 
##    variable (notice that the tilde symbol "~" is used in 
##    much way as for the earlier boxplots, to split the data
##    into groups defined by the values of the "Species" variable: 
##    it is often helpful to interpret "~" as meaning "depends on"
##    in R). For some reason, ggplot2 refers to this process as
##    "facetting".
##  - The next line defines the axis labels and plot title, as an 
##    additional layer. This is fairly uncontroversial. However,
##    ggplot left-justifies the title by default: this is another 
##    thing that apparently makes sense to the author of ggplot,
##    but not to me, so the final line modifies the title 
##    positioning ("horizontal justification") in the underlying
##    "plot theme". A value of zero produces a left-justified title;
##    a value of 1 produces a right-justified title; and the value
##    of 0.5 used here ensures that the title is centred. 
##
##  If you want to know more about ggplot2, see the "Useful books and 
##  online resources" link under the "Course overview and useful
##  materials" tab of the STAT0023 Moodle page - there's a link
##  here to a book on the subject. If you like it as much as I do,
##  however, you may prefer a different way of displaying the data 
##  using "standard" R graphics. Actually, it's quite easy to plot 
##  these data on a single set of axes, but with different colours / 
##  plotting symbols to distinguish the species. This works quite 
##  well here, because the species are well-separated so the plot 
##  doesn't get cluttered. But we should take care to (a) define 
##  colours that are easily distinguishable (using the brewer.pal() 
##  function from the RColorBrewer library) (b) use different 
##  plotting symbols so that we're not relying solely on colour 
##  (c) be clear about what the plotting symbols represent (d) 
##  ensure that the plot labels etc. are in a size that will be 
##  easily readable.
##
plot.colours <- brewer.pal(3,"Dark2") # see help for brewer.pal ...
plot(iris$Petal.Length,iris$Sepal.Length,type="p",
     xlab="Petal length (cm)",ylab="Sepal length (cm)",
     col=plot.colours[iris$Species],  # Plotting colours
     pch=(15:17)[iris$Species],       # Plotting symbols (work it out!)
     cex=1.2,                         # Make points 20% bigger than default
     main="Sepal length versus petal length\nfor specimens of three iris species")
legend("topleft",pch=15:17,col=plot.colours,legend=levels(iris$Species),
       cex=1.2,title="Species",title.col=grey(0.4))
box(lwd=2)  # Draw a frame around the plot, with double line width
##
##  If you like that, you can save it to a PNG file so that you 
##  can include it later in a Powerpoint presentation. Note that
##  the dimensions of PNG files are given in "points" and there
##  are 72 points to the inch. Recall that our graphics window is
##  8 inches wide and 6 inches high. 
##
##  Note also that the dev.off() command is needed to stop 
##  writing graphics output to your PNG file: without it, R
##  will continue writing any subsequent plots to the file 
##  and you'll get in a mess!
##
dev.copy(png,"IrisPlot1.png",width=8*72,height=6*72)
dev.off()
##
##  Of course, this in itself is still only part of the story
##  because there are other variables in the data set as well. 
##  We've already established that it probably isn't necessary 
##  to consider both petal length and width, but that does 
##  leave three numeric variables (sepal length and width, 
##  together with petal length). R has some facilities for
##  interactive display of 3-D scatterplots via the rgl library:
##  these can be very helpful as a way of understanding 
##  how best to summarise a dataset. Here's the iris data in
##  interactive mode, therefore:
##
plot3d(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,
       col=plot.colours[iris$Species],size=10,
       xlab="Sepal length (cm)",
       ylab="Sepal width (cm)",
       zlab="Petal length (cm)")
##
##  Use the mouse to resize the plot, spin it around and look 
##  at it from different angles. Does this tell you anything 
##  that you didn't know already?
##
