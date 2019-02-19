#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######        STAT0023 Workshop 2: regression and ANOVA          ######
######                                                           ######
######    Analysis of minimum January temperatures in the US     ######
######                                                           ######
######  This script illustrates how to build regression          ######
######  models in R, using the ustemps dataset from the          ######
######  SemiPar library. There are 56 data points, each          ######
######  representing one US city. The variables are City         ######
######  (the city name), State (a 2-character abbreviation       ######
######  indicating the state in which the city is located),      ######
######  JanTemp (the average minimum daily temperature           ######
######  during January, for the period 1931 to 1960, in          ######
######  degrees Fahrenheit), Lat (latitude of the city)          ######
######  and Long (longitude of the city). As with the            ######
######  scripts from the previous workshop, this script          ######
######  demonstrates the use of commands from some               ######
######  additional libraries.                                    ######
######                                                           ######
######  Note: in this script, any command producing useful       ######
######  output has a print() statement attached, in order        ######
######  that the script can be source()d without having to       ######
######  make an additional "clean" copy.                         ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##	First step: load the required libraries. The maps library
##  allows you to draw country maps for anywhere in the world, 
##  and more detailed maps of some countries such as the USA. 
##  The robustbase library contains routines for doing robust
##  regression (see below). 
##
##  If you get an error when running the line below, you will need
##  to install the relevant library / libraries. Instructions for 
##  doing this can be found in the "Introduction to R" document 
##  provided on the Moodle page for Week 1 of the course, or in
##  the iris data example script from Workshop 1. The libraries
##  are as follows:
##
##  - maps: enables you to draw maps of any country in the 
##    world (here the USA)
##  - robustbase: to fit regression models in the case where
##    least-squares estimation may be unduly influenced by 
##    a minority of observations. 
##
sink("UStemps.txt")
library(maps); library(robustbase)
##
##  Next: load the data, look at their structure so that you know
##  what variables are available, and read the documentation so that 
##  you know what the variables represent. These are the same steps
##  as were used for the datasets in Workshop 1. 
##
##  The data are actually from another R library called SemiPar.
##  However, we'll take the opportunity to illustrate the use of   
##  an alternative way of storing information when using R, i.e.
##  via the use of an "R data file". This is different from the
##  usual ASCII format that is often used to store data: it
##  contains all of the R objects as they were created. To read
##  an R data file, we need to use the "load" command.
##
load("UStemps.rda")  # You should see the ustemp object in your workspace now.
cat("Structure of the US temperature data:\n") # cat() statements are for
cat("=====================================\n") # labelling output when you
                                               # source() the script
str(ustemp)
##
##  As provided, the longitudes are in degrees WEST, so they increase
##  from right to left. This is phenomenally unhelpful if you want
##  to plot (say) temperature against longitude, because everything 
##  comes out backwards. So: reverse the sign so that longitude is 
##  in degrees EAST from now on.
##
ustemp$longitude <- -ustemp$longitude
##
##  Since these are temperatures in different cities, it might be useful 
##  to start with a map. The "state" database in the map package 
##  (loaded earlier) contains data on the outlines of all the US states.
##  Different version of the map package seem to have different 
##  default settings for the plot margins when you draw a map, so
##  set these explicitly in the command below using the "mar" command. 
##  The four numbers here represent the margin size on the bottom, 
##  left, top and right sides of the plot respectively. The light grey
##  colour (NB the grey scale ranges from 0 for black to 1 for 
##  white) ensures that the map is visible but does not interfere
##  with the visual perception of the actual data. 
##
##  The first command below is to open a new graphics window with
##  a specified size. Comment it out (or just don't run it) if
##  you prefer to use the RStudio graphics window. 
##
if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
##
##  Now, plot a map of the USA
##
map("state",mar=c(1,1,1,1),col=grey(0.7))
##
##  Next we want to plot the temperature data in a way that will help
##  to inform what we're going to do next. In this particular instance,
##  we're interested in studying how the January minimum temperatures 
##  vary with latitude and longitude. One way to show this is by
##  plotting each data point in a different colour. The choice of 
##  colour scale can have a big effect on our perception of such a
##  plot, however, and the appropriate choice will often depend on 
##  the particular context for the analysis. In the present context, 
##  we might choose a colour scale according to the following 
##  considerations (see the guidance on preparation of graphics in 
##  the slides for Lecture 1):
##
##  - The data are temperatures, so viewers are naturally going
##    to associate blue with cold and red with hot (OK, it's 
##    January in the northern hemisphere so red is perhaps "less cold"!)
##  - If someone is going to print it on a black-and-white printer,
##    or show it to someone else who is colourblind, or photocopy it,
##    then we need to use a colour scale that will work well in 
##    black and white. One way to achieve this is by varying the 
##    *intensity* (or "opacity") as well as the actual colour.
##  - In January, presumably the temperatures that people consider
##    most noteworthy are the really cold ones (NB "January" here is
##    the month during which the temperatures were measured, rather 
##    than the month during which the workshop takes place ...). 
##    Psychologically therefore (and graphics are for communication, 
##    so must consider the psychology whenever we prepare a plot) it
##    would make sense to use a deep blue colour to represent very 
##    cold temperatures, and a pale white / red colour to represent
##    warm ones. 
##
##    The basic idea is to define a colour scale with, say, 8 colours 
##    and to associate each colour with a temperature interval. The 
##    first step is to define the intervals. This is most easily 
##    done using the "pretty" function which ensures that the interval
##    boundaries are "sensible" numbers. Then, the "cut" function determines
##    which interval each observation is in. Note, however, that  the pretty()
##    function isn't guaranteed to give exactly the number of intervals 
##    that we asked for, which is why the subsequent command is used to 
##    find out what actually happened.
##
cutpoints <- pretty(ustemp$min.temp,n=8)
Temp.interval <- cut(ustemp$min.temp,breaks=cutpoints, 
                     include.lowest=TRUE) # Because one city has a temp equal to the minimum!
N.intervals <- nlevels(Temp.interval) # Number of intervals we've *actually* used
#
#     The next command defines a vector of N.intervals colours, from 
#     red to deep blue. The "alpha" vector controls the "transparency" 
#     of each colour, ranging from 0 (completely transparent i.e. 
#     invisible) to 1 (completely opaque).The rainbow() part of 
#     this command therefore produces a vector of colours starting 
#     from very dilute (i.e. very transparent) red, and ending at 
#     deep blue. But for the temperature data we want the deep blue 
#     to correspond to the coldest i.e. lowest values - hence the use
#     of rev() to "reverse" the order of the colours. 
#
plot.cols <-                                
  rev(rainbow(N.intervals,start=0,end=4/6,   # red to blue (see help)        
          alpha=seq(0.3,1,length.out=N.intervals))) 
##
##  Phew! Now we're ready to add the data to the map. Note that 
##  longitude is the "x" coordinate and latitude is the "y" coordinate. 
##  Note the use of a filled square as a plotting symbol (pch=15) so 
##  that the colours stand out; and the magnification of the points 
##  (cex=2) so that the overall pattern is clear.
##
points(ustemp$longitude,ustemp$latitude,
       pch=15,cex=2,col=plot.cols[Temp.interval])
##
##  A legend never hurts
##
legend("bottomleft",pch=15,col=plot.cols,legend=levels(Temp.interval),
       title=expression(degree*"F"),ncol=4,bty="n")
##
##  Nor does a title
##
title(main="Mean US January daily minimum temperature, 1931-1960")
##
##  Save to file (note that the previous x11() command specified the 
##  dimensions of the graphics device, so if we use the same values
##  then we should have no distortion).
##
dev.copy(pdf,"UStemps.pdf",width=8,height=6)
dev.off()
##
##  The predominant variation of temperature with location seems
##  roughly linear (the colours on the plot form linear "bands"
##  across most of the USA, with the exception of the west coast).
##  This could perhaps be represented by writing the expected 
##  minimum temperature as a linear function of latitude and 
##  longitude. So regress the temperature on these two 
##  quantities, store the result and look at it. 
##
Temp.Model1 <- lm(min.temp ~ latitude + longitude, data=ustemp)
cat("Initial model for temperature dependence on lat & long:\n")
cat("=======================================================\n")
print(Temp.Model1)
##
##  We can get more information using the summary() command
##
cat("More information on this model:\n")
cat("===============================\n")
print(summary(Temp.Model1))
##
##  This suggests that:
##
##  - Both the latitude and longitude terms are significant 
##  - The error standard deviation is about 6.9 degrees Fahrenheit
##  - The model explains about 73% of the variation in temperatures. 
##
##  Make sure you understand where to find this information in the
##  summary() output - if in doubt, ask. Don't worry about the 
##  F-statistic for the moment, we'll look at this in the next
##  exercise.
##
##  Although you might think that this all looks good, the
##  original plot of the data suggested that the variation
##  isn't exactly linear: perhaps a quadratic surface would 
##  improve the fit? A quadratic function of two variables
##  x1 and x2 has terms corresponding to x1, x2, x1^2, x2^2
##  and x1*x2. Note that:
##
##  - The x1*x2 term is just an interaction (see the lecture 
##    slides / handouts for this week).
##  - To define quadratic terms in model formulae in R, you 
##    can't just write (e.g.) y ~ x1^2 because the "^" character
##    has a special meaning in model formulae. You must instead
##    write, e.g., y ~ I(x^2). 
##  - We could write out the entire formula for the quadratic
##    model from scratch and use the lm() command, but all we're
##    doing here is adding some terms to an existing model. It's
##    easier for us (and more computationally efficient) to 
##    take advantage of what has already been done. This can 
##    be done using the update() command. The dots in the 
##    formula below translate as "the corresponding parts of 
##    the formula for the original model", so we'll use exactly
##    the same response variable as in Temp.Model1, and add
##    the quadratic parts to the right-hand side of the model
##    equation.
##
Temp.Model2 <- 
  update(Temp.Model1, . ~ . 
           + I(latitude^2) + I(longitude^2) + latitude:longitude)
cat("Quadratic model summary:\n")
cat("========================\n")
print(summary(Temp.Model2))
cat("Comparison of linear and quadratic models:\n")
cat("==========================================\n")
print(anova(Temp.Model1,Temp.Model2,test="F"))
##
##    The tiny p-value in this test means that we can reject
##    overwhelmingly the null hypothesis that the data were
##    generated from the simpler of the two models (i.e. the
##    model in which temperature varies linearly with 
##    latitude and longitude). Operationally, the conclusion
##    from this is that the quadratic model is a dramatic 
##    improvement on the original (which you can see in any
##    case from the summary - the residual standard deviation
##    has decreased from 6.9 degrees to 4.1 degrees, and the
##    adjusted R squared has increased from 73% to over 90%).
##
##    Let's now carry out some diagnostics for the quadratic
##    model, by looking at four "standard" residual plots.
##    (see slides from this week's lecture). To look at them 
##    all simultaneously, set up a 2*2 array of plots in the 
##    plotting window. It is helpful to decrease the plot 
##    margins in this case so that the plots are a bit bigger; 
##    and this in turn necessitates moving the axis labels a 
##    bit. The next par() command achieves all of these things.
##
par(mfrow=c(2,2),    # 2 rows and 2 columns of plots
    mar=c(3,3,2,2),  # 3 margin lines at bottom & left, 2 at top & right
    mgp=c(2,0.75,0)  # Position of axis titles, labels and lines
    )
plot(Temp.Model2,which=1:4)
dev.copy(pdf,"UStemp_checks.pdf",width=8,height=6)
dev.off()
##
##    That doesn't look too bad (note that you shouldn't 
##    overinterpret the wiggles in the red lines on these
##    plots: there are only 56 observations so there's 
##    going to be some sampling variation there - see Box
##    1 in the workshop notes for more on this). The 
##    "residuals vs fitted" plot has a fairly random scatter, 
##    the scale-location plot suggests that the variance is 
##    fairly constant over the range of fitted values and 
##    the normal Q-Q plot shows the points falling more or 
##    less on a straight line. The Cook's distance plot 
##    suggests that the most influential observation is 
##    number 52, with a Cook's distance of around 0.35. By 
##    the "rule of thumb" of 8/(n-2k) = 8/(56-12) = 8/44 = 
##    0.18, this does indeed seem to be a cause for concern. 
##    The first question to ask, therefore, is: which 
##    observation is it?
##
cat("\nMost influential observation for quadratic model:\n")
cat("=================================================\n")
print(ustemp[52,])
##
##    It's Seattle. You can understand why it's influential
##    by looking at the original plot: Seattle is the most
##    north-westerly of all the cities in the dataset, so 
##    it is perhaps having a big influence on the precise
##    form of the quadratic fit. In general, when a
##    specific data point has a big influence, there are
##    several options that one could consider. These are
##    as follows:
##    
##    a. Remove the influential observation and refit the model.
##       If the substantive conclusions (e.g. the overall model
##       fit or the predictions) don't change to an extent that
##       is practically important, then there is no problem. 
##       If the substantive conclusions *do* change materially
##       however, you need to decide what to do - which leads
##       to options b and c below. 
##    b. Think about whether it makes sense to model all of the
##       observations together, or whether there is some reason
##       to suspect that the influential observation(s) might
##       behave differently in some way. In the latter case, 
##       you would be justified in discarding these observations
##       and then being clear that your final model is designed
##       only for use in situations representative of the majority
##       of the data points. For the US temperatures however, 
##       there's no reason to believe that Seattle should follow 
##       a different pattern. In this case (and if there's a 
##       problem according to the diagnostic in step a above),
##       a final option is ...
##    c. Use a "robust" regression technique that is designed to
##       reduce the influence of observations that are extreme in 
##       some way. The command lmrob() in the robustbase package
##       provides one way of doing this. If you want to understand
##       exactly what it does, you need to read the references 
##       in the help page (i.e. ?lmrob). Good luck :-) The 
##       disadvantage of this, of course, is that if the influential 
##       observation really *is* genuine and fits the overall true 
##       pattern, you'll lose some precision in your estimation by not
##       making full use of it. Ultimately, it's a judgement call.
##
##    You might think that we could fit the two models (with and
##    without Seattle) and then do a hypothesis test to see which
##    is better. This doesn't work though, because to carry out a 
##    formal comparison of models they need to be fitted to the
##    same dataset! Actually, it *is* possible to carry out a 
##    formal test to see whether Seattle must be dealt with 
##    separately (fit a model with an extra covariate taking the 
##    value 1 for Seattle and 0 everywhere else, and then do an
##    F test), but this is slightly cheating :-) 
##
##    For illustrative purposes, let's see what happens here if 
##    we refit the quadratic model omitting the Seattle data point.
##    Here, the update() command is used to refit the earlier model
##    to the same dataset but omitting row 52. 
##
Temp.Model2b <- update(Temp.Model2, data=ustemp[-52,])

##LINEAR REGRESSION FOR CUBIC
Temp.Model3b <- update(Temp.Model2, . ~ . + I(latitude^3) + I(longitude^3) + 
                        latitude:I(longitude^2) + I(latitude^2):longitude)
##RESULTS FOR CUBIC
summary(Temp.Model3b)
plot(Temp.Model3b)

##
##    Any difference in the coefficients?
##
cat("\nCoefficients in original quadratic model:\n")
cat("=========================================\n")
print(round(coef(Temp.Model2),2))
cat("\nCoefficients in quadratic model omitting Seattle:\n")
cat("=================================================\n")
print(round(coef(Temp.Model2b),2))
##
##    Well: the coefficients in the "quadratic" part are the 
##    same, to two decimal places - but there are differences
##    elsewhere. There is no difference in the *sign* of the 
##    coefficients or in their statistical significance, so 
##    if there was some interest in these then we'd be reassured. 
##    However, for the US temperature data we're probably not 
##    so interested in the significance of the individual terms
##    as in what the model tells us about the regional variation
##    of temperature. One way to invesigate this is to make maps 
##    of the fitted regression functions. To do this, we need to 
##    compute the predicted January minimum temperatures over
##    a grid of latitude and longitude values in the US. This
##    gives us an opportunity to see how to do prediction with 
##    regression models in R. Here goes.  
##    
lat.grid <- pretty(ustemp$latitude,n=50)   # Grid of about 50 latitude points
long.grid <- pretty(ustemp$longitude,n=50) # Ditto, longitude
nlats.new <- length(lat.grid)    # In case the pretty() command decided
nlongs.new <- length(long.grid)  # that 50 wasn't quite right
GridLocs <- data.frame(latitude=rep(lat.grid,each=nlongs.new),
                       longitude=rep(long.grid,nlats.new))
##
##    Get a feel for what GridLocs looks like
##
head(GridLocs)
##
##    Compute the predicted temperatures and their standard errors.
##    NB by including the arguments 
##
##          interval="confidence" 
##
##    or
##
##    interval="prediction" 
##
##    in the following commands, we could compute confidence and 
##    prediction intervals as well. It's hard to display these 
##    however, so content ourselves with standard errors for the 
##    moment. 
##
PredTemp2 <- predict(Temp.Model2,newdata=GridLocs,se.fit=TRUE)
PredTemp2b <- predict(Temp.Model2b,newdata=GridLocs,se.fit=TRUE)
PredTemp3b <- predict(Temp.Model3b,newdata=GridLocs,se.fit=TRUE)
##
##    Show what the results look like (PredTemp2b is similar)
##
cat("\nStructure of predict() output:\n")
cat("==============================\n")
str(PredTemp2)
##
##    To do the comparisons, probably the easiest thing is 
##    just to make maps of the predictions and associated
##    standard errors. 
##
par(mfrow=c(2,2),mar=c(1,1,2,1)) # Ensure we have a 2*2 array of plots
                                 # with narrow margins on all sides but 
                                 # room for a title at the top
##
##    Next commands work out the overall range of both the predictions
##    and standard errors from *both* models, so that we can define
##    some common colour scales. 
##
PredLims <- range(c(range(PredTemp2$fit),range(PredTemp2b$fit)))
SELims <- range(c(range(PredTemp2$se.fit),range(PredTemp2b$se.fit)))
##
##    Here *are* the common colour scales - the same as plot.colscl
##    above, but now with 100 colours to create smooth maps.
##
map.colscl <-                                
  rev(rainbow(100,start=0,end=4/6,alpha=seq(0.3,1,length.out=100))) 
##
##    Now, we can make the maps. To do this, the *vectors* of 
##    predictions and standard errors need to be converted to 
##    *matrices* of dimension nlongs.new*nlats.new; these matrices
##    can then be mapped using the image() command. Remember that
##    matrices in R are filled column-wise: in the next command, 
##    column 1 of matrix.to.plot will be filled with the 
##    first nlongs.new elements of PredTemp2$fit, which are the 
##    predictions at the lowest latitude. Column 2 will be 
##    filled with the next nlongs.new elements, which are the 
##    predictions at the next latitude; and so on. Each column
##    of the matrix therefore contains the predictions at a
##    specific latitude, and each row contains the predictions
##    at a specific longitude. 
##
matrix.to.plot <-  matrix(PredTemp2$fit,nrow=nlongs.new)  
##
##    Now the cool part!
##
image(x=long.grid,y=lat.grid,z=matrix.to.plot,
      zlim=PredLims,col=map.colscl,axes=FALSE,xlab="",ylab="")
##
##    To help the reader interpret the colours, add some labelled
##    contours
##
contour(x=long.grid,y=lat.grid,z=matrix.to.plot,add=TRUE,
        col="red",lwd=2,labcex=0.8)
title(main="Predictions from quadratic model")
map("state",add=TRUE)
box(lwd=2)
##
##    Now plot the standard errors, similarly
##
matrix.to.plot <- matrix(PredTemp2$se.fit,nrow=nlongs.new)
image(x=long.grid,y=lat.grid,z=matrix.to.plot,
      zlim=SELims,col=map.colscl,axes=FALSE,xlab="",ylab="")
contour(x=long.grid,y=lat.grid,z=matrix.to.plot,add=TRUE,
        col="red",lwd=2,labcex=0.8)
title(main="Std Err from quadratic model")
map("state",add=TRUE)
box(lwd=2)
##
##    The same thing, for the model omitting Seattle
##
matrix.to.plot <- matrix(PredTemp2b$fit,nrow=nlongs.new)
image(x=long.grid,y=lat.grid,z=matrix.to.plot,
      zlim=PredLims,col=map.colscl,axes=FALSE,xlab="",ylab="")
contour(x=long.grid,y=lat.grid,z=matrix.to.plot,add=TRUE,
        col="red",lwd=2,labcex=0.8)
title(main="Predictions omitting Seattle")
map("state",add=TRUE)
box(lwd=2)
#
matrix.to.plot <- matrix(PredTemp2b$se.fit,nrow=nlongs.new)
image(x=long.grid,y=lat.grid,z=matrix.to.plot,
      zlim=SELims,col=map.colscl,axes=FALSE,xlab="",ylab="")
contour(x=long.grid,y=lat.grid,z=matrix.to.plot,add=TRUE,
        col="red",lwd=2,labcex=0.8)
title(main="Std Err omitting Seattle")
map("state",add=TRUE)
box(lwd=2)
##
##    There are some minor differences there (particularly
##    in the standard errors), but probably not enough
##    to affect any substantive conclusions. Nonetheless,
##    if you were really worried about this then you might
##    want to use a robust regression model. The following
##    command illustrates how to refit the original quadratic
##    model in a robust way, to reduce any excessive influence
##    from the Seattle observation. It uses the lmrob() 
##    command from the robustbase library, which was loaded
##    at the outset. Notice the use of formula(Temp.Model2)
##    to extract the model formula from the earlier object -
##    it saves us from having to type it all out again, 
##    which would be tedious! If you want to see what it
##    does, type
##
##    formula(Temp.Model2)
##
##    at the prompt.
##
Temp.Model2Rob <- lmrob(formula(Temp.Model2), data=ustemp)
cat("\nCoefficients in quadratic model with robust fit:\n")
cat("================================================\n")
print(round(coef(Temp.Model2Rob),2))
##
##    We might also use confidence intervals to see whether
##    there's any material difference between the different
##    fits:
##
cat("\nConfidence intervals for coefficients in original quadratic model:\n")
cat("==================================================================\n")
print(round(confint(Temp.Model2),3))
cat("\nConfidence intervals for coefficients in robust quadratic model:\n")
cat("================================================================\n")
print(round(confint(Temp.Model2Rob),3))
##
##    This shows that any differences between the two fitted models
##    are small compared with the uncertainties in their coefficients. 
##    Notice also that the confidence intervals from the "robust"
##    model are wider: this is because, in a sense, it uses less of
##    the available information e.g. it downweights the observation
##    from Seattle because this is potentially influential). 
##
##    All of this indicates that the "influential" observation from 
##    Seattle is probably not too important. This completes an 
##    investigation into the results of the earlier residual plots. 
##    HOWEVER, it isn't necessarily the end of the story. In this
##    particular example, it is possible that the real variation in 
##    temperatures is more complex than the current quadratic model. 
##    If this is the case, we might be able to see it if we plotted
##    a *map* of the residuals. Here it is - the logic is exactly
##    the same as the *original* temperature map. Notice that the
##    resid() command can be used to calculate the residuals from 
##    a lm() object. 
##
Model2.resid <- resid(Temp.Model2)
cutpoints <- pretty(Model2.resid,n=8)
Resid.interval <- cut(Model2.resid,breaks <- cutpoints)
N.intervals <- nlevels(Resid.interval)
plot.cols <-                                
  rev(rainbow(N.intervals,start=0,end=4/6,   # red to blue (see help)        
              alpha=seq(0.3,1,length.out=N.intervals))) 
par(mfrow=c(1,1))  # BAck to a single plot
map("state",mar=c(1,1,1,1),col=grey(0.7))
points(ustemp$longitude,ustemp$latitude,
       pch=15,cex=2,col=plot.cols[Resid.interval])
legend("bottomleft",pch=15,col=plot.cols,legend=levels(Resid.interval),
       title=expression(degree*"F"),ncol=4,bty="n")
title(main="Residuals from quadratic model for temperature")
##
##    Actually, they don't look random: the blue and green points
##    tend to cluster for example. This suggests that the model
##    could be improved further, for example by extending to 
##    a cubic surface (try it!) OR by thinking about other factors
##    that may be associated with the average temperature of a 
##    city, other than just latitude and longitude (if you know
##    anything about the geography of the US, you might think
##    about whether there's anything that could conceivably 
##    explain the locations of the "cold" i.e. blue residuals). 
##    On the other hand, you may feel that the remaining residual 
##    structure is unimportant. After all, the range of the 
##    original data was about 70 degrees and the range of the 
##    residuals is about 18 degrees (look at the plot legends). 
##    Perhaps the model is already as accurate as it needs to be. 
## 
#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######                OPTIONAL SECTION BELOW HERE                ######
######                                                           ######
######  The code above, for setting up colour scales etc. and    ######
######  making the map, was quite complicated. Some of you may   ######
######  wonder "can this be done more easily using something     ######
######  like ggplot()? The answer is: you can probably produce   ######
######  a "decent" map more easily using ggplot, but to get a    ######
######  really excellent map then it's just as much work!        ######
######  The code below demonstrates this. Note that this         ######
######  section is optional: it is provided solely for those     ######
######  students who want to learn more about how to use ggplot  ######
######  effectively.                                             ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##    First step: load the ggplot2 library
##
library(ggplot2)
##
##    ggplot2 provides a command that will automatically add a background
##    map to a plot: this requires the map itself to be stored as an 
##    appropriate object. ggplot2 also provides the command map_data(), to 
##    create such an object from the information in the "maps" library that
##    we loaded already ...
##
US.states <- map_data("state")
##
##    Now it's really quick to produce a "decent" map. Code first, 
##    explanation afterwards.
##
plot(ggplot(data=ustemp, mapping=aes(longitude,latitude,colour=min.temp)) +
       coord_quickmap() +
       annotation_map(US.states, fill=NA, colour="grey") +
       geom_point())
##
##  You should understand the first line of that code: it says "create a 
##  plot in which the data are taken from the ustemp data frame, with 
##  the variables "longitude", "latitude" and "min.temp" mapped to
##  the x-axis, y-axis and plot colour respectively (note that I didn't
##  write "x=longitude" and "y=latitude", because I was being lazy: 
##  the first two arguments of the aes() command are always taken to be
##  x and y unless otherwise specified).
##
##  The coord_quickmap() layer provides a very convenient way to ensure
##  that the map is scaled (approximately) correctly - i.e. to ensure
##  that (say) 1cm in a vertical direction on the plotted map corresponds
##  to roughly the same *real* distance as 1cm in a horizontal direction.
##  Neat!
##
##  Next, the annotation_map() layer adds the map outline, taking the
##  information from the US.states object that we just created. By 
##  default, the map would be filled in a dark colour: setting fill=NA 
##  ensures that only the outline is drawn (try removing the fill=NA
##  argument and see what happens!). 
##
##  Finally, geom_point() adds the points, as well as a legend.
##
##  That's not bad, but it isn't *quite* as effective as the earlier plot. 
##  There are a few key differences:
##
##  - No plot title
##  - The legend title is clumsy and doesn't indicate units of measurement
##  - Longitude and latitude axes are labelled, which isn't really 
##    necessary here because everyone can recognise a map of the US. 
##  - The points are too small
##  - The colour scale doesn't obviously correspond to "temperature". 
##  - The map has ggplot's nasty trademark grey background with white
##    gridlines, none of which is necessary or helpful here. 
##
##  The code below shows how to fix all of these things. It's not
##  *quite* as clean as it looks: it uses the Temp.interval and 
##  plot.cols objects that were computed earlier. Notice that the
##  reason for mapping colour to Temp.interval rather than to 
##  min.temp is that min.temp is continuous and it becomes harder
##  to see the clear "linear banding" structure of the plot if
##  min.temp is use. Temp.interval is a discrete variable, and
##  each level can be mapped to one of the colours defined earlier.
##  Here goes ... 
##
plot(ggplot(data=ustemp, 
            mapping=aes(longitude,latitude,colour=Temp.interval)) +
       coord_quickmap() +
       annotation_map(US.states, fill=NA, colour="grey") +
       geom_point(size=5, shape=15) +
       scale_x_continuous(breaks=NULL, name=NULL) +
       scale_y_continuous(breaks=NULL, name=NULL) +
       scale_colour_manual(values=plot.cols) +
       labs(title="Mean US January daily minimum temperature, 1931-1960",
            colour=expression(degree*"F")) +
       theme_void() +
       theme(plot.title=element_text(hjust=0.5,face=2,size=16,
                                     margin=margin(b=12)), 
             legend.position=c(0.05,0),
             legend.justification=c(0,0),
             legend.direction="horizontal"))
##
##  Apart from plotting Temp.interval instead of min.temp, the 
##  differences between this and the previous ggplot() command 
##  are as follows:
##
##  - The geom_point() layer has a size and a shape argument, 
##    controlling (you guessed it) the size and shape of the 
##    points. I found that a size of 5 gives a similar visual
##    impression to the "cex=2" argument in the earlier plot()
##    command. The shape of 15 is the same as the "pch=15"
##    value in the earlier command - it's the code that R 
##    uses for a filled square. 
##  - scale_x_continuous() and scale_y_continuous(): these
##    control how the x and y axes ("scales", in ggplot-speak)
##    are displayed. Setting both breaks and names to NULL
##    prevents any tick marks (breaks) and axis names ("latitude"
##    and "longitude" in the previous ggplot).
##  - scale_colour_manual(): this controls exactly the colours 
##    are mapped to the data values (it's directly analogous to
##    scale_x_continuous() and scale_y_continuous(), which 
##    control exactly how the x- and y-values on the plot are
##    mapped to their respective data values). The "manual"
##    colour scale allows the user to specify their own colours
##    instead of using a built-in set: I have used this here to
##    try and reproduce the appearance of the earlier map, 
##    although the resulting colours are not *identical*, even 
##    though I've specified the same vector of colours (plot.cols)
##    that was created earlier. I'm not 100% sure of the reason
##    for this, but I suspect it's because ggplot2 uses a 
##    slightly different colour model from "standard" R graphics,
##    and it's trying to translate my colour specification into
##    its own model, with some "approximation error" in the 
##    process.
##  - A labs() layer has been added, to specify both the plot
##    title *and the legend title*. The legend title is specified
##    using the "colour" argument, because the legend shows the
##    values for the variable that has been mapped to colour in 
##    the first ggplot() command. 
##  - The theme_void() specification tells ggplot() to use a 
##    different "plot theme" from the default. A "theme", in
##    ggplot-speak, is a collection of graphical settings: the
##    default theme is responsible for the nasty grey background
##    etc. theme_void() overlays the graph on a completely blank
##    plotting region, which is *almost* what we want here. But
##    not quite, so ... 
##  - The theme() command makes some adjustments to the default
##    settings of "theme_void()": use 16-point font in bold
##    typeface ("face=2" is bold) for the title, and control 
##    the position and layout of the legend ("legend.position=c(0.05,0)"
##    means "place the legend 5% of the way along the plot, and at 0%
##    of the way up i.e. at the bottom"; and "legend.justification=c(0,0)"
##    means "this legend position corresponds to the bottom left-hand
##    corner of the legend").
##
##    That's probably enough of ggplot() for one day ... 
##
#######################################################################
#######################################################################
#######################################################################
sink()
