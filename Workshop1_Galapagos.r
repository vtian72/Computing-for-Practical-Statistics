#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######            STAT0023 Workshop 1: revision of R             ######
######                                                           ######
######            Analysis of Galapagos Islands data             ######
######                                                           ######
######  This script contains the commands used in a short        ######
######  (and non-definitive) analysis of the dataset on          ######
######  Galapagos islands plant species that is considered in    ######
######                                                           ######
######  Faraway, J. (2005). Linear Models with R. Chapman &      ######
######   Hall / CRC Press, Boca Raton.                           ######
######                                                           ######
######  Actually, the data are provided in the "faraway" R       ######
######  package: if this package is installed, the data can      ######
######  be accessed here by typing                               ######
######                                                           ######
######  require(faraway); data(gala)                             ######
######                                                           ######
######  at the R prompt. However, (i) the "faraway" package is   ######
######  not installed by default (ii) it is helpful to learn     ######
######  how to read data from a file. The data here are          ######
######  therefore provided in the file galapagos.dat, which      ######
######  should be in the same directory as this script. The      ######
######  file is an ASCII file and is space-delimited. It         ######
######  contains data on seven variables for each of 30 islands. ######
######  The first row contains the variable names, which are as  ######
######  follows:                                                 ######
######                                                           ######
######  Species    - the number of plant species found on the    ######
######               island                                      ######
######  Endemics   - the number of endemic species               ######
######  Area       - the area of the island in square km         ######
######  Elevation  - the maximum height of the island in m       ######
######  Nearest    - the distance to the nearest other island    ######
######               in km                                       ######
######  Scruz      - the distance from Santa Cruz island in km   ######
######  Adjacent   - the area of the adjacent island in km^2     ######
######                                                           ######
######  The remaining rows contain the values of the seven       ######
######  variables for each island, along with the island names.  ######
######  Fields are separated by spaces and are not aligned       ######
######  in columns.                                              ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##	First step: read the data and have a look at them. As an 
##  alternative to the second command below, you could type 
##
##  View(species.data)
##
##  or click on the species.data object in the "Environment" tab
##  of the top right-hand RStudio pane. This shows the data 
##  frame as though it was a spreadsheet in *this* pane - so 
##  you'll then have to close it, or click on this tab, in 
##  order to continue. 
##
species.data <- read.table("galapagos.dat",header=TRUE)
species.data
##
##	Alternatively, just look at the first few rows ...
##
head(species.data)
##
##	... or the column names ...
##
names(species.data)
##
##  ... or get a short summary of the structure of the data ...
##
str(species.data)
##
##	... or look at some useful summaries of each variable.
##
summary(species.data)
##
##	It might be useful to plot the data to visualise any 
##	relationships between variables ...
##
plot(species.data)
##
##	Notice the outlying point on all of the graphs involving "area"
##	variables (i.e. Area and Adjacent). Which island is this? 
##
big.island <- (species.data$Area > 3000)
big.island
species.data[big.island,]	# It's Isabela, with an area of 4669.32km^2
##
##	Next, let's look at some inter-variable relationships. The 
##	earlier plot suggests that there is a linear relationship 
##	between Elevation and Endemics - look at this in more detail.
##	
plot(species.data$Elevation,species.data$Endemics)
##
##	... or, for a publication-quality version
##
plot(species.data$Elevation,species.data$Endemics,
     xlab="Elevation (m)",ylab="No. of species",
     main="Variation of endemic species numbers with\nisland elevation",
     pch=15,col="blue")
box(lwd=2)				# Nice frame round the plot
##
##	And copy to a PDF file ...
##
dev.copy(pdf,"endemics.pdf",width=6,height=6)
dev.off()
##
##	Or, for JPEG (e.g. for embedding in web pages or Powerpoint
##  presentations)
##
#dev.copy(jpeg,"endemics.jpg",width=6*72,height=6*72,quality=99)
#dev.off()
##
##	Or, for PNG (sometimes better quality than JPEG)
##
#dev.copy(png,"endemics.png",width=6*72,height=6*72)
#dev.off()
##
##	Now let's fit a linear regression model to the data
##
endemics.model <- lm(Endemics ~ Elevation, data=species.data)
##
##	Look at the result ...
##
endemics.model
##
##	... and get a more informative summary ...
##
summary(endemics.model)
##
##	Add the fitted regression line to the plot (rerun the previous
##    plot() command first, if necessary)
##
abline(endemics.model, col="red", lty=2, lwd=2)
##
##	And plot some diagnostics (R produces 4 plots by default, 
##	so make space for them)
##
par(mfrow=c(2,2))
plot(endemics.model)
##
##  Finally, let's look at why (e.g.) the plot and summary 
##  commands work in different ways for the species.data and
##  endemics.model objects. These are both "generic" commands
##  which do different things depending on the *class* of
##  the objects to which they are applied. To find out the
##  class an object, use the class() command:
##
class(species.data)
class(endemics.model)
##
##  Now, to see what the summary command does for species.data
##  (don't worry about the details - but DO notice that the
##  arguments are provided as character strings i.e. in quotes)
##
getS3method("summary","data.frame")
##
##  Similarly for the summary of a linear model object
##
getS3method("summary","lm")
##
##  To reiterate: you are *NOT* expected to understand any of 
##  the details of those commands! The point is simply that
##  commands like summary() do different things depending 
##  on the class of the object to which they are applied. 
##  The "class" of an object is therefore an important 
##  concept in R programming.
##
##  The next two commands show a smarter way to do the 
##  same thing as the previous two.
##
getS3method("summary",class(species.data))
getS3method("summary",class(endemics.model))
