#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######        STAT0023 Workshop 2: regression and ANOVA          ######
######                                                           ######
######         Analysis of petal lengths in the iris data        ######
######                                                           ######
######  Note: in this script, any command producing useful       ######
######  output has a print() or cat()statement attached, so      ######
######  that the script can be source()d without having to       ######
######  make an additional "clean" copy.                         ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
sink("Iris_2.txt")
library(RColorBrewer)   # To define some colours later on
##
##  Load the data: you have already studied these in Workshop 1.
##
data(iris)
##
##  Here's a classical one-way ANOVA table to examine 
##  differences in mean petal lengths between species
##
cat("ANOVA table for differences of petal length between species:\n")
cat("============================================================\n")
print(summary(aov(Petal.Length ~ Species, data=iris)))
##
##  To show that the least-squares coefficient estimates in 
##  a linear model (no intercept) are just the sample means 
##  (see the lecture slides / handouts)
##
cat("\nPetal lengths: sample means for each species:\n")
cat("=============================================\n")
print(tapply(iris$Petal.Length, INDEX=iris$Species, FUN=mean))
cat("\nPetal lengths: linear model with no intercept:\n")
cat("==============================================\n")
iris.Model0 <- lm(Petal.Length ~ Species - 1, data=iris)
print(iris.Model0)
print(summary(iris.Model0))
##
##  The next command shows how R has defined the dummy variables
##  for this model
##
model.matrix(iris.Model0)
##
##  What if we don't exclude the intercept?
##
iris.Model1 <- lm(Petal.Length ~ Species, data=iris)
cat("Petal lengths: linear model with intercept:\n")
cat("===========================================\n")
print(summary(iris.Model1))
##
##  There's no Setosa coefficient now; but the intercept is 
##  the same as the Setosa coefficient for the previous
##  model. Also, the other coefficients have changed: they
##  now represent the differences between the respective
##  means and the Setosa mean. To check this:
##
coef(iris.Model0)[2]-coef(iris.Model0)[1]
coef(iris.Model0)[3]-coef(iris.Model0)[1]
str(iris)
##
##  The reason for this was explained in the lecture: see
##  the slides / handouts for details. Notice, however, 
##  that the fitted values (i.e. the predictions for each
##  observation) from the two models are identical:
##
all.equal(fitted(iris.Model0), fitted(iris.Model1))
##
##  In iris.Model1 the Setosa species is taken as a 
##  "reference" level, so that the other coefficients
##  measure differences from this one. The p-values
##  in the summary table therefore test the null hypotheses
##  that the mean petal lengths for the other two 
##  species are the same as that for Setosa. This 
##  might not be of much scientific interest (although
##  it is useful in situations like clinical trials, 
##  where people might want to compare one or more
##  new treatments with a "standard" which can
##  be taken as the reference level). Perhaps it  
##  would be more useful to compare the mean petal 
##  lengths for each species with an overall mean 
##  petal length for all iris flowers? This would 
##  enable us to see whether individual species are 
##  larger or smaller than average. The way to do 
##  this is via the "contrasts" argument to the 
##  lm command: "contr.sum" means "ensure that the 
##  coefficients for Species sum to zero".
##
iris.Model2 <- lm(Petal.Length ~ Species, data=iris, 
                  contrasts=list(Species="contr.sum"))
cat("Petal lengths: sum-to-zero constraints:\n")
cat("=======================================\n")
print(summary(iris.Model2))
##
##  The intercept is now 3.758: this is the overall mean
##  petal length for all the iris flowers (check this ...).
##  The next coefficient is rther unhelpfully labelled 
##  "Species 1" and is -2.296. For flowers of this species,
##  the fitted value is therefore 3.758-2.296 = 1.462,
##  which you have seen before ... For flowers of "Species 2",
##  the fitted value is 3.758 + 0.502 = 4.26. You've seen 
##  this before, too. R doesn't give you the coefficient for 
##  Species 3 (Virginica), because the coefficients are
##  constrained to sum to zero: its coefficient is therefore
##  -(-2.296+0.502) = 1.794, and the fitted value for this
##  species is 3.758 + 1.794 = 5.552 (bingo!). 
##
##  As well as the coefficients themselves changing, the 
##  t statistics have changed compared with Model 1. In 
##  this particular case, the p values are still effectively
##  zero; but in general they will be different depending
##  on how the coefficients are constrained. This is 
##  because the coefficients have different interpretations
##  in the two models: in Model1 they represented 
##  differences relative to the Setosa species, whereas
##  in Model2 they represent deviations from an overall
##  mean. The hypotheses being tested are different,  
##  therefore: beware! 
##
##  Of course, if we're going to test hypotheses about 
##  whether the mean petal lengths are equal to the 
##  overall mean then we're basically back to the 
##  one-way ANOVA from line 28 above. Go back to 
##  the output of that command, and make a note of (a) 
##  the F statistic (b) the degrees of freedom. Next,
##  look at the output from summary(iris.Model2) above. 
##  Can you find the same F-statistic and degrees of 
##  freedom there? What about the earlier output from
##  summary(iris.Model1)? 
##
##  Thus: the summary method for an lm object gives you
##  (among other things) the F-statistic for testing the
##  null hypothesis that all of the observations come
##  from the same distribution i.e. that none of the 
##  covariates have any effect. For ANOVA models, this 
##  is the "standard" F-test of no difference between 
##  groups; but the theory can be generalised to 
##  *any* linear model. Essentially, it's a comparison
##  of nested models, where the simpler model contains
##  only an intercept (you might notice that the reported
##  F-statistic in the summary of iris.Model0 is 
##  *different*: this is because that model contains no
##  intercept and R then gets a bit confused about what
##  the "null model" is supposed to be).
##
##  Finally in this exercise, let's illustrate how 
##  to build regression models incorporating both 
##  continuous and factor covariates. For the sake
##  of illustration, we'll look at the relationship
##  between petal length and sepal length.
##
iris.Model3 <- lm(Petal.Length ~ Species + Sepal.Length, data=iris)
cat("Petal lengths vs Species and sepal length:\n")
cat("==========================================\n")
print(summary(iris.Model3))
##
##  Everything looks significant and the residual standard
##  error is very small (0.28cm, compared with the overall
##  standard deviation of petal lengths which is 1.77cm).
##  However, we shouldn't really judge the "significance" of
##  a factor covariate based on the p-values in a summary 
##  table like this, for reasons explained above. We'll 
##  come back to that in a moment. First: it's possible
##  that the slopes of the Sepal Length : Petal Length
##  relationship are different for the three species. If
##  so, the we should include an interaction term in the
##  model. Here goes:
##
iris.Model4 <- update(iris.Model3, . ~ . + Species:Sepal.Length)
cat("Same model with interaction added:\n")
cat("==================================\n")
print(summary(iris.Model4))
##
##  That's interesting! First, how to interpret the 
##  coefficients? As with iris.Model3, the main effect
##  coefficients for Versicolor and Virginica represent
##  differences relative to Setosa. But now we also 
##  have two *additional* coefficients for the interactions.
##  These represent differences in the *slopes* of the 
##  Petal,Length:Sepal.Length relationship for these two
##  species, relative to the slope for Setosa flowers.
##  The fact that these two interaction terms are highly
##  significant suggests that the slopes are indeed different.
##  However, none of the other terms seems significant 
##  any more! You may be tempted to say that they should
##  be removed from the model, therefore. This is incorrect:
##  recall from the lecture that if you include an interaction
##  term in a model then you should almost always include
##  the corresponding main effects as well. What we should 
##  do, therefore, is to carry out a formal comparison of
##  the two models:
##  
cat("Test for common slopes in all species:\n")
cat("======================================\n")
print(anova(iris.Model3,iris.Model4,test="F"))
##
##  The p-value for that test is very small so we can reject
##  the null hypothesis that the data were generated from the
##  simpler of the two models. This provides evidence that the 
##  slope of the relationship does indeed vary between species. 
##  For completeness we should really do the usual diagnostics 
##  by plotting the model, but to save time here we'll just 
##  plot the fitted regression lines for the three species
##  with confidence intervals. This gives an opportunity
##  to see again how the predict() command works for lm
##  objects. Start by creating a data frame with values of the
##  covariates: for each species, make a grid of sepal lengths
##  covering the range of values in the dataset.
##
SepLength.grid <- 
  seq(min(iris$Sepal.Length),max(iris$Sepal.Length),length.out=50)
new.data <- data.frame(Species=rep(levels(iris$Species),each=50),
                       Sepal.Length=rep(SepLength.grid,3))
##
##  Next command shows what this new data frame looks like
##
head(new.data)
##
##  Now, do the prediction, with confidence intervals for the 
##  predicted values - and look at the result.
##
PetLength.preds <- 
  predict(iris.Model4,newdata = new.data,interval="confidence")
head(PetLength.preds)
##
##  You can see that for each row of new.data we get a fitted
##  value (i.e. a prediction) along with a lower and upper 
##  confidence limit for that fitted value. The first 50 rows 
##  are for Setosa, the second 50 rows for Versicolor and 
##  the final 50 for Virginica (because this is how the
##  new.data object was set up). So: set up a color scale
##  as in Workshop 1, then plot the predictions and confidence
##  limits for each species separately. Need to ensure that
##  the plotting limits will accommodate the confidence
##  intervals for each species.
##
plot.colours <- brewer.pal(3,"Dark2") # Same as in Workshop 1
plot.limits <- c(min(PetLength.preds[,2]),max(PetLength.preds[,3]))
##
##  Ensure we have a single plot in the graphics window and the 
##  margins are big enough to get the axis labels in, then plot
##  each species separately. 
##
par(mfrow=c(1,1),mar=c(3,3,3,2))
##
##  Setosa
##
plot(SepLength.grid,PetLength.preds[1:50,1],type="l",
     xlab="Sepal Length (cm)",ylab="Petal Length (cm)",
     ylim=plot.limits,col=plot.colours[1],lwd=2,
     main=paste("Regression functions for iris species",
                "(dashed lines are 95% confidence intervals)",sep="\n"))
lines(SepLength.grid,PetLength.preds[1:50,2], # Lower end of CI
      col=plot.colours[1],lty=2,lwd=2)        # 
lines(SepLength.grid,PetLength.preds[1:50,3], # Upper end of CI
      col=plot.colours[1],lty=2,lwd=2)        # 
##
##  Versicolor
##
lines(SepLength.grid,PetLength.preds[51:100,1],
      col=plot.colours[2],lty=1,lwd=2)
lines(SepLength.grid,PetLength.preds[51:100,2],
      col=plot.colours[2],lty=2,lwd=2) 
lines(SepLength.grid,PetLength.preds[51:100,3],
      col=plot.colours[2],lty=2,lwd=2)
##
##  Virginica
##
lines(SepLength.grid,PetLength.preds[101:150,1],
      col=plot.colours[3],lty=1,lwd=2)
lines(SepLength.grid,PetLength.preds[101:150,2],
      col=plot.colours[3],lty=2,lwd=2) 
lines(SepLength.grid,PetLength.preds[101:150,3],
      col=plot.colours[3],lty=2,lwd=2)
legend("topleft",lwd=2,col=plot.colours,
       legend=c("Setosa","Versicolor","Virginica"))
##
##  Notice the difference between the regression slopes for
##  the three species: that's what the interaction is for!
##
##  For ggplot aficionados, the basic ggplot code for 
##  producing a plot *something like* that is very simple:
##
library(ggplot2)
ggplot(data=iris,
          mapping=aes(Sepal.Length,Petal.Length,colour=Species)) +
  geom_smooth(method="lm", fullrange=TRUE)
##
##  I concede that this is easier than the "basic" version! Note, 
##  however, that ggplot itself fits some linear models internally
##  here - this is computationally inefficient given that we've
##  already stored them as R objects. 
##
##  The final thing we'll illustrate in this script is the 
##  use of *stepwise regression* to try and automatically
##  identify an "optimal" model. NOTE: IN GENERAL, THIS
##  IS NOT RECOMMENDED - THERE IS NO SUBSTITUTE FOR 
##  INTELLIGENT THOUGHT WHEN BUILDING MODELS!!! However,
##  you should at least be aware of its existence. In R,
##  the command is step(), and it works by deleting terms
##  one-by-one from a model where possible and comparing the
##  Akaike Information Criterion (AIC) at each stage. The
##  "best" model, in terms of AIC, is the one with the
##  smallest value (i.e. closest to -infinity, *not* closest
##  to zero!). When R can no longer delete any more terms 
##  without increasing the AIC, it stops and claims that
##  it has found the optimal model. This tool is very 
##  tempting, because it saves you from having to think. It 
##  is therefore worth repeating: IN GENERAL, THIS
##  IS NOT RECOMMENDED - THERE IS NO SUBSTITUTE FOR 
##  INTELLIGENT THOUGHT WHEN BUILDING MODELS!!! You should
##  only use stepwise regression (or other "automatic" 
##  model-building techniques) if it is not feasible to 
##  think carefully about the models that you're building -
##  for example, if you had to build 10000 separate models to 
##  predict the sales of 10000 different products. Anyway,
##  this is how step() works if you ever have to use it:
##
cat("Stepwise regression to see if iris model can be simplified:\n")
cat("===========================================================\n")
print(step(iris.Model4))
##
##  This perhaps isn't a very good example (or perhaps it *is*!).
##  The R output says: the starting model (i.e. iris.Model4) has an 
##  AIC of -396.96; and the only way that this model can be simplified
##  in the first instance is by removing the interaction between 
##  sepal length and species (because until this has been removed,
##  all of the main effects must remain in the model). But if this
##  term is removed, the AIC *increases* to -375.21. R therefore
##  doesn't try any more, and concludes that the original model
##  is the best among those considered. 
##
##  The step() command can also be used to start with a simple
##  model and add terms (rather than starting from a complex 
##  model and deleting terms as we have just done). However, since 
##  you should never use it, it seems pointless to tell you how it's 
##  done :-)
sink()
