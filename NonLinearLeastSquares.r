#Method for non-linear least squares

#Read in the data
NLSdata <- read.table("nls2.dat", header = TRUE)

#Find the structure of the data
str(NLSdata)

#Plot data
plot(NLSdata$x, NLSdata$Y, col = "blue", pch =16, xlab = "x", ylab = "Y", 
     main = "Data for nonlinear regression example")

#Linear Regression to compare the log of Y with x

model <- lm(log(NLSdata$Y) ~ NLSdata$x)
summary(model)

#Sum function to find the sum of squares
sumsqerr <- function(theta,x,Y) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  mu <- beta0*exp(beta1*x)
  sum((Y-mu)^2)
}

#Fit a NLS(nonlinear least squares) model using results from linear regression
#beta0 is the intercept --> ie log(0.3) --> 1.3
#beta1 is the NLSdata$x value --> -0.3
NLS.fit <- nlm(sumsqerr, c(1.3, -0.3), x = NLSdata$x, Y = NLSdata$Y, hessian = TRUE)
NLS.fit

#Check convergence by plotting the fitted function
beta0 <- NLS.fit$estimate[1]
beta1 <- NLS.fit$estimate[2]
x.grid <- seq(0,10,0.1)
mu <- beta0 * exp(beta1 * x.grid)
plot(NLSdata$x, NLSdata$Y, xlab = "x", ylab = "Y", col = "blue", pch = 16)
lines(x.grid, mu, col = "red")



