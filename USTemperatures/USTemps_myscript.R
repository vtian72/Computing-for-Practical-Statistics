#Script to compute the least-squares estimates and the standard errors for USTemp

#Load the dataset
load("UStemps.rda")

#Create the design matrix, with first column of ones, second column: latitudes, third column:
#longitudes

ones <- rep(1, nrow(ustemp))
latitude <- c(ustemp$latitude)
longitude <- c(ustemp$longitude)

#Bind the vectors side by side to form a design matrix, X

X <- cbind(ones, latitude, longitude)

#Create a response vector, Y to contain the minimum temperatures

Y <- ustemp$min.temp

#Calculate the least squares estimates (lse) of the regression coefficients

lse <- solve(t(X) %*% X) %*% t(X) %*% Y

#Calculate vector of fitted values (Y.fitted) 

Y.fitted <- X %*% lse

#Calculate the vector of residuals (e)

e <- Y - Y.fitted

#Calculate the estimated error standard deviation (sdEE)
#k is the number of coefficients estimated
#n is the number of observations
#summation is the sum of each residual ^ 2

sdEE <- 0

k <- ncol(X)
n <- nrow(X)
summation <- sum(e^2)

sdEE <- sqrt((1/(n-k)) * summation)

#Calculate the estimated covariance matrix of least squares estimate (Covmat)

Covmat <- sdEE^2 * solve((t(X) %*% X))

#Calculate the standard errors of least-squares estimate (standard.errors)

standard.errors <- sqrt(diag(Covmat))

