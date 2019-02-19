#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######        Stock Sale Price Example Using Monte Carlo         ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################

# Number of simulations
nsims <- 10000       

# Define parameter values
mu <- 0.5; sigsq <- 0.01

# Times of sale
T <- rgamma(nsims,2,3)  

# Values of B[T] Note: rnorm() takes in a vector
# 10,000 results will be generated
Bt <- rnorm(nsims,mean=0,sd=sqrt(T))     

#Find a vector of 10,000 possible sale prices                                    
SalePrice <- exp( (mu-0.5*sigsq)*T+(sqrt(sigsq)*Bt) )

#Calculate the sample mean of your SalePrice vector
meanSalePrice <- mean(SalePrice)

#Calculate the sample variance 
varSalePrice <- var(SalePrice)

#Probability that price is less than 2, (SalePrice < 2) generates a vector to 10,000 values
#either FALSE(0) or TRUE(1)
mean(SalePrice < 2)

#Plot the density function of the sale price
plot(density(SalePrice), main = "Density Function of Sale Price")

#Function to see how expected price varies with mu
MeanPrice <- function(mu,sigsq,nsims) {
  set.seed(2000)
  T <- rgamma(nsims,2,3)
  Bt <- rnorm(nsims,mean=0,sd=sqrt(T))
  SalePrice <- exp( (mu-0.5*sigsq)*T+(sqrt(sigsq)*Bt) )
  mean(SalePrice)
}

#Run the MeanPrice function with mu = 0.5, sigsq = 0.01, nsims = 10000
MeanPrice(0.05, 0.01, 10000)

#See how expected price varies with mu
#Create a grid of values
#mapply command finds the function MeanPrice separately for all values of mu on the mu.grid
#with the sigsq and nsims fixed each time
mu.grid <- seq(0.01, 1, 0.01)
PriceVec <- mapply(MeanPrice, mu=mu.grid, MoreArgs = list(sigsq = 0.01, nsims = 1000))

#Plot the resulting PriceVec graph
plot(mu.grid, PriceVec, ylab = "Sale Price", xlab = "mu", main = "Sale Price relationship with mu", 
     type = "l")
