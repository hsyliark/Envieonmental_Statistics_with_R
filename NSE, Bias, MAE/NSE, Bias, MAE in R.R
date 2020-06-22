### Compute NSE
## reference1 : https://www.rdocumentation.org/packages/hydroGOF/versions/0.4-0/topics/NSE
## reference2 : https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

install.packages("hydroGOF")
library(hydroGOF)

# NOT RUN {
obs <- 1:10
sim <- 1:10
NSE(sim, obs)

obs <- 1:10
sim <- 2:11
NSE(sim, obs)

#################
# Computing NSE on the (natural) logarithm of simulated and observed values
obs <- 1:10/10
sim <- 2:11/10
NSE(sim=sim, obs=obs, FUN=log)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
NSE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'NSE'
NSE(sim=sim, obs=obs)
# }




### Compute Bias
## reference : https://www.rdocumentation.org/packages/SimDesign/versions/1.14/topics/bias

my_bias <- function(sim, obs) {
  bias = sum(sim-obs)/length(sim)
  return(bias)
}









### Compute MAE
## reference1 : https://www.rdocumentation.org/packages/Metrics/versions/0.1.4/topics/mae
## reference2 : https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

install.packages("hydroGOF")
library(hydroGOF)

obs <- 1:10
sim <- 1:10
mae(sim, obs)
obs <- 1:10
sim <- 2:11
mae(sim, obs)
##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
# Generating a simulated daily time series, initially equal to the observed series
sim <- obs
# Computing the mean absolute error for the "best" case
mae(sim=sim, obs=obs)
# Randomly changing the first 2000 elements of 'sim', by using a normal distribution
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)
# Computing the new mean absolute error
mae(sim=sim, obs=obs)
