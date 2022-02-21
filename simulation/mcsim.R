########################################################
### Monte Carlo Simulation
########################################################

source('./auxiliary/rgev.R') # random numbers generator
source('./auxiliary/mlegev.R') # MLE

R = 1000 # Number of MC replications
cont = 0 # count replications
fail = 0 # count of replications which fail
n = 100 # sample size

# set of parameters
mu = 0
sigma = 1
delta = 1

# MC replicates
MCmu <- NULL
MCsigma <- NULL
MCdelta <- NULL

while(cont < R){
  
  cont <- cont + 1
  
  x <- rgev(n = n, mu = mu, sigma = sigma, delta = delta)
  
  est <- tryCatch(expr = maxllog(x), error = function(e) 'erro')
  if(est != 'erro'){
    MCmu[cont] <- est$estimate[1]
    MCsigma[cont] <- est$estimate[2]
    MCdelta[cont] <- est$estimate[3]    
  }
  else{
    cont <- cont - 1
    fail <- fail + 1
  }
}

# Bias
mean(MCmu) - mu
mean(MCsigma) - sigma
mean(MCdelta) - delta

# MSE
mean( (MCmu - mu)^2 )
mean( (MCsigma - sigma)^2 )
mean( (MCdelta - delta)^2 )

# RMSE
sqrt( mean( (MCmu - mu)^2 ) )
sqrt( mean( (MCsigma - sigma)^2 ) )
sqrt( mean( (MCdelta - delta)^2 ) )
