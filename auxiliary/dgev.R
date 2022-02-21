################################################################################
# Density Distribution Function
################################################################################

dgev <- function(x, mu, sigma, delta){
  
  y <- (1/sigma) * (delta + 1) * abs(x)^delta * exp( - (x*abs(x)^delta - mu)/sigma - exp( -(x*abs(x)^delta - mu)/sigma ) )
  return(y)
}


x <- dgev(x = c(1,0,-1), mu = 4, sigma = 1, delta = 2)
plot(density(x))
