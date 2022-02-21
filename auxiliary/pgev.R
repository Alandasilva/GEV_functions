################################################################################
# Cumulative Distribution Function
################################################################################

pgev <- function(x, mu, sigma, delta){
  
  y <- exp( -exp( -(x*abs(x)^delta - mu)/sigma ) )
  return(y)
}


x <- pgev(x = c(1,0,-1), mu = 4, sigma = 1, delta = 2)
plot(density(x))
