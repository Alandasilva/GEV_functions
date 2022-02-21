################################################################################
# Cumulative Distribution Function
################################################################################

pbgum <- function(x, mu, sigma, delta){
  
  y <- exp( -exp( -(x*abs(x)^delta - mu)/sigma ) )
  return(y)
}
