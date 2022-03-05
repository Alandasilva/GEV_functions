################################################################################
# Density Distribution Function
################################################################################

dbgum <- function(x, mu, sigma, delta){
  
  y <- (1/sigma) * (delta + 1) * abs(x)^delta * exp( - (x*abs(x)^delta - mu)/sigma - exp( -(x*abs(x)^delta - mu)/sigma ) )
  return(y)
}
