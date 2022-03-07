################################################################################
### GEV - Random Numbers Generator
################################################################################

rgev <- function(n,xi,mu,sigma,delta){
  if(sigma <= 0 || delta <= -1 || xi == 0){stop('the parameters must be sigma > 0, delta >-1 and xi != 0, respectively.')}
  
  # if( (mu + ((-log(p))^(-xi) - 1)/xi) > 0){
  #   q <- ( ( mu/sigma + ((-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)) )
  # }
  # if( (mu + ((-log(p))^(-xi) - 1)/xi) < 0){
  #   q <-  ( - mu/sigma - ( (-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1))
  # }
  p <- runif(n)
  q <- ifelse((mu + ((-log(p))^(-xi) - 1)/xi) > 0,
              ( ( mu/sigma + ((-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)) ),
              -( - mu/sigma - ( (-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)))
  return(q)
}

rgev(n = 10000, mu = 1, sigma = 1, delta = 1, xi = 1)

