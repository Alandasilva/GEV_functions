################################################################################
### GEV - QDF
################################################################################

qgev <- function(p,xi,mu,sigma,delta){
  if(sigma <= 0 || delta <= -1 || xi == 0){stop('the parameters must be sigma > 0, delta >-1 and xi != 0, respectively.')}
  
  # if( (mu + ((-log(p))^(-xi) - 1)/xi) > 0){
  #   q <- ( ( mu/sigma + ((-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)) )
  # }
  # if( (mu + ((-log(p))^(-xi) - 1)/xi) < 0){
  #   q <-  ( - mu/sigma - ( (-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1))
  # }
  q <- ifelse((mu + ((-log(p))^(-xi) - 1)/xi) > 0,
              ( ( mu/sigma + ((-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)) ),
              ( - mu/sigma - ( (-log(p))^(-xi) - 1)/(sigma*xi) )^(1/(delta + 1)))
  return(q)
}

mu = 0; sigma = 1; delta = 1; xi = 1
qgev(p = seq(0.01,0.99,0.01), mu = 0, sigma = 1, delta = 1, xi = 1)

