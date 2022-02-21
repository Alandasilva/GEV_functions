################################################################################
# Quantile Function
################################################################################

qbgum <- function(p, mu, sigma, delta){
  
  y <- ifelse(test = p > exp(-exp(-mu/sigma)),
              yes = (- sigma * log(-log(p)) - mu)^(1/(delta + 1)), 
              no = -(mu + sigma * log(-log(p)))^(1/(delta + 1))
  )
  return(y)
}

