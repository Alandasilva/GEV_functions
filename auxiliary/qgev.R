################################################################################
# Quantile Function
################################################################################

qgev <- function(p, mu, sigma, delta){
  
  y <- ifelse(test = p > exp(-exp(-mu/sigma)),
              yes = (- sigma * log(-log(p)) - mu)^(1/(delta + 1)), 
              no = -(mu + sigma * log(-log(p)))^(1/(delta + 1))
  )
  return(y)
}


x <- qgev(p = c(0.2, 0.975), mu = 4, sigma = 1, delta = 2)

