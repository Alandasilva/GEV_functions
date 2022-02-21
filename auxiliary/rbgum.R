################################################################################
# Random Numbers Generator
################################################################################

rgev <- function(n, mu, sigma, delta){
  x <- runif(n)
  y <- ifelse(test = x > exp(-exp(-mu/sigma)),
              yes = (- sigma * log(-log(x)) - mu)^(1/(delta + 1)), 
              no = -(mu + sigma * log(-log(x)))^(1/(delta + 1))
              )
  return(y)
}
