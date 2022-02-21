################################################################################
# Log likelihood estimation
################################################################################

# Packages

library(maxLik)
library(bgumbel)

maxllog <- function(x){
  
  # start
  theta0 <- bgumbel::mlebgumbel(data = x, auto = TRUE)
  theta0 <- abs(theta0$estimate$estimate)
  
  # log-likelihood function
  llog <- function(theta,x){
    n <- length(x)
    mu <- theta[1]
    sigma <- theta[2]
    delta <- theta[3]
    ll <- n * log(delta + 1) - n*log(sigma) + delta * sum(log(abs(x))) - sum(x * abs(x)^delta + mu) / sigma -
      sum(exp(- (x * abs(x)^delta + mu) / sigma ))
    return(ll)
  }
  
  # first derivative of log-likelihood function with respect to theta
  gradlik <- function(theta,x){
    mu <- theta[1]
    sigma <- theta[2]
    delta <- theta[3]
    grmu <- sum( 1/sigma * exp(- (x * abs(x)^delta + mu) / sigma) -1/sigma )
    grsigma <- sum( -1/sigma + (x * abs(x)^delta + mu)/sigma^2 - 
                      exp(- (x * abs(x)^delta + mu) / sigma ) * (x * abs(x)^delta + mu)/sigma^2
    )
    grdelta <- sum( 1/(delta + 1) + log(abs(x)) - x * abs(x)^delta * log(abs(x))/sigma +
                      exp(- (x * abs(x)^delta + mu) / sigma) * x * abs(x)^delta * log(abs(x))/sigma
    )
    return(c(grmu, grsigma, grdelta))
  }
  
  est <- maxLik(logLik = llog, grad = gradlik, start = theta0, x = x, method = 'BFGS')
  return(est)
}
