################################################################################
### MLE - GEV
################################################################################

library(maxLik)

llik <- function(theta,x){
  mu      <- theta[1]
  sigma   <- theta[2]
  delta   <- theta[3]
  xi      <- theta[4]
  n <- length(x)
  loglike <- n*log(sigma) + n*log(delta + 1) + sum( delta*log(abs(x)) - 
              (1 + 1/xi)*log( 1 + xi*(sigma*x*abs(x)^delta - mu) ) - 
                (1 + xi*(sigma*x*abs(x)^delta - mu) )^(-1/xi) )
  return(-loglike)
}

grlik <- function(theta,x){
  
}

theta <- c(1,1,1,1)
x <- rgev(n = 1000, mu = 2, sigma = 1, delta = 1, xi = 2)

mlegev <- optim(fn = llik, par = theta, x = x, method = 'L-BFGS-B',
                lower = c(-Inf, 0, -1, -Inf), upper = c(Inf, Inf, Inf, Inf),
                control = list(maxit = 500))
