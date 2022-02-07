################################################################################
# Log likelihood estimation
################################################################################

# Packages

library(maxLik)

llog <- function(theta,x){
  mu <- theta[1]
  sigma <- theta[2]
  delta <- theta[3]
  ll <- sum(
    log(delta + 1) - log(sigma) + delta * log(abs(x)) - (x * abs(x)^delta + mu) / sigma
    - exp(- (x * abs(x)^delta + mu) / sigma )
  )
  return(ll)
}

gradlik <- function(theta,x){
  mu <- theta[1]
  sigma <- theta[2]
  delta <- theta[3]
  grmu <- sum( 1/sigma * exp(- (x * abs(x)^delta + mu) / sigma) -1/sigma )
  grsigma <- sum( -1/sigma + (x * abs(x)^delta + mu)/sigma^2 - 
                    exp(- (x * abs(x)^delta + mu) / sigma ) * (x * abs(x)^delta + mu)/sigma^2
                  )
  grdelta <- sum(1/(delta + 1) + log(abs(x)) - x * abs(x)^delta / sigma * log(x * abs(x)^delta / sigma)
                 + exp(mu/sigma) * exp(-(x * abs(x)^delta)/sigma) * x * abs(x)^delta / sigma * 
                   log((x * abs(x)^delta) / sigma)
                   )
  return(c(grmu, grsigma, grdelta))
}


