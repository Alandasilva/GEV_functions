################################################################################
### GEV - PDF
################################################################################

dgev <- function(x,xi,mu,sigma,delta,log = FALSE){
  if(sigma <= 0 | delta <= -1){stop('the parameters must be sigma > 0 and delta >=-1, respectively.')}
  t  <- function(x) sigma*x*abs(x)^delta
  tl <- function(x) sigma*(delta + 1)*abs(x)^delta
  f <- (xi != 0) * (1 + xi*(t(x) - mu))^(-1/xi - 1) * exp(-(1 + xi*(t(x) - mu))^(-1/xi)) * tl(x) +
    (xi == 0) * exp(-(t(x) - mu) - exp(-(t(x) - mu))) * tl(x)
  if(log == TRUE){f <- log(f)}
  return(f)
}

mu = 0; sigma = 1; delta = 0; xi = 1
dgev(x = 1:10,mu = 0, sigma = 1, delta = 1, xi = 10)
