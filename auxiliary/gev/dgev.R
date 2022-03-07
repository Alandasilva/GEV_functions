################################################################################
### GEV - PDF
################################################################################

dgev <- function(x,xi,mu,sigma,delta,log = FALSE){
  if(sigma <= 0 || delta <= -1 || xi == 0){stop('the parameters must be sigma > 0, delta >-1 and xi != 0, respectively.')}
  t  <- function(x) {sigma*x*abs(x)^delta} # T(x) function
  #dt <- function(x) {sigma*(delta + 1)*abs(x)^delta} # T'(x) function
  tl <- function(x) { 1 + xi*(sigma*x*abs(x)^delta - mu) } # t(x) function
  
  if(xi > 0){
    f <- sigma*(delta + 1)*(abs(x)^delta)*(tl(x))^(-1/xi - 1)*exp(-tl(x)^(-1/xi)) * ( sign(mu - 1/xi)*(abs(mu - 1/xi)/sigma)^(1/(delta + 1)) < x )
  }
  if(xi < 0){
    f <- sigma*(delta + 1)*(abs(x)^delta)*(tl(x)^(-1/xi - 1))*exp(-tl(x)^(-1/xi)) * ( sign(mu - 1/xi)*(abs(mu - 1/xi)/sigma)^(1/(delta + 1)) > x )
  }
  # f <- ifelse(xi > 0,
  #             (sigma*(delta + 1)*(abs(x)^delta)*(tl(x))^(-1/xi - 1)*exp(-tl(x)^(-1/xi)) * ( sign(mu - 1/xi)*(abs(mu - 1/xi)/sigma)^(1/(delta + 1)) < x )),
  #             (sigma*(delta + 1)*(abs(x)^delta)*(tl(x)^(-1/xi - 1))*exp(-tl(x)^(-1/xi)) * ( sign(mu - 1/xi)*(abs(mu - 1/xi)/sigma)^(1/(delta + 1)) > x ))
  #             )
  # if(is.nan(f)){f <- 0}
  if(log == TRUE){f <- log(f)}
  return(f)
}

mu = 0; sigma = 1; delta = 0; xi = 1
dgev(x = -1,mu = 0, sigma = 1, delta = 0, xi = 1)

curve(expr = dgev(x = x,mu = 1, sigma = 1, delta = 2, xi = 1), from = -4, to = 40)
