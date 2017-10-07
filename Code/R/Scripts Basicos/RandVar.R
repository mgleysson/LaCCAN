X <- function(alpha, gamma) {
  return(rgamma(shape = -alpha, rate = gamma, n = 10))
}

Y <- function(Looks) {
  return(rgamma(shape = Looks, rate = 1, n = 10))
}

Z <- function(X, Y) {
  return(X/Y)
}

x = X(-2,4)
y = Y(3)

Z(x,y)
