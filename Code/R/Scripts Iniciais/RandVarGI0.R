X <- function(alpha, gamma) {
  return(rgamma(shape = -alpha, rate = gamma, n = 100))
}

Y <- function(Looks) {
  return(rgamma(shape = Looks, rate = 1, n = 100))
}

Z <- function(X, Y) {
  return(X/Y)
}

x = X(-2,4)
y = Y(3)

Z(x,y)

x = X(-10,8)
y = Y(4)

Z(x,y)


x = X(-50,25)
y = Y(10)

Z(x,y)

