# Gerando variáveis aleatórias GI0 como razão de variáveis aleatórias gama

library(ggplot2)

X <- function(alpha, gamma) {
  return(rgamma(shape = -alpha, rate = gamma, n = 400))
}

Y <- function(Looks) {
  return(rgamma(shape = Looks, rate = 1, n = 400))
}

Z <- function(X, Y) {
  return(X/Y)
}

x = X(-3,2)
y = Y(8)
RandVarGI0 <- Z(x,y)

seqx <- seq(from=0.5, to=4, length = 400)
densd1 <- densGI0(seqx, -3, 2, 8)

hist(RandVarGI0, probability = TRUE, breaks = 30)
#lines(density(RandVarGI0))
lines(density(densd1))


x = X(-10,8)
y = Y(4)

Z(x,y)


x = X(-50,25)
y = Y(10)

Z(x,y)

