# Analisando o funcionamento dos Geradores Lineares Congruenciais

library(ggplot2)

lcg <- function(M, a, c, seed, size) {
  r = numeric(size)
  r[1] = seed
  
  for(i in 1:(size-1))
    r[i+1] = (a*r[i] + c) %% M
  
  return (r/M)
}

seq1 <- lcg(M = 16, a = 5, c = 1, seed = 5, size = 34)
seq2 <- lcg(M = 86436, a = 1093, c = 18257, seed = 7, size = 100)
seq3 <- lcg(M = 394, a = 34, c = -102, seed = 404, size = 50)

dados1 <- data.frame(y = seq1)

h1 <- ggplot(dados1, aes(y)) + geom_histogram(binwidth = 0.08) +
xlab("Dados da seq 1") + ylab("Frequencia")

h1

dados2 <- data.frame(y = seq2)

h2 <- ggplot(dados2, aes(y)) + geom_histogram(binwidth = 0.08) +
  xlab("Dados da seq 2") + ylab("Frequencia")

h2

dados3 <- data.frame(y = seq3)

h3 <- ggplot(dados3, aes(y)) + geom_histogram(binwidth = 0.08) +
  xlab("Dados da seq 3") + ylab("Frequencia")

h3


#hist(seq1, breaks = 10)
#hist(seq2, breaks = 15)
#hist(seq3,breaks = 20)

