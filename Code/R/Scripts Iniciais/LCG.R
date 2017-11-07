# Analisando o funcionamento dos Geradores Lineares Congruenciais

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

hist(seq1)
hist(seq2)
hist(seq3)

