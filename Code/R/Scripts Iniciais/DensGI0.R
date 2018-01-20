# Explorando a função densidade da GI0

densGI0 <- function(z, alpha, gamma, Looks) {
  
  return(((Looks^Looks * gamma(Looks-alpha))/ (gamma^alpha * gamma(Looks-1) * gamma(-alpha))) *
    (z^(Looks-1) / (gamma + Looks * z)^(Looks-alpha)))
}

d1 <- densGI0(0.07996402, -2, 4, 7)

d2 <- densGI0(0.07996402, -2, 4, 8)

d3 <- densGI0(0.07996402, -2, 4, 9)

d4 <- densGI0(0.07996402, -2, 1, 9)

d5 <- densGI0(0.07996402, -2, 2, 9)

d6 <- densGI0(0.07996402, -2, 3, 9)

d7 <- densGI0(0.07996402, -3, 3, 9)

d8 <- densGI0(0.07996402, -2, 3, 9)

d9 <- densGI0(0.07996402, -1, 3, 9)

dn <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9) 

densityGIO <- data.frame(y = dn)

ggplot(densityGIO, aes(y)) + geom_density(adjust = 1) +
  xlab("Densidades") + ylab("Frequencia")




