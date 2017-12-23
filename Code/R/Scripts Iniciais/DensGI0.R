# Explorando a função densidade da GI0

densGI0 <- function(z, alpha, gamma, Looks) {
  
  return(((Looks^Looks * gamma(Looks-alpha))/ (gamma^alpha * gamma(Looks-1) * gamma(-alpha))) *
    (z^(Looks-1) / (gamma + Looks * z)^(Looks-alpha)))
}

densGI0(0.07996402, -2, 4, 7)

densGI0(0.07996402, -2, 4, 8)

densGI0(0.07996402, -2, 4, 9)

densGI0(0.07996402, -2, 1, 9)

densGI0(0.07996402, -2, 2, 9)

densGI0(0.07996402, -2, 3, 9)

densGI0(0.07996402, -3, 3, 9)

densGI0(0.07996402, -2, 3, 9)

densGI0(0.07996402, -1, 3, 9)



