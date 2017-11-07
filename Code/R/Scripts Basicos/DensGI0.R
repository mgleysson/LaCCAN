densGI0 <- function(z, alpha, gamma, Looks) {
  
  return(((Looks^Looks * gamma(Looks-alpha))/ (gamma^alpha * gamma(Looks-1) * gamma(-alpha))) *
    (z^(Looks-1) / (gamma + Looks * z)^(Looks-alpha)))
}

densGI0(0.07996402, -2, 4, 3)

densGI0(0.51461313, -10, 8, 4)

densGI0(0.3716623, -50, 25, 10)

