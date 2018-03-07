# Explorando a função densidade da GI0

library(ggplot2)

densGI0 <- function(x, alpha, gamma, Looks) {
  
  return(((Looks^Looks * gamma(Looks-alpha))/ (gamma^alpha * gamma(Looks) * gamma(-alpha))) *
    (x^(Looks-1) / (gamma + Looks * x)^(Looks-alpha)))
}

x <- seq(from=0.001, to=4, by = 0.01)

dens1 <- densGI0(x, -3, 2, 1)
dens2 <- densGI0(x, -3, 2, 3)
dens3 <- densGI0(x, -3, 2, 8)

qplot (xlab="Intensity", ylab="GI0 Densities") +
  ggtitle("Probability density function of a GI0 distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens1, colour="black"), size=1.3) +
  geom_line(aes(x,dens2, colour="blue"), size=1.3) +
  geom_line(aes(x,dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("GI0(-3,2,1)", "GI0(-3,2,3)", "GI0(-3,2,8)"))


dens4 <- densGI0(x, -1.5, 0.5, 1)
dens5 <- densGI0(x, -3, 2, 1)
dens6 <- densGI0(x, -8, 7, 1)

qplot (xlab="Intensity", ylab="GI0 Densities") +
  ggtitle("Probability density function of a GI0 distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens4, colour="black"), size=1.3) +
  geom_line(aes(x,dens5, colour="blue"), size=1.3) +
  geom_line(aes(x,dens6, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("GI0(-1.5,0.5,1)", "GI0(-3,2,1)", "GI0(-8,7,1)"))

dens7 <- densGI0(x, -1.5, 0.5, 3)
dens8 <- densGI0(x, -3, 2, 3)
dens9 <- densGI0(x, -8, 7, 3)

qplot (xlab="Intensity", ylab="GI0 Densities") +
  ggtitle("Probability density function of a GI0 distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens7, colour="black"), size=1.3) +
  geom_line(aes(x,dens8, colour="blue"), size=1.3) +
  geom_line(aes(x,dens9, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("GI0(-1.5,0.5,3)", "GI0(-3,2,3)", "GI0(-8,7,3)"))

