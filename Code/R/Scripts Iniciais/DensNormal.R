library(ggplot2)

densNormal <- function (x, mi, sigmaQuad) {
  
  return ((1/sqrt(2*pi*sigmaQuad)) * exp ((-1/(2*sigmaQuad))*(x - mi)^2))
  
}

x <- seq(from=-6, to=6, by = 0.001)

dens1 <- densNormal(x, 0, 1)
dens2 <- densNormal(x, 0, 4)
dens3 <- densNormal(x, 0, 9)
 
qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Normal distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens1, colour="black"), size=1.3) +
  geom_line(aes(x,dens2, colour="blue"), size=1.3) +
  geom_line(aes(x,dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("N(0,1)", "N(0,4)", "N(0,9)"))


dens4 <- densNormal(x, 0, 1)
dens5 <- densNormal(x, 1, 1)
dens6 <- densNormal(x, 1, 4)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Normal distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens4, colour="black"), size=1.3) +
  geom_line(aes(x,dens5, colour="blue"), size=1.3) +
  geom_line(aes(x,dens6, colour="red"), size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("N(0,1)", "N(1,1)", "N(1,4)"))


dens7 <- densNormal(x, 1, 1)
dens8 <- densNormal(x, 2, 1)
dens9 <- densNormal(x, 1, 9)


qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Normal distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens7, colour="black"), size=1.3) +
  geom_line(aes(x,dens8, colour="blue"), size=1.3) +
  geom_line(aes(x,dens9, colour="red"), size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("N(1,1)", "N(2,1)", "N(1,9)"))


