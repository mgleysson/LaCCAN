library(ggplot2)

densGama <- function (x, alpha, beta) {
 
  if(x >= 0){
      return ((beta^alpha * x^(alpha-1) * exp(-beta*x))/gamma(alpha))
  }else{
      return (0)
  } 
  
}

x <- seq(from=0, to=10, by = 0.001)

dens1 <- densGama(x, 2, 0.5)
dens2 <- densGama(x, 2, 1)
dens3 <- densGama(x, 2, 2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Gama distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens1, colour="black"), size=1.3) +
  geom_line(aes(x,dens2, colour="blue"), size=1.3) +
  geom_line(aes(x,dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Gama(2,1/2)", "Gama(2,1)", "Gama(2,2)"))

dens4 <- densGama(x, 4, 0.5)
dens5 <- densGama(x, 4, 1)
dens6 <- densGama(x, 4, 2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Gama distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens4, colour="black"), size=1.3) +
  geom_line(aes(x,dens5, colour="blue"), size=1.3) +
  geom_line(aes(x,dens6, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Gama(4,1/2)", "Gama(4,1)", "Gama(4,2)"))


dens7 <- densGama(x, 2, 0.5)
dens8 <- densGama(x, 4, 0.5)
dens9 <- densGama(x, 4, 2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Gama distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens7, colour="black"), size=1.3) +
  geom_line(aes(x,dens8, colour="blue"), size=1.3) +
  geom_line(aes(x,dens9, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Gama(2,1/2)", "Gama(4,1/2)", "Gama(4,2)"))

