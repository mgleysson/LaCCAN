library(ggplot2)

densCauchy <- function(x, alpha, beta) {
  
  return (1/(pi*beta*(1 + ((x - alpha)/beta)^2)))
  
}

x <- seq(from = -5, to = 5, by = 0.001)

dens1 <- densCauchy(x, 0, 4)
dens2 <- densCauchy(x, 0, 2)
dens3 <- densCauchy(x, 0, 1/2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Cauchy distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens1, colour="black"), size=1.3) +
  geom_line(aes(x,dens2, colour="blue"), size=1.3) +
  geom_line(aes(x,dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Cauchy(alpha=0, beta=4)", "Cauchy(alpha=0, beta=2)", "Cauchy(alpha=0, beta=1/2)"))


dens4 <- densCauchy(x, 0, 2)
dens5 <- densCauchy(x, 2, 2)
dens6 <- densCauchy(x, 2, 1/2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Cauchy distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens4, colour="black"), size=1.3) +
  geom_line(aes(x,dens5, colour="blue"), size=1.3) +
  geom_line(aes(x,dens6, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Cauchy(alpha=0, beta=2)", "Cauchy(alpha=2, beta=2)", "Cauchy(alpha=2, beta=1/2)"))


dens7 <- densCauchy(x, -2.5, 1/4)
dens8 <- densCauchy(x, 0, 1/3)
dens9 <- densCauchy(x, 2.5, 1/2)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Cauchy distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens7, colour="black"), size=1.3) +
  geom_line(aes(x,dens8, colour="blue"), size=1.3) +
  geom_line(aes(x,dens9, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Cauchy(alpha=-2.5, beta=1/4)", "Cauchy(alpha=0, beta=1/3)", "Cauchy(alpha=2.5, beta=1/2)"))