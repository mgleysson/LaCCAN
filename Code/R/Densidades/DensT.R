library(ggplot2)

densT <- function(x, v) {
  return  (((gamma((v+1)/2))/(sqrt(v*pi) * gamma(v/2))) * (1 + (x^2/v))^(-1*((v+1)/2)))
}

x <- seq(from=-4, to=4, by = 0.001)

dens1 <- densT(x, 1)
dens2 <- densT(x, 5)
dens3 <- densT(x, 15)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a T distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x,dens1, colour="black"), size=1.3) +
  geom_line(aes(x,dens2, colour="blue"), size=1.3) +
  geom_line(aes(x,dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("t(1)", "t(5)", "t(15)"))