library(ggplot2)

densExp <- function(x, lambda) {
  
  ifelse(x>=0, (1/lambda) * exp(-x/lambda), 0)
  
}

x <- seq(from=0.001, to=4, by = 0.001)

dens1 <- densExp(x, 1)
dens2 <- densExp(x, 2)
dens3 <- densExp(x, 3)

qplot (xlab="x", ylab="density") +
  ggtitle("Probability density function of a Exponencial distribution") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(aes(x, dens1, colour="black"), size=1.3) +
  geom_line(aes(x, dens2, colour="blue"), size=1.3) +
  geom_line(aes(x, dens3, colour="red") , size=1.3) +
  scale_color_discrete(name = "parameters", labels = c("Exp(1)", "Exp(2)", "Exp(3)"))
