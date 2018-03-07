# Gerando variáveis aleatórias GI0 como razão de variáveis aleatórias gama

library(ggplot2)

randGI0 <- function(n, alpha, gamma, Looks) {
  
  X = rgamma(n, shape = -alpha, rate = gamma)
  Y = rgamma(n, shape = Looks, rate = Looks)
  
  return (Y/X)
  
}

randVarGI0_1 <- randGI0(2000, -3, 2, 8)

hist1 <- hist(randVarGI0_1, probability = TRUE, breaks = "FD")
densd1 <- densGI0(hist1$mids, -3, 2, 8)
#lines(hist1$mids, densd1)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-3, 2, 8)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_1, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist1$mids, densd1), size=1.1, color="blue")
 
###########################################################################################

randVarGI0_2 <- randGI0(2000, -3, 2, 3)

hist2 <- hist(randVarGI0_2, probability = TRUE, breaks = "FD")
densd2 <- densGI0(hist2$mids, -3, 2, 3)
#lines(hist2$mids, densd2)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-3, 2, 3)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_2, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist2$mids, densd2), size=1.1, color="blue")


###########################################################################################

randVarGI0_3 <- randGI0(2000, -3, 2, 1)

hist3 <- hist(randVarGI0_3, probability = TRUE, breaks = "FD")
densd3 <- densGI0(hist3$mids, -3, 2, 1)
#lines(hist3$mids, densd3)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-3, 2, 1)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_3, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist3$mids, densd3), size=1.1, color="blue")

###########################################################################################

randVarGI0_4 <- randGI0(2000, -1.5, 0.5, 1)

hist4 <- hist(randVarGI0_4, probability = TRUE, breaks = "FD")
densd4 <- densGI0(hist4$mids, -1.5, 0.5, 1)
#lines(hist4$mids, densd4)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-1.5, 0.5, 1)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_4, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist4$mids, densd4), size=1.1, color="blue")

###########################################################################################

randVarGI0_5 <- randGI0(2000, -3, 4, 1)

hist5 <- hist(randVarGI0_5, probability = TRUE, breaks = "FD")
densd5 <- densGI0(hist5$mids, -3, 4, 1)
#lines(hist5$mids, densd5)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-3, 4, 1)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_5, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist5$mids, densd5), size=1.1, color="blue")

###########################################################################################

randVarGI0_6 <- randGI0(2000, -8, 7, 1)

hist6 <- hist(randVarGI0_6, probability = TRUE, breaks = "FD")
densd6 <- densGI0(hist6$mids, -8, 7, 1)
#lines(hist6$mids, densd6)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-8, 7, 1)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_6, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist6$mids, densd6), size=1.1, color="blue")

###########################################################################################

randVarGI0_7 <- randGI0(2000, -1.5, 0.5, 3)

hist7 <- hist(randVarGI0_7, probability = TRUE, breaks = "FD")
densd7 <- densGI0(hist7$mids, -1.5, 0.5, 3)
#lines(hist7$mids, densd7)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-1.5, 0.5, 3)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_7, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist7$mids, densd7), size=1.1, color="blue")

###########################################################################################

randVarGI0_8 <- randGI0(2000, -3, 4, 3)

hist8 <- hist(randVarGI0_8, probability = TRUE, breaks = "FD")
densd8 <- densGI0(hist8$mids, -3, 4, 3)
#lines(hist6$mids, densd8)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-3, 4, 3)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_8, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist8$mids, densd8), size=1.1, color="blue")

###########################################################################################

randVarGI0_9 <- randGI0(2000, -8, 7, 3)

hist9 <- hist(randVarGI0_9, probability = TRUE, breaks = "FD")
densd9 <- densGI0(hist9$mids, -8, 7, 3)
#lines(hist6$mids, densd9)

qplot (xlab="Intensity", ylab="GI0 Densities") + 
  ggtitle("Histogram with Density curve of GI0(-8, 7, 3)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_histogram(aes(randVarGI0_9, y=..density..), fill="white", bins = 150, color="black") +
  geom_line(aes(hist9$mids, densd9), size=1.1, color="blue")

