setwd("~/Documents/Master/ada")
Combined <- read.csv (file = 'Combined.csv',header = TRUE)
Combined1   <- Combined[1:47,1]
Extrinstic1 <- Combined[1:23,1]
Intrinstic1 <- Combined[24:47,1]
par(mfrow= c(1,2))
Extrin_d <- density(Extrinstic1)
Intrin_d <- density(Intrinstic1)
Combin_d <- density(Combined1)
plot(Extrin_d)
plot(Intrin_d)
par(mfrow = c(1,1))
plot(Combin_d)



