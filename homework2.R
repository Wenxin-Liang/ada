t.test(chickwts$weight[chickwts$feed=="meatmeal"],chickwts$weight[chickwts$feed=="casein"])

wilcox.test(chickwts$weight[chickwts$feed=="meatmeal"],chickwts$weight[chickwts$feed=="casein"])

chi_meat <- chickwts$weight[chickwts$feed=="meatmeal"]
chi_casein <- chickwts$weight[chickwts$feed=="casein"]
chi_casein_bar <- chi_casein+mean(chi_meat)-mean(chi_casein)
na <- length(chi_meat)
nb <- length(chi_casein)
z_obser <- (mean(chi_meat)-mean(chi_casein))/sqrt(var(chi_meat)/na+var(chi_casein)/nb)
z_star  <- c()
k=1000
for (i in 1:k) {
  chi_meat_star   <-sample(chi_meat,na,replace=T)
  chi_casein_star <- sample(chi_casein_bar,nb,replace=T)
  z_star[i] <- (mean(chi_meat_star)-mean(chi_casein_star))/sqrt(var(chi_meat_star)/na+var(chi_casein_star)/nb)
}

p_value <- sum(abs(z_star)>=abs(z_obser))/k

#=====================================================================
S <- c(SA,SB)
theta <- function(x1,x2) theta <- mean(x1)-mean(x2)
theta0 <- theta(SA,SB)
theta_star <- c()
K <- 500
for(i in 1:K){
  S_star <- S[sample.int(na+nb)]
  theta_star[i] <- theta(S_star[1:na],S_star[(na+1):(na+nb)])
}

p_value <- sum(abs(theta_star)>abs(theta0))/K
hist(theta_star,main="", xlab="Simulated theta",xlim = c(-4.2, 4.2),freq=F)
abline(v=theta0, lty = 3)
abline(v=-theta0, lty = 3)
#==================================================================
#=============2a===================
chicksoy <- chickwts[23:36,1]
chicksun <- chickwts[37:48,1]
library(bootstrap)
mediansoy <- bootstrap(chicksoy,1000,median)
mediansun <- bootstrap(chicksun,1000,median)
quantile(mediansoy$thetastar-mediansun$thetastar,c(0.025,0.975))

#=========2b=============
varsoy <- bootstrap(chicksoy,1000,var)
varsun <- bootstrap(chicksun,1000,var)
quantile(varsoy$thetastar/varsun$thetastar,c(0.025,0.975))


#==========2c============
var.test(chicksoy,chicksun)

#=======explanation======
par(mfrow=c(2,2))
hist(chicksoy)
qqnorm(chicksoy)
qqline(chicksoy)
hist(chicksun)
qqnorm(chicksun)
qqline(chicksun)
shapiro.test(chicksoy)
shapiro.test(chicksun)

#===========3a========

meatlow <- chi_meat[which(chi_meat<258)]
soylow  <- chicksoy[which(chicksoy<258)]
n_mlow  <-length(meatlow)
n_slow  <-length(soylow)
n_m     <-length(chi_meat)
n_s     <-length(chicksoy)

x       <-c(n_mlow,n_slow)
n       <-c(n_m,n_s)
prop.test(x,n)

x <- c
m.low <- chick.m[which(chick.m$weight<258),]
soy.low <- chick.soy[which(chick.soy$weight<258),]
length(m.low$weight)
length(soy.low$weight)

x <- c(4,9)
n <- c(11,14)
prop.test(x,n)

