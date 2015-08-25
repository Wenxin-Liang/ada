#Question 1 part a
par.n_1a <- function (n,alpha,delta,sigma){
  #Calculate Z_alpha/2
  z1_1a <-qnorm(1-alpha/2)
  #Calculate the power
  z2_1a <-sqrt(n)*abs(delta)/sqrt(2)/abs(sigma)-z1_1a
  ans_1a <- pnorm(z2_1a)
  return(ans_1a)
}

n1_1a <- seq(5,100,by=5)
power_1a <- par.n_1a(n1_1a,0.05,4,10)

par(mfrow=c(1,2))
plot(n1_1a,power_1a,type="l")
plot(power_1a,n1_1a,type="l")

#Question 1 part b
par.n_1b <- function(n,alpha,beta,sigma){
  z1_1b <-qnorm(1-alpha/2)
  z2_1b <- qnorm(1-beta)
  ans_1b <- sqrt(2)*abs((z1_1b+z2_1b)*sigma)/sqrt(n)
  return(ans_1b)
}  
n2_1b <- seq(5,100,by=5)
effective_delta_1b <- par.n_1b(n2_1b,0.05,0.2,10)
par(mfrow=c(1,2))
plot(n2_1b,effective_delta_1b,type="l")
plot(effective_delta_1b,n2_1b,type="l")

#Question 2 part a
par.n_2a <- function (n,alpha,p1,p2){
  z1_2a <-qnorm(1-alpha/2)
  p_bar <- (p1+p2)/2
  #Calculate the power
  z2_2a <-(sqrt(n)*abs(p2-p1)-z1_2a*sqrt(2*p_bar*(1-p_bar)))/sqrt(p1*(1-p1)+p2*(1-p2))
  ans_2a <- pnorm(z2_2a)
  return(ans_2a)
}

n1_2a_100 <- seq(5,100,by=5)
power_2a_100 <- par.n_2a(n1_2a_100,0.05,0.8,0.9)

par(mfrow=c(1,2))
plot(n1_2a_100,power_2a_100,type="l")

n1_2a_500 <- seq(5,500,by=5)
power_2a_500 <- par.n_2a(n1_2a_500,0.05,0.8,0.9)

plot(n1_2a_500,power_2a_500,type="l")



par.n_2a_1 <- function (power,alpha,p1,p2){
  z1_2a <-qnorm(1-alpha/2)
  p_bar <- (p1+p2)/2
  #Calculate the power
  n_1=(1/(p2-p1)^2)*(qnorm(1-alpha/2)*sqrt(2*p_bar*(1-p_bar))+qnorm(power)*sqrt(p1*(1-p1)+p2*(1-p2)))^2
  return(n_1)
}
power=seq(0,1,by=0.1)
n_2a <- par.n_2a_1(power,0.05,0.8,0.9)
par(mfrow=c(1,2))
plot(n_2a,power,type="l")
plot(power,n_2a,type="l")


#Question 2 part b
par.n_2b <- function(delta,alpha,beta,p1){
  z1_2b  <- qnorm(1-alpha/2)
  z2_2b  <- qnorm(1-beta)
  p2     <- p1+delta
  p_bar  <- (p1+p2)/2
  ans_n  <- (z1_2b*sqrt(2*p_bar*(1-p_bar))+z2_2b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/((p2-p1)^2)
  return(ans_n)
}  
delta <- seq(-0.8,0.2,by=0.05)
n_2b <- par.n_2b(delta,0.05,0.2,0.8)
par(mfrow=c(1,2))
plot(delta,n_2b,type="l")
plot(n_2b,delta,type="l")

#Question 2 part c
par.n_2c <- function (n,alpha,p1,p2){
  z1_2c <-qnorm(1-alpha/2)
  #Calculate the power
  delta=abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
  z2_2c <-sqrt(n)*abs(delta)-z1_2c
  ans_2c <- pnorm(z2_2c)
  return(ans_2c)
}

#Since sample sizes in increments of 5 to 10
n1_2c <- seq(5,100,by=5)
power_2c <- par.n_2c(n1_2c,0.05,0.8,0.9)

par(mfrow=c(1,2))
plot(n1_2c,power_2c,type="l")
plot(power_2c,n1_2c,type="l")

#Question 2 part d
par.n_2d <- function(n,alpha,beta,p1,p2){
  z1_2d <-qnorm(1-alpha/2)
  z2_2d <- qnorm(1-beta)
  ans_2d <- abs(z1_2d+z2_2d)/abs(sqrt(n))
  return(ans_2d)
}  
n2_2d <- seq(5,100,by=5)
effective_delta_2d <- par.n_2d(n2_2d,0.05,0.2,0.8,0.9)
par(mfrow=c(1,2))
plot(n2_2d,effective_delta_2d,type="l")
plot(effective_delta_2d,n2_2d,type="l")