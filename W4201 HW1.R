boxplot(ChickWeight$weight[ChickWeight$Diet==1]~
     ChickWeight$Time[ChickWeight$Diet==1],
     xlab="Time", ylab="Weight")

jackknife(ChickWeight$weight[ChickWeight$Diet==2&ChickWeight$Time==0],sd)

bt_results <- bootstrap(ChickWeight$weight[ChickWeight$Diet==2&ChickWeight$Time==0],
                       500,sd)
bt_sd <- mean(bt_results$thetastar)
bias <- bt_sd - sd(ChickWeight$weight[ChickWeight$Diet==2&ChickWeight$Time==0])

var1 <- bootstrap(ChickWeight$weight[ChickWeight$Diet==1&ChickWeight$Time==0],
                                500,var)$thetastar
var2 <- bootstrap(ChickWeight$weight[ChickWeight$Diet==2&ChickWeight$Time==0],
                  500,var)$thetastar
boxplot(var1,var2)

SA <- ChickWeight$weight[ChickWeight$Diet==1&ChickWeight$Time==0]
SB <- ChickWeight$weight[ChickWeight$Diet==2&ChickWeight$Time==0]
na <- length(SA)
nb <- length(SB)
S <- c(SA,SB)
theta <- function(x1,x2) theta <- sd(x1)-sd(x2)
theta_star <- c()
K <- 500
for(i in 1:K){
  S_star <- S[sample.int(na+nb)]
  theta_star[i] <- theta(S_star[1:na],S_star[(na+1):(na+nb)])
}

p_value <- sum(abs(theta_star)>abs(theta0))/K
hist(theta_star,main="", xlab="Simulated theta")
abline(v=theta0, lty = 3)
abline(v=-theta0, lty = 3)