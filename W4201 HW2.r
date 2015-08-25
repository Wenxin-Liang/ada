t.test(crabs$CL[crabs$sp=="B"],crabs$CL[crabs$sp=="O"])

wilcox.test(crabs$CL[crabs$sp=="B"],crabs$CL[crabs$sp=="O"])

SA <- crabs$CL[crabs$sp=="B"]
SB <- crabs$CL[crabs$sp=="O"]
na <- length(SA)
nb <- length(SB)
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

boxplot(crabs$CL~crabs$sex, xlab="Sex", ylab="Carapace Length (mm)")

cor.test(crabs$CW, crabs$BD, method = c("pearson"))

cor.test(crabs$CW, crabs$BD, method = c("spearman"))