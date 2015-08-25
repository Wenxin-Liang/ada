#Homework 8
# Question 1
Data_Mental <- read.table("/Users/Wenxin_AN/Documents/Master/ada/MENTAL HEALTH.csv",sep=";",header=TRUE)
Education <- 1*(!Data_Mental$Education.Level =="No College Degree")
Mental <- 1*(!Data_Mental$Mental.Health == "Normal")
table(Mental,Education)

# Part a
fit1_1 <- glm(Mental ~ Education,family = "binomial")
summary(fit1_1)
exp(0.02445)

# Part b
Gender <- 1*(Data_Mental$Gender == "Male")
table(Mental,Education,Gender)
fit2 <- glm(Mental ~ Education+Gender, family ="binomial")
summary(fit2)
exp(0.030933)
fit2_1 <- glm(Mental ~ Education+Gender+Education*Gender, family ="binomial")
summary(fit2_1)
exp(-0.01802)
# Part c
woolf <- function(x) { 
  x <- x + 1 / 2 
  k <- dim(x)[3] 
  or <- apply(x, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1])) 
  w <-  apply(x, 3, function(x) 1 / sum(1 / x)) 
  1 - pchisq(sum(w * (log(or) - weighted.mean(log(or), w)) ^ 2), k - 1) 
}
woolf(table(Mental,Education,Gender))

# Question 2
# "No College Degree" as the reference group
Edu.ud <- 1*(!Data_Mental$Education.Level =="Undergrad Degree")
Edu.pd <- 1*(!Data_Mental$Education.Level =="Post-grad Degree")

# Part a
fit3 <- glm(Mental ~ Edu.ud+Edu.pd, family = "binomial")
summary(fit3)
exp(-0.046324)
exp(0.005831)

# Part b
fit4 <- glm(Mental ~ Edu.ud+Edu.pd+Gender, family = "binomial")
summary(fit4)
exp(-0.049738)
exp(-0.004591)

# Part c


# Question 3
library(glmnet)
X <- model.matrix(Mental~Education+Gender)
y <- Mental
fit <- glmnet(X,y)
cvfit <- cv.glmnet(X,y)
plot(cvfit)
cv_out <- cv.glmnet(X,y,alpha=1,family="binomial")
bestlammin <- cv_out$lambda.min
result <- glmnet(X,y,alpha=1,family="binomial")
lasso.coef <- predict(result,type="coefficients",s=bestlammin)
lasso.coef

