#Homework6
#Question 1
library(MASS)
birthwt$race[birthwt$race!=1]<- 0
#data <- birthwt[,2:9]

#Lasso
library(glmnet)

X <- model.matrix(bwt~.,data=birthwt[,-1])
y <- birthwt$bwt

fit <- glmnet(X,y)
cvfit <- cv.glmnet(X,y)
plot(cvfit)

cv_out <- cv.glmnet(x,y,alpha=1)
bestlammin <- cv_out$lambda.min

result <- glmnet(X,y,alpha=1)
lasso.coef <- predict(result,type="coefficients",s=bestlammin)
lasso.coef

# Stepwise
fit <- lm(bwt~.,data = birthwt[,-1])
step <- stepAIC(fit,direction = "both")
step$anova #show the result we obtain
step$coefficients

#Question 2
mylm <- lm(stack.loss~.,data=stackloss)

# Subquestion 1
# Variance inflation factor
library(car) #needed for access to vif function
vif(mylm) 

# Subquestion 2
# Part a
stackloss_new <- stackloss
stackloss_new$stack.loss[20] <- 1500
stackloss_new$Water.Temp[13] <- 170
stackloss_new$Acid.Conc.[13] <- 10
mylm1 <- lm(stack.loss~.,data=stackloss_new)
mylm1 <- lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss_new)
summary(mylm1)

# Part b
nrow(stackloss_new)

IF <- influence.measures(mylm1)
DFFITS1 <- IF$is.inf[,5]
DFBETAS1 <- IF$is.inf[,1:4]
HAT1 <- IF$is.inf[,8]
COOK1 <- IF$is.inf[,7]
which(DFFITS1 == TRUE)
which(DFBETAS1 == TRUE)
which(HAT1 == TRUE)
which(COOK1 == TRUE)

# Identify influential observations

# Identify influential observations using DFFITS
DFFITS <- dffits(mylm1)
which(abs(DFFITS) > 2*sqrt(4/21))

# Index plot of DFFITS
n <- nrow(data)
plot(DFFITS)

# Identify influential observations using Cook's Distances
D <- cooks.distance(mylm1)
which(D >= qf(.5, 4, 21-4)) # none clearly identified as influential (though 

# Index plot of Cook's Distances
plot(D, ylab = "Cook's Distance")

# Identify influential observations using DFBETAS
DFBETAS <- dfbetas(mylm1)
max.DFBETAS <- apply(abs(DFBETAS), 1, max)
which(max.DFBETAS > 2/sqrt(21))

# Index plot of studentized residuals vs observation number
plot(rstandard(mylm1), ylab = "studentized residuals", xlab = "observation")
# No unusually large studentized residuals

# Determine whether any deleted studentized residuals exceed
# what is expected (at a .95 confidence level) for an F distribution with p
# numerator degrees of freedom and n - p denominator degrees of freedom.
which(abs(rstudent(mylm1)) >= qf(1 - .05/(2 * nrow(stackloss)), df1 = 4, df2 = 17))
# named integer(0) means that non exceeded the treshhold



# Part c
# OLS
mylm1_Bef <- lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
mylm1_Bef$coefficients
mylm1_After <- lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss_new)
mylm1_After$coefficients

# Least Median of Square Error
set.seed(1)
mylm2_Bef <- lmsreg(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
mylm2_Bef$coefficients
# mylm2_After1 <- lmsreg(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss_new)
mylm2_After2 <- lmsreg(stack.loss~.,data=stackloss_new)
# mylm2_After1$coefficients
mylm2_After2$coefficients

# Least trimmed squares robust regression
set.seed(1)
mylm3_Bef <- ltsreg(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
mylm3_Bef$coefficients
mylm3_After <- ltsreg(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,stackloss_new)
mylm3_After$coefficients

#M-estimates of regression with Huber weights
set.seed(1)
mylm4_Bef <- rlm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss,scale.est="Huber")
mylm4_Bef$coefficients
mylm4_After <- rlm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,stackloss_new,scale.est="Huber")
mylm4_After$coefficients
