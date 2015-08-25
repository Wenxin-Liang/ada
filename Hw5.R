#ADA Homework 5
#Question 1 Part a
library(MASS)
data <- Pima.te[,c(1,2,3,4,5,7)]
mylm <- lm(glu ~ npreg+bp+skin+bmi+age,data = data )
summary(mylm)

mylm1 <- lm(data$glu ~data$npreg+data$bp+data$skin+data$bmi+data$age)
summary(mylm1)

#Question 1 Part b
e1 <- residuals(mylm) # OLS
r <- rstandard(mylm) # Studentized
t <- rstudent(mylm)  # Deleted Studentized

# Assessing Linearity 
# plot of studentized residuals vs fitted values
plot(mylm$fit, e1, xlab = "fitted values", ylab = "studentized residuals")

# Assessing interaction term
library(phia)
testInteractions(mylm)
mylm2 <- lm(glu ~ npreg+bp+skin+bmi+age+npreg*bp+npreg*skin+npreg*bmi+npreg*age+bp*skin+bp*bmi+bp*age+skin*bmi+skin*age+bmi*age,data = data )
summary(mylm2)
mylm3 <- lm(glu ~ npreg+bp+skin+bmi+age+skin*age,data = data )
summary(mylm3)
mylm4_1 <- lm(glu ~.+skin*age+bp*age,data=data)
summary(mylm4_1)
mylm4_2 <- lm(glu ~.+skin*age+bp*age+bmi*age,data=data)
summary(mylm4_2)


# Assessing Normality
# normal probability plot of studentized residuals
qqnorm(y=residuals(mylm),ylab="Redisuals",main="QQplot of the residuals") 
qqline(residuals(mylm),lwd=2)
# See whether the residucal is from normal distribution
# quatity-quatity plot

# Shapiro-Wilk test for studentized residuals
shapiro.test(r)
#Ho:Normal data
#Ha:Non-normal data

# Assessing homoscedasticity
# Breush-Pagan Test for homoscedasticity
# load lmtest package to gain access to bptest function
library(lmtest)
bptest(mylm, studentize = FALSE)
#H0:Variances of errors are constant
#Ha:Variances of errors are not constant(if we change the predictor
#variance, the variances of errors are also changes)
 e<- mylm$residuals

# Assessing Correlation of Error
plot(seq(1,332),e,type="l",xlab="observation sequence",ylab="residuals",main="Series of Residuals")
library("lmtest")
library("zoo")
dwtest(glu~npreg+bp+skin+bmi+age,data=data)

# Assessing Outlier
# Identify outlying responses

# Index plot of studentized residuals vs observation number
plot(rstandard(mylm), ylab = "studentized residuals", xlab = "observation")
# No unusually large studentized residuals

# Determine whether any deleted studentized residuals exceed
# what is expected (at a .95 confidence level) for an F distribution with p
# numerator degrees of freedom and n - p denominator degrees of freedom.
which(abs(rstudent(mylm)) >= qt(1 - .05/(2 * nrow(data)), df = 332-6))
# named integer(0) means that non exceeded the treshhold

# Identify outlying X values

# Scatterplot of thigh circumference vs triceps skinfold thickness
# Label points by observation number.  type = "n" means don't plot the points
plot(body$tri_skin_thick, body$thigh_circ, xlab = "Triceps Skinfold Thickness",
     ylab = "Thigh Circumference", type = "n")
text(body$tri_skin_thick, body$thigh_circ, lab = 1:20)
# 3 and 15 may be a bit unusual

# Determine whether any leverages are large (i.e., larger than 2p/n)
h <- hatvalues(mylm)
which(h >= 2 * 6/nrow(data))
# 3 and 15 are a bit large

# Identify influential observations

# Identify influential observations using DFFITS
DFFITS <- dffits(mylm)
which(abs(DFFITS) > 2*sqrt(6/nrow(data)))
# For small data set, observation 3 influential

# Index plot of DFFITS
n <- nrow(data)
plot(DFFITS)
text(1:n,dffits(mylm),lab=1:n)

# Identify influential observations using Cook's Distances
D <- cooks.distance(mylm)
which(D >= qf(.5, 6, 332-6)) # none clearly identified as influential (though 


# Index plot of Cook's Distances
plot(D, ylab = "Cook's Distance")

# Identify influential observations using DFBETAS
DFBETAS <- dfbetas(mylm)
max.DFBETAS <- apply(abs(DFBETAS), 1, max)
which(max.DFBETAS > 2/sqrt(332)) # For small data set, observation 3 influential

IF <- influence.measures(mylm)
DFFITS1 <- IF$is.inf[,7]
DFBETAS1 <- IF$is.inf[,1:6]
HAT1 <- IF$is.inf[,10]
COOK1 <- IF$is.inf[,9]
which(DFFITS1 == TRUE)
which(DFBETAS1 == TRUE)
which(HAT1 == TRUE)
which(COOK1 == TRUE)

