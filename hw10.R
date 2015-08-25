#ADA Homework 10
library(survival)
data = lung
# Part I
# Kaplan-Meier
attach(data)
fit <- survfit(formula=Surv(data$time,data$status)~sex,type="kaplan-meier",data)
plot(fit,lty=2:4,main="Kaplan-Meier",xlab="Survival time in days",ylab="The survival probability",col=c("red","blue"))
legend("topright",c("Male","Female"),lty=2:4,col=c("red","blue"))

# Fleming-Harrington
fit_1 <- survfit(formula=Surv(data$time,data$status)~sex,type="fleming-harrington",data)
fit_1
plot(fit_1,lty=2:4,main="Fleming-Harrington",xlab="Survival time in days",ylab="The survival probability",col=c("red","blue"))
legend("topright",c("Male","Female"),lty=2:4,col=c("red","blue"))

# Part II
# Kaplan-Meier
plot(fit,lty=2:4,main="Kaplan-Meier",xlab="Survival time in days",ylab="The survival probability",col=c("red","blue"))
legend("topright",c("Male","Female"),lty=2:4,col=c("red","blue"))
abline(h=0.5,lwd=2)
#calculate male 
kaplan_male=data.frame(time=summary(fit)$time[summary(fit)$strata=="sex=1"],surv=summary(fit)$surv[summary(fit)$strata=="sex=1"])
kaplan_male
kaplan_male_median=kaplan_male[kaplan_male[,2]==max(kaplan_male[kaplan_male[,2]<0.5,2]),]
kaplan_male_median
lines(x=rep(kaplan_male_median[1],100),y=seq(-0.5,as.numeric(kaplan_male_median[2]),length=100),col="blue",lwd=3,lty=2) 
text(x=kaplan_male_median[1]+20,y=0,labels=kaplan_male_median[1],cex=0.8,col="blue")
#calculate female
kaplan_female=data.frame(time=summary(fit)$time[summary(fit)$strata=="sex=2"],surv=summary(fit)$surv[summary(fit)$strata=="sex=2"])
kaplan_female
kaplan_female_median=kaplan_female[kaplan_female[,2]==max(kaplan_female[kaplan_female[,2]<0.5,2]),]
kaplan_female_median
lines(x=rep(kaplan_female_median[1],100),y=seq(-0.5,as.numeric(kaplan_female_median[2]),length=100),col="red",lwd=1,lty=2) 
text(x=kaplan_female_median[1]+20,y=0,labels=kaplan_female_median[1],cex=0.8,col="red")
fit

# Fleming-Harrington
plot(fit_1,lty=2:4,main="Fleming-Harrington",xlab="Survival time in days",ylab="The survival probability",col=c("red","blue"))
legend("topright",c("Male","Female"),lty=2:4,col=c("red","blue"))
abline(h=0.5,lwd=2)
#calculate male 
fleming_male=data.frame(time=summary(fit_1)$time[summary(fit_1)$strata=="sex=1"],surv=summary(fit_1)$surv[summary(fit_1)$strata=="sex=1"])
fleming_male
fleming_male_median=fleming_male[fleming_male[,2]==max(fleming_male[fleming_male[,2]<0.5,2]),]
fleming_male_median
#abline male
lines(x=rep(fleming_male_median[1],100),y=seq(-0.5,as.numeric(fleming_male_median[2]),length=100),col="blue",lwd=3,lty=2)
text(x=fleming_male_median[1]+20,y=0,labels=fleming_male_median[1],cex=0.8,col="blue")
#calculate female 
fleming_female=data.frame(time=summary(fit_1)$time[summary(fit_1)$strata=="sex=2"],surv=summary(fit_1)$surv[summary(fit_1)$strata=="sex=2"])
fleming_female
fleming_female_median=fleming_female[fleming_female[,2]==max(fleming_female[fleming_female[,2]<0.5,2]),]
fleming_female_median
#abline female
lines(x=rep(fleming_female_median[1],100),y=seq(-0.5,as.numeric(fleming_female_median[2]),length=100),col="red",lwd=1,lty=2) 
text(x=fleming_female_median[1]+20,y=0,labels=fleming_female_median[1],cex=0.8,col="red")
fit_1


