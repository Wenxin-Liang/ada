# Ada Homework 9 
library("Sleuth3")
data=ex2220
year1=data$YearsAfter
year=data$YearsAfter
risk=data$AtRisk
rads=data$Exposure
rads2=rads^2
death=data$Deaths

#a
fit1=glm(death~offset(log(risk))+year+rads+rads2,family=poisson)
fit1_1=glm(death~offset(log(risk))+factor(year)+rads,family=poisson)
summary(fit1_1)
1-pchisq(deviance(fit1_1)-deviance(fit1),df.residual(fit1_1)-df.residual(fit1))
fit1
summary(fit1)
anova(fit1,test='Chi')

qchisq(0.95,33)
var(fit1$fitted)
mean(fit1$fitted)
var(death)
mean(death)
var(fit1$fitted)\mean(fit1$fitted)

#b
time=rep(c(3.5,9.5,13.5,17.5,21.5,25.5,29.5),6)
fit2=glm(death~offset(log(risk))+log(time)+rads+rads2,family=poisson)
fit2
anova(fit2,test='Chi')
qchisq(0.95,38)

pchisq(73.68,38,lower.tail=F)
pchisq(46.69,33,lower.tail=F)


#c
interaction=log(time)*rads
fit3=glm(death~offset(log(risk))+log(time)+rads+rads2+interaction,family=poisson)
fit3
anova(fit3,test='Chisq')
summary(fit3)

#d
summary(fit1)
pchisq(46.69,33,lower.tail=F)
pchisq(73.68,38,lower.tail=F)
pchisq(72.43,37,lower.tail=F)
pchisq(50.106,34,lower.tail=F)
exp(0.0018316-1.96*0.0004392)
exp(0.0018316+1.96*0.0004392)
coef(fit1_1)
exp(confint(fit1_1))

time1<-rep(0,nrow(data))
time1[which(Years=="8-11")]=1
time2<-rep(0,nrow(data))
time2[which(Years=="12-15")]=1
time3<-rep(0,nrow(data))
time3[which(Years=="16-19")]=1
time4<-rep(0,nrow(data))
time4[which(Years=="20-23")]=1
time5<-rep(0,nrow(data))
time5[which(Years=="24-27")]=1
time6<-rep(0,nrow(data))
time6[which(Years=="28-31")]=1
fit4=glm(Deaths~offset(log(Risk))+time2+time3+time4+time5+time6+rads,family = poisson,data = data)
summary(fit4)
coef(fit4)
confint(fit4)
exp(confint(fit4))


