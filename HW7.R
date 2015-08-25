# Homework 7
# Question 1
# Part a
data   <- c(0.11,0.11,0.11,0.19,0.21,0.22,0.24,0.25,0.31,0.18,0.27,
          0.36,0.37,0.39,0.47,0.37,0.57,0.29,0.30,0.40,0.45,0.47,
          0.52,0.57,1.10)
team   <- rep(c("Controls","Gallstone","Ulcer"),c(9,8,8))
team   <- factor(team)
data_new <- data.frame(team,data)
# For F test
anova1 <- aov(data ~ team)
summary(anova1)
# For simultaneous confidence interval procedures
# Bonferroni Procedure
pairwise.t.test(data_new$data,data_new$team,p.adj="bonferroni")
# Scheffe Procedure
library(agricolae)
(scheffe.test(anova1,"team",alpha=0.05,group=TRUE,main = NULL,console =FALSE))
# Fisher's Least Significant Difference
df <- df.residual(anova1)
MSerror <- deviance(anova1)/df
(LSD.test(data_new$data,data_new$team,df,MSerror))
# Tukey HSD Procedure
TukeyHSD(anova1)
# Dunnett's Procedure
#library(multcomp)
#Dunnett_Test <- glht(anova1)
#summary(Dunnett_Test)

# Part b
bartlett.test(data ~ team)
shapiro.test(resid(anova1))
Controls=c(0.11,0.11,0.11,0.19,0.21,0.22,0.24,0.25,0.31)
Gallstone=c(0.18,0.27,0.36,0.37,0.39,0.47,0.37,0.57)
Ulcer=c(0.29,0.30,0.40,0.45,0.47,0.52,0.57,1.10)
par(mfrow=c(1,3))
#plot(density(Controls),main="CCK level of Controls Group") 
#plot(density(Gallstone),main="CCK level of Gallstone Group") 
#plot(density(Ulcer),main="CCK level of Ulcer Group") 
#plot(density(team),main="CCK level of Groups")
#shapiro.test(Controls)$p.value
#shapiro.test(Gallstone)$p.value
#shapiro.test(Ulcer)$p.value
par(mfrow=c(1,2))
qqnorm(resid(anova1))
hist(resid(anova1))

# Part c
kruskal.test(data,team)

# Question 2
# Part a
# sub question 1
library("Sleuth3")
summary(aov(ex1319$IQ~ex1319$Adoptive*ex1319$Biological))
mylm <- lm(ex1319$IQ~ex1319$Adoptive+ex1319$Biological)
summary(mylm)
anova2=aov(ex1319$IQ~ex1319$Adoptive+ex1319$Biological)
anova3=aov(ex1319$IQ~ex1319$Adoptive*ex1319$Biological)
TukeyHSD(anova3)
anova4=aov(ex1319$IQ~ex1319$Biological*ex1319$Adoptive)
TukeyHSD(anova4)
SSado<-1478/(1478+2291+2+5941)
SSado
SSbio<-2291/(1478+2291+2+5941)
SSbio
fit1<-lm(IQ~ado+bio+ado*bio)
summary(fit1)
mylm <- lm(ex1319$IQ~ex1319$Adoptive+ex1319$Biological+ex1319$Adoptive*ex1319$Biological)
summary(mylm)
# Part b
# assumption
par(mfrow=c(1,1))
plot(mylm)
shapiro.test(resid(anova2))
par(mfrow=c(1,2))
hist(resid(anova2))
qqnorm(resid(anova2))
IQ<-c(136,99,121,133,125,131,103,115,116,117,94,103,99,125,111,93,
      101,94,125,91,98,99,91,124,100,116,113,119,92,91,98,83,99,68,76,115,86,116)
ado<-c(rep("High",20),rep("Low",18))
bio<-c(rep("High",10),rep("Low",10),rep("High",8),rep("Low",10))
ado<-factor(ado)
bio<-factor(bio)
bartlett.test(split(IQ,list(ado,bio)))

bartlett.test(ex1319$IQ~ex1319$Adoptive+ex1319$Biological)


