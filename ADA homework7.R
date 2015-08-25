---
title: "ADA Homework7"
author: "Jiong Chen"
date: "October 22, 2014"
output: pdf_document
---
Problem 1
a)
```{r}
######Problem 1######
#####Question 1#####
####input the data######
cck<-c(rep(0.11,3),0.19,0.21,0.22,0.24,0.25,0.31,0.18,0.27,0.36,0.37,0.39,0.47,0.37,0.57,0.29,0.3,0.4,0.45,0.47,0.52,0.57,1.1)
group<-c(rep("controls",9),rep("gallstone",8),rep("ulcer",8))
group<-factor(group)
####fit a regression line#####
fit<-lm(cck~group)
####anova####
anova1<-aov(fit)
summary(anova1)
```
We can see from the result shown above that the p-value for the oneway anova is small,
we can reject the null hypothesis, which is the means in each group are equal. Therefore, there is a significant difference among the group means.
```{r}
####simultaneous confidence interval, bonf####
pairwise.t.test(cck,group,p.adjust.method="bonf")
```
Using simultaneous confidence interval, we can see that the means in control group and
ulcer group are different. Therefore, there is a significant difference among the group means.

b)
```{r}
####Question 2###
####check normality###
hist(resid(anova1))
qqnorm(resid(anova1))
shapiro.test(resid(anova1))
####check whether unequal var using levene's test####
library(lawstat)
levene.test(cck,group)
```
We can see from the results that the residuals are not normally distributed.
For the variance in each group, we use levene test, the p-value is bigger than 0.05, so we accept the null hypothesis, which is the variance is equal. Therefore, the results show that there does not exist unequal variance.

c)
```{r}
####Question 3####
kruskal.test(cck,group)
```
Using non-parametric method which is kruskal test, we have the p-value very small, which indicate that the null hypothesis is false. Therefore, the means are different in each group.

Problem 2 
a)
```{r}
####Problem 2####
####Question 1####
###input the value####
IQ<-c(136,99,121,133,125,131,103,115,116,117,94,103,99,125,111,93,
      101,94,125,91,98,99,91,124,100,116,113,119,92,91,98,83,99,68,76,115,86,116)
ado<-c(rep("High",20),rep("Low",18))
bio<-c(rep("High",10),rep("Low",10),rep("High",8),rep("Low",10))
ado<-factor(ado)
bio<-factor(bio)
###perform 2-way anova####
anova2<-(aov(IQ~ado*bio))
summary(anova2)
SSado<-1478/(1478+2291+2+5941)
SSado
SSbio<-2291/(1478+2291+2+5941)
SSbio
fit1<-lm(IQ~ado+bio+ado*bio)
summary(fit1)

```
We can see from the anova table that the interaction term does not exist, therefore, the biological parents have no affect on adoptive parents.
We can also see from the variance that the sum of square of ado is smaller than sum of square of bio, SSado explains almost 15.22% while SSbio explains 23.59%. In addition, the absolute value of regression coefficients for bio is larger than ado. Therefore, the effects of biological parents are bigger.

b)
```{r}
#####Question 2#####
####check normality####
hist(resid(anova2))
qqnorm(resid(anova2))
shapiro.test(resid(anova2))

###check whether equal var####
bartlett.test(split(IQ,list(ado,bio)))

```
We can see from the normal test that the residuals are normally distributed, so we can use bartlett test to test whether the variance are equal. The p-value for bartlett test is 0.8734, so we accept the null hypothesis, the variance are equal. 


