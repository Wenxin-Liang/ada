#ADA Homework 11
library(survival)
data = lung

# Question 1
data3 <- data
data3$age[which(data3$age<65)]  <- 1
data3$age[which(data3$age>65)]  <- 0
data3$age[which(data3$age==65)] <- 0
data3$age[which(data3$age == 0)] <- "Old"
data3$age[which(data3$age == "1")] <- "Young"
fitcox_old_young <- coxph(Surv(data3$time,data3$status)~+factor(age,levels=c('Young','Old')),data=data3)
fitcox_young_old <- coxph(Surv(data3$time,data3$status)~+factor(age,levels=c('Old','Young')),data=data3)
young_index <- which(data$age <65)
young_index
young_time <- data$time[young_index]
mean(young_time)
old_index2 <- which (data$age >= 65)
old_index2
old_time <- data$time[old_index2]
mean(old_time)
# young >old
# choose smaller one
# old/young
# 0.7419

# Question 2
prop <- cox.zph(fitcox_old_young)
plot(prop)
score <- resid(fitcox_old_young,type="score")
score_frame <- data.frame(score)
plot(data$age,score_frame[,1],ylab="Score Residuals",xlab ="AGE")

# Question 3
data3$sex[which(data3$sex == 1)] <- "Male"
data3$sex[which(data3$sex == "2")] <- "Female"
fitcox_male_female <- coxph(Surv(data3$time,data3$status)~+factor(sex,levels=c('Female','Male')),data=data3)
fitcox_female_male <- coxph(Surv(data3$time,data3$status)~+factor(sex,levels=c('Male','Female')),data=data3)

fitcox_male_female1 <- coxph(Surv(data$time,data$status)~sex,data=data)
summary(fitcox_male_female1)

male_index <- which(data$sex== 1)
male_time <- data$time[male_index]
mean(male_time)
female_index <-which(data$sex == 2)
female_time <- data$time[female_index]
mean(female_time)