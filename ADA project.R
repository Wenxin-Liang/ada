data_white <- read.table("/Users/Wenxin_AN/Documents/Master/ada/project/winequality-white.csv",sep=";",header=TRUE)
head(data_white)
data_white_x <- data_white[,-12]
# Data Source
attach(data_white)
Class_variables<- sapply(data_white,is.numeric)
str(data_white)
# Sample R code for EDA/Preparation
library(Hmisc)
describe(data_white,digit=5)
library(caret)
featurePlot(x=data_white[,-12],y=data_white[,12],between=list(x=1,y=1),type=c("g","p","smooth"))
library(MASS)
# Plot the histogram plots
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)
truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
truehist(residual.sugar, h = 0.5, col="slategray3")
mtext("residual sugar", side=1, outer=F, line=2, cex=0.8)
truehist(chlorides, h = 0.01, col="slategray3")
mtext("chlorides", side=1, outer=F, line=2, cex=0.8)
truehist(free.sulfur.dioxide, h = 0.05, col="slategray3")
mtext("free sulfur dioxide", side=1, outer=F, line=2, cex=0.8)
truehist(total.sulfur.dioxide, h = 0.1, col="slategray3")
mtext("total sulfur dioxide", side=1, outer=F, line=2, cex=0.8)

truehist(density, h = 0.5, col="slategray3")
mtext("density", side=1, outer=F, line=2, cex=0.8)
truehist(pH, h = 0.5, col="slategray3")
mtext("pH", side=1, outer=F, line=2, cex=0.8)
truehist(sulphates, h = 0.05, col="slategray3")
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)
truehist(alcohol, h = 0.1, col="slategray3")
mtext("alcohol", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(chlorides)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("chlorides", side=1, outer=F, line=2, cex=0.8)
barplot((table(residual.sugar)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("residual.sugar", side=1, outer=F, line=2, cex=0.8)
barplot((table(free.sulfur.dioxide)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("free.sulfur.dioxide", side=1, outer=F, line=2, cex=0.8)
barplot((table(total.sulfur.dioxide)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("total.sulfur.dioxide", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(density)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("density", side=1, outer=F, line=2, cex=0.8)
barplot((table(pH)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("pH", side=1, outer=F, line=2, cex=0.8)
barplot((table(sulphates)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)
barplot((table(alcohol)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("alcohol", side=1, outer=F, line=2, cex=0.8)


# Plot the boxplot for outlier checking
par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
attach(data_white)
boxplot(fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)
boxplot(residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)

par(mfrow=c(1,6), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(free.sulfur.dioxide, col="slategray2", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(total.sulfur.dioxide, col="slategray2", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(density, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(pH, col="slategray2", pch=19)
mtext("pH Value", cex=0.8, side=1, line=2)
boxplot(sulphates, col="slategray2", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)
boxplot(alcohol, col="slategray2", pch=19)
mtext("alcohol", cex=0.8, side=1, line=2)

# Correlation checking
Correlation<- cor(data_white[,-12])
Correlation
cor(data_white[,-12], method="spearman")
par(mfrow=c(1,1))
library(corrplot)
corrplot(Correlation)

# Summary Statistics of White Wine
summary(data_white)
library("psych", lib.loc="C:/Users/sbasu/Documents/R/R-3.1.0/library")
describe(data_white_x)
options(digits=1)
cor(data_white_x)
cor(data_white_x, method="spearman")
pairs(data_white_x, gap=0, pch=19, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

# Sample R code for Preparing Data
limout <- rep(0,11)
for (i in 1:11){
  t1 <- quantile(data_white[,i], 0.75)
  t2 <- IQR(data_white[,i], 0.75)
  limout[i] <- t1 + 1.5*t2
}
WhiteWineIndex <- matrix(0, 4898, 11)
for (i in 1:4898)
  for (j in 1:11){
    if (data_white[i,j] > limout[j]) WhiteWineIndex[i,j] <- 1
  }
WWInd <- apply(WhiteWineIndex, 1, sum)
WhiteWineTemp <- cbind(WWInd, data_white)
Indexes <- rep(0, 208)
j <- 1
for (i in 1:4898){
  if (WWInd[i] > 0) {Indexes[j]<- i
                     j <- j + 1}
  else j <- j
}
WhiteWineLib <-data_white[-Indexes,]   # Inside of Q3+1.5IQR
set.seed(1)
indexes = sample(1:nrow(WhiteWineLib), size=0.75*nrow(WhiteWineLib))
WWTrain75 <- WhiteWineLib[indexes,]
WWTest25 <- WhiteWineLib[-indexes,]
X.train1 <- WWTrain75[,-12]
X.test1  <- WWTest25[,-12]
Y.train1 <- WWTrain75[,12]
Y.test1  <- WWTest25[,12]

# Sample R code for Multiple Linear Regression
options(digits=7)
library(car)
Qfit1 <- lm(quality ~ ., data=WWTrain75)
summary(Qfit1)
#vif(Qfit1)
#postResample(round(Pred.LM),Y.test1)
Pred.LM=predict(Qfit1,X.test1)
postResample((Pred.LM),Y.test1)
(err <- sum(round(Pred.LM)!=Y.test1)/length(Y.test1))

par(mfrow=c(1,1))
truehist(residuals(Qfit1), h = 0.25, col="slategray3")
#qqPlot(residuals(Qfit1), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals for Qfit1", outer=T, side=1, line = 2)

# Polynomial Regression
Qfit6 <- lm(quality ~ poly(alcohol,2) + poly(volatile.acidity,2) + residual.sugar + poly(free.sulfur.dioxide,2) + chlorides + sulphates + poly(pH,2), data=WWTrain75)
summary(Qfit6)
Pred.LM_6=predict(Qfit6,X.test1)
postResample((Pred.LM_6),Y.test1)
(err <- sum(round(Pred.LM_6)!=Y.test1)/length(Y.test1))
residualPlots(Qfit6, pch=19, col="blue", cex=0.6)

# Logistic Regression
#Qfit7 <- glm( quality ~ fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+alcohol,data=WWTrain75,family="binomial")
#library(nnet)
#Qfit7 <- multinom(quality ~ fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+alcohol,data=WWTrain75)
#summary(Qfit7)
#Pred.LM_7=predict(Qfit7,X.test1)
#postResample((Pred.LM_7),Y.test1)
#(err <- sum(round(Pred.LM_7)!=Y.test1)/length(Y.test1))

data <-WWTrain75
data$quality <- as.factor(WWTrain75$quality)
is.factor(WWTrain75$quality)




indexes = sample(1:nrow(WhiteWineLib), size=0.5*nrow(WhiteWineLib))
WWTrain50 <- WhiteWineLib[indexes,]
WWTest50 <- WhiteWineLib[-indexes,]
X.train2 <- WWTrain50[,-12]
X.test2  <- WWTest50[,-12]
Y.train2 <- WWTrain50[,12]
Y.test2  <- WWTest50[,12]

# Data Spliting
library(caret)
Y=data_white[,12]
Y=data.frame(Y)
PRA1=preProcess(data_white[,-12],method=c("center", "scale"))
preproc.x=predict(PRA1,data_white[,-12])
train1<-createDataPartition(data_white[,12],p=3/4,list=F)
X.train<-preproc.x[train1,]
X.test<-preproc.x[-train1,]
Y.train<-Y[train1,]
Y.test<-Y[-train1,]

#Linear Regression
LMFit=lm(Y.train~.,data=X.train)
summary(LMFit)
Pred.LM=predict(LMFit,X.test)
postResample(round(Pred.LM),Y.test)
postResample((Pred.LM),Y.test)
(err <- sum(round(Pred.LM)!=Y.test)/length(Y.test))

LMFit1=lm(Y.train1~.,data=X.train1)
summary(LMFit1)
Pred.LM=predict(LMFit1,X.test1)
postResample(round(Pred.LM),Y.test1)

LMFit2=lm(Y.train2~.,data=X.train2)
summary(LMFit2)
Pred.LM=predict(LMFit1,X.test2)
postResample(round(Pred.LM),Y.test2)

# Sample R code for Multiple Regression
options(digits=7)
library(car)
Qfit1 <- lm(quality ~ ., data=WWTrain75)
summary(Qfit1)
vif(Qfit1)
Pred.LM=predict(Qfit1,X.test1)
postResample(round(Pred.LM),Y.test1)
(err <- sum(round(Pred.LM)!=Y.test1)/length(Y.test1))

#Qfit2 <- lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates,data=WWTrain75)
#Pred.LM=predict(Qfit2,X.test1)
#postResample(round(Pred.LM),Y.test1)

Qfit3 <- step(lm(quality ~ 1, WWTrain75), scope=list(lower=~1,  upper = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol), direction="forward")

Qfit4 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + free.sulfur.dioxide + chlorides + sulphates + pH , data=WWTrain75)
summary(Qfit4)
vif(Qfit4)
Pred.LM=predict(Qfit4,X.test1)
postResample(round(Pred.LM),Y.test1)

Qfit4_1 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + free.sulfur.dioxide + chlorides + sulphates + density + pH + fixed.acidity , data=WWTrain75)
summary(Qfit4_1)
vif(Qfit4_1)
Pred.LM=predict(Qfit4_1,X.test1)
postResample(round(Pred.LM),Y.test1)

par(mfrow=c(1,1), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit4), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)

truehist(residuals(Qfit4_1), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4_1), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)

par(mfrow=c(1,1))
truehist(residuals(Qfit1), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit1), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals for Qfit1", outer=T, side=1, line = 2)
boxplot(data_white_x)


par(mfrow=c(1,1))
pred.val <- round(fitted(Qfit4))
plot(pred.val, residuals(Qfit4))
ts.plot(residuals(Qfit4))
residualPlots(Qfit4, pch=19, col="blue", cex=0.6)
influencePlot(Qfit4,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
boxcox(lm(quality~alcohol), data=WWTrain50, lambda=seq(-0.2, 1.0, len=20), plotit=T)
std.del.res<-studres(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)
d.fit <- dffits(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
truehist(d.fit, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)
cook.d <- cooks.distance(Qfit4)
ts.plot(cook.d)
par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(std.del.res, h = 0.55, col="slategray3")
mtext("Studentized Deleted Residuals", side=1, line=2, cex=0.8)
truehist(d.fit, h = 0.05, col="slategray3")
mtext("DFITS", side=1, line=2, cex=0.8)
par(mfrow=c(1,1), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
ts.plot(cook.d, col="dark blue")

IF <- influence.measures(Qfit4)
view(IF)

Qfit6 <- lm(quality ~ poly(alcohol,2) + poly(volatile.acidity,2) + residual.sugar + poly(free.sulfur.dioxide,2) + chlorides + sulphates + poly(pH,2), data=WWTrain75)
summary(Qfit6)
residualPlots(Qfit6, pch=19, col="blue", cex=0.6)

# Sample R code for Tree-based Models and Random Forest

FactQ <- as.factor(quality)
WhiteWineLib <- cbind(WhiteWineLib, FactQ)
temp <- recode(WhiteWineLib$FactQ, "c('3','4','5')='10'; c('6')='20'; else='40'")
Ptemp <- recode(temp, "c('10')='5'; ('20')='6'; else='7'")
WhiteWineLib$FactQ <- Ptemp
prop.table(table(WhiteWineLib$FactQ))
WhiteWineTree <- tree(FactQ ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol+density, data=WhiteWineLib, method="class")
plot(WhiteWineTree)
text(WhiteWineTree, pretty=0, cex=0.6)
misclass.tree(WhiteWineTree, detail=T)
Treefit1 <- predict(WhiteWineTree, WhiteWineLib, type="class")
table(Treefit1, WhiteWineLib$FactQ)

WWrf50_super <- randomForest(FactQ ~ . , data=WWTrain50T[,-12], ntree=150, importance=T, proximity=T)
WWTest50_rf_pred_super <- predict(WWrf50_super, WWTest50, type="class")
table(WWTest50_rf_pred_super, WWTest50$FactQ1)
plot(WWrf50_super, main="")
varImpPlot(WWrf50_super,  main="", cex=0.8)

svmFit=ksvm(x=as.matrix(X.train1),y=Y.train1,kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)


