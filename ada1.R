getwd()
setwd("/Users/stinelee/Desktop")
data<-read.csv(file="/Users/stinelee/Desktop/data.csv",header=T,sep=";")
View(data)
EXTRINSIC<-data[1:23,1]
INTRINSIC<-data[24:47,1]
density(EXTRINSIC)
plot(density(EXTRINSIC),main="Density estimate of Extrinsic scores")
plot(density(INTRINSIC),main="Density estimate of Intrinsic scores")
plot(density(data[,1]),main="Density estimate of combined scores")
qqnorm(EXTRINSIC,pch=16,main="Normal QQplot of Extrinsic scores")
qqnorm(INTRINSIC,pch=16,main="Normal QQplot of Intrinsic scores")
qqnorm(data[,1],pch=18,main="Normal QQplot of combined data")

median(INTRINSIC)-median(EXTRINSIC)
stem(cdata)
mean(INTRINSIC)-mean(EXTRINSIC)

plot(data[,1])
par(mfrow=c(1,2))
boxplot(EXTRINSIC,main="boxplot of Extrinsic scores")
boxplot(INTRINSIC,main="boxplot of INTRINSIC scores")

qut<-1.5*(quantile(EXTRINSIC,0.75)-quantile(EXTRINSIC,0.25))
qut+quantile(EXTRINSIC,0.75)
quantile(EXTRINSIC,0.25)-qut
#[1.95,29.15] for Extrinsic

qut2<-1.5*(quantile(INTRINSIC,0.75)-quantile(INTRINSIC,0.25))
qut2+quantile(INTRINSIC,0.75)
quantile(INTRINSIC,0.25)-qut2
#[10.1125,29.6125] for Intrinsic


#3 Recall from the notes that the estimate of the bias is given by the difference between the
#mean of the bootstrap values and the initial estimate
par(mfrow=c(1,1))
cdata=data[,1]
plot(density(cdata),main="PDF of combined data set")

dx <- density(cdata) 
mode=dx$x[which.max(dx$y)] #19.31687

length(cdata)
mean(cdata)

x1=EXTRINSIC
x2=INTRINSIC
#---------------
  MD <- function(x) {
    bbb<-density(x)
    bmode<-bbb$x[which.max(bbb$y)]
    return(bmode)
  }
MD(cdata)#19.31687
#---------------
Boo<-rep(0,10000)
for (j in 1:10000)
{  
Boo[j]=MD(sample(cdata,replace=T))
}

bias=mean(Boo)-MD(cdata)
bias#-0.001267766

var(Boo)#1.141201

var(cdata)#27.4347


#---------jackknife----------------------------

#mode
dx <- density(cdata) 
dx$x[which.max(dx$y)] #19.31687


jack=numeric((length(cdata)-1))#46
(length(cdata))#47
spl<-matrix(1:(46*47),nrow=47)
spl=spl*0
dim(spl)
for (i in 1:length(cdata))
{
  for(j in 1:length(cdata))
  {
    if(j<i)
      jack[j] <- cdata[j] 
    else if(j > i) 
      jack[j-1] <- cdata[j]
  }
  spl[i,]=jack
}
View(spl)

Jaa<-rep(0,47)
for (j in 1:47){
  Jaa[j]=MD(spl[j,])
}
Jaa
biasJ=mean(Jaa)-MD(cdata)
biasJ#-0.00968987
var(Jaa)#0.01961604

#One can easily show that if f(x) = x, so there is no bias, the jackknife estimates are identical to
#the estimates from the original data points


