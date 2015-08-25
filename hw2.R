# ================Problem 2================
# ================Q1================
# ========Parametric========
library (MASS)
data(chickwts)
View(chickwts)
data.sub <- chickwts[49:71,]
chick.m <- data.sub[1:11,]
chick.c <- data.sub[12:23,] 

# ====normality====
hist(chick.m$weight)
d.m <- density(chick.m$weight)
plot(d.m)
boxplot(chick.m$weight)
qqnorm(chick.m$weight,main="chicks fed meatmeal")
qqline(chick.m$weight)

hist(chick.c$weight)
d.c <- density(chick.c$weight)
plot(d.c)
boxplot(chick.c$weight)
qqnorm(chick.c$weight,main="chicks fed meatmeal")
qqline(chick.c$weight)

# ====F-test of equality of two variances====
var.test(chick.m$weight,chick.c$weight)

# ====two-sample t-test====
two.mean <- t.test(chick.m$weight,chick.c$weight,var.equal=T)
two.mean
two.mean$statistic

# ========Non-parametric========
# ====wilcoxon-rank-sum-test====
wilcox.test(chick.m$weight,chick.c$weight)


# ========Re-sampling========
diff<-rep(0,1000)

for (i in 1:1000){
  sam<-sample(c(rep(1,11),rep(2,12)),23) 
  diff[i]<-mean(data.sub[sam==1,1])-mean(data.sub[sam==2,1])
}

sample.diff<-mean(chick.c$weight)-mean(chick.m$weight)
quantile(diff,c(0.025,0.975))
sample.diff

sum(abs(diff)>abs(sample.diff))
sum(abs(diff)>abs(sample.diff))/length(diff)


# ================Q2================
# ====mean confidence interval====
library(bootstrap)
data.subs <- chickwts[23:48,]
chick.soy <- data.subs[1:14,]
chick.sun <- data.subs[15:26,]

# ====a bootstrap for median====
median.x<-bootstrap(chick.soy[,1],1000,median)
median.x
median.y<-bootstrap(chick.sun[,1],100,median)
median.y
quantile(median.x$thetastar-median.y$thetastar,c(0.025,0.975))

# ====b bootstrap for variances====
var.x <- bootstrap(chick.soy[,1],1000,var)
var.x
var.y <- bootstrap(chick.sun[,1],1000,var)
var.y
quantile(var.x$thetastar/var.y$thetastar,c(0.025,0.975))


# ================Q3================
m.low <- chick.m[which(chick.m$weight<258),]
c.low <- chick.c[which(chick.c$weight<258),]
length(m.low$weight)
length(c.low$weight)

x <- c(4,2)
n <- c(11,12)
prop.test(x,n)
