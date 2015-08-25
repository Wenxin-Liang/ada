#####Problem 2#######
#####(a)#######
###Input a sequence of power, plot the power vs sample size###
power=seq(0,1,by=0.1)
power.function<-function(power){
  dif=4
  sigma=10
  alpha=0.05
  n=vector()
  for (i in 1:length(power)){
    n[i]=(1/dif^2)*2*sigma^2*(qnorm(1-alpha)+qnorm(power[i]))^2
  }
  par(mfrow=c(1,2))
  plot(power,n,type="b",xlab="Power of the Test",ylab="Sample Size")
  plot(n,power,type="b",xlab="Sample Size",ylab="Power of the Test")
  par(mfrow=c(1,1))
}



#####(b)#####
###Input a sequence of effect size, plot the effect size vs sample size###
dif=seq(0,15,by=1)
dif.function<-function(dif){
  sigma=10
  alpha=0.05
  power=0.8
  n=vector()
  for (i in 1:length(dif)){
    n[i]=(1/dif[i]^2)*2*sigma^2*(qnorm(1-alpha)+qnorm(power))^2
  }
  plot(dif,n,type="b",xlab="Effect Size",ylab="Sample Size",ylim=c(0,350))
}

#####Problem 3#####

####(1) For Normal Approximation#####
#####(a)#######
###Input a sequence of power, plot the power vs sample size###
power=seq(0,1,by=0.1)
power.function2<-function(power){
  p1=0.8
  q1=1-p1
  p2=0.9
  q2=1-p2
  p.bar=(p1+p2)/2
  q.bar=1-p.bar
  alpha=0.05
  n=vector()
  for (i in 1:length(power)){
    n[i]=(1/(p2-p1)^2)*(qnorm(1-alpha/2)*sqrt(2*p.bar*q.bar)+qnorm(power[i])*sqrt(p1*q1+p2*q2))^2
    
  }

  par(mfrow=c(1,2))
  plot(power,n,type="b",xlab="Power of the Test",ylab="Sample Size")
  plot(n,power,type="b",xlab="Sample Size",ylab="Power of the Test")
  par(mfrow=c(1,1))

}





#####(b)#####
###Input a sequence of effect size, plot the effect size vs sample size###
dif=seq(0,1,by=0.1)
dif.function2<-function(dif){
  p1=0.8
  q1=1-p1
  p2=0.9
  q2=1-p2
  p.bar=(p1+p2)/2
  q.bar=1-p.bar
  alpha=0.05
  power=0.8
  n=vector()
  for (i in 1:length(dif)){
    
    
    n[i]=(1/dif[i]^2)*(qnorm(1-alpha/2)*sqrt(2*p.bar*q.bar)+qnorm(power)*sqrt(p1*q1+p2*q2))^2
    
  }
  
  plot(dif,n,type="b",xlab="Effect Size",ylab="Sample Size")
}

####(2) For Arcsin Transformation#####
#####(a)#######
###Input a sequence of power, plot the power vs sample size###
power=seq(0,1,by=0.1)
power.function3<-function(power){
  p1=0.8
  q1=1-p1
  p2=0.9
  q2=1-p2
  p.bar=(p1+p2)/2
  q.bar=1-p.bar
  alpha=0.05
  n=vector()
  for (i in 1:length(power)){
    
    
    n[i]=((1/((2*asin(sqrt(p2)))-(2*asin(sqrt(p1)))))^2)*(qnorm(1-alpha/2)+qnorm(power[i]))^2
    
  }
  par(mfrow=c(1,2))
  plot(power,n,type="b",xlab="Power of the Test",ylab="Sample Size")
  plot(n,power,type="b",xlab="Sample Size",ylab="Power of the Test")
  par(mfrow=c(1,1))
  
}

#####(b)#####
###Input a sequence of effect size, plot the effect size vs sample size###
dif=seq(0,1,by=0.1)
dif.function3<-function(dif){
  p1=0.8
  q1=1-p1
  p2=0.9
  q2=1-p2
  p.bar=(p1+p2)/2
  q.bar=1-p.bar
  alpha=0.05
  power=0.8
  n=vector()
  for (i in 1:length(dif)){
    
    n[i]=(1/dif[i]^2)*(qnorm(1-alpha/2)+qnorm(power))^2
    
  }
  
  plot(dif,n,type="b",xlab="Effect Size",ylab="Sample Size")
}