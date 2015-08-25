reg <- lm(medv~crim+zn+indus+nox+rm+age+tax+ptratio,data=Boston)

e <- reg$residuals

lmi <- lm.influence(reg)
h <- lmi$hat
si <-lmi$sigma 
student.resid <-  e/(si*(1-h)^0.5) 
critical.value <- qt(1-0.05/2/506,506-8-1)
abline(h=critical.value)
abline(h=-critical.value)
which(abs(student.resid)>critical.value)
