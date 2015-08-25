library(MASS)
survey
table(survey$Smoke)
table(survey$Sex)
Female_Heavy <- which(survey[,1]=='Female'&survey[,9]=="Heavy")
length(Female_Heavy)
Male_Heavy <- which(survey[,1]=='Male'&survey[,9]=="Heavy") 
length(Male_Heavy)
Female_Never <- which(survey[,1]=='Female'&survey[,9]=="Never")
length(Female_Never)
Male_Never <- which(survey[,1]=='Male'&survey[,9]=="Never") 
length(Male_Never)
Female_Occas <- which(survey[,1]=='Female'&survey[,9]=="Occas")
length(Female_Occas)
Male_Occas <- which(survey[,1]=='Male'&survey[,9]=="Occas") 
length(Male_Occas)
Female_Regul <- which(survey[,1]=='Female'&survey[,9]=="Regul")
length(Female_Regul)
Male_Regul <- which(survey[,1]=='Male'&survey[,9]=="Regul") 
length(Male_Regul)
Q1 <- matrix(c(5,99,9,5,6,89,10,12),ncol=4,byrow=TRUE)
colnames(Q1) <- c('Heavy''Never''Occas''Regul')
rownames(Q1) <- c('Female' 'Male')
Q1 <- as.table(Q1)

Q2 <- survey[,c(1,8,9,6)]
head(Q2)
length(Q2[,4])
for (i in 1:length(Q2[,4])){
  if(is.na(Q2[i,4])==TRUE){Q2[i,5]=NA}  
  else if(Q2[i,4]>80){Q2[i,5]="HIGH"}
  else if(Q2[i,4]<65){Q2[i,5]="LOW"}
  else{Q2[i,5]="MEDIUM"}}
names(Q2)[5]="PULSE"
#Use data with n.a.
head(Q2)
table(Q2[,c(3,5,1,2)])
TableQ2=array(as.matrix(table(Q2[,c(3,5,1,2)])),c(4,3,6))
#Use woolf test for homogeneity
woolf <- function(x) { 
  x <- x + 1 / 2 
  k <- dim(x)[3] 
  or <- apply(x, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1])) 
  w <-  apply(x, 3, function(x) 1 / sum(1 / x)) 
  1 - pchisq(sum(w * (log(or) - weighted.mean(log(or), w)) ^ 2), k - 1) 
} 
woolf(Q2) 
#p-value=0.9733905, indicating that there is no significant heterogeneity. Hence the Mantel-Haenszel test can be used.

#use Mantel-Haenszel test for the association test
#H0:No association between PULSE and smoke status
mantelhaen.test(Q2) 
#p-value = 0.845 indicated that there is no association between PULSE level and smoke status at level α=0.05

names(survey)
data3a=survey[survey[,8]=="None",c(6,9)]
head(data3a)
for (i in 1:length(data3a[,1])){ 
  if(is.na(data3a[i,1])==TRUE){data3a[i,3]=NA}
  else if(data3a[i,1]<80){data3a[i,3]="LOW"}
  else {data3a[i,3]="HIGH"}}
names(data3a)[3]="PULSE"

for (i in 1:length(data3a[,2])){ 
  if(is.na(data3a[i,2])==TRUE){data3a[i,4]=NA}
  else if(data3a[i,2]=="Never"){data3a[i,4]="NEVER"}
  else {data3a[i,4]="EVER"}}
names(data3a)[4]="SMOKE"
head(data3a)
tab3a=as.matrix(table(data3a[,c(3,4)]))
tab3a
#Fisher's Exact Test 
#H0:No association between pulse and smoke status
fisher.test(tab3a) 
#p-value=1 indicates there is no association between pulse and smoke status at level α=0.05 

#Pearson's chi-square test 
#H0:No association between pulse and smoke status
chisq.test(tab3a) 
#p-value=1 indicates there is no association between pulse and smoke status at level α=0.05

head(Q2)
Table_2b_1=as.matrix(table(Q2[,c(1,3)]))
Table_2b_1
fisher.test(Table_2b_1) 

Table_2b_2=as.matrix(table(Q2[,c(1,5)]))
Table_2b_2
fisher.test(Table_2b_2) 

Q2_c=Q2[!is.na(Q2[,5]),]
head(Q2_c)
table(Q2_c[,c(3,5,1,2)])
Table_2c=array(as.matrix(table(Q2_c[,c(3,5,1,2)])),c(4,3,6))

names(survey)
Q3=survey[survey[,8]=="None",c(6,9)]
head(Q3)
for (i in 1:length(Q3[,1])){ 
  if(is.na(Q3[i,1])==TRUE){Q3[i,3]=NA}
  else if(Q3[i,1]<80){Q3[i,3]="LOW"}
  else {Q3[i,3]="HIGH"}}
names(Q3)[3]="PULSE"

for (i in 1:length(Q3[,2])){ 
  if(is.na(Q3[i,2])==TRUE){Q3[i,4]=NA}
  else if(Q3[i,2]=="Never"){Q3[i,4]="NEVER"}
  else {Q3[i,4]="EVER"}}
names(Q3)[4]="SMOKE"
head(Q3)
Table_3a=as.matrix(table(Q3[,c(3,4)]))
Table_3a
#Fisher's Exact Test 
#H0:There is no association between pulse and smoke status
fisher.test(Table_3a) 
#p-value=1 indicates there is no association between pulse and smoke status at level α=0.05 

#Pearson's chi-square test 
#H0:There is no association between pulse and smoke status
chisq.test(Table_3a) 
#p-value=1 indicates there is no association between pulse and smoke status at level α=0.05

