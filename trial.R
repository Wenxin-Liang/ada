age <- c(1:100)
num <- c(659,46,28,22,17,15,14,12,11,9,8,9,12,19,29,39,49,58,66,74,82,90,96,98,97,
         96,95,95,95,97,100,103,106,111,115,121,128,136,146,158,172,187,205,226,248,
         271,294,318,345,375,407,440,473,506,538,572,610,651,698,748,804,862,924,988,
         1057,1135,1222,1312,1403,1496,1590,1698,1821,1953,2085,2214,2343,2479,2628,
         2781,2918,3037,3153,3278,3378,3457,3518,3540,3510,3426,3286,3091,2848,2565,
         2255,1930,1607,1298,1017,770,1680)
ind <- c(NA)
pop <- data.frame(cbind(age,num))
for (i in 1:99)
{
  if (pop$num[i]==pop$num[i+1])
  {
    ind <- c(ind,i)
  }
}
ind <- na.omit(ind)
pop <- pop[-ind,]

indmin <- c(NA)
indmax <- c(NA)
n <- nrow(pop)
for (i in 2:(n-1)){
  if (pop$num[i]<pop$num[i-1]&pop$num[i]<pop$num[i+1])
  {
    indmin <- c(indmin,i)
  }
}
for (i in 2:(n-1)){
  if (pop$num[i]>pop$num[i-1]&pop$num[i]>pop$num[i+1])
  {
    indmax <- c(indmax,i)
  }
}
indmin <- na.omit(indmin)
indmax <- na.omit(indmax)
pop[indmin,]
pop[indmax,]
