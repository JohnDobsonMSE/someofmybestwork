rm(list=ls())
library('foreign')

df = read.dta('quarterofbirth updated.dta')

df$old = 0
df$old[df$yob>49] = 1

x = which(df$old == 1)
df2 = df[-x,]

df2$cohort1 = 0
df2$cohort1[df2$yob <= 39] = 1

df2$cohort2 = 0
df2$cohort2[df2$yob >= 40] = 1

df2$year30 = 0
df2$year30[df2$yob == 30] = 1

df2$year31 = 0
df2$year31[df2$yob == 31] = 1

df2$year32 = 0
df2$year32[df2$yob == 32] = 1

df2$year33 = 0
df2$year33[df2$yob == 33] = 1

df2$year34 = 0
df2$year34[df2$yob == 34] = 1

df2$year35 = 0
df2$year35[df2$yob == 35] = 1

df2$year36 = 0
df2$year36[df2$yob == 36] = 1

df2$year37 = 0
df2$year37[df2$yob == 37] = 1

df2$year38 = 0
df2$year38[df2$yob == 38] = 1

df2$year39 = 0
df2$year39[df2$yob == 39] = 1

df2$year40 = 0
df2$year40[df2$yob == 40] = 1

df2$year41 = 0
df2$year41[df2$yob == 41] = 1

df2$year42 = 0
df2$year42[df2$yob == 42] = 1

df2$year43 = 0
df2$year43[df2$yob == 43] = 1

df2$year44 = 0
df2$year44[df2$yob == 44] = 1

df2$year45 = 0
df2$year45[df2$yob == 45] = 1

df2$year46 = 0
df2$year46[df2$yob == 46] = 1

df2$year47 = 0
df2$year47[df2$yob == 47] = 1

df2$year48 = 0
df2$year48[df2$yob == 48] = 1

df2$year49 = 0
df2$year49[df2$yob == 49] = 1


library('astsa')


#2
df3 = df2[df2$cohort1==1,]

df4 = df3[with(df3, order(df3$yob, df3$qob)),]



x = which(df3$educ==30 & df3$qob == 1)
df2$educ2 = 0
meaned = vector()
for (i in 30:49) {
  for (j in 1:4) {
    x = mean(df2$educ[df2$yob == i & df2$qob == j])
   meaned = c(meaned,x)
   df
  }
}

result = vector()
MA = vector()
for (i in 1:76) {
  result[i] = meaned[i+2] - ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
  MA[i] = ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
}

firstcohort = barplot(result[1:38],c(1930:1939), xlab = "year of birth")
secondcohort = barplot(result[39:76])

#Creating new education collumn

df2$newed = 0
df4 = df2[with(df2, order(df2$yob, df2$qob)),]
c =1 
for (i in 2:length(df4$newed)) {
  if(df4$qob[i]!= df4$qob[i-1]){
    c = c+1
  }
  df4$newed[i] = c
}

df4$newed2 = 0
for (i in 1:length(df4$newed)) {
  for (j in 1:length(result)) {
    if(df4$newed[i] == j){
      df4$newed2[i] = df4$educ[i] - result[j]
    }
  }
  print(i)
}

x = which(df4$newed > 2)
df5 = df4[x,]
x = which(df5$newed < 79)
df5 = df5[x,]
df5$newed2 = 0
for (i in 1:length(df4$newed2)) {
x = df5$newed[i]
df5$newed2[i] = df5$educ[i] - meaned[x]
print(i)
}


x = which(df4$newed > 2)
df5 = df4[x,]
x = which(df5$newed < 79)
df5 = df5[x,]
df5$newed2 = 0
for (i in 1:length(df5$newed2)) {
  x = df5$newed[i]
  df5$newed2[i] = df5$educ[i] - MA[x]
  print(i)
}

load('FinalizedDetrendededucation.Rda')

cohort1q1 = mean(df$educ[df$qob == 1 & df$cohort1 == 1])
cohort1q2 = mean(df$educ[df$qob == 2 & df$cohort1 == 1])
cohort1q3 = mean(df$educ[df$qob == 3 & df$cohort1 == 1])
cohort1q4 = mean(df$educ[df$qob == 4 & df$cohort1 == 1])

cohort2q1 = mean(df$educ[df$qob == 1 & df$cohort2 == 1])
cohort2q2 = mean(df$educ[df$qob == 2 & df$cohort2 == 1])
cohort2q3 = mean(df$educ[df$qob == 3 & df$cohort2 == 1])
cohort2q4 = mean(df$educ[df$qob == 4 & df$cohort2 == 1])

cohort3q1 = mean(df$educ[df$qob == 1 & df$cohort3 == 1])
cohort3q2 = mean(df$educ[df$qob == 2 & df$cohort3 == 1])
cohort3q3 = mean(df$educ[df$qob == 3 & df$cohort3 == 1])
cohort3q4 = mean(df$educ[df$qob == 4 & df$cohort3 == 1])

cohort1vector = c(cohort1q1,cohort1q2,cohort1q3,cohort1q4)
cohort2vector = c(cohort2q1,cohort2q2,cohort2q3,cohort2q4)
cohort3vector = c(cohort3q1,cohort3q2,cohort3q3,cohort3q4)

library('astsa')
df$cohort1 = 0
df$cohort1[df2$yob <= 39] = 1

df$cohort2 = 0
df$cohort2[df2$yob >= 40] = 1

df$cohort3 = 0
df$cohort3[df$yob > 50] = 1

ts.plot(cohort1vector)
ts.plot(cohort2vector, col = 2)
plotframe = data.frame(cohort1vector,cohort2vector,cohort3vector)

ts.plot(plotframe, col = c(1:3), ylab = "Education", xlab = "quarter")
points(cohort3vector)
points(cohort2vector)
points(cohort1vector)
legend("right", legend = c("1930    ","1940   ","1920   "),lty = 1,col = 1:3, box.lty = 0, cex = .5)

#5
load('FinalizedDetrendededucation.Rda')

x = which(df4$newed <= 2)
df5 = rbind(df5,df4[x,])
x = which(df4$newed >= 79)
df5 = rbind(df5,df4[x,])

df5$highschool = 0
df5$highschool[df5$educ>11] = 1
df5$college = 0
df5$college[df5$educ > 15] = 1
df5$masters = 0
df5$masters[df5$educ>17] = 1
df5$phd = 0 
df5$phd[df5$educ>19] = 1

df5$qob1 = 0
df5$qob1[df5$qob == 1] = 1

df5$qob2 = 0
df5$qob2[df5$qob == 2] = 1

df5$qob3 = 0
df5$qob3[df5$qob == 3] = 1

#Total Years of Education
mean(df5$educ[df5$cohort1==1])
#12.74977
mean(df5$educ[df5$cohort2==1])
#13.56001



#High School Graduate
HS30 = sum(df5$highschool[df5$cohort1==1])/length(df5$highschool[df5$cohort1==1])
# 0.7684926
HS40 = sum(df5$highschool[df5$cohort2==1])/length(df5$highschool[df5$cohort2==1])

COL30 = sum(df5$college[df5$cohort1==1])/length(df5$college[df5$cohort1==1])
#0.2356244
COL40 = sum(df5$college[df5$cohort2==1])/length(df5$college[df5$cohort2==1])
#0.2995881


meanedHS30 = vector()
for (i in 1:4) {
  meanedHS30[i] = mean(df5$educ[df5$cohort1 == 1 & df5$qob == i])
}



resultHS30 = vector()
for (i in 1:) {
  resultHS30[i] = meanedH[i+2] - ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
}



result = vector()
for (i in 1:76) {
  result[i] = meaned[i+2] - ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
}



HS40 = sum(df5$highschool[df5$cohort2==1])/length(df5$highschool[df5$cohort2==1])
# 0.8636907


df5$qob1 = 0
df5$qob1[df5$qob == 1] = 1

df5$qob2 = 0
df5$qob2[df5$qob == 2] = 1

df5$qob3 = 0
df5$qob3[df5$qob == 3] = 1

df6 = df5

df5$newed3 = 0
for (i in 1:length(df5$newed3)) {
  x = df5$newed[i]
  df5$newed3[i] = df5$educ[i] - result[x]
  print(i)
}

#length(df6$educ[df6$cohort1==1])
#312718

qobmod1 = lm(df5$educ[df5$cohort1==1]~df5$qob1[df5$cohort1==1] + df5$qob2[df5$cohort1==1] + df5$qob3[df5$cohort1==1])
summary(qobmod1)



qobmod2 = lm(df5$educ[df5$cohort2==1]~df5$qob1[df5$cohort2==1] + df5$qob2[df5$cohort2==1] + df5$qob3[df5$cohort2==1])
summary(qobmod2)


qobhs1 = lm(df6$educ[df5$cohort1==1] ~ df5$qob1[df5$cohort1==1] + df5$qob2[df5$cohort1==1] + df5$qob3[df5$cohort1==1])


qobhs1 = lm(df6$highschool[df5$cohort1==1] ~ df6$qob1[df5$cohort1==1] + df6$qob2[df5$cohort1==1] + df6$qob3[df5$cohort1==1])
summary(qobhs1)

qobhs2 = lm(df6$highschool[df5$cohort2==1] ~ df6$qob1[df5$cohort2==1] + df6$qob2[df5$cohort2==1] + df6$qob3[df5$cohort2==1])
summary(qobhs2)

Col1 = lm(df6$college[df6$cohort1==1] ~ df6$qob1[df6$cohort1==1] + df6$qob2[df6$cohort1==1] + df6$qob3[df5$cohort1==1])
summary(Col1)

Col2 = lm(df6$college[df6$cohort2==1] ~ df6$qob1[df6$cohort2==1] + df6$qob2[df6$cohort2==1] + df6$qob3[df5$cohort2==1])
summary(Col2)

df6$college2 = 0
for (i in 3:78) {
  df6$college2[df6$newed==i]=mean(df6$college[df6$newed==i])-result[i]
}


Col2 = lm(df6$college2[df6$cohort1==1] ~ df6$qob1[df6$cohort1==1] + df6$qob2[df6$cohort1==1] + df6$qob3[df5$cohort1==1])
summary(Col1)

df7 = df4
result = vector()
for (i in 1:76) {
  result[i] = meaned[i+2] - ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
}

firstcohort = barplot(result[1:38])
secondcohort = barplot(result[39:76])

#Creating new education collumn

df2$newed = 0
df4 = df2[with(df2, order(df2$yob, df2$qob)),]
c =1 
for (i in 2:length(df4$newed)) {
  if(df4$qob[i]!= df4$qob[i-1]){
    c = c+1
  }
  df4$newed[i] = c
}



for (i in 1:length(finaled)+2) {
  df4$finaled[i] = df4$educ[i] -  ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
print(i)
  }

df7 = df5

df7$finaled = 0

x = which(df4$yob == 30 & df4$qob < 3)
df7 = rbind(df7,df4[x,])
x = which(df4$yob == 49 & df4$qob >2 )
df7 = rbind(df7,df4[x,])



for (i in 3:length(finaled)+2) {
  df7$finaled[i] = df7$educ[i] -  ((meaned[i]+meaned[i+1]+meaned[i+3]+meaned[i+4])/4)
}



newqob1 = lm(df5$newed2[df5$cohort1==1] ~ df5$qob1[df5$cohort1==1] + df5$qob2[df5$cohort1==1] + df5$qob3[df5$cohort1==1])


HSC1 =lm(df5$highschool[df5$cohort1==1] ~ df5$qob1[df5$cohort1==1] + df5$qob2[df5$cohort1==1] + df5$qob3[df5$cohort1==1])
summary(HSC1)

HSC2 =lm(df5$highschool[df5$cohort2==1] ~ df5$qob1[df5$cohort2==1] + df5$qob2[df5$cohort2==1] + df5$qob3[df5$cohort2==1])
summary(HSC2)

COLC1 =lm(df5$college[df5$cohort1==1] ~ df5$qob1[df5$cohort1==1] + df5$qob2[df5$cohort1==1] + df5$qob3[df5$cohort1==1])
summary(COLC1)

COLC2 =lm(df5$college[df5$cohort2==1] ~ df5$qob1[df5$cohort2==1] + df5$qob2[df5$cohort2==1] + df5$qob3[df5$cohort2==1])
summary(COLC2)





#using df4
df4$qob1 = 0
df4$qob1[df4$qob == 1] = 1

df4$qob2 = 0
df4$qob2[df4$qob == 2] = 1

df4$qob3 = 0
df4$qob3[df4$qob == 3] = 1


df4$highschool = 0
df4$highschool[df4$educ>11] = 1
df4$college = 0
df4$college[df4$educ > 15] = 1
df4$masters = 0
df4$masters[df4$educ>17] = 1
df4$phd = 0 
df4$phd[df4$educ>19] = 1


COLC1 =lm(df4$college[df4$cohort1==1] ~ df4$qob1[df4$cohort1==1] + df4$qob2[df5$cohort1==1] + df4$qob3[df4$cohort1==1])
summary(COLC1)

COLC2 =lm(df4$college[df4$cohort2==1] ~ df4$qob1[df5$cohort2==1] + df4$qob2[df4$cohort2==1] + df4$qob3[df4$cohort2==1])
summary(COLC2)

