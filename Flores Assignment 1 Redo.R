#Flores Assignment 1
rm(list=ls())
library('foreign')
df = read.dta('fl89-91eco526W17.dta')
df$dmage2 = df$dmage^2
probit<- glm(df$dead ~ df$dmeduc + df$alcohol + df$dmage2+ df$dmage + df$dmar + df$foreignb+df$tobacco+df$mblack+df$motherr+df$mhispan, family = binomial(link = "probit"))
summary(probit)

#3 A
#"What can you infer from the coefficients and the standard errors?"
# We can note that with education, marriage, youth, tobacco, and whether or not the mother is black are all statistically 
#different from zero at the .001% level. Given that our largest coefficient is on black we can conclude that there is 
#a strong correlation between infant deaths and black mothers and this could very likely be a reflection of the unfortunate oppurtunity
#inequalities among African-American communities. We can also conclude that alcohol and Tobacco have negative affects on infant mortality.

#B
#Test the null hypothesis that all your slope coefficients are zero. Also, give the percentage of observations correctly predicted.
n<-length(df$dead) 
linearHypothesis(probit,c("df$dmeduc=0","df$alcohol=0","df$dmage=0","df$dmage2=0","df$dmar=0","df$foreignb=0","df$tobacco=0","df$mblack=0","df$motherr=0","df$mhispan=0"))

#Large Chisq, extremely unlikely that these ind. variables don't influence or correlate with dep. variable

x = matrix(c(rep(1,565943),df$dmeduc, df$alcohol,df$dmage2,df$dmage,df$dmar,df$foreignb,df$tobacco,df$mblack,df$motherr,df$mhispan), ncol = 11)
b = matrix(coef(probit))
xt = t(x)
yhat = x%*%b
y = df$dead
#yhat - y
pnorm(yhat)
pnorm(probit$fitted.values)
mean(pnorm(yhat))
mean(pnorm(probit$fitted.values))
p<-1-sum(df$dead)/length(df$dead)
#0.9915363% of predicted values are correct, very safe to guess baby will not die as a result of point estimate


#C
#average affect is B*(1/n) * sum pdf(X'B)
yhat2 = dnorm(yhat,0,1)
meanaffect = b/n * sum(yhat2)
meanaffect
#(Intercept) -4.349118e-02
#df$dmeduc   -3.966562e-04
#df$alcohol   1.462170e-03
#df$dmage    -6.755720e-04
#df$age2      1.195734e-05
#df$dmar      2.216344e-03
#df$foreignb -1.351720e-03
#df$tobacco   3.452787e-03
#df$mblack    5.767846e-03
#df$motherr   3.336086e-03
#df$mhispan   5.504314e-04

#All affects very small in contributing to likelihood of infant mortality

#D
df1<-as.data.frame(x)

#affect at mean values for X
# beta coefficients * dy/dx x
xbars<-c(mean(df1[,1]),mean(df1[,2]),mean(df1[,3]),mean(df1[,4]),mean(df1[,5]),mean(df1[,6]),mean(df1[,7]),mean(df1[,8]),mean(df1[,9]),mean(df1[,10]),mean(df1[,11]))
afatavg<-b%*%dnorm(xbars%*%b)
max(abs(avgaff-afatavg))
avgaff
afatavg


#(Intercept) -4.046003e-02
#df$dmeduc   -3.690110e-04
#df$alcohol   1.360263e-03
#df$dmage    -6.284875e-04
#df$age2      1.112397e-05
#df$dmar      2.061874e-03
#df$foreignb -1.257511e-03
#df$tobacco   3.212142e-03
#df$mblack    5.365851e-03
#df$motherr   3.103575e-03
#df$mhispan   5.120687e-04

#The affects are pretty close with the largest difference between affects being 0.003 I personally prefer the affect
# at the average affect

#E
#Two important variables from a policy perspective are tobacco and alcohol. 
#Rather than using the continuous approximation in (c) to look at their effect, 
#compute the actual average effect for these two variables
#(i.e., using the “first difference” instead of derivatives).
#In this particular application, how good is the continuous approximation in (c)?



