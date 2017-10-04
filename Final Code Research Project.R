#Research Code Clean
#John Dobson
rm(list=ls())
df = read.csv('Cleanest Dataset.csv')
library(car)
library(forecast)
library(astsa)
library(tseries)
#Initial Data Frame
#TS Mod
x = sarima(diff(diff(df$Fundamental.1[1:362])), p =, d = 0, q = 1, P = 1 , D = 0, Q = 1, S = 12)
x = arima(diff(diff(df$Fundamental.1[1:362])), c(12,0,3))
plot(x$residuals)
y = arima(diff(diff(df$Case.Schiller.Home.Price.Index[1:362])), c(12,0,2))
y2 = sarima(df$Case.Schiller.Home.Price.Index[1:362], p = 2, d = 2, q = 2, P = 1, D = 0 , Q = 1, S = 12)
y2fit = fitted.Arima(y2)
yfit = fitted.Arima(y)
xfit = fitted.Arima(x)
ts.plot(df$Case.Schiller.Home.Price.Index[1:362])
lines(yfit, col = 2)
lines(y2fit, col = 3)

adf.test((diff(df$Case.Schiller.Home.Price.Index[1:362])))
adf.test(diff(diff(df$Case.Schiller.Home.Price.Index[1:362])))

adf.test((diff(df$Fundamental.1[1:362]))

adf.test(diff(diff(df$Fundamental.1[1:362]))))

ts.plot(df$Fundamental.1[1:362])
lines(xfit, col =2)

tsmod = lm(yfit[1:360]~0 + df$initial.bubble[1:360] + xfit[1:360])
summary(tsmod)

#Non-TS Mod
IV = lm(df$Fundamental.1[1:362] ~ df$Disposable.Income[1:362] + df$Unemployment[1:362] + df$Mortgage.Rate[1:362])
IV2 = lm(df$Fundamental.1[1:362] ~ df$Disposable.Income[1:362])
Fundamental = IV$fitted.values[1:362]
Fundamental2 = IV2$fitted.values
summary(Fundamental)
summary(IV2)
bubblemod = lm(df$Case.Schiller.Home.Price.Index[1:362]~0 + df$initial.bubble[1:362] + Fundamental[1:362])
summary(bubblemod)

bubblemod2 = lm(df$Case.Schiller.Home.Price.Index[1:362]~0 + df$initial.bubble[1:362] + Fundamental2[1:362])
summary(bubblemod2)


#8 city dataset
library(AER)
library(car)

ivtest = ivreg(df$Housing.Price[1:208]~0+df$Bubble.Term[1:208] + df$Fundamentala[1:208] + df$City[1:208]|0+df$Bubble.Term[1:208] + df$Per.Capita.Income[1:208] + df$Unemployment2[1:208] + df$City[1:208])


(df$Housing.Price[1:208]~0+df$Bubble.Term[1:208] + fundamental8$fitted.values + df$City[1:208])

fundamental8 = lm(df$Fundamentala[1:208]~df$Per.Capita.Income[1:208] + df$Unemployment2[1:208])
summary(fundamental8)

bubblemod8 = lm(df$Housing.Price[1:208]~0+df$Bubble.Term[1:208] + fundamental8$fitted.values + df$City[1:208])
bubblemod8two = lm(df$Housing.Price[1:208]~0+df$Bubble.Term[1:208] + fundamental8$fitted.values + df$SF[1:208] + df$NY[1:208] + df$Boston[1:208] + df$Miami[1:208] + df$San.Diego[1:208] + df$Chi[1:208] + df$DEN[1:208] + df$LA[1:208])
summary(bubblemod8two)

bubblemod8three = lm(df$Housing.Price[1:208]~0+df$Bubble.Term[1:208] + fundamental8$fitted.values + df$SF[1:208] + df$NY[1:208] + df$Boston[1:208] + df$Miami[1:208] + df$San.Diego[1:208] + df$Chi[1:208] + df$DEN[1:208] + df$LA[1:208] + df$SF[1:208]*df$Bubble.Term[1:208] + df$NY[1:208]*df$Bubble.Term[1:208] + df$Boston[1:208]*df$Bubble.Term[1:208] + df$Miami[1:208]*df$Bubble.Term[1:208] + df$San.Diego[1:208]*df$Bubble.Term[1:208] + df$Chi[1:208]*df$Bubble.Term[1:208] + df$DEN[1:208]*df$Bubble.Term[1:208] + df$LA[1:208]*df$Bubble.Term[1:208])
summary(bubblemod8three)


summary(bubblemod8)
summary(tsmod)
summary(bubblemod) #This isnt valid IV
summary(bubblemod2) #This is

#Plots


plot.ts(df$Case.Schiller.Home.Price.Index[1:362])
lines(df$Fundamentala[1:362], col = 2)



Date = c(Trainingdf2$Training.Year,Testdf2$Test.Year)
df3$Year = c(1850:2016)
fit = as.ts(df3$fit)
plot(df$Case.Schiller.Home.Price.Index[1:362],df$Fundamentala[1:362], xlab = "Year",ylab = "$")
points(df3$Year[126:167],df3$y[126:167], xlab = "Year",ylab = "Deviations", cex = .75, col = 2)

lines(df3$Year[1:125],df3$fit[1:125], col = 1, cex = 3)
lines(df3$Year[126:167],df3$fit[126:167], col = 4)

lines(df3$Year[1:125],df3$lwr[1:125], col = 3)
lines(df3$Year[126:167],df3$upr[126:167], col = 2)

lines(df3$Year[126:167],df3$lwr[126:167], col = 2)
lines(df3$Year[1:125],df3$upr[1:125], col = 3)