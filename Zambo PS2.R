#Zambrano PS 2 Redo

rm(list=ls())
#A
df = read.csv('zdata.csv')
temp = read.csv('temp.csv')
temp$Date = temp$V1
temp$Median = temp$V2

#B
trainingdf = df[,14:15]
trainingdf = trainingdf[1:125,]
trainingdf$Temperature = temp$V2[1:125]
testdf = df[,16:17]
testdf = testdf[1:42,]
testdf$temp = temp$V2[126:167]

mod = lm(trainingdf$Temperature~trainingdf$Training.CO2)
summary(mod)

alpha = -2.9079443
Beta = 0.0088878

#C
#95% condifence intervals with and without uncertainty
#Note we use standard error for confidence interview, standard errors
#come from errors of model prediction, 
#deviation comes from deviations from mean of variable
#Residual standard error: **0.1295**
x = testdf$Test.CO2
z = 1.96
t = qt(.025,123,lower.tail = FALSE)

s = sqrt(mean((mod$fitted.values - trainingdf$Temperature)^2))

smean = sqrt(mean((mod$fitted.values - trainingdf$Temperature)^2))/sqrt(125)

#Sxx = std(x)^2 * (n-1)

Sxx = sqrt(var(trainingdf$Training.CO2))*124
Sst = ((testdf$Test.CO2 - mean(trainingdf$Training.CO2))^2)

#without uncertainty
# use parameters from model (alpha, and betas), then add multiplication
#of z score and standard error
testdf$uppercc = (alpha) + (Beta*x) + (z * smean)
testdf$lowercc = (alpha) + (Beta*x) - (z * smean)

#with uncertainty
#interval is longer
#confidenceuncertantyupper = -2.907944279  + (0.008887751*Testdf$Test.CO2) + (t*s*sqrt((1/125)+(sst/Sxx)))
testdf$uppercu = (alpha) + (Beta*x) + ((t * smean)) + (Sst/Sxx)
testdf$lowercu = (alpha) + (Beta*x) - ((t * smean) + (Sst/Sxx))

#Test
which(testdf$temp < testdf$uppercu & testdf$temp > testdf$lowercu)
length(which(testdf$temp < testdf$uppercu & testdf$temp > testdf$lowercu))/length(testdf$temp)


#D
# Prediction Intervals
testdf$upperpc = -2.907944279  + (0.008887751*testdf$Test.CO2) + (z*s)
testdf$lowerpc = -2.907944279  + (0.008887751*testdf$Test.CO2) - (z*s)

testdf$upperpu = (alpha) + (Beta*x) + ((t * smean * sqrt(1+(1/125))) + (Sst/Sxx))
testdf$lowerpu = (alpha) + (Beta*x) - ((t * smean * sqrt(1+(1/125))) + (Sst/Sxx))

#how many observations reside within the prediction intervals?

length(which(testdf$temp < testdf$upperpu & testdf$temp > testdf$lowerpu))
length(which(testdf$temp < testdf$upperpc & testdf$temp > testdf$lowerpc))

trainingdf$CO2 = trainingdf$Training.CO2
trainingdf$temp = trainingdf$Temperature
testdf$CO2 = testdf$Test.CO2


mod2 = lm(temp~CO2, data = trainingdf)
testci = predict(mod2,testdf, interval = "confidence")
testpi = predict(mod2,testdf, interval = "prediction")

#E
#Plotting point predictions, and prediction intervals
trainingpredictionci = predict(mod2,trainingdf, interval = "confidence")
trainingpredictionpi = predict(mod2,trainingdf, interval = "prediction")
testpredictci = predict(mod2,testdf, interval = "confidence")
testpredictpi = predict(mod2,testdf, interval = "predict")

x = rbind(trainingpredictionpi, testpredictpi)
y = c((trainingdf$Temp),(testdf$temp))
df3 = cbind(x,y)
df3 = as.data.frame(df3)
Date = c(trainingdf$Training.Year,testdf$Test.Year)
df3$Year = c(1850:2016)
fit = as.ts(df3$fit)
plot(df3$Year,df3$y, xlab = "Year",ylab = "deviations")
points(df3$Year[126:167],df3$y[126:167], xlab = "Year",ylab = "Deviations", cex = .75, col = 2)

lines(df3$Year[1:125],df3$fit[1:125], col = 1, cex = 3)
lines(df3$Year[126:167],df3$fit[126:167], col = 4)

lines(df3$Year[1:125],df3$lwr[1:125], col = 3)
lines(df3$Year[126:167],df3$upr[126:167], col = 2)

lines(df3$Year[126:167],df3$lwr[126:167], col = 2)
lines(df3$Year[1:125],df3$upr[1:125], col = 3)

legend("topleft", c("Confidence interval" , "Fitted Training", "Fitted Test", "Training Temp" , "Test Temp", "Prediction Interval"),lty = c(1,1,1,0,0,1), col = c(3,1,4,1,2,2),pch = c(NA,NA,NA,1,1,NA),cex =.75)


#F
qnorm(df3$y, mean = mean(df3$y), var(df3$y))
pnorm(df3$y,df3$fit,s)
#actual y, fitted y, rootmean squared error
pnorm(Testdf2$Temp,df3$fit[125:167],s)
pnorms = pnorm(df3$y[126:167],df3$fit[126:167],s)
pnorms = sort(pnorms)
PITvalues = pnorms
Probability = (1:42/42)
plot(PITvalues,Probability)
abline(0,1, col = 3)
hist(PITvalues)




