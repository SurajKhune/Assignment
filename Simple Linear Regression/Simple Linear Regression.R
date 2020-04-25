########################Simple Linear Regression:####################################
Cc <- read.csv(file.choose())  
plot(Calories.Consumed,Weight.gained..grams.)
attach(Cc)
regCc <- lm(Weight.gained..grams.~Calories.Consumed)
cor(Weight.gained..grams.,Calories.Consumed)#0.946991
summary(regCc)
predict(regCc)-Cc$Weight.gained..grams.
sqrt(sum(regCc$residuals^2)/nrow(Cc))#RMSE=103.3025
library(Metrics)
rmse(Weight.gained..grams.,regCc$fitted.values)
#R-squared:  0.8968
cor(log(Weight.gained..grams.),Calories.Consumed)
plot(log(Weight.gained..grams.),Calories.Consumed)
logY <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(logY)
predict(logY)
logY$fitted.values
exp(logY$fitted.values)-Cc$Weight.gained..grams.
logY$residuals
sum(logY$residuals)
sqrt(sum(logY$residuals^2)/nrow(Cc))#RMSE=0.3068228    =118.0452
rmse((Weight.gained..grams.),exp(logY$fitted.values))
#R-squared:  0.8776
cor(Weight.gained..grams.,log(Calories.Consumed))
plot(Weight.gained..grams.,log(Calories.Consumed))
logx <- lm(Weight.gained..grams.~log(Calories.Consumed))
summary(logx)
logx$fitted.values
logx$fitted.values-Cc$Weight.gained..grams.
sqrt(sum(logx$residuals^2)/nrow(Cc))#RMSE=141.0054
rmse(Weight.gained..grams.,logx$fitted.values)
logx$residuals
#R-squared:  0.8077
cor(log(Weight.gained..grams.),log(Calories.Consumed))
plot(log(Weight.gained..grams.),log(Calories.Consumed))
logxy <- lm(log(Weight.gained..grams.)~log(Calories.Consumed))
exp(logxy$fitted.values)
summary(logxy)
sqrt(mean(logxy$residuals^2))
sqrt(sum(logxy$residuals^2)/nrow(Cc))#RMSE= 0.3436048
rmse(log(Weight.gained..grams.),(logxy$fitted.values))
#R-squared:  0.8465
cor (Weight.gained..grams.,sqrt(Calories.Consumed))
plot(Weight.gained..grams.,sqrt(Calories.Consumed))
sqrtx <- lm(Weight.gained..grams.~sqrt(Calories.Consumed))
summary(sqrtx)
sqrtx$residuals
sqrtx$fitted.values
predict(sqrtx)
sqrtx$residuals
sqrt(sum(sqrtx$residuals^2)/nrow(Cc))#RMSE= 121.7122
rmse(Weight.gained..grams.,sqrtx$fitted.values)
#R-squared:  0.8567
cor(sqrt(Weight.gained..grams.),sqrt(Calories.Consumed))
plot(sqrt(Weight.gained..grams.),sqrt(Calories.Consumed))
sqrtxy <- lm(sqrt(Weight.gained..grams.)~sqrt(Calories.Consumed))
summary(sqrtxy)
sqrtxy$fitted.values^2
sqrtxy$residuals
rmse(sqrt(Weight.gained..grams.),sqrtxy$fitted.values)#RMSE=2.626416
sqrt(mean(sqrtxy$residuals^2))
#R-squared:  0.8887
cor(sqrt(Weight.gained..grams.),(Calories.Consumed))
plot(sqrt(Weight.gained..grams.),(Calories.Consumed))
sqrty <- lm(sqrt(Weight.gained..grams.)~(Calories.Consumed))
summary(sqrty)
predict(sqrty)
sqrt(Weight.gained..grams.)
rmse(sqrt(Weight.gained..grams.),sqrty$fitted.values)#RMSE=2.310718
sqrt(mean(sqrty$residuals^2))
sqrty$residuals
#R-squared:  0.9139
cor(Weight.gained..grams.,Calories.Consumed*Calories.Consumed)
plot(Weight.gained..grams.,Calories.Consumed*Calories.Consumed)
sqx <- lm(Weight.gained..grams.~Calories.Consumed*Calories.Consumed)
summary(sqx)
sqx$fitted.values
sqx$residuals
rmse(Weight.gained..grams.,sqx$fitted.values)#RMSE=103.3025
sqrt(mean(sqx$residuals^2))
#R-squared:  0.8968
cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
sqy <- lm(Weight.gained..grams.*Weight.gained..grams.~Calories.Consumed)
summary(sqy)
sqy$fitted.values
sqrt(sqy$fitted.values)
sqy$residuals
sqrt(mean(sqy$residuals^2))#RMSE=157606.1
rmse(sqrt(Weight.gained..grams.),sqy$fitted.values)
#R-squared:  0.8033
cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed)
sqxy <- lm(Weight.gained..grams.*Weight.gained..grams.~Calories.Consumed*Calories.Consumed)
summary(sqxy)
#R-squared:  0.8033

cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
sqxylogx <- lm(Weight.gained..grams.*Weight.gained..grams.~Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
summary(sqxylogx)
#R-squared:  0.9795

cor(Weight.gained..grams.,Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
plot(Weight.gained..grams.,Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
sqxlogx <- lm(Weight.gained..grams.~Calories.Consumed*Calories.Consumed+log(Calories.Consumed))
summary(sqxlogx)
#R-squared:  0.9639



cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed+log(Calories.Consumed))
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed+log(Calories.Consumed))
sqylogx <- lm(Weight.gained..grams.*Weight.gained..grams.~Calories.Consumed+log(Calories.Consumed))
summary(sqylogx)
#R-squared:  0.9795

cor(Weight.gained..grams.,Calories.Consumed+log(Calories.Consumed))
plot(Weight.gained..grams.,Calories.Consumed+log(Calories.Consumed))
pyxylogx <- lm(Weight.gained..grams.~Calories.Consumed+log(Calories.Consumed))
summary(pyxylogx)
#R-squared:  0.9639

cor(Weight.gained..grams.,Calories.Consumed+log(Weight.gained..grams.))
plot(Weight.gained..grams.,Calories.Consumed+log(Weight.gained..grams.))
pyxylogy <- lm(Weight.gained..grams.~Calories.Consumed+log(Weight.gained..grams.))
summary(pyxylogy)
#R-squared:  0.9257

