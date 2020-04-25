Cc <- read.csv(file.choose())  
attach(Cc)
library(sjPlot)
library(performance)
library(ggplot2)

#### Simple Linear Regression model  ####

cor(Weight.gained..grams.,Calories.Consumed)#0.946991
plot(Calories.Consumed,Weight.gained..grams.)
BasicCc <- lm(Weight.gained..grams.~Calories.Consumed)
summary(BasicCc)
BasicCc$fitted.values
BasicCc$residuals
rmse(BasicCc)#103.3025
plot(BasicCc)


# visualization
ggplot(data = Cc, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cc, aes(x=Calories.Consumed, y=BasicCc$fitted.values))


#R-squared:  0.8968


cor(log(Weight.gained..grams.),Calories.Consumed)
plot(log(Weight.gained..grams.),Calories.Consumed)
logY <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(logY)
#R-squared:  0.8776

#### Logrithamic Model  ####

cor(Weight.gained..grams.,log(Calories.Consumed))#0.8987253
plot(Weight.gained..grams.,log(Calories.Consumed))
logmodel<- lm(Weight.gained..grams.~log(Calories.Consumed))
summary(logmodel)
logmodel$fitted.values
logmodel$residuals
confint(logmodel)
rmse(logmodel)#141.0054
plot(logmodel)


# visualization

ggplot(data = Cc, aes(x = log(Calories.Consumed), y = Weight.gained..grams.)) + 
  geom_point(color='green') +
  geom_line(color='black',data = Cc, aes(x=log(Calories.Consumed), y=logmodel$fitted.values))


#R-squared:  0.8077

#### Exponential Model  ####


cor(log(Weight.gained..grams.),Calories.Consumed)#0.9368037
plot(log(Weight.gained..grams.),Calories.Consumed)
expmodel <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(expmodel)
expmodel$fitted.values
expmodel$residuals
confint(expmodel)
rmse(expmodel)#0.3068228
plot(expmodel)

# visualization

ggplot(data = Cc, aes(x = Calories.Consumed, y = log(Weight.gained..grams.))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cc, aes(x=Calories.Consumed, y=expmodel$fitted.values))


#R-squared:  0.8776

#####Polynomial With LogX   [Y^2=X^2+logX]##### 


cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed+I(log(Calories.Consumed)))#0.9476707
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed*Calories.Consumed+I(log(Calories.Consumed)))
poly_logX <- lm(Weight.gained..grams.*Weight.gained..grams.~Calories.Consumed*Calories.Consumed+I(log(Calories.Consumed)))
summary(poly_logX)
poly_logX$fitted.values
poly_logX$residuals
confint(poly_logX)
rmse(poly_logX)#50897.01    
plot(poly_logX)

# visualization
ggplot(data = Cc, aes(x = Calories.Consumed*Calories.Consumed+I(log(Calories.Consumed)), y = Weight.gained..grams.*Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cc, aes( x = Calories.Consumed*Calories.Consumed+I(log(Calories.Consumed)), y=poly_logX$fitted.values))


#R-squared:  0.9795



#Interpretation:
library(sjPlot)
tab_model(BasicCc,logmodel,expmodel,poly_logX)


#####Conclusion#####
#Polynomial With LogX Model Has Higher Value Of R^2 #0.9795














#########RMSE##################
sqrt(mean(BasicCc$residuals^2)) #103.3025
sqrt(mean(logmodel$residuals^2))#141.0054
sqrt(mean(expmodel$residuals^2))#0.3068228
sqrt(mean(poly_logX$residuals^2))#50897.01


