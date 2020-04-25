sh <- read.csv(file.choose())
View(sh)
attach(sh)
library(sjPlot)
library(performance)
library(ggplot2)

#### Simple Linear Regression model  ####



plot(Salary,YearsExperience)
cor(Salary,YearsExperience)#0.9782416
cor(YearsExperience,Salary)
basicsh <- lm(Salary~YearsExperience)
summary(basicsh)
basicsh$fitted.values
basicsh$residuals
rmse(basicsh)#5592.044
plot(basicsh)


# visualization
ggplot(data = sh, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes(x=YearsExperience, y=basicsh$fitted.values))


#R-squared:  0.957




#### Logrithamic Model  ####


cor(Salary,log(YearsExperience))#0.9240611
plot(Salary,log(YearsExperience))
logmodel <- lm(Salary~log(YearsExperience))
summary(logmodel)
logmodel$fitted.values
logmodel$residuals
confint(logmodel)
rmse(logmodel)#10302.89
plot(logmodel)


# visualization

ggplot(data = sh, aes(x = log(YearsExperience), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes(x=log(YearsExperience), y=logmodel$fitted.values))


#R-squared:   0.8539


#### Exponential Model  ####

cor(log(Salary),YearsExperience)#0.9653844
plot(log(Salary),YearsExperience)
expmodel <- lm(log(Salary)~YearsExperience)
summary(expmodel)
expmodel$fitted.values
expmodel$residuals
confint(expmodel)
rmse(expmodel)#0.09457437
plot(expmodel)

# visualization

ggplot(data = sh, aes(x = YearsExperience, y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes(x=YearsExperience, y=expmodel$fitted.values))



#R-squared:  0.932


#### Quadratic model ####


cor(Salary,YearsExperience+YearsExperience*YearsExperience)#0.959966
plot(Salary,YearsExperience+YearsExperience*YearsExperience)
quadmodel <- lm(Salary~YearsExperience+I(YearsExperience*YearsExperience))
summary(quadmodel)
quadmodel$fitted.values
quadmodel$residuals
confint(quadmodel)
rmse(quadmodel)#5590.841
plot(quadmodel)

# visualization

ggplot(data = sh, aes(x = YearsExperience+I(YearsExperience*YearsExperience), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes(x=YearsExperience+I(YearsExperience*YearsExperience),  y=quadmodel$fitted.values))

#R-squared:  0.957


#### Polynomial With LogX  ####Y^2=X^2+logX


cor(Salary*Salary,YearsExperience*YearsExperience+log(YearsExperience))#0.9773674
plot(Salary*Salary,YearsExperience*YearsExperience+log(YearsExperience))
poly_logX <- lm(Salary*Salary~YearsExperience*YearsExperience+log(YearsExperience))
summary(poly_logX)
poly_logX$fitted.values
poly_logX$residuals
confint(poly_logX)
rmse(poly_logX)#805635950      #28383.73
plot(poly_logX)

# visualization
ggplot(data = sh, aes(x = YearsExperience*YearsExperience+I(log(YearsExperience)), y = Salary*Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes( x =YearsExperience*YearsExperience+I(log(YearsExperience)), y=poly_logX$fitted.values))


#R-squared:   0.9661


#### Polynomial With LogY  ####

cor(Salary,YearsExperience*YearsExperience+log(Salary))#0.9579291
plot(Salary,YearsExperience*YearsExperience+log(YearsExperience))
poly_logY <- lm(Salary~I(YearsExperience*YearsExperience)+I(log(YearsExperience)))
summary(poly_logY)
poly_logY$fitted.values
poly_logY$residuals
confint(poly_logY)
rmse(poly_logY)#6089.17
plot(poly_logY)

# visualization
ggplot(data = sh, aes(x = YearsExperience*YearsExperience+log(Salary), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sh, aes(x=YearsExperience*YearsExperience+log(Salary), y=poly_logY$fitted.values))

#R-squared:  0.949








#Interpretation:
library(sjPlot)
tab_model(basicsh,logmodel,expmodel,quadmodel,poly_logX,poly_logY)


#####Conclusion#####
#Polynomial With LogX Model Has Higher Value Of R^2 #0.9661














#########RMSE##################
sqrt(mean(basicsh$residuals^2)) #5592.044
sqrt(mean(logmodel$residuals^2))#10302.89
sqrt(mean(expmodel$residuals^2))#0.09457437
sqrt(mean(quadmodel$residuals^2))#5590.841
sqrt(mean(poly_logY$residuals^2))#6089.17
sqrt(mean(poly_logX$residuals^2))#805635950
