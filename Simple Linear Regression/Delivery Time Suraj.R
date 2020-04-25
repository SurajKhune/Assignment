dt <- read.csv(file.choose())
View(dt)
attach(dt)
library(performance)

#### Simple Linear Regression model  ####
plot(Sorting.Time,Delivery.Time)
cor(Sorting.Time,Delivery.Time)#0.8259973
basicdt <- lm(Delivery.Time~Sorting.Time)
summary(basicdt)
basicdt$fitted.values
basicdt$residuals
rmse(basicdt)#2.79165
plot(basicdt)


# visualization
library(ggplot2)
ggplot(data = dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time, y=basicdt$fitted.values))

#R-squared:  0.6823




#### Logrithamic Model  ####


cor(Delivery.Time,log(Sorting.Time))#0.8339325
plot(Delivery.Time,log(Sorting.Time))
logmodel <- lm(Delivery.Time~log(Sorting.Time))
summary(logmodel)
logmodel$fitted.values
logmodel$residuals
confint(logmodel)
rmse(logmodel)#2.733171
plot(logmodel)


# visualization

library(ggplot2)
ggplot(data = dt, aes(x = log(Sorting.Time), y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=log(Sorting.Time), y=logmodel$fitted.values))

#R-squared:   0.6954



#### Exponential Model  ####
cor(log(Delivery.Time),Sorting.Time)#0.8431773
plot(log(Delivery.Time),Sorting.Time)
expmodel <- lm(log(Delivery.Time)~Sorting.Time)
summary(expmodel)
expmodel$fitted.values
expmodel$residuals
confint(expmodel)
rmse(expmodel)#0.1669628
plot(expmodel)

# visualization
library(ggplot2)
ggplot(data = dt, aes(x = Sorting.Time, y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time, y=expmodel$fitted.values))



#R-squared:  0.7109

#### Quadratic model ####


cor(Delivery.Time,Sorting.Time+Sorting.Time*Sorting.Time)# 0.7971156
plot(Delivery.Time,Sorting.Time+Sorting.Time*Sorting.Time)
quadmodel <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time*Sorting.Time))
summary(quadmodel)
quadmodel$fitted.values
quadmodel$residuals
confint(quadmodel)
rmse(quadmodel)#2.79165
plot(quadmodel)

# visualization
ggplot(data = dt, aes(x = Sorting.Time+Sorting.Time*Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time+Sorting.Time*Sorting.Time, y=quadmodel$fitted.values))


#R-squared:  0.6823



#### Polynomial With LogY [Y=X+logY] ####    


cor(Delivery.Time,Sorting.Time+log(Delivery.Time))#0.8566868
plot(Delivery.Time,Sorting.Time+log(Delivery.Time))
poly_logY <- lm(Delivery.Time~Sorting.Time+log(Delivery.Time))
summary(poly_logY)
poly_logY$fitted.values
poly_logY$residuals
confint(poly_logY)
rmse(poly_logY)#0.9480634
plot(poly_logY)

# visualization
ggplot(data = dt, aes(x = Sorting.Time+log(Delivery.Time), y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time+log(Delivery.Time), y=poly_logY$fitted.values))

#R-squared:  0.9634

##### Polynomial With LogX & LogY [Y=X+logX+logY] #####  
#

cor(Delivery.Time,Sorting.Time+log(Sorting.Time)+log(Delivery.Time))#0.8553475
plot(Delivery.Time,Sorting.Time+log(Sorting.Time)+log(Delivery.Time))
poly_logX_logY <- lm(Delivery.Time~Sorting.Time+log(Sorting.Time)+log(Delivery.Time))
summary(poly_logX_logY)
poly_logX_logY$fitted.values
poly_logX_logY$residuals
confint(poly_logX_logY)
rmse(poly_logX_logY)# 0.6066018
plot(poly_logX_logY)

# visualization
ggplot(data = dt, aes(x = Sorting.Time+log(Sorting.Time)+log(Delivery.Time), y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time+log(Sorting.Time)+log(Delivery.Time), y=poly_logX_logY$fitted.values))

#R-squared:  0.985


#Interpretation:
library(sjPlot)
tab_model(basicdt,logmodel,expmodel,quadmodel,poly_logY,poly_logX_logY)


#####Conclusion#####
#Polynomial With LogX & LogY Model Has Higher Value Of R^2
































################RMSE#####################
sqrt(mean(basicdt$residuals^2)) #2.79165
sqrt(mean(logmodel$residuals^2))#2.733171
sqrt(mean(expmodel$residuals^2))#0.1669628
sqrt(mean(quadmodel$residuals^2))#2.79165
sqrt(mean(poly_logY$residuals^2))#0.9480634
sqrt(mean(poly_logX_logY$residuals^2))#0.6066018