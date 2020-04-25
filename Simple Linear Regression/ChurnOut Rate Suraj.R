cor <- read.csv(file.choose())
View(cor)
attach(cor)
library(sjPlot)
library(performance)
library(ggplot2)

#### Simple Linear Regression model  ####

cor(Churn_out_rate,Salary_hike)#-0.9117216
plot(Churn_out_rate,Salary_hike)
BasicCor <- lm(Churn_out_rate~Salary_hike)
summary(BasicCor)
BasicCor$fitted.values
BasicCor$residuals
rmse(BasicCor)#3.997528
plot(BasicCor)


# visualization
ggplot(data = cor, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cor, aes(x=Salary_hike, y=BasicCor$fitted.values))

#R-squared:  0.8312

#### Exponential Model  ####



cor(log(Churn_out_rate),Salary_hike)#-0.9346361
plot(log(Churn_out_rate),Salary_hike)
expmodel <- lm(log(Churn_out_rate)~Salary_hike)
summary(expmodel)
expmodel$fitted.values
expmodel$residuals
confint(expmodel)
rmse(expmodel)#0.04641748
plot(expmodel)


# visualization

ggplot(data = cor, aes(x = Salary_hike, y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cor, aes(x=Salary_hike, y=expmodel$fitted.values))

#R-squared:  0.8735

#### Logrithamic Model  ####

cor(Churn_out_rate,log(Salary_hike))#-0.9212077
plot(Churn_out_rate,log(Salary_hike))
logmodel <- lm(Churn_out_rate~log(Salary_hike))
summary(logmodel)
logmodel$fitted.values
logmodel$residuals
confint(logmodel)
rmse(logmodel)# 3.786004
plot(logmodel)


# visualization

ggplot(data = cor, aes(x = log(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='green') +
  geom_line(color='black',data = cor, aes(x=log(Salary_hike), y=logmodel$fitted.values))


#R-squared:   0.8486



#####Polynomial With LogY   [Y^2=X+logY]##### 


cor(Churn_out_rate*Churn_out_rate,Salary_hike+log(Churn_out_rate))#-0.8857098
plot(Churn_out_rate*Churn_out_rate,Salary_hike+log(Churn_out_rate))
poly_logY <- lm(Churn_out_rate*Churn_out_rate~Salary_hike+log(Churn_out_rate))
summary(poly_logY)
poly_logY$fitted.values
poly_logY$residuals
confint(poly_logY)
rmse(poly_logY)#77.84029
plot(poly_logY)

# visualization
ggplot(data = cor, aes(x = Salary_hike+log(Churn_out_rate), y = Churn_out_rate^2)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cor, aes(x=Salary_hike+log(Churn_out_rate), y=poly_logY$fitted.values))


#R-squared:  0.9972


##### Linear Model With Log X   [Y=X+logY]##########
cor(Churn_out_rate,Salary_hike+log(Churn_out_rate))#-0.9115033
plot(Churn_out_rate,Salary_hike+log(Churn_out_rate))
Linear_logY <- lm(Churn_out_rate~Salary_hike+log(Churn_out_rate))
summary(Linear_logY)
Linear_logY$fitted.values
Linear_logY$residuals
confint(Linear_logY)
rmse(Linear_logY)#0.2432379
plot(Linear_logY)

# visualization
ggplot(data = cor, aes(x = Salary_hike+log(Churn_out_rate), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cor, aes(x=Salary_hike+log(Churn_out_rate), y=Linear_logY$fitted.values))

#R-squared:  0.9994



##### Linear Model With LogX & LogY  [Y=X+logX+logY]##########



cor(Churn_out_rate,Salary_hike+log(Salary_hike)+log(Churn_out_rate))#-0.9115092
plot(Churn_out_rate,Salary_hike+log(Salary_hike)+log(Churn_out_rate))
Linear_logXY <- lm(Churn_out_rate~Salary_hike+log(Salary_hike)+log(Churn_out_rate))
summary(Linear_logXY)
Linear_logXY$fitted.values
Linear_logXY$residuals
confint(Linear_logXY)
rmse(Linear_logXY)#0.2055874
plot(Linear_logY)

# visualization
ggplot(data = cor, aes(x = Salary_hike+log(Churn_out_rate)+log(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cor, aes(x=Salary_hike+log(Churn_out_rate)+log(Salary_hike), y=Linear_logXY$fitted.values))


#R-squared:  0.9996







#Interpretation:
library(sjPlot)
tab_model(BasicCor,expmodel,logmodel,poly_logY,Linear_logY,Linear_logXY)

#####Conclusion#####
#Linear model With LogX & logY  Has Higher Value Of R^2 #0.9996
















################RMSE#####################
sqrt(mean(BasicCor$residuals^2)) #3.997528
sqrt(mean(logmodel$residuals^2))#3.786004
sqrt(mean(expmodel$residuals^2))#0.04641748
sqrt(mean(poly_logY$residuals^2))#77.84029
sqrt(mean(Linear_logY$residuals^2))#0.2432379
sqrt(mean(Linear_logXY$residuals^2))#0.2055874




