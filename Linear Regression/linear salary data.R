library(readr)
install.packages("readr")
Salary_Data <- read.csv("C:\\Users\\Ranjitha\\Downloads\\Salary_Data.csv")

View(Salary_Data) #view dataset

summary(Salary_Data)  #summary of dataset/EDA

#Scatterplot of input Vs otput
plot(Salary_Data$YearsExperience, Salary_Data$Salary)  # plot(X,Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#attached dataset
attach(Salary_Data)

#Correlation between output to input
cor(YearsExperience , Salary)          #cor(x,y)
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model
reg <- lm(Salary~ YearsExperience) # lm(Y ~ X)

#Summary of regression model
summary(reg)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is more than 0.80 so we can say that model is bestfit.
#We can write eq. as SH=25792+9450(YE)

#check fitted values(predicted)
reg$fitted.values
reg$residuals

 #but we have to check with predicted values
pred <- predict(reg)

#Check for error associated with each obs.
reg$residuals
sum(reg$residuals)

#check for mean of sum of errors is equal to 0
mean(reg$residuals)
hist(reg$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))  #RMSE
sqrt(mean(reg$residuals^2))

#interval for 5% of confidence
confint(reg,level=0.95)

predict(reg,interval="predict")



#Inferences-
#From all above  value or correlatio coe.r is 0.97 
# Coe. are significant and coe.of Determination value (R^2) is 0.957 
# mean of errors is -2.615537e-13 which is almost 0 ans errors are  normally distributed.
# RMSE value is 5592.044 
# so as model best fitted but we need to go with this transformation.

