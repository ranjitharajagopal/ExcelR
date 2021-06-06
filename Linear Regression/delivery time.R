library(readr)
install.packages("readr")
Delivery_time <- read.csv("C:\\Users\\Ranjitha\\Downloads\\Delivery_time.csv")

View(Delivery_time) #view dataset

summary(Delivery_time)  #summary of dataset/EDA

#Scatterplot of input Vs otput
plot(Delivery_time$Delivery.Time, Delivery_time$Sorting.Time)

#attached dataset
attach(Delivery_time)

#Correlation between output to input
cor.test(Delivery.Time , Sorting.Time)

# Simple Linear Regression model
reg <- lm(Delivery.Time~ Sorting.Time) # lm(Y ~ X)
summary(reg)

#check fitted values(predicted)
reg$fitted.values
reg$residuals


pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
hist(reg$residuals)
#Check for RMSE value
sqrt(sum(reg$residuals^2)/nrow(Delivery_time))  #RMSE
sqrt(mean(reg$residuals^2))
#interval for 5% of confidence
confint(reg,level=0.95)
predict(reg,interval="predict")

library(ggplot2)

ggplot(data = Delivery_time, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Delivery_time, aes(x=Sorting.Time , y=pred))




