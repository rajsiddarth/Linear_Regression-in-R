rm(list=ls(all=TRUE))

#install.packages("RCurl")
library(RCurl)

# Reading the data 
data = read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Regression_in_R/master/car_price_age.csv") ,header=T,sep=",")

names(data)
str(data)
summary(data)

# correlation between the attributes
cor(data)
colnames(data)[2]="age"

#Dependent variable=Price, Ind variable=age
plot(data$age, data$Price)

# Linear regression model in one variable
lm_model = lm(formula = Price~age, data=data)
summary(lm_model)

# plotting Confidence Intervals 
# plotting Prediction Intervals 
conf_int=data.frame(predict(lm_model, data, interval="confidence",level=0.95))
pred_int=data.frame(predict(lm_model, data, interval="prediction",level=0.95))

# Residual  plot
plot(data$age, lm_model$residuals)
plot(data$age, resid(lm_model),ylab="Residuals", xlab="age",main="Residuals plot") 
abline(0, 0)                  # the horizon



plot(data$age,data$Price)
# Plotting data, fitted line and confidence limits

points(data$age,conf_int$fit,type="l", col="red", lwd=2)
points(data$age,conf_int$lwr,pch="-", col="red", lwd=4)
points(data$age,conf_int$upr,pch="-", col="red", lwd=4)
points(data$age,pred_int$lwr,pch="-", col="blue", lwd=4)
points(data$age,pred_int$upr,pch="-", col="blue", lwd=4)
plot(lm_model)

# Error metrics evaluation on train data and test data

#Split the data into train and test data sets
rows=seq(1,nrow(data),1)
set.seed(50)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,]

# Linear regression model on train data 
lm_train=lm(Price~age,data=train)
summary(lm_train)
#
install.packages("DMwR")
library(DMwR)

#Error verification on train data
regr.eval(train$Price, lm_train$fitted.values) 

#Error verification on test data
pred_test=predict(lm_train,test)
regr.eval(test$Price, pred_test)
