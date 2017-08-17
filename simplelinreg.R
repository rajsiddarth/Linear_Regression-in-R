rm(list=ls(all=TRUE))

#install.packages("RCurl")

library(RCurl)

# Reading the data 
data = read.table(getURL("https://raw.githubusercontent.com/rajsiddarth/Regression_in_R/master/car_price_age.csv") ,header=T,sep=",")
names(data)

# Drop the Id, Model attributes
data = data[,-c(1,2)]

# Summary
str(data)
summary(data)

# correlation between the attributes
cor(data)

plot(data$Age_06_15, data$Price)

# Linear regression model
LinReg = lm(Price~Age_06_15, data=data)
coefficients(LinReg)

# summary of model
summary(LinReg)

#Estimate 'Price', if 'Age_06_15' is 60
as.numeric(coefficients(LinReg)[1] + coefficients(LinReg)[2]*60)

# Using the predictions
testdata = data.frame(Age_06_15=60)
testdata
predict(LinReg, testdata)

#Confidence Intervals at 0.95 for Price, if 'Age_06_15' is 60
#confidence limits value for the predicted value at 0.95 significance level
predict(LinReg, testdata, interval="confidence",level=0.95)


# Confidence Intervals talk about the average values intervals
# Prediction Intervals talk about the all individual values intervals
Pred<-data.frame(predict(LinReg, data, interval="confidence",level=0.95))
Pred1<-data.frame(predict(LinReg, data, interval="prediction",level=0.95))

names(Pred)

# plot the residuals
plot(data$Age_06_15, LinReg$residuals)

resid(LinReg)
LinReg$residuals

plot(data$Age_06_15, resid(LinReg), 
     ylab="Residuals", xlab="Age_06_15", 
     main="Residuals plot") 
abline(0, 0)                  # the horizon

# plot for the original data
plot(data$Age_06_15,data$Price)
# Plotting data, fitted line and confidence limits

points(data$Age_06_15,Pred$fit,type="l", col="red", lwd=2)
points(data$Age_06_15,Pred$lwr,pch="-", col="red", lwd=4)
points(data$Age_06_15,Pred$upr,pch="-", col="red", lwd=4)
points(data$Age_06_15,Pred1$lwr,pch="-", col="blue", lwd=4)
points(data$Age_06_15,Pred1$upr,pch="-", col="blue", lwd=4)

plot(LinReg)

# Error metrics evaluation on train data and test data

########################################################

rm(list=ls(all=TRUE))
data<-read.csv("Toyota_SimpleReg.csv",header=T)
names(data)
# drop the Id, Model attributes
data = data[,-c(1,2)]
str(data)
summary(data)


#Split the data into train and test data sets
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,]

# Linear regression model
LinReg1<-lm(Price~Age_06_15,data=train)
summary(LinReg1)
library(DMwR)
#Error verification on train data
regr.eval(train$Price, LinReg1$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg1,test)
regr.eval(test$Price, Pred)