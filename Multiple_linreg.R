
# multiple linear regression model building
rm(list=ls(all=T))
library(RCurl)

data=read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Regression_in_R/master/CrudeOilOutput.csv"), header = T, sep = ",")

str(data)

#Correlation
cor(data)
plot(data)

#install.packages("MASS")
#install.packages("car")

library(MASS)
library(car)

lm_model=lm(data$WorldOil~.,data=data)
# Using stepAIC to build model based on AIC
stepAIC(lm_model, direction = "both")

#Choosing variables from stepAIC
model_AIC=lm(data$WorldOil ~ data$USEnergy+data$USAutoFuelRate + data$USCoal, data = data)
model_AIC
plot(model_AIC)

vif(model_AIC)


