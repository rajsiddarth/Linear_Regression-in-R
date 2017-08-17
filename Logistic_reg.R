
rm(list=ls(all=T))
library(RCurl)

data=read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Regression_in_R/master/FlierResponse.csv"),header = T,sep = ",")

str(data)

#Converting response to factors
data$Response = as.factor(data$Response)

#Building Logistic model

model_glm= glm(Response~Age, data = data, family = "binomial")
model_glm

summary(model_glm)

library(caTools)

# Randomly split the data into training and testing sets
set.seed(123)
split = sample.split(data$Response, SplitRatio = 0.70)

# Split up the data using subset
train = subset(data, split==TRUE)
test = subset(data, split==FALSE)

#Model on train
model_train= glm(Response~., data = train, family = "binomial")

#Accuracy on the training set
predict_train = predict(model_train, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$Response, predict_train > 0.5)

# Predictions on the test set
predict_test = predict(model_train, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$Response, predict_test > 0.5)

# AUC 
install.packages("ROCR")
library(ROCR)

ROCR_pred = prediction(predict_test, test$Response)
as.numeric(performance(ROCR_pred, "auc")@y.values)
ROCRperf=performance(ROCR_pred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
