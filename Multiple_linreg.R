
# multiple linear regression model building
rm(list=ls(all=T))

CrudeOilOutput <- read.csv("CrudeOilOutput.csv", header = T, sep = ",")
CrudeOilOutput

# Check for correlations
correlation <- cor(CrudeOilOutput)
correlation
plot(CrudeOilOutput)

CrudeOilOutputlm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy
                       + CrudeOilOutput$USAutoFuelRate
                       + CrudeOilOutput$USNuclear + CrudeOilOutput$USCoal
                       + CrudeOilOutput$USDryGas, CrudeOilOutput)
summary(CrudeOilOutputlm)
par(mfrow=c(2,2))
plot(CrudeOilOutputlm)

# Load required libraries
library(MASS)
library(car)

# Use stepAIC to build model based on AIC
stepAIC(CrudeOilOutputlm, direction = "both")
CrudeOilOutputStepAIC <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy + 
                              CrudeOilOutput$USAutoFuelRate + CrudeOilOutput$USCoal, data = CrudeOilOutput)
CrudeOilOutputStepAIC
summary(CrudeOilOutputStepAIC)
plot(CrudeOilOutputStepAIC)
vif(CrudeOilOutputlm)

ToxinConc <- read.csv("FungalToxinContamination.csv", header = T, sep = ",")
ToxinConc
correlation <- cor(ToxinConc)
correlation
plot(ToxinConc)
ToxinConclm <- lm(ToxinConc$Toxin ~ ToxinConc$Rain
                  + ToxinConc$NoonTemp
                  + ToxinConc$Sunshine + ToxinConc$WindSpeed
                  , ToxinConc)
summary(ToxinConclm)

par(mfrow=c(2,2))
plot(ToxinConclm)
ToxinConclm1 <- stepAIC(ToxinConclm, direction = "both")
ToxinConclm1 <- lm(ToxinConc$Toxin ~ ToxinConc$Rain
                   + ToxinConc$NoonTemp
                   + ToxinConc$WindSpeed
                   , ToxinConc)
summary(ToxinConclm1)
plot(ToxinConclm1)
car::vif(ToxinConclm)
car::vif(ToxinConclm1)


# CASE STUDY - MONEYBALL (OAKLAND A's)

# Read in Moneyball data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include Moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Scatterplot of teams and their wins
plot(moneyball$W, moneyball$Team, 
     col=ifelse(moneyball$Playoffs < 1, 'black','red'), pch=20, cex=2,
     abline(v=95, col='blue', lwd=3))

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# install.packages("lattice")
library(lattice)

# Scatterplot to check for linear relationship
xyplot(moneyball$W~moneyball$RD, type=c('p', 'smooth'), col='steelblue2',
       cex=1.5, col.line='red', pch=19, lwd=2,
       ylab=list(label='Wins', cex=2), xlab=list(label='Run Difference',cex=2),
       scales=list(x=list(cex=2), y=list(cex=2)))

# Correlation between Wins and Run Difference
cor(moneyball$W, moneyball$RD)
car::vif(moneyballlm)
# Model Building for Wins
MoneyballlmStep1a <- lm(moneyball$W ~ moneyball$RD)
summary(MoneyballlmStep1a)
MoneyballlmStep1b <- lm(moneyball$W ~ moneyball$OBP)
summary(MoneyballlmStep1b)
MoneyballlmStep1c <- lm(moneyball$W ~ moneyball$SLG)
summary(MoneyballlmStep1c)
MoneyballlmStep1d <- lm(moneyball$W ~ moneyball$BA)
summary(MoneyballlmStep1d)
MoneyballlmStep1e <- lm(moneyball$W ~ moneyball$OOBP)
summary(MoneyballlmStep1e)
MoneyballlmStep1f <- lm(moneyball$W ~ moneyball$OSLG)
summary(MoneyballlmStep1f)

MoneyballlmStep2a <- lm(moneyball$W ~ moneyball$RD + moneyball$OBP)
summary(MoneyballlmStep2a)
stepAIC(MoneyballlmStep2a)
MoneyballlmStep2b <- lm(moneyball$W ~ moneyball$RD + moneyball$SLG)
summary(MoneyballlmStep2b)
MoneyballlmStep2c <- lm(moneyball$W ~ moneyball$RD + moneyball$BA)
summary(MoneyballlmStep2c)
MoneyballlmStep2d <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP)
summary(MoneyballlmStep2d)
MoneyballlmStep2e <- lm(moneyball$W ~ moneyball$RD + moneyball$OSLG)
summary(MoneyballlmStep2e)

MoneyballlmStep3a <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$OBP)
summary(MoneyballlmStep3a)
MoneyballlmStep3b <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$SLG)
summary(MoneyballlmStep3b)
MoneyballlmStep3c <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$BA)
summary(MoneyballlmStep3c)
MoneyballlmStep3d <- lm(moneyball$W ~ moneyball$RD + moneyball$OOBP + moneyball$OSLG)
summary(MoneyballlmStep3d)

moneyballlm <- lm(W ~ RD + SLG + OBP + BA + OOBP + OSLG, data = moneyball)
moneyballlm

stepAIC(moneyballlm)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# Regression model to predict runs scored
RunsScoredReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsScoredReg)

RunsScoredReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsScoredReg)

# Regression model to predict runs allowed
RunsAllowedReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAllowedReg)


# LOGISTIC REGRESSION

# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)

flierresponseglm <- glm(Response~1, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)

# CASE STUDY - The Framingham Heart Study

# Read in the Framingham dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

#Accuracy on the training set
predictTrain = predict(framinghamLog, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)

# Accuracy on Train Set
(2170+30)/(2170+30+357+9)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy on Test Set
(915+12)/(915+12+158+7)

# Confusion matrix with threshold of 0.9
table(test$TenYearCHD, predictTest > 0.9)
# Confusion matrix with threshold of 0.7
table(test$TenYearCHD, predictTest > 0.7)
# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
# Confusion matrix with threshold of 0.3
table(test$TenYearCHD, predictTest > 0.3)
# Confusion matrix with threshold of 0.1
table(test$TenYearCHD, predictTest > 0.1)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
