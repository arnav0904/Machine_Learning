rm(list = ls(all.names = TRUE))
getwd()
setwd("C:/Users/Arnav/Desktop/Semester 3/ML/R")
install.packages("CaTools")
library(caTools)
install.packages("DAAG")
library(DAAG)
install.packages("car")
library(car)

#Read in Flier data
framingham = read.csv("framingham.csv")
framingham$education = factor(framingham$education)
str(framingham)
summary(framingham)

#Random split the data
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

#Split the data using subset
train = subset(framingham, split== TRUE)
test = subset(framingham, split== FALSE)


#Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
car::vif(framinghamLog)



#Accuracy of the training set
predictTrain = predict(framinghamLog, type = "response", newdata = train)
predictTrain

#Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)

#Accuracy TP +TN / TP +TN +FP +FN
(2169+29)/(2169+29+358+10)

#Precision TP / TP +FP
(2169)/(2169+358)

#Sensitivity aka Recall
(2169)/(2169+10)

#Specitifity
(29)/(29+358)


#Prediction on the test set
predictTest = predict(framinghamLog, type = 'response', newdata = test)



install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)


ROCRperf <- performance(ROCRpred,"tpr", "fpr")
par(mfrow=c(1,1)) 
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))





