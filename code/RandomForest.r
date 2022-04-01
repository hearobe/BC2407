
#--------- Random Forest -----------
library(randomForest)

#Prepping the data
data.clean=read.csv("data/Training_clean.csv", stringsAsFactors = TRUE, strip.white = TRUE)
data.clean.fac<-data.frame(data.clean)
for(i in 1:ncol(data.clean.fac)){ #Factorise all the columns
  data.clean.fac[,i] <- as.factor(data.clean.fac[,i])
}
summary(data.clean.fac)
sum(is.na(data.clean.fac)) #No missing data


#Training the model
set.seed(1) 
m.RF.1 <- randomForest(Severity ~ . , data = data.clean.fac, importance = T) #Default B=500, RSF= floor(sqrt(ncol(x))) since categorical Y
m.RF.1
## OOB Error rate ("Test Set error") = 0.28%
## Very low OOB Error rate, sow signs of high predictive accuracy (Different from train set error)

##Check to ensure that B chosen is large enough
## The error stabilised when B is about 50, so the default B=500 is large enough as error has already stabilised
plot(m.RF.1)

##Variable Importance
## Sweating (MDA=26.46), altered_sensorium(MDA=22.175) and chest_pain(MDA=22.11) are the 3 most important variables as they have the highest MeanDecreaseAccuracy(How much accuracy the model losses by excluding each variable)
## Small_dents_in_nails (MDA=1.42), irritation_in_anus (MDA= 1.24) and fluid_overload (MDA=0.000) are the 3 least important variables
var.impt <- importance(m.RF.1)
var.impt<-data.frame(var.impt,key="MeanDecreaseAccuracy")
var.impt<-var.impt[order(var.impt[,4],decreasing=TRUE),]
var.impt
varImpPlot(m.RF.1, type = 1)


#Train Set error
##Prediction
y.train.predict = predict(m.RF.1, newdata = data.clean.fac[-ncol(data.clean.fac)])
y.train.predict

##Confusion matrix
library(caret)
confusion_matrix_train<-table(data.clean.fac[,ncol(data.clean.fac)], y.train.predict, deparse.level = 2)
confusion_matrix_train
confusionMatrix(confusion_matrix_train)
### Train set accuracy rate is 0.9976
###But not as meaningful because the trees in Random Forest are grown to maximum to maintain accuracy as Random Subset Feature increase diversity of model at the expense of accuracy (Controlled instability in Random Forest)
###Hence likely there is only one data point in most terminal nodes, which would therefore always result in very high trainset accuracy rate in random forest
### https://stats.stackexchange.com/questions/162353/what-measure-of-training-error-to-report-for-random-forests


#Test Set Error
##Prepping test set data
data.test.clean = read.csv("data/Testing_clean.csv", stringsAsFactors = TRUE, strip.white = TRUE)
summary(data.test.clean)
for(i in 1:ncol(data.test.clean)){ #Factorise all the columns
  data.test.clean[,i] <- as.factor(data.test.clean[,i])
}
summary(data.test.clean)

##Prediction
y.predict = predict(m.RF.1, newdata = data.test.clean[-ncol(data.test.clean)])
y.predict

##Confusion matrix
library(caret)
confusion_matrix<-table(data.test.clean[,ncol(data.test.clean)], y.predict, deparse.level = 2)
confusion_matrix
confusionMatrix(confusion_matrix)
### Test set accuracy rate is 0.9762


# ------------------------------------------------
# ----------------70-30 Testset-------------------
# ------------------------------------------------

library(randomForest)
library(caTools)

train.clean=read.csv("data/Training_clean.csv")
test.clean=read.csv("data/Testing_clean.csv")
names(test.clean)[names(test.clean) == "severity"] <- "Severity"

train.clean = data.frame(train.clean)
test.clean = data.frame(test.clean)

data = rbind(train.clean, test.clean)

for(i in 1:ncol(data)){ #Factorise all the columns
  data[,i] <- as.factor(data[,i])
}
summary(data)
sum(is.na(data)) #No missing data

set.seed(2)

train <- sample.split(Y = data$Severity, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

set.seed(1) 
m.RF.2 <- randomForest(Severity ~ . , data = trainset, importance = T) #Default B=500, RSF= floor(sqrt(ncol(x))) since categorical Y
m.RF.2

#Train Set error
##Prediction
y2.train.predict = predict(m.RF.2, newdata = trainset[-ncol(trainset)])
y2.train.predict

##Confusion matrix
library(caret)
confusion_matrix_train2<-table(trainset[,ncol(trainset)], y2.train.predict, deparse.level = 2)
confusion_matrix_train2
confusionMatrix(confusion_matrix_train2)

##Prediction
y2.predict = predict(m.RF.2, newdata = testset[-ncol(testset)])
y2.predict

##Confusion matrix
library(caret)
confusion_matrix2<-table(testset[,ncol(testset)], y2.predict, deparse.level = 2)
confusion_matrix2
confusionMatrix(confusion_matrix2)