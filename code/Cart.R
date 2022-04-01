library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)

data.clean=read.csv("Training_clean.csv")

data.clean$Severity_num = ifelse(data.clean$Severity == "A&E", 1,
                                 ifelse(data.clean$Severity == "Polyclinic", 2,
                                        ifelse(data.clean$Severity == "No_Medical_Attention_Req", 3, 3)))

data.clean = data.frame(data.clean)

data = subset(data.clean, select = -c(Severity))


# ============================ Severity_num ==============================

#Creating the max CART model for Severity_num.
cartMax <- rpart(Severity_num~. , data = data, method = 'class', control = rpart.control(minsplit = 2,cp = 0)) 

rpart.plot(cartMax)

printcp(cartMax)
plotcp(cartMax, main = "Subtrees")

# ------------- Below is used to find the CP error Cap. --------------------------

#store the cptable
dt <- data.table(cartMax$cptable)

#number the sequence of the trees 
dt[, index := 1:nrow(dt)]

#find out minimum index where xerror is min
min_cp_index <- min(dt[(xerror == min(xerror)), index])

#find the errorcap
errorcap <- dt[min_cp_index, xerror+xstd]

#find out the optimal index for the cp
optimal_cp_index <- min(dt[(xerror < errorcap), index])

#Find the geometric mean of the cp for that index and one cp appearing before it 
cp.opt = sqrt(dt[index ==optimal_cp_index , CP]*dt[index == optimal_cp_index -1, CP])
cp.opt

----------------------------------------------------------------------------------------------
  #Compute optimal CP on model
  ## Compute min CVerror + 1SE in maximal tree cart_max.
  CVerror.cap <- cartMax$cptable[which.min(cartMax$cptable[,"xerror"]), "xerror"] + cartMax$cptable[which.min(cartMax$cptable[,"xerror"]), "xstd"]
## Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_max.
i <- 1; j<- 4
while (cartMax$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cartMax$cptable[i,1] * cartMax$cptable[i-1,1]), 1)
cp.opt
# --------------------------------- Pruning ----------------------------------------------------

cart<- prune(cartMax,cp=cp.opt)
print(cart)
summary(cart)

rpart.plot(cart, nn = T, main = "Optimal Tree for Severity_num")

cart$variable.importance
scaledVarImpt <- round (100*cart$variable.importance/sum(cart$variable.importance))
scaledVarImpt

#----------------------------------- Accuracy ----------------------------------------------------

# Confusion Matrix on Trainset
cart.predict.train <- predict(cart, newdata = data, type = "class")

library(caret)
confusion_matrix_train <- table(data[,ncol(data)], cart.predict.train, deparse.level = 2)
confusion_matrix_train
confusionMatrix(confusion_matrix_train)
# Accuracy : 1



# Confusion Matrix on Testset
data.test=read.csv("Testing_clean.csv")

data.test$Severity_num = ifelse(data.test$severity == "A&E", 1,
                                ifelse(data.test$severity == "Polyclinic", 2,
                                       ifelse(data.test$severity == "No_Medical_Attention_Req", 3, 3)))

data.test = data.frame(data.test)

data1 = subset(data.test, select = -c(severity))


cart.predict.test <- predict(cart, newdata = data1, type = "class")

confusion_matrix_test <- table(data.test[,ncol(data.test)], cart.predict.test, deparse.level = 2)
confusion_matrix_test
confusionMatrix(confusion_matrix_test)
# Accuracy : 0.9762


# ------------------------------------------------
# ----------------70-30 Testset-------------------
# ------------------------------------------------

library(caTools)

train.clean=read.csv("Training_clean.csv")
test.clean=read.csv("Testing_clean.csv")
names(test.clean)[names(test.clean) == "severity"] <- "Severity"

train.clean$Severity_num = ifelse(train.clean$Severity == "A&E", 1,
                                  ifelse(train.clean$Severity == "Polyclinic", 2,
                                         ifelse(train.clean$Severity == "No_Medical_Attention_Req", 3, 3)))
test.clean$Severity_num = ifelse(test.clean$Severity == "A&E", 1,
                                 ifelse(test.clean$Severity == "Polyclinic", 2,
                                        ifelse(test.clean$Severity == "No_Medical_Attention_Req", 3, 3)))

train.clean = data.frame(train.clean)
test.clean = data.frame(test.clean)

data = rbind(train.clean, test.clean)
data = subset(data, select = -c(Severity))



set.seed(2)

train <- sample.split(Y = data$Severity_num, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

cartMax <- rpart(Severity_num~. , data = trainset, method = 'class', control = rpart.control(minsplit = 2,cp = 0)) 

----------------------------------------------------------------------------------------------
  #Compute optimal CP on model
  ## Compute min CVerror + 1SE in maximal tree cart_max.
  CVerror.cap <- cartMax$cptable[which.min(cartMax$cptable[,"xerror"]), "xerror"] + cartMax$cptable[which.min(cartMax$cptable[,"xerror"]), "xstd"]
## Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_max.
i <- 1; j<- 4
while (cartMax$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cartMax$cptable[i,1] * cartMax$cptable[i-1,1]), 1)
cp.opt
# --------------------------------- Pruning ----------------------------------------------------

cart<- prune(cartMax,cp=cp.opt)

summary(cart)

# Confusion Matrix on Trainset
cart.predict.train <- predict(cart, newdata = trainset, type = "class")

library(caret)
confusion_matrix_train <- table(trainset[,ncol(trainset)], cart.predict.train, deparse.level = 2)
confusion_matrix_train
confusionMatrix(confusion_matrix_train)
# Accuracy : 1



# Confusion Matrix on Testset

test_temp = subset(testset, select = -c(Severity_num))


cart.predict.test <- predict(cart, newdata = test_temp, type = "class")

confusion_matrix_test <- table(testset[,ncol(testset)], cart.predict.test, deparse.level = 2)
confusion_matrix_test
confusionMatrix(confusion_matrix_test)

