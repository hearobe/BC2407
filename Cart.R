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
passexam.dt$Outcome <- factor(passexam.dt$Outcome)


# ============================ Severity_num ==============================
set.seed(6)

#Splitting into trainset and testset
train <- sample.split(Y = data$Severity_num, SplitRatio = 0.7)
train
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

#Checking the similarity of split of Y variable
summary(trainset$Severity_num)
summary(testset$Severity_num)

#Creating the max CART model for Severity_num.
cartMax <- rpart(Severity_num~. , data = trainset, method = 'class', control = rpart.control(minsplit = 10,cp = 0)) 

#rpart.plot(cartMax)

printcp(cartMax)
plotcp(cartMax, main = "Subtrees")

# ------------- Below is used to find the CP error Cap. --------

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

# ----------------------------------------------------

cart<- prune(cartMax,cp=cp.opt)
print(cart)
summary(cart)

rpart.plot(cart, nn = T, main = "Optimal Tree for Severity_num")

cart$variable.importance
scaledVarImpt <- round (100*cart$variable.importance/sum(cart$variable.importance))
scaledVarImpt

#----------------------------------- Accuracy ----------------------------------------------------

# Confusion Matrix on Trainset
cart.predict.train <- predict(cart, newdata = trainset, type = "class")

table.train <- table(Trainset.Actual = trainset$Severity_num, cart.predict.train, deparse.level = 2)
table.train
round(prop.table(table.train),3)

# Overall Accuracy
mean(cart.predict.train == trainset$Severity_num)


# Confusion Matrix on Testset
cart.predict.test <- predict(cart, newdata = testset, type = "class")

table.test <- table(Testset.Actual = testset$Severity_num, cart.predict.test, deparse.level = 2)
table.test
round(prop.table(table.test),3)

# Overall Accuracy
mean(cart.predict.test == testset$Severity_num)
