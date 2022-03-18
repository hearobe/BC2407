library(neuralnet)
library(nnet)

data.clean=read.csv("Training_clean.csv")

data.clean$l1 = ifelse(data.clean$Severity == "A&E", 1, 0)
data.clean$l2 = ifelse(data.clean$Severity == "Polyclinic", 1, 0)
data.clean$l3 = ifelse(data.clean$Severity == "No_Medical_Attention_Req", 1, 0)
data.clean$Severity_num = ifelse(data.clean$Severity == "A&E", 1,
                                 ifelse(data.clean$Severity == "Polyclinic", 2,
                                        ifelse(data.clean$Severity == "No_Medical_Attention_Req", 3, 3)))

data.clean = data.frame(data.clean)

data = subset(data.clean, select = -c(Severity))

# Model 1
m1 = neuralnet(l1+l2+l3~., data = data, hidden=2,
               act.fct='logistic', linear.output = FALSE)

par(mfrow=c(1,1))
plot(m1)

m1$net.result
m1$result.matrix  # summary
m1$startweights
m1$weights
m1$generalized.weights


# Generalized weights
par(mfrow=c(2,2))
gwplot(m1,selected.covariate="itching", min=-2.5, max=5)
gwplot(m1,selected.covariate="combined_yellowskin_yelloweye_darkurine", min=-2.5, max=5)
gwplot(m1,selected.covariate="acute_liver_failure", min=-2.5, max=5)
gwplot(m1,selected.covariate="fatigue", min=-2.5, max=5)

pred.m1 = max.col(data.frame(m1$net.result))
table(data.clean$Severity_num, pred.m1)

# Model 2
m2 = neuralnet(l1+l2+l3~., data = data, hidden=c(20,10,3),
               act.fct='logistic', linear.output = FALSE)
pred.m2 = max.col(data.frame(m2$net.result))
table(data.clean$Severity_num, pred.m2)

# Model 3
m3 = neuralnet(l1+l2+l3~., data = data, hidden=c(100,75,50,25,12,6,3),
               act.fct='logistic', linear.output = FALSE)
pred.m3 = max.col(data.frame(m3$net.result))
table(data.clean$Severity_num, pred.m3)



# Run on test dataset
data.test = read.csv("Testing_clean.csv")
data.test = data.frame(data.test)
data.test$Severity_num = ifelse(data.test$severity == "A&E", 1,
                                ifelse(data.test$severity == "Polyclinic", 2,
                                       ifelse(data.test$severity == "No_Medical_Attention_Req", 3, 3)))

test.m1 = compute(m1, data.test)
pred.test.m1 = max.col(data.frame(test.m1$net.result))
table(data.test$Severity_num,pred.test.m1)

test.m2 = compute(m2, data.test)
pred.test.m2 = max.col(data.frame(test.m2$net.result))
table(data.test$Severity_num,pred.test.m2)

test.m3 = compute(m3, data.test)
pred.test.m3 = max.col(data.frame(test.m3$net.result))
table(data.test$Severity_num,pred.test.m3)


# ----------------Cross-Validation and Largest Testset------------------

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

data.clean = data.frame(trainset)
data.clean = data.frame(testset)
trainset$l1 = ifelse(trainset$Severity_num == 1, 1, 0)
trainset$l2 = ifelse(trainset$Severity_num == 2, 1, 0)
trainset$l3 = ifelse(trainset$Severity_num == 3, 1, 0)

testset$l1 = ifelse(testset$Severity_num == 1, 1, 0)
testset$l2 = ifelse(testset$Severity_num == 2, 1, 0)
testset$l3 = ifelse(testset$Severity_num == 3, 1, 0)

trainset.noS = subset(trainset, select = -c(Severity_num))
testset.noS = subset(testset, select = -c(Severity_num))


m4 = neuralnet(l1+l2+l3~., data = trainset.noS, hidden=2,
               act.fct='logistic', linear.output = FALSE)

pred.m4 = max.col(data.frame(m4$net.result))
table(trainset$Severity_num, pred.m4)

#Test the resulting output
temp_test <- subset(testset, select = -c(l1, l2, l3, Severity_num))
head(temp_test)
m4.test.results <- compute(m4, testset.noS)
results <- data.frame(actual = testset$dividend, prediction = nn.results$net.result)

pred.m4.test = max.col(data.frame(m4.test.results$net.result))
table(testset$Severity_num, pred.m4.test)
