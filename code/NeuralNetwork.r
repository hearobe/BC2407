library(neuralnet)
library(caTools)
library(caret)


############# Import data

train.clean=read.csv("data/Training_clean.csv")
test.clean=read.csv("data/Testing_clean.csv")
names(test.clean)[names(test.clean) == "severity"] <- "Severity"

data = rbind(train.clean, test.clean)

data$Severity_num = ifelse(data$Severity == "A&E", 1,
                                  ifelse(data$Severity == "Polyclinic", 2,
                                         ifelse(data$Severity == "No_Medical_Attention_Req", 3, 3)))
data = subset(data, select = -c(Severity))

data$l1 = ifelse(data$Severity_num == 1, 1, 0)
data$l2 = ifelse(data$Severity_num == 2, 1, 0)
data$l3 = ifelse(data$Severity_num == 3, 1, 0)

set.seed(2)

train <- sample.split(Y = data$Severity_num, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

trainset.noS = subset(trainset, select = -c(Severity_num))
testset.noS = subset(testset, select = -c(Severity_num))


############# Create Neural Network Models

# Model 1 - 1 layer, 2 nodes
m1 = neuralnet(l1+l2+l3~., data = trainset.noS, hidden=2,
               act.fct='logistic', linear.output = FALSE)

pred.m1 = max.col(data.frame(m1$net.result))
cmat.train.m1 = table(trainset$Severity_num, pred.m1)
confusionMatrix(cmat.train.m1)

par(mfrow=c(1,1))
plot(m1)

m1$net.result
m1$result.matrix  # summary
m1$startweights
m1$weights
m1$generalized.weights

par(mfrow=c(2,2))
gwplot(m1,selected.covariate="itching", min=-2.5, max=5)
gwplot(m1,selected.covariate="combined_yellowskin_yelloweye_darkurine", min=-2.5, max=5)
gwplot(m1,selected.covariate="acute_liver_failure", min=-2.5, max=5)
gwplot(m1,selected.covariate="fatigue", min=-2.5, max=5)



# Model 2 - 3 layers with 20,10 and 3 nodes
m2 = neuralnet(l1+l2+l3~., data = trainset.noS, hidden=c(20,10,3),
               act.fct='logistic', linear.output = FALSE)
pred.m2 = max.col(data.frame(m2$net.result))
cmat.train.m2 = table(trainset$Severity_num, pred.m2)
confusionMatrix(cmat.train.m2)


# Model 3 - 7 layers with 100,75,50,25,12,6 and 3 nodes
m3 = neuralnet(l1+l2+l3~., data = trainset.noS, hidden=c(100,75,50,25,12,6,3),
               act.fct='logistic', linear.output = FALSE)
pred.m3 = max.col(data.frame(m3$net.result))
cmat.train.m3 = table(trainset$Severity_num, pred.m3)
confusionMatrix(cmat.train.m3)


######## Compare Neural Network Models using Test Set

test.m1 = compute(m1, testset)
pred.test.m1 = max.col(data.frame(test.m1$net.result))
cmat.test.m1 = table(testset$Severity_num, pred.test.m1)
confusionMatrix(cmat.test.m1)


test.m2 = compute(m2, testset)
pred.test.m2 = max.col(data.frame(test.m2$net.result))
cmat.test.m2 = table(testset$Severity_num, pred.test.m2)
confusionMatrix(cmat.test.m2)


test.m3 = compute(m3, testset)
pred.test.m3 = max.col(data.frame(test.m3$net.result))
cmat.test.m3 = table(testset$Severity_num, pred.test.m3)
confusionMatrix(cmat.test.m3)
