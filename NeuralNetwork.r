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