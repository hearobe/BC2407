library(data.table)
library(arules)
library(arulesViz)
library("reshape2")
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plyr) 
library(ggstatsplot)
library(ggcorrplot)
library(lares)
library(correlation)
library('psych')
library(janitor)


##################################################################################
#Using association rule to reduce variables
##################################################################################

#----------------------------Importing and cleaning the data--------------------------
data = read.csv("data/training.csv")

# Separate diagnosis and symptoms - only looking for associations between symptoms
diagnosis = subset(data, select = prognosis)
symptoms = subset(data, select = -c(prognosis, X))
symptoms = data.frame(lapply(symptoms, as.logical))

# Convert to transactions datatype
# symptomsTrans <- as(symptoms, "transactions")  
# symptomsTrans.df<-as(symptomsTrans,"data.frame")
# inspect(symptomsTrans)

#-----Converting with wide data format with factor variables
symptomsFac<-data.frame(symptoms)
symptomsFac[] <- lapply(symptomsFac, factor)
symptomsTransFac <- as(symptomsFac, "transactions")  
symptomsTransFac.df<-as(symptomsTransFac,"data.frame")
inspect(symptomsTransFac)

# # Generating rules at support 0.1: 131 rules
# rules1 <- apriori(data = symptomsTrans, parameter = 
#                     list(minlen = 2, supp=0.1, conf = 0.1, target = "rules"))
# summary(rules1)
# inspect(head(rules1, n = 10, by ="lift"))
# rule.table1 <- inspect(rules1)
# 
# # cleaning redundant rules from rules1
# # 102 left, minimum count 510
# sum(!is.redundant(rules1))
# rules1.clean = rules1[!is.redundant(rules1)]
# rules1.clean.df = as(rules1.clean, "data.frame")
# inspect(head(rules1.clean, n = 10, decreasing = FALSE, by ="count"))
# 
# 
# # Generating rules at support 0.01: 1630161 rules
# # minimum count is 66
# rules2 <- apriori(data = symptomsTrans, parameter = 
#                     list(minlen = 2, supp=0.01, conf = 0.1, target = "rules"))
# 
# # Too big, takes a very long time to run. Refer to cleaned df rules2.clean.df below instead
# # rule.table2 <- inspect(rules2)
# inspect(head(rules2, n = 10, decreasing = TRUE, by ="lift"))


# # 10227 not redundant rules, min count 78
# sum(is.redundant(rules2))
# rules2.clean = rules2[!is.redundant(rules2)]
# rules2.clean.df = as(rules2.clean, "data.frame")
# inspect(head(rules2.clean, n = 10, decreasing = FALSE, by ="count"))

#----------------------------Experiment with different hyperparamter to find optimum--------------------------

############## Generating rules at support 0.1: 72,422,843 rules
rules3 <- apriori(data = symptomsTransFac, parameter = 
                    list(minlen = 2, supp=0.1, conf = 0.1, target = "rules"))
summary(rules3)
inspect(head(rules3, n = 10, by ="lift"))

# cleaning redundant rules from rules1
# 2,930,561 unredundant rules
sum(!is.redundant(rules3))
rules3.clean = rules3[!is.redundant(rules3)]
rules3.clean.df = as(rules3.clean, "data.frame")
inspect(head(rules3.clean, n = 10, decreasing = FALSE, by ="count"))

#Filter to find key rules
##Filtering to find rules with 1 item in antecedent. Then  rules with a true->b true, a false->b false
##21,952 rules have 1 item in their antecedent
rules3.singleante.df=rules3.clean.df[!grepl(",", rules3.clean.df[["rules"]]), ]
rules3.singleante2.df<-rules3.singleante.df %>% separate(rules, c("antecedent", "consequent"), " => ")

#Formating the dataframe to atomize the rules to antecedent and consequent
##Formating Antecedent
rules3.singleante3.df<-rules3.singleante2.df[(grepl("TRUE", rules3.singleante2.df[["antecedent"]]) & grepl("TRUE", rules3.singleante2.df[["consequent"]])) | (grepl("FALSE", rules3.singleante2.df[["antecedent"]]) & grepl("FALSE", rules3.singleante2.df[["consequent"]])), ]
rules3.singleante3.df<-rules3.singleante3.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules3.singleante3.df$antecedent=gsub("\\{","",rules3.singleante3.df$antecedent)
rules3.singleante3.df$antecedent_logic=gsub("\\}","",rules3.singleante3.df$antecedent_logic)
##Formating Consequent
rules3.singleante3.df<-rules3.singleante3.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules3.singleante3.df$consequent=gsub("\\{","",rules3.singleante3.df$consequent)
rules3.singleante3.df$consequent_logic=gsub("\\}","",rules3.singleante3.df$consequent_logic)


#Filtering to find rules with only true -> true or false->false
key_rules1<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules3.singleante3.df$antecedent)){
  tmp.df<-rules3.singleante3.df[rules3.singleante3.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules1 <- rbind(key_rules1, rules3.singleante3.df[(grepl(tmp.df[row_no,"antecedent"],rules3.singleante3.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules3.singleante3.df[["consequent"]])),])
    }
  }
}
key_rules1

############### Generating rules at support 0.1 and conf 0.9: 56,571,092 rules
rules4 <- apriori(data = symptomsTransFac, parameter = 
                    list(minlen = 2, supp=0.1, conf = 0.9, target = "rules"))
summary(rules4)
inspect(head(rules4, n = 10, by ="lift"))
rule.table4 <- inspect(rules4)

# cleaning redundant rules from rules1
# 71,854 unredundant rules
sum(!is.redundant(rules4))
rules4.clean = rules4[!is.redundant(rules4)]
rules4.clean.df = as(rules4.clean, "data.frame")
inspect(head(rules4.clean, n = 10, decreasing = FALSE, by ="count"))

#Filter to find key rules
##Filtering to find rules with 1 item in antecedent. Then  rules with a true->b true, a false->b false
##16,749 rules have 1 item in their antecedent
rules4.singleante.df=rules4.clean.df[!grepl(",", rules4.clean.df[["rules"]]), ]
rules4.singleante2.df<-rules4.singleante.df %>% separate(rules, c("antecedent", "consequent"), " => ")

#Formating the dataframe to atomize the rules to antecedent and consequent
##Formating Antecedent
rules4.singleante3.df<-rules4.singleante2.df[(grepl("TRUE", rules4.singleante2.df[["antecedent"]]) & grepl("TRUE", rules4.singleante2.df[["consequent"]])) | (grepl("FALSE", rules4.singleante2.df[["antecedent"]]) & grepl("FALSE", rules4.singleante2.df[["consequent"]])), ]
rules4.singleante3.df<-rules4.singleante3.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules4.singleante3.df$antecedent=gsub("\\{","",rules4.singleante3.df$antecedent)
rules4.singleante3.df$antecedent_logic=gsub("\\}","",rules4.singleante3.df$antecedent_logic)
##Formating Consequent
rules4.singleante3.df<-rules4.singleante3.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules4.singleante3.df$consequent=gsub("\\{","",rules4.singleante3.df$consequent)
rules4.singleante3.df$consequent_logic=gsub("\\}","",rules4.singleante3.df$consequent_logic)


#Filtering to find rules with only true -> true or false->false
key_rules2<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules4.singleante3.df$antecedent)){
  tmp.df<-rules4.singleante3.df[rules4.singleante3.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules2 <- rbind(key_rules2, rules4.singleante3.df[(grepl(tmp.df[row_no,"antecedent"],rules4.singleante3.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules4.singleante3.df[["consequent"]])),])
    }
  }
}
key_rules2

############### Same as rules3, but limit maxlen of rules to 2
# 21952 rules 
rules5 = apriori(data = symptomsTransFac, parameter = 
                   list(minlen = 2, maxlen = 2, supp=0.1, conf = 0.1, target = "rules"))
summary(rules5)
inspect(head(rules5, n = 10, by ="lift"))

# No redundant rules by definition since no subset is possible
sum(is.redundant(rules5))

rules5.df = as(rules5, "data.frame")

# Breaks the rules column into antecedent and consequent columns
rules5.singleante.df<-rules5.df %>% separate(rules, c("antecedent", "consequent"), " => ")

rules5.singleante2.df<-rules5.singleante.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules5.singleante2.df$antecedent=gsub("\\{","",rules5.singleante2.df$antecedent)
rules5.singleante2.df$antecedent_logic=gsub("\\}","",rules5.singleante2.df$antecedent_logic)

rules5.singleante2.df<-rules5.singleante2.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules5.singleante2.df$consequent=gsub("\\{","",rules5.singleante2.df$consequent)
rules5.singleante2.df$consequent_logic=gsub("\\}","",rules5.singleante2.df$consequent_logic)

#Filtering to find rules with only true -> true or false->false
key_rules3<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules5.singleante2.df$antecedent)){
  tmp.df<-rules5.singleante2.df[rules5.singleante2.df$antecedent==i,]
  # dont understand
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules3 <- rbind(key_rules3, rules5.singleante2.df[(grepl(tmp.df[row_no,"antecedent"],rules5.singleante2.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules5.singleante2.df[["consequent"]])),])
    }
  }
}
key_rules3

############### support 0.1, confidence 0.5 
rules6 = apriori(data = symptomsTransFac, parameter = 
                   list(minlen = 2, maxlen = 2, supp=0.1, conf = 0.5, target = "rules"))
summary(rules6)

rules6.df = as(rules6, "data.frame")

# Breaks the rules column into antecedent and consequent columns
rules6.singleante.df<-rules6.df %>% separate(rules, c("antecedent", "consequent"), " => ")

rules6.singleante2.df<-rules6.singleante.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules6.singleante2.df$antecedent=gsub("\\{","",rules6.singleante2.df$antecedent)
rules6.singleante2.df$antecedent_logic=gsub("\\}","",rules6.singleante2.df$antecedent_logic)

rules6.singleante2.df<-rules6.singleante2.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules6.singleante2.df$consequent=gsub("\\{","",rules6.singleante2.df$consequent)
rules6.singleante2.df$consequent_logic=gsub("\\}","",rules6.singleante2.df$consequent_logic)

rules6.singleante2.df = rules6.singleante2.df[(grepl("1", rules6.singleante2.df[["antecedent_logic"]]) & grepl("1", rules6.singleante2.df[["consequent_logic"]])) | (grepl("0", rules6.singleante2.df[["antecedent_logic"]]) & grepl("0", rules6.singleante2.df[["consequent_logic"]])), ]

key_rules5<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules6.singleante2.df$antecedent)){
  tmp.df<-rules6.singleante2.df[rules6.singleante2.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules5 <- rbind(key_rules5, rules6.singleante2.df[(grepl(tmp.df[row_no,"antecedent"],rules6.singleante2.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules6.singleante2.df[["consequent"]])),])
    }
  }
}
key_rules5


##################################################################################
#Data visualisation
##################################################################################

#--------- 1. Finding correlation between the variables -----------#
data.all=read.csv("data/Training_all.csv", stringsAsFactors = TRUE, strip.white = TRUE)
data.all.symptoms=select(data.all,-"prognosis",-"Severity")
data.all.symptoms
merged_symptoms <- select(data.all, dark_urine, yellowish_skin,yellowing_of_eyes )
summary(merged_symptoms)

# correlogram to show the correlation between 3 symptoms, namely yellowing of eyes, yellowish skin and dark urine
ggstatsplot::ggcorrmat(
  data = merged_symptoms,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

#cross-correlations between the 3 symptoms
corr_cross(merged_symptoms, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)


correlation::correlation(merged_symptoms,
                         include_factors = TRUE, method = "auto"
)
#rho refers to correlation coefficients between the 2 variables


#There are many more symptoms that are highly correlated but they are not clinically proven to be associated with each other
corr_cross(data.all.symptoms, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)

#--------- 2. Finding the most and least frequent symptoms -----------#
data.clean=read.csv("data/Training_clean.csv")
data.clean.symptoms=data.clean[,-length(data.clean)]

#Identify the most and least frequent symptoms
##Most frequent symptoms= Fatigue, vomitting, high_fever, loss_of_apetite, nausea
##Least frequent symptoms= foul_smell_of.urine,nodal_skin_eruptions,shivering,ulcers_on_tongue,muscle_wasting
for(i in 1:ncol(data.clean.symptoms)){ #Factorise all the columns
  data.clean.symptoms[,i] <- as.factor(data.clean.symptoms[,i])
}
data.clean.symptoms$ignore=0
data.clean.symptoms.long=melt(data.clean.symptoms,id.vars = "ignore")
data.clean.symptoms.long=count(data.clean.symptoms.long, c("variable", "value"))
##Go through each row and determine if a value is zero
row_sub = apply(data.clean.symptoms.long, 1, function(row) all(row !=0 ))
##Subset as usual
data.clean.symptoms.long.present=data.clean.symptoms.long[row_sub,]
data.clean.symptoms.long.present=data.clean.symptoms.long.present[order(data.clean.symptoms.long.present$freq),]
data.clean.symptoms.long.present


#Visualisation of the 3 most and 3 least frequent symptoms
## fatigue
p_fatigue<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = fatigue)) + 
  labs(x="Fatigue", y="Count",title= "Fatigue Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))


## Vomitting
p_vomiting<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = vomiting)) + 
  labs(x="Vomitting", y="Count",title= "Vomitting Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))

## High Fever
p_high_fever<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = high_fever)) + 
  labs(x="High fever", y="Count",title= "High fever Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))
p_high_fever


grid.arrange(p_fatigue,p_vomiting,p_high_fever, nrow = 1)

## foul_smell_of_urine
p_foul_smell_of_urine<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = foul_smell_of.urine)) + 
  labs(x="Foul smell of urine", y="Count",title= "Foul smell of urine Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))

## Nodal Skin Eruptions
p_nodal_skin_eruptions<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = nodal_skin_eruptions)) + 
  labs(x="Nodal Skin Eruptions", y="Count",title= "Nodal Skin Eruptions Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))

## High Fever
p_shivering<-ggplot(data=data.clean.symptoms) + 
  geom_bar(aes(x = shivering)) + 
  labs(x="Shivering", y="Count",title= "Shivering Count")+
  theme_bw() +
  scale_x_discrete(labels=c("0" = "Absent", "1" = "Presence"))

grid.arrange(p_foul_smell_of_urine,p_nodal_skin_eruptions,p_shivering, nrow = 1)
grid.arrange(p_fatigue,p_vomiting,p_high_fever,p_foul_smell_of_urine,p_nodal_skin_eruptions,p_shivering, nrow = 2)


#------------ 3.Finding Symptoms of each Prognosis -------------#

data.clean=read.csv("data/Training_clean.csv", stringsAsFactors = TRUE, strip.white = TRUE)
summary(data.clean$Severity)

############### function to get frequency table of all symptoms
getFreq = function(data){
  
  # removes prognosis from list of columns names to get symptoms
  symptoms = colnames(data)[-length(colnames(data))]
  
  count = c()
  for(i in 1:(ncol(data)-1)){
    frequencies = as.integer(table(data[,i]))
    count = c(count, frequencies[length(frequencies)])
  }
  output = data.frame(symptoms = symptoms, count = count)
  return(output) 
}

df.AnE = data.clean[data.clean$Severity == 'A&E',]
df.Poly = data.clean[data.clean$Severity == 'Polyclinic',]
df.NoMed = data.clean[data.clean$Severity == 'No_Medical_Attention_Req',]

df.AnE = df.AnE[, colSums(df.AnE != 0) > 0]
df.Poly = df.Poly[, colSums(df.Poly != 0) > 0]
df.NoMed = df.NoMed[, colSums(df.NoMed != 0) > 0]

freq.AnE = getFreq(df.AnE)
freq.Poly = getFreq(df.Poly)
freq.NoMed = getFreq(df.NoMed)

freq.AnE = freq.AnE[order(freq.AnE$count, decreasing=TRUE),]
prop.AnE = freq.AnE
prop.AnE$count = prop.AnE$count/nrow(df.AnE)

ggplot(prop.AnE, aes(x = reorder(symptoms,(count)), y = count)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Proportion of Symptoms (A&E)\n", x = "\nSymptoms", y = "Proportion\n") +
  coord_flip()+
  theme_classic()

# con
prop.test(x=freq.AnE$count[freq.AnE$symptoms == "vomiting"], n=nrow(df.AnE), conf.level=.95, correct=FALSE)

ggplot(freq.AnE, aes(x = reorder(symptoms,(count)), y = count)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency of Symptoms (A&E)\n", x = "\nSymptoms", y = "Frequency\n") +
  coord_flip()+
  theme_classic()

ggplot(freq.Poly, aes(x = symptoms, y = count)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency of Symptoms (Polyclinic)\n", x = "\nSymptoms", y = "Frequency\n") +
  coord_flip()+
  theme_classic()

ggplot(freq.NoMed, aes(x = symptoms, y = count)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency of Symptoms (No Medical Attention)\n", x = "\nSymptoms", y = "Frequency\n") +
  coord_flip()+
  theme_classic()

############### Import dataset to look at symptoms and prognosis links
data.all = read.csv("data/Training_all.csv", stringsAsFactors = TRUE, strip.white = TRUE)
data.small = subset(data.all, select = -c(yellowish_skin,yellowing_of_eyes, dark_urine, Severity))

data.fungal = data.small[data.small$prognosis == "Fungal infection",]
data.fungal = data.fungal[, colSums(data.fungal != 0) > 0]
fungal.freq = getFreq(data.fungal)

data.allergy = data.small[data.small$prognosis == "Allergy",]
data.allergy = data.allergy[, colSums(data.allergy != 0) > 0]
allergy.freq = getFreq(data.allergy)

data.gastro = data.small[data.small$prognosis == "Gastroenteritis",]
data.gastro = data.gastro[, colSums(data.gastro != 0) > 0]
gastro.freq = getFreq(data.gastro)

data.hepB = data.small[data.small$prognosis == "Hepatitis B",]
data.hepB = data.hepB[, colSums(data.hepB != 0) > 0]
hepB.freq = getFreq(data.hepB)

ggplot(hepB.freq, aes(x = reorder(symptoms,(count)), y = count)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency of Symptoms (HepB)\n", x = "\nSymptoms", y = "Frequency\n") +
  coord_flip()+
  theme_classic()

# In general most diseases have 4-10 symptoms
# Each patient with the disease has most or all symptoms,
# so we should be able to get an accurate prediction
