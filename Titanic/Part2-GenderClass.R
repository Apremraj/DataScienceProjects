#Name : Ashish
#Date : 10/10/2017
#Purpose : Kaagle - Part2 - Gender Class

#1. Predict based on Gender

#Set Working directory and import train and test data files
setwd('G:\\Ashish- Technical\\R\\Datasets\\Titanic')

train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = F)
#survivors percentage based on sex
table(train$Sex,train$Survived)
prop.table(table(train$Sex,train$Survived))

#Survivors based on row percentage - 74% females survived, 18% males survived
prop.table(table(train$Sex,train$Survived),1)

test$survived <- 0
#if sex = 'female' then 1
test$survived[test$Sex == "female"] <- 1

#Submission
submit <- data.frame(PassengerID = test$PassengerId, survived = test$survived)
write.csv(submit, file = "Titanic_2.csv", row.names = FALSE)

#2. Now predict based on Age and gender

summary(train$Age)

#categorize based on Children below 18 years - create a new variable and assign
train$child <- 0
train$child[train$Age < 18] <- 1
prop.table(table(train$child, train$Survived),1)

#use aggregate function to sum up all survived based on sex and child
aggregate(Survived ~ child+Sex, data = train, FUN = sum)

#use length function to find out the totals
aggregate(Survived ~ child + Sex, data= train, FUN = length)

#Find proportions based on sum and length
aggregate(Survived ~ child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

#Create separate bins based on Ticket fares
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >=20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10 ] <- '10-20'
train$Fare2[train$Fare <10] <- '<10'

#Longer aggregate function based on Fare, class and sex
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x){sum(x)/length(x)})

#Predict test variable based on the findings
test$survived <- 0
test$survived[test$Sex == 'female'] <- 1 
test$survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#Submission
submit <- data.frame(PassengerID = test$PassengerId, survived = test$survived)
write.csv(submit, file = "Titanic_2.csv", row.names = FALSE)






