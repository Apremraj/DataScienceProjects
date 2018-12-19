#Name : Ashish
#Date : 10/10/2017
#Purpose : Kaagle - Titanic Machine Learning competition

#Set Working directory and import train and test data files
setwd('G:\\Ashish- Technical\\R\\Datasets\\Titanic')

str(train)
train <- read.csv("train.csv", stringsAsFactors = FALSE)
table(train$Survived)
prop.table(table(train$Survived))

test$survived <- rep(0,418)

test$survived
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$survived)

write.csv(submit, file = "Titanic_1.csv", row.names = FALSE)
