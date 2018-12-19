#Name : Ashish
#Date : 10/12/2017
#Purpose : Kaagle - Part3 - Decision Tree

#Set Working directory and import train and test data files
setwd('G:\\Ashish- Technical\\R\\Datasets\\Titanic')

train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = F)

colnames(train)

library(rpart)

# 1.Create a decision tree using Rpart on the target variable Survived vs non categorical values  
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = "class")

rpart.plot(fit)

# 2.Use Rattle package to plot fancyrpartplot which runs rpart.plot() internally
install.packages('rattle')
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

# 3.Use predict() to predict based on the decision tree variable and submit
prediction <- predict(fit,test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'MyFirstTree.csv', row.names = F)

#?rpart.control





# Playing around with rpart.control to display interactive decision trees
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = "class", control = rpart.control(cp =0, minsplit = 2))

fancyRpartPlot(fit)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, 
             method = "class", 
             control = rpart.control(cp =0, minsplit = 2))

new.fit<- prp(fit, fallen.leaves = F, snip=TRUE)
fancyRpartPlot(new.fit)



