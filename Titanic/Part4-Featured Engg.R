#Name : Ashish
#Date : 10/12/2017
#Purpose : Kaagle - Part4 - Feature Engineering

# Set working directory and import datafiles
setwd('G:\\Ashish- Technical\\R\\Datasets\\Titanic')
train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = F)

# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# What's in a name?
train$Name[1]


# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train,test)


# Convert to a string
str(combi)
combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]


# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]][[2]]


# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][[2]]})
combi$Title <- sub(' ','',combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'  
combi$Title[combi$Title %in% c('Capt','Major','Don','Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona','Jonkheer','Lady','theCountess')] <- 'Lady'
# Convert to a factor
combi$Title <- as.factor(combi$Title)


# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1


# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][[1]]})
table(combi$FamilySize)
combi$FamilyID <- paste(combi$FamilySize, combi$Surname, sep ='')
str(combi)
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)


# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]


# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data = train, 
             method = "class"
             )
fancyRpartPlot(fit)



# Now let's make a prediction and write a submission file
prediction <- predict(fit,test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'FeaturedEngg.csv', row.names = F)

