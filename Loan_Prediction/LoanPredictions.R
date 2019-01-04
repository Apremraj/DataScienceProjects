
rm(list = ls())

#Set working directory
setwd("C:/Users/Ashish/Documents/Edwisor/Projects/LoanPredictions")
getwd()

#Objective : To identify if the customer is eligible for loan or not

#Procedures:
  #1. Load libraries
  #2. Load Data
  #3. Missing Value Analysis
    #a. Create a dataframe to store variables and missing counts
        # If missing value percentage is higher, drop that variable
    #b. Plot bar graph to visualize missing values
  #4. Exploratory Data Analysis
    #a. Analysis on Dependent variable - loan status
    #b. Analysis on numerical data (Applicant Income, CoApplicant Income, Loan Amount, Loan Amount Term) - Box plot, Histogram 
    #c. Analysis on Categorical data (Gender, Married, Dependents, Education, Self-Employed, Credit History, Property Area) - Bar Plot
    #d. Dependent Variable (Loan Status) vs Independent variables
  #5. Outlier Analysis
    #a. Check for outliers on Numerical variables
    #b. Remove outliers using boxplot method
    #c. Method 2: Replace all outliers from boxplot.stats()$out with NAs
  #6. Impute missing values
    #a. For categorical variables, use mode method to impute data
    #b. For numerical variables, use mean method
  #7. Model Development
    #a. Split training data 
    #b. Logistic Regression

########################################################################################################
#1.Load libraries
library(dplyr)
library(caret)
library(ggplot2)

#2.Load train and test data
train = read.csv("train_u6lujuX_CVtuZ9i.csv", na.strings = c(""," ",NA))
test = read.csv("test_Y3wMUE5_7gLdaTN.csv", na.strings = c(""," ",NA))

str(train) #614 obs and 13 variables
str(test) #367 obs and 12 variables

#Variable consolidation
train$Credit_History = as.factor(train$Credit_History)
test$Credit_History = as.factor(test$Credit_History)
#train$Loan_Amount_Term = as.factor(train$Loan_Amount_Term)
#test$Loan_Amount_Term = as.factor(test$Loan_Amount_Term)

################################## 3.Missing Values Analysis############################################
#Check for missing values in data 
missing_val = as.data.frame(apply(train, 2, function(x){sum(is.na(x))}))

#Convert rownames to columns
missing_val$columns = row.names(missing_val)
row.names(missing_val) = NULL

#Rename column to count and rearrange in descending order
names(missing_val)[1] = "Count"
missing_val = missing_val[order(-missing_val$Count),]
missing_val = missing_val[,c(2,1)]
missing_val$Count = (missing_val$Count/nrow(train))

ggplot(data = missing_val, aes(x = reorder(missing_val$columns, - Count), y = Count)) +
  geom_bar(stat = "identity", fill = "grey") +
  xlab("Columns") + ylab("Count")

################################## 4. Exploratory Data Analysis#########################################
#Analysis on Dependent variable - Loan Status
barplot(table(train$Loan_Status))
prop.table(table(train$Loan_Status))

#Analysis on independent varaibles
#Categorical variables analysis
  #Gender factor has two levels M/F, There are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Gender),main = "Train set")
  barplot(table(test$Gender), main = "Test set")

  prop.table(table(train$Gender))
  prop.table(table(test$Gender))

  #Married has two levels Yes/No, there are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Married),main = "Train set")
  barplot(table(test$Married), main = "Test set")

  prop.table(table(train$Married))
  prop.table(table(test$Married))
  
  #Dependents has 4 levels : 0,1,2,3+ - There are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Dependents), main = "Train Set")
  barplot(table(test$Dependents), main = "Test set")
  
  prop.table(table(train$Dependents))
  prop.table(table(test$Dependents))
  
  #Education has 2 levels : Graduate, Not Graduate - There are no NAs
  par(mfrow = c(1,2))
  barplot(table(train$Education),main = "Train set")
  barplot(table(test$Education), main = "Test set")
  
  prop.table(table(train$Education))
  prop.table(table(test$Education))
  
  #Self Employed has 2 levels : Yes/No - There are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Self_Employed))
  barplot(table(test$Self_Employed))
  
  prop.table(table(train$Self_Employed))
  prop.table(table(test$Self_Employed))
  
  #Credit History - Convert to factor with two levels : 1/0 - There are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Credit_History), main = "Train Set")
  barplot(table(test$Credit_History), main = "Test Set")
  
  prop.table(table(train$Credit_History))
  prop.table(table(test$Credit_History))
  
  #Property Area has 2 levels : Y/N - there are no NAs
  par(mfrow = c(1,2))
  barplot(table(train$Property_Area),main = "Train set")
  barplot(table(test$Property_Area), main = "Test set")
  
  prop.table(table(train$Property_Area))
  prop.table(table(test$Property_Area))
  
  #Loan Amount Term, there are NAs
  par(mfrow = c(1,2))
  barplot(table(train$Loan_Amount_Term),main = "Train set")
  barplot(table(test$Loan_Amount_Term), main = "Test set")
  
#Analysis on Numerical variables
  #Application Income and CoApplicant Income - There are many outliers but no NAs
  par(mfrow = c(1,2))
  boxplot(train$ApplicantIncome, train$CoapplicantIncome, names = c("App Inc","CoApp Inc"), main = "Train set")
  boxplot(test$ApplicantIncome, test$CoapplicantIncome, names = c("App Inc","CoApp Inc"), main = "Test set")
  
  #Loan Amount - There are NAs
  par(mfrow = c(1,2))
  boxplot(train$LoanAmount, main = "Train set")
  boxplot(test$LoanAmount, main = "Test set")
  
  #Loan Amount terms , there are NAs
  #par(mfrow = c(1,2))
  #hist(train$Loan_Amount_Term, breaks = 500, main = "Train set")
  #hist(test$Loan_Amount_Term, breaks = 500, main = "Test set")
  
#Dependent Variable (Loan Status) vs Independent variables
  #Loan Status by Gender
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Gender)+
    ggtitle("Loan Status by Gender")+
    xlab("Loan Status")
  
  #Loan Status by Marital Status
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Married)+
    ggtitle("Loan Status by Marital Status")+
    xlab("Loan Status")
  
  #Loan status by Dependents
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Dependents)+
    ggtitle("Loan Status by Dependents")+
    xlab("Loan Status")
  
  #Loan status by Education
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Education)+
    ggtitle("Loan Status by Education")+
    xlab("Loan Status")
  
  #Loan Status by Employment Status
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Self_Employed)+
    ggtitle("Loan Status by Self Employment")+
    xlab("Loan Status")
  
  #Loan status by Loan Amount Term
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Loan_Amount_Term)+
    ggtitle("Loan Status by Loan Amount Term")+
    xlab("Loan Status")
  
  #Loan Status by Credit History
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Credit_History)+
    ggtitle("Loan Status by Credit History")+
    xlab("Loan Status")
  
  #Loan Status by Property Area
  ggplot(data = train, aes(x = train$Loan_Status))+
    geom_bar()+
    facet_grid(.~Property_Area)+
    ggtitle("Loan Status by Property Area")+
    xlab("Loan Status")
  
  #Loan Status by Applicant Income
  ggplot(data = train, aes(x = train$Loan_Status, y = train$ApplicantIncome))+
    geom_boxplot()+
    ggtitle("Loan Status by Applicant Income")+
    xlab("Loan Status")
  
  #Loan Status by coapplicant Income
  ggplot(data = train, aes(x = train$Loan_Status, y = train$CoapplicantIncome))+
    geom_boxplot()+
    ggtitle("Loan Status by CoApplicant Income")+
    xlab("Loan Status")
  
################################## 5. Outlier Analysis#########################################
#Check for outliers on Numerical variables
  Num_var = sapply(train, is.numeric)
  Num_data = train[,Num_var]
  Cnames = colnames(Num_data)
  
  for(i in 1:length(Cnames)){
    assign(paste0("Gn",i), 
           ggplot(data = train, aes_string(y = Cnames[i], x = train$Loan_Status))+
             stat_boxplot(geom = "errorbar", width = 0.5)+
             geom_boxplot(outlier.colour = "red", outlier.size = 1, outlier.shape = 18,
                          fill = "grey", notch = FALSE)+
             theme(legend.position="bottom")+
             labs(y=Cnames[i],x=train$Loan_Status)+
             ggtitle(paste("Box plot of Loan Status for",Cnames[i]))
             )
  }
  
  gridExtra::grid.arrange(Gn1,Gn2,Gn3,Gn4)
  
  #Replace all outliers with NA for imputation with mean, median or mode
  for(i in Cnames){
    Out = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
    train[,i][train[,i] %in% Out] = NA
  }

################################## 6. Imputation of Missing Values ############################
#Impute CategoricalNumerical variables using mean method
  train$ApplicantIncome[is.na(train$ApplicantIncome)] = mean(train$ApplicantIncome, na.rm = T)
  train$CoapplicantIncome[is.na(train$CoapplicantIncome)] = mean(train$CoapplicantIncome, na.rm = T)
  train$LoanAmount[is.na(train$LoanAmount)] = mean(train$LoanAmount, na.rm = T)
  train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)] = 360
  
  #Impute Categorical variables using mode method
  train$Credit_History[is.na(train$Credit_History)] = 1
  train$Self_Employed[is.na(train$Self_Employed)] = 'No'
  train$Dependents[is.na(train$Dependents)] = "0"
  train$Gender[is.na(train$Gender)] = "Male"
  train$Married[is.na(train$Married)] = "Yes"
  
################################### 7. Model Development ######################################  
#Logistic Regression
  #Train model on train set
  train_data = train[,-1]
  test_data = test[,-1]
  train_data$Loan_Status = ifelse(train_data$Loan_Status == "Y",1,0)
  
  train.index = createDataPartition(train_data$Loan_Status, p = 0.8, list = F)
  train_data = train_data[train.index,]
  test_data = train_data[-train.index,]
  
  logit_model = glm(train_data$Loan_Status ~ ., data = train_data, family = "binomial")
  summary(logit_model)
  
  #Prediction
  predict_glm = predict(logit_model, newdata = train_data, type = "response")
  
  #Convert to prob
  predict_glm = ifelse(predict_glm > 0.5, 1, 0)
  
  #Confusion matrix
  table(train_data$Loan_Status, predict_glm)
  (82+414)/(82+110+8+414)
  #Accuracy : 80.7%  
  
  logit_model_test = glm(test_data$Loan_Status ~ test_data$Credit_History, data = test_data, family = "binomial")
  
  predict_glm_test = predict(logit_model_test, newdata = test_data, type = "response")
  
  predict_glm_test = ifelse(predict_glm_test > 0.5, 1,0)
  
  table(test_data$Loan_Status, predict_glm_test)
  (17+49)/(17+11+49)
  #Accuracy : 85.7%
  
  
  
  
  
  
  