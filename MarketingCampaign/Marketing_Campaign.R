
#Objctive : To predict who is going to respond to the campaign?

#Procedures:
  #1.Load Libraries
  #2.Load Data
  #3.Exploratory Data Analysis
    #1.Univariate analysis and variable consolidation
  #4.Missing value analysis
    #1.Create dataframe to store missing percentage and variables and order them
    #2.Plot bar graph to visualize the missing data percentages
    #3.Manually create missing value and check which imputation method(Mean,Median,KNN) is close to actual value and impute.
    #4.Convert string factor categories to numeric factor categories to save disk space
  #5.Outlier Analysis
    #1.Save the numeric data in a variable as outliers can be seen only in a numeric variable
    #2.Visualize outliers using boxplot
    #3.Method 1 - Remove outliers using boxplot.stats(df)$out method
    #4.Method 2 - Replace outliers with NA
    #5.Impute NAs with best imputation method for this case(#4.3)
  #6.Feature Selection - perform dimension reductionality to reduce features
    #1.Correlation plot - using corr method, we can find correlation between numerical variables and remove
       #higly correlated features as we might have duplicate features
    #2.ChiSquare Test - is performed on categorical variables to find independence of data using p-value.
       #If p-value < 0.05 (Dependent) if p-value > 0.05 (Independent) - Remove features
  #7.Feature scaling - 2 methods to transform data in a range to a common ground
    #Normalizatin - use if data is not normally distributed. Range - 0 to 1
    #Standardization - use if data is normally distributed. Metrics - Std dev - zero mean and unique variance
  #8.Sampling - to take subset of data from population for experiment
    #1.Simple random sampling - pick random samples, specify count
    #2.Stratified sampling - picking up unique proportions from each category so as not to miss any possiblity
    #3.Systematic sampling - pick up kth value in dataset
  #9.Model development
    #1.Data Partition - train(80%) and test data(20%) using caret library
    #2.C5.0 Decision tree algorithm
      #a.Evaluate model using support-20%, confidence - 80%, Lift > 1
      #b.Predict model accuracy - confusion matrix

###########################################Load the data##########################################
#Clean R environment
rm(list = ls(all = T))

#Set working directory
setwd("C:/Users/Ashish/Documents/Edwisor/Data")
getwd()

#Load libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#install.packages(x)
lapply(x, require, character.only = TRUE)

#Load Data
marketing_train <- read.csv("Marketingtr.csv",header = T, na.strings = c(" ","","NA"))

###########################################Explore the data##########################################
str(marketing_train)

## Univariate Analysis and Variable Consolidation
marketing_train$schooling[marketing_train$schooling %in% "illiterate"] = "unknown"
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default = as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

##################################Missing Values Analysis###############################################
#Create dataframe with missing percentage, 2- specifies column. So function will be applied to all columns
missing_val = data.frame(apply(marketing_train, 2, function(x){sum(is.na(x))}))

#Convert rownames into column
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL

#Renaming the variable name
names(missing_val)[1] = "Missing_Percentage"

#Calculate percentage of missing value based on whole data
missing_val$Missing_Percentage = (missing_val$Missing_Percentage/nrow(marketing_train))*100

#Arrange in descending order
missing_val = missing_val[order(-missing_val$Missing_Percentage),]

#Rearranging the columns
missing_val = missing_val[,c(2,1)]

#write output results back to disk
write.csv(missing_val, "Missing_Perc.csv", row.names = F)

#Plot bar graph to visualize missing data
ggplot(data = missing_val[1:3,],
       aes(x = reorder(Columns, -Missing_Percentage), y = Missing_Percentage))+
  geom_bar(stat = "identity", fill = "grey")+
  xlab("Parameters")+
  ggtitle("Missing data percentage (Train")+
  theme_bw()

#Creating NA manually for an observation each time and validating the 3 imputation methods
#Imputing mean for CustAge
marketing_train[71,1]

#Actual value = 29, convert to NA
marketing_train[71,1] = NA

#Mean method
#marketing_train$custAge[is.na(marketing_train$custAge)] = mean(marketing_train$custAge, na.rm = T)

#Median method
#marketing_train$custAge[is.na(marketing_train$custAge)] = median(marketing_train$custAge, na.rm = T)

#KNN Method - using DMwR library
marketing_train = knnImputation(marketing_train, k = 3)

#Actual value = 29
#mean method = 40.01
#Median = 38 
#KNN = 36.18
#Conclusion - KNN imputation method is closest to the actual value so it is the best method for this case

#For Data Manipulation, convert string categories into factor numerics which also reduces disk space
for(i in 1:ncol(marketing_train)){
  if(class(marketing_train[,i]) == "factor"){
    marketing_train[,i] = factor(marketing_train[,i], labels = (1:length(levels(factor(marketing_train[,i])))))
  }
}

############################################Outlier Analysis#############################################
#Outlier Analysis - analysis can be done only in numeric variables
#Boxplot - Distribution and outlier check

#check which of the variables are numeric and store them in numeric_index
numeric_index = sapply(marketing_train, is.numeric)
#slice only numeric variables into numeric_data
numeric_data = marketing_train[,numeric_index]

cnames = colnames(numeric_data)#Column names of numeric variables
for(i in 1:length(cnames)){
  assign(paste0("Gn",i),
         ggplot(data = marketing_train, aes_string(y = cnames[i], x = 'responded'))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
                       geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                                    outlier.size=1, notch=FALSE) +
                       theme(legend.position="bottom")+
                       labs(y=cnames[i],x="responded")+
                       ggtitle(paste("Box plot of responded for",cnames[i])))
}
 
#Plotting box plots in a grid
gridExtra::grid.arrange(Gn1,Gn4,Gn2,ncol=3)
gridExtra::grid.arrange(Gn6,Gn7,ncol=2)
gridExtra::grid.arrange(Gn8,Gn9,ncol=2)

gridExtra::grid.arrange(Gn1,Gn2,Gn5,Gn6,Gn7,Gn8,Gn9, ncol = 3,nrow = 3)

#Remove outliers using boxplot method
df = marketing_train
marketing_train = df
 
val = marketing_train$previous[marketing_train$previous %in% boxplot.stats(marketing_train$previous)$out]
 
marketing_train = marketing_train[which(!marketing_train$previous %in% val),]

#loop through to remove from all variables
for(i in cnames){
  print(i)
  val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  print(length(val))
  marketing_train = marketing_train[which(!marketing_train[,i] %in% val),]
}

#Next method - replace all outliers with NA and impute
#for(i in cnames){
#  val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
#  marketing_train[,i][marketing_train[,i] %in% val] = NA
#}

#Impute using knn imputation - Not sufficient observations as there are only 1017 NAs. We need more data to perform KNN imputation.
#marketing_train = knnImputation(marketing_train, k =3)

#Conclusion: We will go with first method of removing the outliers

##################################Feature Selection################################################
#check which of the variables are numeric and store them in numeric_index
numeric_index = sapply(marketing_train, is.numeric)

#Correlation plot - only on continuous or numeric variable
corrgram(marketing_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#Conclusion : pdays is negatively correlated with previous and positively correlated with pmonths
            #emp.var.rate is positively correlated with cons.price,eurbor and nr.employed.
            #So we will remove pdays and emp.var.rate as they are highly correlated with other variables.

#Null hypo - two variables are independent
#Alternate hypo - two variables are dependent
#Chi-Square test of independence
factor_index = sapply(marketing_train, is.factor)
factor_data = marketing_train[,factor_index]

for(i in 1:10){
  print(names(factor_data[i]))
  print(chisq.test(table(factor_data$responded, factor_data[,i])))
}
# We will need to consider highly dependent values using p-value where, p-value < 0.05 then they are highly correlated(dependent)
#Conclusion : we nned to remove day_of_week, loan and housing as they have p-values > 0.05, then they are not dependent on target variable
  #so there would not be any difference if we select these features for our modeling.
  #All other features have p-values < 0.05, i.e they are highly dependent on target variable.

#Dimensionality Reduction
marketing_train_deleted = subset(marketing_train,
                                 select = -c(day_of_week, loan, housing, pdays, emp.var.rate))

##################################Feature Scaling################################################
#Normality check
#When you get an error : figure margins too large, run below code
#par(mar=c(1,1,1,1))
qqnorm(marketing_train_deleted$custAge)
hist(marketing_train_deleted$custAge)
#We can see from output that they are right skewed and not normally distributed, so we'll go for normal distribution technique

#Normalisation
#Store continuous variables in cnames
cnames = c("custAge","campaign","previous","cons.price.idx","cons.conf.idx","euribor3m","nr.employed",
           "pmonths","pastEmail")

for(i in cnames){
  print(i)
  marketing_train_deleted[,i] = (marketing_train_deleted[,i] - min(marketing_train_deleted[,i]))/
                                (max(marketing_train_deleted[,i]) - min(marketing_train_deleted[,i]))
}

#Check the range of variables
range(marketing_train_deleted[,cnames])
View(marketing_train_deleted[,cnames])

#Standardization
for(i in cnames){
  print(i)
  marketing_train_deleted[,i] = (marketing_train_deleted[,i] - mean(marketing_train_deleted[,i]))/
                                sd(marketing_train_deleted[,i])
}
#Check the range of variables
range(marketing_train_deleted[,cnames])
View(marketing_train_deleted[,cnames]) 

#############################################Sampling#############################################
#Simple random sampling - replace = F (don't pick the obs again a second time)
data_sample = marketing_train_deleted[sample(nrow(marketing_train_deleted), 4000, replace = F),]

table(marketing_train_deleted$profession)
#Stratified sampling, size - how much propotions do you want from each category, srswor - simple random sampling without replacement
stratas = strata(marketing_train_deleted, c("profession"), size = c(100, 199, 10, 5), method = 'srswor')
stratified_data = getdata(marketing_train_deleted, stratas)

#Systematic sampling - there is no inbuilt function for this so we will write the algo
#Function to generate kth index
sys.sample = function(N,n){
  k = round(N/n)
  print(k)
  r = sample(1:k,1)
  sys.samp = seq(r,r+k*(n-1),k) #seq(from, to, by = k) so get sequence of kth value
  #sys.samp = seq(r,7414,k) - gives same distribution of data
}

lis = sys.sample(7414, 100) #Select the respective rows

#Create index variable in the data
marketing_train_deleted$create_index = 1:7414
#Extract subset from whole data
systematic_data = marketing_train_deleted[which(marketing_train_deleted$create_index %in% lis),]

###################################Model Development#######################################
marketing_train = marketing_train_deleted

#clean the environment
library(DataCombine)
rmExcept("marketing_train")

#Divide data into train and test using stratified sampling method because of binary output
library("caret")
set.seed(1234)
train.index = createDataPartition(marketing_train$responded, p = 0.8, list = F)
train = marketing_train[train.index,]
test = marketing_train[-train.index,]

######## Decision Tree ################
#Decision tree for classification
#Develop model on training data
library("C50")
C50_model = C5.0(responded ~.,train, trials = 100, rules = TRUE)

summary(C50_model)
#Evaluate the model using Support - 20%, confidence - 80% and Lift - >1. At the end we have list of variables that are highly dependent on target variable

#write rules into disk
write(capture.output(C50_model),"C50_model.txt")

#Let's predict for test cases
c50_predictions = predict(C50_model, test[,-17], type = "class")

#Evaluate the performance of classification model
conf_matrix_c50 = table(test$responded,c50_predictions)
confusionMatrix(conf_matrix_c50)

#Conclusion based on Decision Tree analysis
#Summary:
#1-Will not respond, 2- will respond.
#FP-106, says that business user guessed that 106 users will not respond but actually they did respond.
#So, business would want this data to not miss on potential customers
#FN-29, says business spent time and money to campaign on 29 customers who did not respond.

#False negative rate - here TP = 62 as 2-will respond, FN = 106
FNR = FN/FN+TP
106/(106+62) #63% of customers actually responded and business missed their campaign

#False positive rate
FPR = FP/(FP+TN)
29/(29+1285) #2% of customers did not respond but business spent on their campaign

#Accuracy : 90.89%
#FNR : 63%

#### Random Forest ###############
RF_model = randomForest(responded ~ .,train, importance = TRUE, ntree = 100)

#Extract rules from random forest
#Transform rf object to an intrees format
treeList = RF2List(RF_model)

#Extract rules
exec = extractRules(treeList, train[,-17]) #R-executable conditions

#Visualize some rules
exec[1:2,]

#make rules more readable
readableRule = presentRules(exec, colnames(train))
readableRule[1:2,]

#Get rule metrics:
ruleMetric = getRuleMetric(exec, train[,-17], train$responded) #Get rule metrics
ruleMetric[1:2,]

#Predict test data using random forest
RF_Predictions = predict(RF_model, test[,-17])

#Evaluate the performance of classification model
conf_matrix_rf = table(test$responded, RF_Predictions)
confusionMatrix(conf_matrix_rf)

#False negative rate - here TP = 60 as 2-will respond, FN = 34
FNR = FN/FN+TP
108/(108+60)

#Accuracy : 90.42%
#FNR : 64.2%

########### Logistic Regression ############
logit_model = glm(responded ~., data = train, family = "binomial")

summary(logit_model)
#Deviance reiduals - (-2.12) to (2.89) - There is not much variation between the errors
#p-value to calculate the significance levels of correlation
#Null deviance : predicitng the testcase but only with intercept - 4191.9
#Residual deviance : how well the response variable is predicted using null deviance and all other independent variables.
#Performance : Diff btw null deviance and reidual deviance must be high. if low, then data does not have much variance to explain target variable.
#AIC(Alkaike information criteria) : 3366.4 - Low AIC among diff models, better model

#Prediction
logit_pred = predict(logit_model, newdata = test, type = "response")

#Convert to prob
logit_pred = ifelse(logit_pred > 0.5, 1, 0)

#Confusion matrix
confusion_lr = table(test$responded, logit_pred)

#Tp - 54
#TN - 1293
#FP - 21
#FN - 114

#Accuracy - TP+TN/(TP+TN+FP+FN) = 90.89%
(1293+54)/(1293+21+114+54)

#False Negative Rate - FN/(FN+TP) = 67.85%
114/(114+54)

#Conclusion
#Accuracy : 90.89%
#FNR : 67.85%

########### KNN Implementation ############
library(class)

#Predict test data
prediction_knn = knn(train[,1:16], test[,1:16], train$responded, k = 1)

#Confusion matrix
conf_matrix_knn = table(prediction_knn, test$responded)

#Accuracy
sum(diag(conf_matrix_knn))/nrow(test)

#Predict test data, k =3
prediction_knn = knn(train[,1:16], test[,1:16], train$responded, k = 7)

#False Negative Rate
#FN/FN+TP
33/(33+47)

#Conclusion
#Accuracy : 89.6
#FNR : 41.25

########### Naive Bayes ############
library(e1071)

#Develop model
model_nb = naiveBayes(responded ~ .,data = train)

#Predict on test cases 
predictions_nb = predict(model_nb, test[,1:16], type = "class")

#Confusion matrix
conf_matrix_nb = table(observed = test$responded, predicted = predictions_nb)
confusionMatrix(conf_matrix_nb)

#False Negative Rate
90/(90+78)

#Accuracy : 85.16 %
#FNR : 53.57%

##################################### Conclusion #######################
#Compared to all models built, The model built on KNN algorithm gives us better results
#Accuracy : 89.6%
#False Negative Ratio : 41.25%
#This model gives us better accuracy with minimal error rate.
###############################################################################################
