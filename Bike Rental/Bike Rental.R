
#Objctive : To predict bike rental count on a daily basis based on environmental and seasonal settings

#Procedures:
  #1.Load Libraries
  #2.Load Data and data analysis
  #3.Feature Engineering and Variable consolidation
  #3.Exploratory Data Analysis
    #1.Analysis on Target variable - Count
    #2.Analysis of independent variables over dependent variable
    #3.Analysis of Continuous variables using distribution
  #4.Data Preprocessing
    #1.Missing Value Analysis
    #2.Outlier Analysis
    #3.Feature Selection
      #a.Correlation Analysis
      #b.Random Forest - importance metric
  #5.Model Development
    #1.Data Splitting into train and test data
    #2.Check for multicollinearity
    #3.Linear Regression
      #a.Stepwise model selection on Linear Regression
      #b.Prediction
      #Evaluation Metrics using MAPE, RMSE, MSE and MAE
    #4.Decision Tree 
      #a.Prediction
      #b.Evaluation Metrics using MAPE, RMSE, MSE and MAE
    #5.Random Forest
      #a.Prediction
      #b.Evaluation Metrics using MAPE, RMSE, MSE and MAE
  #6.Conclusion

########################################### Load Libraries #########################################
library(ggplot2)
library(dplyr)
library(corrplot)
library(randomForest)
library(caret)
library(DMwR)
library(usdm)
library(rpart)

###########################################Load the data############################################
##Clean R Environment##
rm(list = ls())

#Set working directory
setwd("C:/Users/Ashish/Documents/Edwisor/Projects/Bike Rental")

#Load data
data_df = read.csv("day.csv")

View(data_df)
summary(data_df)
str(data_df)

################################# Feature Engineering ##############################################
data_df$season = as.factor(data_df$season)
levels(data_df$season) = c("Spring","Summer","Fall","Winter")
data_df$yr = as.factor(data_df$yr)
levels(data_df$yr) = c("2011","2012")
data_df$mnth = as.factor(data_df$mnth)
levels(data_df$mnth) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
data_df$holiday = as.factor(data_df$holiday)
levels(data_df$holiday) = c("No","Yes")
data_df$weekday = as.factor(data_df$weekday)
levels(data_df$weekday) = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
data_df$workingday = as.factor(data_df$workingday)
levels(data_df$workingday) = c("No", "Yes")
data_df$weathersit = as.factor(data_df$weathersit)
levels(data_df$weathersit) = c("Clear + Few Clouds","Mist + Cloudy", "Light Snow + Rain + Thunderstorm")
data_df$dteday = as.POSIXct(data_df$dteday)
data_df$day = strftime(data_df$dteday, '%e')
data_df$day = as.factor(data_df$day)

################################# Exploratory Data Analysis ########################################
#Analysis on dependent variable - Count
ggplot(data = data_df, aes(cnt))+
  geom_histogram()

#Analysis of Independent variabls vs dependent variable
  #Season vs Rental counts
  ggplot(data = data_df, aes(data_df$season, data_df$cnt, fill = season))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Season")+
    ylab("Rental Count")+
    labs(title = "Bike Rental counts during seasons")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$yr, data_df$cnt, fill = data_df$yr))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Year")+
    ylab("Rental Count")+
    labs(title = "Year wise Bike Rental counts")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$mnth, data_df$cnt, fill = data_df$mnth))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Month")+
    ylab("Rental Count")+
    labs(title = "Month wise Bike Rental counts")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$day, data_df$cnt, fill = data_df$day))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Day")+
    ylab("Rental Count")+
    labs(title = "Day wise Bike Rental counts")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$holiday, data_df$cnt, fill = data_df$holiday))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Holiday")+
    ylab("Rental Count")+
    labs(title = "Bike Rental counts during Holidays")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$weekday, data_df$cnt, fill = data_df$weekday))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Weekday")+
    ylab("Rental Count")+
    labs(title = "Bike Rental counts during weekdays")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$workingday, data_df$cnt, fill = data_df$workingday))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Workingday")+
    ylab("Count")+
    labs(title = "Bike Rental counts on a workingday")+
    theme(plot.title = element_text(hjust=0.5))
  
  ggplot(data = data_df, aes(data_df$weathersit, data_df$cnt, fill = data_df$weathersit))+
    geom_boxplot(show.legend = FALSE)+
    xlab("Weather Condition")+
    ylab("Count")+
    labs(title = "Bike Rental counts based on weather conditions")+
    theme(plot.title = element_text(hjust=0.5))
  
  #Distribution of Continuous variables
  Hg1 = ggplot(data_df, aes(x = casual))+
    geom_histogram()+
    xlab("Casual Users")
  
  Hg2 = ggplot(data_df, aes(x = windspeed))+
    geom_histogram()+
    xlab("WindSpeed")
  
  Hg3 = ggplot(data_df, aes(x = temp))+
    geom_histogram()+
    xlab("Normalized Temperature")
  
  Hg4 = ggplot(data_df, aes(x = atemp))+
    geom_histogram()+
    xlab("Normalized feeling temperature")
  
  Hg5 = ggplot(data_df, aes(x = hum))+
    geom_histogram()+
    xlab("Humidity")
  
  Hg6 = ggplot(data_df, aes(x = registered))+
    geom_histogram()+
    xlab("Registered Users")
  
  gridExtra::grid.arrange(Hg1, Hg2,Hg3,Hg4,Hg5,Hg6)

################################## Missing Values Analysis #########################################
  Missing_val = data.frame(apply(data_df, 2, function(x){sum(is.na(x))}))
  Missing_val$Columns = row.names(Missing_val)
  row.names(Missing_val) = NULL
  names(Missing_val)[1] = "Count"
  #Display Missing Value counts - There are no missing values
  Missing_val = Missing_val[c(2,1)]

######################################## Outlier Analysis #########################################
#Check for numeric variables and store them in numeric_index
  numeric_index = sapply(data_df, is.numeric)
  numeric_data = data_df[,numeric_index]
  
  boxplot.stats(numeric_data$temp)$out
  boxplot.stats(numeric_data$atemp)$out
  boxplot.stats(numeric_data$hum)$out
  boxplot.stats(numeric_data$windspeed)$out
  boxplot.stats(numeric_data$casual)$out
  boxplot.stats(numeric_data$registered)$out
  
  numeric_data = data_df[,c("windspeed","casual","hum")]
  cnames = colnames(numeric_data)
  for(i in 1:length(cnames)){
    assign(paste0("Gn",i),
           ggplot(data_df, aes_string(y = cnames[i]))+
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.color = "red", outlier.shape = 18, outlier.size = 2))
  }
  
  gridExtra::grid.arrange(Gn1,Gn2,Gn3,ncol=3)
  
################################## Feature Selection ##############################################
  #Remove Casual and Registered from the dataset as this sums up total counts 
  #Remove instant variable and dteday as they do not give much information 
  data_df = data_df[,-c(1,2,14,15)]
  
  #Correlation Analysis
  numeric_index = sapply(data_df, is.numeric)
  numeric_data = data_df[,numeric_index]
  
  train_cor = cor(numeric_data)
  corrplot(train_cor, method = 'color', addCoef.col = 'black')
  
  #Feature selection through Random Forest
  RF_model = randomForest(cnt ~., data = data_df, ntree = 100, importance = TRUE)  
  pd = as.data.frame(importance(RF_model, type = 1))
  pd$columns = row.names(pd)
  row.names(pd) = NULL
  pd = pd[order(-pd$`%IncMSE`),]
  pd = pd[,c(2,1)]
  View(pd)
  
  #Plot features with imporatance from High to Low
  ggplot(pd, aes(x = reorder(columns, -pd$`%IncMSE`), y = pd$`%IncMSE`, fill = 'blue'))+
    geom_bar(stat = 'identity', show.legend = FALSE)+
    xlab("Variable Names")+
    ylab("Feature Importance Rate (%IncMSE)")+
    ggtitle("Features with Importance value")+
    theme(plot.title = element_text(hjust=0.5))
  
################################### Model Development ############################################
  #Split data 
  set.seed(1234)
  train_index = createDataPartition(data_df$cnt, p = 0.8, list = F)
  train = data_df[train_index,]
  test = data_df[-train_index,]
  
  #Check for multicollinearity
  #library(usdm)
  vif(numeric_data[,-5])
  
  vifcor(numeric_data[,-5], th = 0.9)
  #Multicollinearity issues are found between temp and atemp features
  
  ################# Linear Regression Modelling #################
  lm_model = lm(data = train, cnt~ .)
  summary(lm_model)
  
  predict_lm = predict(lm_model, test[,c(-12)])
  
  #Finding error metrics
  #Calculating MAPE - y= Actual value, yhat = Predicted values
  mape = function(y,yhat){
    mean(abs((y-yhat)/y))*100
  }
  
  mape(test[,12], predict_lm)
  
  #Alternate method - regr.eval() from DMwR library gives completed evaluation of regression model
  regr.eval(test[,12],predict_lm, stats = c("rmse","mse","mape","mae"))
  
  #Error Rate - 16.22%
  #Accuracy - 83.78%
  
  #Since we have multicollinearity issue between temp and atemp features we will regularize it
  #Feature modeling using Stepwise modeling selection - both forward and backward
  lm_AIC = stepAIC(lm_model, direction = 'both')
  predict_lm_AIC = predict(lm_AIC, test[,-12])
  summary(predict_lm_AIC)
  
  mape(test[,12], predict_lm_AIC)
  regr.eval(test[,12],predict_lm_AIC, stats = c("rmse","mse","mape","mae"))
  
  #Error Rate - 15.94%
  #Accuracy - 84.06%
  
  ######################### Decision Tree #########################
  dt_model = rpart(cnt ~., data = train, method = 'anova')
  
  predict_dt = predict(dt_model, test[,-12])
  summary(predict_dt)  
  
  mape(test[,12],predict_dt)
  
  #Alternate method - regr.eval() from DMwR library gives completed evaluation of regression model
  regr.eval(test[,12],predict_dt, stats = c("rmse","mse","mape","mae"))
  
  #Error Rate - 20.50%
  #Accuracy - 79.50%
  
  ######################### Random Forest #########################
  set.seed(3210)
  rf_model = randomForest(cnt ~.,data = train, ntree = 100)
  
  predict_rf = predict(rf_model, test[,-12])
  summary(predict_rf)
  
  mape(test[,12], predict_rf)
  
  #Alternate method - regr.eval() from DMwR library gives completed evaluation of regression model
  regr.eval(test[,12],predict_rf, stats = c("rmse","mse","mape","mae"))
  
  #Error Rate - 20.16%
  #Accuracy - 79.84%
  
########################################## Conclusion ############################################  
  # Among the three ML algorithms chosen for study, Linear Regression model after a Stepwise model
  # selection performed better with minimal error rate of 15.94% and Accuracy of 84.06%
################################################################################################## 

    