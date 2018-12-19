#Part 1: Setup
  #1.1 Load Libraries
library(data.table)
library(knitr)
library(ggplot2)
library(dplyr)
library(GGally)
library(lubridate)
library(Matrix)
library(taRifx)
library(caret)
library(xgboost)
library(parallel)
library(doParallel)
library(parallelMap)

  #1.2 Load Data
    setwd("C://Ashish/Others/R/Projects/NYC Taxi Duration/Data")
    train <- fread('train.csv')
    test <- fread('test.csv')

    summary(train)
        
#Part 2: Data Cleaning
  #Note: There are no missing values in the dataset to impute
  #2.1 Convert integer variables to numeric for both train and test to run XGBoost  
    train <- japply(train, which(sapply(train,class)== "integer"),as.numeric)  
    test <- japply(test, which(sapply(test, class)== "integer"),as.numeric)
  
  #2.2 Convert pickup datetime to POSIXct for both train and test dataset
    train$pickup_datetime <- as.POSIXct(train$pickup_datetime)
    test$pickup_datetime <- as.POSIXct(test$pickup_datetime)
    
  #2.3 Filter passenger counts <= 0 from train 
    train <- train%>%
      filter(train$passenger_count > 0)
  
  #2.4 Remove geospatial outliers not in NY and NJ coordinates
    usa <- map_data("usa")
    state <- map_data("state")
    ny_state <- state[state$region == "new york",]
    nj_state <- state[state$region == "new jersey",]
    kable(summary(ny_state))
    kable(summary(nj_state))
    
    table(ny_state$region)
    
    #Remove data not in NY and NJ based on max and min long and lat coord.
    train <- train  %>%
      filter(pickup_longitude<=-71.88) %>%
      filter(pickup_longitude>=-79.77) %>%
      filter(dropoff_longitude>=-79.77) %>%
      filter(dropoff_longitude<=-71.88) 
    train <- train %>%
      filter(pickup_latitude<=45.01) %>%
      filter(pickup_latitude>=38.93) %>%
      filter(dropoff_latitude>=38.93) %>%
      filter(dropoff_latitude<=45.01)
    
    kable(summary(train))
    kable(summary(test))    
    
    # Clear memory of the map_data dataframes as these wil no longer be needed
    rm(nystate)
    rm(njstate)
    rm(states)
    rm(usa)
    
  #2.5 Trip duration, let's remove trip duration hours around 3 hours(10800s) as there are some outliers 
    train <- train%>%
      filter(trip_duration < 10800)
  
  #2.6 Add column logduration to be used as a response variable for the model
    train <-  train %>%
      mutate(logduration=log(trip_duration+1))
  
  #2.7 Let's quickly visualize a sample of our cleaned train dataset
    set.seed(123)
    trainsample <- sample_n(train, 10000)
    
  #2.8 Plot train data categorical variables using ggpairs
    ggpairs(trainsample[,c("vendor_id","passenger_count","store_and_fwd_flag","trip_duration","logduration")],
            upper = list(continuous = "points", combo = "box"), lower = list(continuous = "points", combo = "box"))
    
  #2.9 Plot train data numerical variables
    ggpairs(trainsample %>% select(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,logduration,trip_duration))
    
#Part 3. Modeling
  #3.1 Model selection
    #a. Since we know the outcome continuous variable, we will use a supervised ML algorithm.
      # As it appears from our EDA that we will need a non linear regression model, we will choose XGBoost algorithm.
      # We will use RMSE as our metric and finally select the final model based on the lowest RMSE.
    #b. In R, XGBoost package uses the following
      # a. A matrix of input data instead of a data frame
      # b. Only numeric variables
      # c. The target variable separately which we will code to y.
    
  #3.2 Preprocessing 
    #3.2.1 Remove Near Zero variance but for XGBoost we don't need to elimiate these variables
    #nsv <- nearZeroVar(train, saveMetrics = TRUE)
    #kable(nsv[nsv$nzv == TRUE,])
    
    # Remove near zero variance variables
    # train <- train %>%  
    # dplyr::select(-starts_with("store_and_fwd_flag"))
    
    # Apply to the test set
    # test <- test %>%  
    # dplyr::select(-starts_with("store_and_fwd_flag"))
    
    #3.2.2 Preprocessing with Caret - We do not need for XGBoost
    #preProc <- preProcess(train, method = c('center','scale'))
    #preProc
    
  #3.3 Feature Engineering
    #3.3.1 Date and Time
    # Convert the datetime variables into separate date and time variables with class POSIXct. Using dplyr R package functions and isoweek from the R package lubridate
    train <-  train %>%
      dplyr::mutate(pickup_wday= wday(pickup_datetime)) %>%
      dplyr::mutate(pickup_hour= as.numeric(hour(pickup_datetime))) %>%
      dplyr::mutate(pickup_minute= as.numeric(minute(pickup_datetime))) %>%
      dplyr::mutate(pickup_month= month(pickup_datetime)) %>%
      dplyr::mutate(pickup_weekofyear= lubridate::isoweek(pickup_datetime)) %>%
      dplyr::mutate(pickup_weekhour= pickup_wday*24+pickup_hour)
    test <-  test %>%
      dplyr::mutate(pickup_wday= wday(pickup_datetime)) %>%
      dplyr::mutate(pickup_hour= as.numeric(hour(pickup_datetime))) %>%
      dplyr::mutate(pickup_minute= as.numeric(minute(pickup_datetime))) %>%
      dplyr::mutate(pickup_month= month(pickup_datetime)) %>%
      dplyr::mutate(pickup_weekofyear= lubridate::isoweek(pickup_datetime)) %>%
      dplyr::mutate(pickup_weekhour= pickup_wday*24+pickup_hour)

    # Convert the datetime variables into the weekday, hour and month and rush hour categorical features. 
    train <-  train  %>%
      mutate(latehour= ifelse(pickup_hour>20,"Yes",ifelse(pickup_hour<6,"Yes","No")))  %>%  #daily 50-cent surcharge from 8pm to 6am.
      mutate(pmrushhour=ifelse(pickup_wday %in% c(1,2),"No", ifelse(pickup_hour<16,"No",ifelse(pickup_hour>20,"No","Yes")))) # $1 surcharge from 4pm to 8pm on weekdays, excluding holidays.
    
    test <-  test   %>%
      mutate(latehour= ifelse(pickup_hour>20,"Yes",ifelse(pickup_hour<6,"Yes","No")))  %>%  # daily 50-cent surcharge from 8pm to 6am.
      mutate(pmrushhour=ifelse(pickup_wday %in% c(1,2),"No", ifelse(pickup_hour<16,"No",ifelse(pickup_hour>20,"No","Yes")))) # $1 surcharge 
    # from 4pm to 8pm on weekdays, excluding holidays.

    #3.3.2 Pick up and Drop Location Features
    
    # Add Airport  pickup / drop off features
    la_guardia_lon = -73.872611
    la_guardia_lat = 40.77725
    jfk_lon = -73.778889
    jfk_lat = 40.639722
    nw_lon = -73.179
    nw_lat = 40.669385
    # Set a radius for the airport locations
    airport_radius <- 0.03 
    airport_radius_lg <- 0.01 
    test <- test %>%
      mutate(lga_pickup = ifelse(pickup_longitude >= la_guardia_lon - airport_radius_lg & 
                                   pickup_longitude <= la_guardia_lon + airport_radius_lg & 
                                   pickup_latitude >= la_guardia_lat - airport_radius_lg & 
                                   pickup_latitude <= la_guardia_lat + airport_radius_lg, 1, 0))
    train <- train %>%
      mutate(lga_pickup = ifelse(pickup_longitude >= la_guardia_lon - airport_radius_lg &  
                                   pickup_longitude <= la_guardia_lon + airport_radius_lg &  
                                   pickup_latitude >= la_guardia_lat - airport_radius_lg & 
                                   pickup_latitude <= la_guardia_lat + airport_radius_lg, 1, 0))
    test <- test %>%
      mutate(lga_dropoff = ifelse(dropoff_longitude >= la_guardia_lon - airport_radius_lg &   
                                    dropoff_longitude <= la_guardia_lon + airport_radius_lg & 
                                    dropoff_latitude >= la_guardia_lat - airport_radius_lg &  
                                    dropoff_latitude <= la_guardia_lat + airport_radius_lg, 1, 0))
    train <- train %>%
      mutate(lga_dropoff = ifelse(dropoff_longitude >= la_guardia_lon - airport_radius_lg & 
                                    dropoff_longitude <= la_guardia_lon + airport_radius_lg &
                                    dropoff_latitude >= la_guardia_lat - airport_radius_lg & 
                                    dropoff_latitude <= la_guardia_lat + airport_radius_lg, 1, 0))
    test <- test %>%
      mutate(jfk_pickup = ifelse(pickup_longitude >= jfk_lon - airport_radius &
                                   pickup_longitude <= jfk_lon + airport_radius &
                                   pickup_latitude >= jfk_lat - airport_radius &
                                   pickup_latitude <= jfk_lat + airport_radius, 1, 0))
    train <- train %>%
      mutate(jfk_pickup = ifelse(pickup_longitude >= jfk_lon - airport_radius &
                                   pickup_longitude <= jfk_lon + airport_radius &
                                   pickup_latitude >= jfk_lat - airport_radius &
                                   pickup_latitude <= jfk_lat + airport_radius, 1, 0))
    test <- test %>%
      mutate(jfk_dropoff = ifelse(dropoff_longitude >= jfk_lon - airport_radius &
                                    dropoff_longitude <= jfk_lon + airport_radius &
                                    dropoff_latitude >= jfk_lat - airport_radius &
                                    dropoff_latitude <= jfk_lat + airport_radius, 1, 0))
    train <- train %>%
      mutate(jfk_dropoff = ifelse(dropoff_longitude >= jfk_lon - airport_radius &
                                    dropoff_longitude <= jfk_lon + airport_radius &
                                    dropoff_latitude >= jfk_lat - airport_radius &
                                    dropoff_latitude <= jfk_lat + airport_radius, 1, 0))
    test <- test %>%
      mutate(nw_pickup = ifelse(pickup_longitude >= nw_lon - airport_radius &
                                  pickup_longitude <= nw_lon + airport_radius &
                                  pickup_latitude >= nw_lat - airport_radius &
                                  pickup_latitude <= nw_lat + airport_radius, 1, 0))
    train <- train %>%
      mutate(nw_pickup = ifelse(pickup_longitude >= nw_lon - airport_radius &
                                  pickup_longitude <= nw_lon + airport_radius &
                                  pickup_latitude >= nw_lat - airport_radius &
                                  pickup_latitude <= nw_lat + airport_radius, 1, 0))
    test <- test %>%
      mutate(nw_dropoff = ifelse(dropoff_longitude >= nw_lon - airport_radius &
                                   dropoff_longitude <= nw_lon + airport_radius &
                                   dropoff_latitude >= nw_lat - airport_radius &
                                   dropoff_latitude <= nw_lat + airport_radius, 1, 0))
    train <- train %>%
      mutate(nw_dropoff = ifelse(dropoff_longitude >= nw_lon - airport_radius &
                                   dropoff_longitude <= nw_lon + airport_radius &
                                   dropoff_latitude >= nw_lat - airport_radius &
                                   dropoff_latitude <= nw_lat + airport_radius, 1, 0))
    train$jfk_trip <- ifelse(train$jfk_pickup | train$jfk_dropoff, 1, 0)
    train$lga_trip <- ifelse(train$lga_pickup | train$lga_dropoff, 1, 0)
    train$nw_trip <- ifelse(train$nw_pickup | train$nw_dropoff, 1, 0)
    test$jfk_trip <- ifelse(test$jfk_pickup | test$jfk_dropoff, 1, 0)
    test$lga_trip <- ifelse(test$lga_pickup | test$lga_dropoff, 1, 0)
    test$nw_trip <- ifelse(test$nw_pickup | test$nw_dropoff, 1, 0)
    
    #3.3.3. Filter out the predictor variables that are not useful
    
    # Remove the identifier variables that do not have predictive value, in train and test sets
    train <- train %>% 
      dplyr::select(-starts_with("Id"))  %>% 
      dplyr::select(-starts_with("dropoff_datetime"))  %>% 
      dplyr::select(-starts_with("pickup_datetime")) 
    test_id <- test %>% 
      dplyr::select(id)
    test <- test %>% 
      dplyr::select(-starts_with("Id")) %>% 
      dplyr::select(-starts_with("dropoff_datetime"))%>% 
      dplyr::select(-starts_with("pickup_datetime")) 
    
    #3.3.4. Numerical Variables for XGBoost
    
    # Convert integer  to numerical variables for both train and test sets in order to run XGBoost
    train <-  train %>%
      dplyr::mutate(store_and_fwd_flag= as.numeric(store_and_fwd_flag)) %>%
      dplyr::mutate(latehour= as.numeric(latehour)) %>%
      dplyr::mutate(pmrushhour= as.numeric(pmrushhour)) 
    test <-  test %>%
      dplyr::mutate(store_and_fwd_flag= as.numeric(store_and_fwd_flag)) %>%
      dplyr::mutate(latehour= as.numeric(latehour)) %>%
      dplyr::mutate(pmrushhour= as.numeric(pmrushhour))
    
  #3.4 Data Splitting
    set.seed(222)
    #We use createDataPartition to create balanced splits of data
    inTrain <- createDataPartition(train$logduration, p= 0.8, list = F, times = 1)
    training <- train[inTrain,]
    valid <- train[-inTrain,]
  
  #3.5 Data Matrix
    #One method to convert dataframe to a sparse matrix is through R caret package. This will transform all categorical features to binary values except logduration which is our target variable.
    #But to use advanced features in xgboost as recommended we will use xgb.DMatrix function
    y = training$logduration
    dTrain <- xgb.DMatrix(data = data.matrix(select(training, -logduration, -trip_duration)),
                          label = y)
    dValid <- xgb.DMatrix(data = data.matrix(select(valid, -logduration, - trip_duration)),
                          label = valid$logduration)
    dTest <- xgb.DMatrix(data.matrix(test))
    #We will use watchlist to measure the progress with a second dataset which is already classified
    watchlist <- list(train = dTrain, test = dValid)
    nrow(dTest)
    
  #3.6 Model Parameters 
    #eval_metric [no default, depends on objective selected]
    #These metrics are used to evaluate a model's accuracy on validation data. For regression, default metric is RMSE.
    
  #3.7 Cross Validation
    #Using the inbuilt xgb.cv function for k-fold cross validation, let's calculate the best nrounds for this model. In addition, this function also returns CV error, which is an estimate of test error.
    set.seed(1234)
    #use gblinear booster with a large nrounds = 400
    xgbcv <- xgb.cv(params = list(booster = "gblinear",
                    objective = "reg:linear",
                    lambda = 0,
                    alpha = 1,
                    eta = 0.3,
                    gamma = 0,
                    max_depth = 6,
                    min_child_weight = 1,
                    subsample = 1,
                    colsample_bytree = 1),
                    data = dTrain,
                    label = y,
                    nrounds = 400,
                    nfold = 5,
                    showsd = T,
                    stratified = T,
                    print_every_n = 20, #when verbose = 0
                    early_stopping_rounds = 20,
                    maximize = F,
                    verbose = 0
                    )
    #nrounds best iteration is:
    best_iteration <- xgbcv$best_iteration
    #xgbcv gblinear minimum train RMSE mean
    min(xgbcv$evaluation_log$train_rmse_mean)
    #xgbcv gblinear minimum test RMSE mean
    min(xgbcv$evaluation_log$test_rmse_mean)
  
  #3.8 Model Training
    #Lets use basic XGBoost function with the nrounds from the best iteration determined by the xgb.cv function
    set.seed(1234)
    bstlinear <- xgboost(data = dTrain,
                         label = y,
                         objective = "reg:linear",
                         booster = "gblinear",
                         nrounds = best_iteration,
                         lambda= 0,
                         gamma = 0,
                         alpha = 1,
                         verbose = 0)
    bst_tree <- xgboost(data = dTrain,
                        label = y,
                        params = list(booster = "gbtree",
                                      objective = "reg:linear",
                                      lambda = 0,
                                      alpha = 1,
                                      gamma = 0,
                                      eta = 0.3,
                                      max_depth = 6,
                                      min_child_weight = 1,
                                      sub_sample = 1,
                                      colsample_bytree = 1),
                        nrounds = best_iteration,
                        verbose = 0)
    
    #bstlinear minimum train RMSE mean
    min(bstlinear$evaluation_log$train_rmse)
    #bst_tree minimum train RMSE mean
    min(bst_tree$evaluation_log$train_rmse)
    
    #Tabulate cross validation's predictions of the model
    xgbpred_linear <- predict(bstlinear, dValid)
    xgbpred_tree <- predict(bst_tree, dValid)
    
    #Evaluation using RMSE of linear model, R-Squared of the models
    postResample(xgbpred_linear, valid$logduration)
    #Evaluation using RMSE of tree model, R-Squared of the models
    postResample(xgbpred_tree, valid$logduration)
    #Concusion: It appears that the gbtree booster gives a much lower RMSE than the gblinear booster
    
    #One way to measure progress in learning of a model is using xgb.train, providing a second dataset already classified. Therefore it can learn on the first dataset and test its model on the second one. Metrics are measured after each round during the learning.
    # Parameters for xgb.train, which can be tuned in future iterations.
    params <- list(colsample_bytree = 1, #variables per tree 
                   subsample = 1, 
                   booster = "gbtree",
                   max_depth = 6,             
                   min_child_weight = 1,#added to list
                   eta = 0.3, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   lambda=0, 
                   alpha=1,
                   gamma = 0)
    
    set.seed(321)
    #Train model with gbtree and above params
    bst <- xgb.train(params = params,
                     data = dTrain,
                     print_every_n = 20, #visualizing the errors minimizing with each round
                     watchlist = watchlist,
                     nrounds = best_iteration,
                     verbose = 1
                     )

    #Calculate predictions for XGBoost
    predictvalid <- predict(bst, dValid)
    postResample(predictvalid, valid$logduration)
    
  #3.9 Importance Matrix
    #Lets get the trained model(bst) and dump in text format
    model = xgb.dump(bst,with_stats = T)
    names = dimnames(dTrain)[[2]]
    #Compute feature important matrix
    important_matrix = xgb.importance(feature_names = names, model = bst)
    head(important_matrix)
    #plot the important matrix
    gp = xgb.plot.importance(important_matrix)
    gp
    #plot graph
    xgb.plot.importance(important_matrix[1:10,])

#Part 4.Predictions
    predictXgb <- predict(bst, dTest)
    #convert the log of the duration back to duration in seconds
    pred_var <- test_id %>% mutate(trip_duration = exp(predictXgb) - 1)
    
    # Create Kaggle Submission File
    my_solution <- data.frame(Id = test_id, trip_duration = pred_var$trip_duration)
    # Check the number of rows in the solution file is 625,134
    nrow(my_solution)
    # Write solution to file submissionFile1.csv
    write.csv(my_solution, file = "submissionFile.csv", quote=F, row.names=F)
 