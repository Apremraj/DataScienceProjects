#Load Libraries
library(knitr)
library(data.table)
library(dplyr)
library(GGally)
library(leaflet)

#Part 1: Data

#1.1 Load Data
setwd("C://Ashish/Others/R/Projects/NYC Taxi Duration/Data")
train <- fread("train.csv")
test <- fread("test.csv")

#1.2 Summary Data
kable(summary(train)[,1:5])
kable(summary(train)[,6:11])
View(test)
View(sample_n(train, 100))
summary(train)
glimpse(train)

#1.3 Missing Values
sum(is.na(train)) #There are no missing values in the data

#1.4 Sample Train Data
#Since this is a large dataset we will use random sampling to do our EDA plots
#convert variables to factors and numeric classes
train$vendor_id <- as.factor(train$vendor_id)
train$passenger_count <- as.factor(train$passenger_count)
train$trip_duration <- as.numeric(train$trip_duration)
train$store_and_fwd_flag <- as.factor(train$store_and_fwd_flag)
#Random sample subset of train data for EDA, set seed to be reproducible
set.seed(1234)
trainsample <- sample_n(train, 10000)
testsample <- sample_n(test, 10000)

#Part 2: Exploratory Data Analysis

#2.1 Plots of Categorical variables
ggpairs(trainsample[,c("vendor_id","passenger_count","store_and_fwd_flag","trip_duration")], 
        upper = list(continuous = "points", combo = "box"), lower = list(continuous = "points", combo = "box"))

  #2.1.1 Response variable trip_duration is right skewed with very long tail. we will therefore take the log to normalise the distribution.
  train <- train %>%
    mutate(logduration = log(trip_duration + 1))
  g <- ggplot(data = train, aes(logduration))
  g + geom_histogram(col = "pink", bins = 100)    
  
  #2.1.2 Passenger Count - It is a right skewed distribution. Most cab drivers have single or few passengers.
  train %>%
    group_by(passenger_count)%>%
    count() %>%
    ggplot(aes(x = passenger_count, y = n, fill = passenger_count))+
    geom_col()+
    theme(legend.position = "none")
  
  kable(summary(train$passenger_count))
  
  #Let's view the passenger numbers against the trip duration and split by the vendor id in a boxplot
  g <- ggplot(train, aes(passenger_count, trip_duration, color = passenger_count))
  g + geom_boxplot()+ 
    scale_y_log10()+
    facet_wrap(~ vendor_id) +
    labs(title = "Trip duration by number of passengers", x = "Number of Passengers", y = "Trip duration (s)")
  
  #2.1.3 Store_Fwd_Flag - There seems to be little to no correlation between store and forward flag and trip duration.
  #2.1.4 Vendor ID - There is little to no correlation between vendor id and trip duration.
  
#2.2 Plots of Numerical Variables
  #2.2.1 Pick Up Date Time
  train$pickup_datetime <- as.POSIXct(train$pickup_datetime)
  
  #Based on this plot there seems to be outliers for vendor id 2 scattered above 10000s.
  g <- ggplot(trainsample, aes(pickup_datetime, trip_duration, color = vendor_id))
  g + geom_point()
  
  #2.2.2 Latitudes and longitudes - There are outliers probably outside NYC
  ggpairs(trainsample %>% select(pickup_latitude,pickup_longitude, dropoff_latitude, dropoff_longitude))
  
#2.3 Produce map using leaflet R Package
  map1 <- leaflet(trainsample) %>% #initiate leaf instance
                    addTiles() %>% #add map tiles, default is openstreet map
                    setView(-73.9, 40.75, zoom = 11)%>%
                    addCircles( ~pickup_longitude,~pickup_latitude, weight = 1, radius = 10,
                               color = "red", stroke = T, fillOpacity = 0.8) %>%
                    addCircles( ~dropoff_longitude, ~dropoff_latitude, weight = 1, radius = 10,
                               color = "blue", stroke = T, fillOpacity = 0.8)%>%
                    addLegend("bottomright", colors = "blue", labels = "Drop off location in Train Data", title = "In NewYork city")%>%
                    addLegend("topright", colors = "red", labels = "Pickup location in Train Data", title = "In NewYork City") 
  map1
  