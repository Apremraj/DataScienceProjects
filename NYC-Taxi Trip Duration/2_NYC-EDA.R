# 1.Introduction
  # 1.1 Load libraries and helper functions
  library(ggplot2) #visualisation
  library(dplyr) #manipulation
  library(data.table) #manipulation
  library(tibble) #wrangling
  library(tidyr) #wrangling
  library(stringr) #string manipulation
  library(lubridate) #date and time
  library(leaflet) #maps
  library(geosphere) #maps
  library(xgboost) #modeling
  library(caret) #modeling

  #1.2 Load Data
    setwd("C://Ashish/Others/R/Projects/NYC Taxi Duration/Data")
    train <- as.tibble(fread('train.csv'))
    test <- as.tibble(fread('test.csv'))
  
  #1.3 File Structure and content
    summary(train)
    glimpse(train)
    summary(test)
    glimpse(test)
    View(train)
  
  #1.4 Find Missing Values
    sum(is.na(train))
    sum(is.na(test))
  
  #1.5 Reformatting features
    train <- train %>%
      mutate(pickup_datetime <- ymd_hms(pickup_datetime),
             dropoff_datetime <- ymd_hms(dropoff_datetime),
             vendor_id <- as.factor(vendor_id),
             passenger_count <- as.factor(passenger_count))
  
  #1.6 Checking consistency if the trip duration is consistent with the date intervals
    train %>%
      mutate(check = abs(int_length(interval(dropoff_datetime, pickup_datetime))+ trip_duration) > 0)%>%
      select(check,pickup_datetime, dropoff_datetime, trip_duration)%>%
      group_by(check)%>%
      count()
    
#2 Individual Feature Visualizations
    #2.1 Map using leaflet - We can see that most of the pickups are from Manhattan and some towards south east - JFK airport
    set.seed(1234)
    foo <- sample_n(train,8e3)
    leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap")%>%
      addCircleMarkers(~ foo$pickup_longitude, ~ foo$pickup_latitude, radius = 1,
                       color = "blue", opacity = 0.8)
    
    #2.2 Plotting Target feature variable - Trip duration
      #1.Majority of rides follow a smooth distribution that looks almost log-normal with a peak of just short of (1e+03)1000s i.e 17 minutes
      #2.There are suspisciously shorter rides with less than 10 seconds (1e+01) duration.
    train %>%
      ggplot(aes(trip_duration))+
      geom_histogram(fill = "red", bins = 150)+
      scale_x_log10()+
      scale_y_sqrt()
    
    #2.3 distribution of passenger count and vendor id
    train %>%
      group_by(passenger_count)%>%
      count()%>%
      ggplot(aes(passenger_count, n, fill = passenger_count))+
      geom_col()+
      scale_y_sqrt()
      
    train %>%
      ggplot(aes(vendor_id, fill = vendor_id))+
      geom_bar()
    
    #Day of the week
    train %>%
      mutate(wday = wday(pickup_datetime,label = TRUE))%>%
      group_by(wday, vendor_id)%>%
      count()%>%
      ggplot(aes(wday, n, colour = vendor_id))+
      geom_point(size = 4)+
      labs(x = "Day Of the Week", y = "Total number of pickups")+
      theme(legend.position = "None")
    
    #Hour of the day pickup timings
    train %>%
      mutate(hour = hour(pickup_datetime))%>%
      group_by(hour, vendor_id)%>%
      count()%>%
      ggplot(aes(hour,n,color = vendor_id))+
      geom_point(size = 8)+
      labs(x = "Hour of the day", y = "Total number of pickups")
    
    #Month of the year and hour of the day for pickups
    train %>%
      mutate(hpick = hour(pickup_datetime),
             Month = factor(month(pickup_datetime, label = T)))%>%
      group_by(hpick, Month)%>%
      count()%>%
      ggplot(aes(hpick,n, color = Month))+
      geom_line(size = 2)+
      labs(x="Hour of the day", y = "Count")
    
    #Weekday and hour of the day for pickups
    train %>%
      mutate(hpick = hour(pickup_datetime),
             wday = wday(pickup_datetime, label = T))%>%
      group_by(hpick, wday)%>%
      count()%>%
      ggplot(aes(hpick,n,color = wday))+
      geom_line(size = 2)+
      labs(x="Hour of the day", y = "Count")
    
#3.Feature Relations - How the features are related to eachother and to target variable - Trip duration
    #3.1 Pickup date time vs Trip duration
      #a. how does the variation in trip numbers through out the day and the week affect the average trip duration?
      #b. Do quieter days and hours lead to faster trips?
    train%>%
      mutate(wday = wday(pickup_datetime, label = T))%>%
      group_by(wday, vendor_id)%>%
      summarise(median_duration = median(trip_duration)/60)%>%
      ggplot(aes(wday, median_duration, color = vendor_id))+
      geom_point(size = 8)+
      labs(x = "Hour of the day", y = "Median trip duration(Min)")+
      theme(legend.position = 'none')

    train %>%
      mutate(hour = hour(pickup_datetime))%>%
      group_by(hour,vendor_id)%>%
      summarise(median_duration = median(trip_duration)/60)%>%
      ggplot(aes(hour, median_duration, color = vendor_id))+
      geom_point(size = 6)+
      geom_smooth(method = "loess", span= 1/2)+
      labs(x = "Hour of the day", y = "Median trip duration(Min)")+
      theme(legend.position = 'none')
    #The weekday and hour of a trip appears to be important features for predicting its duration and should be included in prediction model.
    
    #3.2 Passenger count and vendor vs trip duration
    train%>%
      ggplot(aes(factor(passenger_count), trip_duration, color = passenger_count),group = 1)+
      geom_boxplot()+
      scale_y_log10()+
      facet_wrap(~ vendor_id)+
      labs(x = "Number of passengers", Y= "Trip duration [s]")
      
    #3.3 Densities of trip duration for two vendors
    train %>%
      ggplot(aes(trip_duration, fill = factor(vendor_id)))+
      geom_density(position = 'stack')+
      scale_x_log10()
    
#4.Feature Engineering - finding better prediction variables for target variable
    #4.1 Finding direct distrance of the trip
    jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
    la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)
    pick_coord <- train %>%
      select(pickup_longitude,pickup_latitude)
    drop_coord <- train %>%
      select(dropoff_longitude,dropoff_latitude)
    train$dist <- distCosine(pick_coord, drop_coord) #Distance
    train$bearing <- bearing(pick_coord, drop_coord) #Direction of Travel
    #Calculating distance coordinates for JFK and laGuardia airport
    train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
    train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
    train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
    train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)
    
    train <- train %>%
      mutate(speed = dist/trip_duration*3.6)
    
    set.seed(123)
    train %>%
      sample_n(5e4)%>%
      ggplot(aes(dist,trip_duration))+
      geom_point()+
      scale_x_log10()+
      scale_y_log10()+
      labs(x = "Direct Distance[m]", y ="Trip Duration")
    #a.We find that distance generally increases with increasing trip duration
    
    #Let's filter data to remove the extreme data points(>24hour trips) and < 2mins
    train %>%
      filter(trip_duration < 3600 & trip_duration > 120)%>%
      filter(dist > 100 & dist < 100e3)%>%
      ggplot(aes(dist,trip_duration))+
      geom_bin2d(bins = c(500,500))+
      scale_x_log10()+
      scale_y_log10()+
      labs(x = "Direct Distance[m]", y ="Trip Duration")
    
    #Calculating speed based on hour and day of the week
    train%>%
      mutate(hour = hour(pickup_datetime),
             wday = wday(pickup_datetime, label = T))%>%
      group_by(wday, hour)%>%
      summarise(median_speed = median(speed))%>%
      ggplot(aes(hour,wday, fill = median_speed))+
      geom_tile()+
      scale_fill_distiller(palette = "Spectral")+
      labs(x="Hour of the Day", y="Day of the Week")
    #a.Our taxis seem to be travelling faster on the weekends than on the weekdays
    
#5.Data Cleaning
    #5.1 Day plus trips
    day_plus_trips <- train %>%
      filter(trip_duration > 24*3600)
    day_plus_trips %>% select(pickup_datetime, dropoff_datetime, speed)
    
    #Plotting in map after cleaning - Longer than a day trip
    ny_map <- as.tibble(map_data('state', region = 'new york:manhattan'))
    
    t_pick <- day_plus_trips %>%
      select(lon = pickup_longitude, lat = pickup_latitude)
    t_drop <- day_plus_trips %>%
      select(lon = dropoff_longitude, lat = dropoff_latitude)
    p1 <- ggplot()+
      geom_polygon(data = ny_map, aes(x = long, y = lat), fill = "grey60")+
      geom_point(data = t_pick, aes(x = lon, y = lat), size =1, color = "red")+
      geom_point(data = t_drop, aes(x = lon, y = lat), size =1, color = "blue")
    
    for(i in seq(1, nrow(t_pick))){
      inter <- as.tibble(gcIntermediate(t_pick[i,], t_drop[i,],n=30, addStartEnd = TRUE))
      p1 <- p1+geom_line(data = inter, aes(x = lon, y = lat), color = "blue")
    }
    
    p1+ggtitle("Longer than a day trips related to Manhattan")
    
    #5.2 Day long trips in manhattan - between 22 and 24 hours
    day_trips <- train %>%
      filter(trip_duration < 24*3600 & trip_duration > 22*3600)
    
    day_trips %>%
      arrange(desc(dist))%>%
      select(dist,pickup_datetime,dropoff_datetime, speed)%>%
      head(5)
    
    ny_map <- as.tibble(map_data('state', region = "new york:manhattan"))
    
    set.seed(2018)
    day_trips <- day_trips %>%
      sample_n(100)
    
    tpick <- day_trips %>%
      select(d_lon = pickup_longitude, d_lat = pickup_latitude)
    tdrop <- day_trips %>%
      select(d_lon = dropoff_longitude, d_lat = dropoff_latitude)
    
    p1 <- ggplot()+
      geom_polygon(data = ny_map, aes(x = long, y = lat), fill = "grey") +
      geom_point(data = tpick, aes(x = d_lon, y = d_lat), size = 1, alpha = 1, color = "red")+
      geom_point(data = tdrop, aes(x = d_lon, y = d_lat), size = 1, alpha = 1, color ="blue")
    
    for(i in seq(1, nrow(tpick))){
      inter_d <- as.tibble(gcIntermediate(tpick[i,], tdrop[i,], addStartEnd = TRUE, n = 30))
      p1 <- p1+geom_line(data = inter_d, aes(x = lon, y = lat), color = "blue")
    }
    
    p1 + ggtitle("Day Long trips in Manhattan")
    #Conclusion : there are about 1800 connections. these are extreme trip duration. So we will remove trip duration > 22 hours.
    
    
    
    #5.3 Minute long trips
    ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
    
    
    min_trips <- train %>%
      filter(trip_duration < 5*60 & dist > 0)
    
    set.seed(1234)
    foo <- min_trips %>%
      sample_n(600)
    
    tpick <- foo %>%
      select(lon = pickup_longitude, lat = pickup_latitude)
    
    tdrop <- foo %>%
      select(lon = dropoff_longitude, lat = dropoff_latitude)
    
    p1 <- ggplot() +
      geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
      geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
      geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)
    
    for (i in seq(1,nrow(tpick))){
      inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
      p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
    }
    
    p1 + ggtitle("Minute-long trips in relation to Manhattan")
    
    #5.4 Final Cleaning based on above observations
    train <- train %>%
      filter(trip_duration < 22*3600,
             dist > 0 | (near(dist,0) & trip_duration < 60),
             jfk_dist_pick < 3e5 & jfk_dist_drop <3e5,
             trip_duration >10,
             speed <100
      )

#6.External Data
  #6.1 Data import, overview, formating and joining
    weather <- as.tibble(fread("weather_data_nyc_centralpark_2016(1).csv"))   
    glimpse(weather)
    
    #Turn date into lubridate object, convert Traces("T") of snow and percipitation into smaller numeric amounts. 
    weather <- weather %>%
      mutate(date = dmy(date),
             rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
             s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
             s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
             all_precip = rain + s_fall,
             has_snow = (s_fall > 0) | (s_depth >0),
             has_rain = rain > 0,
             max_temp = `maximum temperature`,
             min_temp = `minimum temperature`
             )
    glimpse(weather)
    foo <- weather %>%
      select(date, rain, s_fall, all_precip, s_depth, has_snow, has_rain, max_temp, min_temp)
    
    train$date <- date(train$pickup_datetime)
    train <- left_join(train, foo, by = "date")
    
  #6.2 Visualization and impact on trip duration
    #how many trips per day 
    train %>%
      group_by(date) %>%
      count() %>%
      ggplot(aes(date,n/1000)) +
      geom_line(size = 1.5, color = "red") +
      labs(x = "", y = "Kilo trips per day")
    
    #mean of snowfall based on date
    train %>%
      group_by(date)%>%
      summarise(trips = n(),
                snow_fall = mean(s_fall)
                )%>%
      ggplot(aes(date, snow_fall))+
      geom_line(color = "blue", size = 2)+
      scale_x_date(limits = ymd(c("2015-12-29", "2016-06-30")))
    
    #snow depth based on date
    train%>%
      group_by(date)%>%
      summarise(trips = n(),
                snow_depth = mean(s_depth)
                )%>%
      ggplot(aes(date, snow_depth))+
      geom_line(color = "blue", size = 2)
    
    #Median speed based on dates
    train %>%
      group_by(date)%>%
      summarise(trips = n(),
                median_speed = median(speed)
                )%>%
      ggplot(aes(date, median_speed))+
      geom_line(color = "red", size =2)
    
    #Speed during snow days
    train %>%
      filter(has_snow == TRUE)%>%
      group_by(date)%>%
      summarise(speed = median(speed),
                duration = median(trip_duration))%>%
      arrange(desc(duration,date))%>%
      head(50)
      
    #Conclusion : There has been heavy snow fall on jan 23rd,2016 with top 5 slowest snow days which leads to slow speed and less commuters. 

#7.Correlations overview    
    foo <- train %>%
      select(-id, -pickup_datetime, -dropoff_datetime, -jfk_dist_pick,
             -jfk_dist_drop, -lg_dist_pick, -lg_dist_drop, -date,
             -store_and_fwd_flag, -rain, -s_fall, -all_precip,
             -has_rain, -s_depth, -min_temp, -max_temp
             ) %>%
      mutate(passenger_count = as.integer(passenger_count),
             vendor_id = as.integer(vendor_id),
             has_snow = as.integer(has_snow)) %>%
      select(trip_duration, speed, passenger_count, vendor_id, has_snow, dist)

    foo %>%
      cor(use="complete.obs", method = "spearman") %>%
      corrplot(type="lower", method="circle", diag=FALSE)
