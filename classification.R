'/1. Load the library'
library(lubridate)
source("P:/Duy/R/Chillidrive Analysis/connect_to_MySQL.R")
setwd("C:/Users/duy.bui/Documents/GitHub/driver-classification/")

# Declare constants
TODAY = Sys.Date()
DST.CONV.RATE = 0.000621371 #(1 meter = 0.000621371 miles)
MIN.AGG.DST = 100/DST.CONV.RATE #(in meters)
DAYTIME = 6
NIGHTTIME = 21

'/2. Collect data'

# Select trip data from telematics
sql = "select * from trips
inner join user_profiles
on trips.driver_id = user_profiles.id
AND trips.status = 'OK'"

trip_data <- getDataFromTelematics(sql)

# strptime(trip_data$date[1], format = "%Y-%m-%d %H:%M%S")
# t = format(trip_data$date[1], format = "%Y-%m-%d %H:%M%S")
# hour(trip_data$date[1])
# day(trip_data$date[1])

trip_data$time_of_day = ""
trip_data$trip_hour = hour(trip_data$date)
trip_data$time_of_day = ifelse(trip_data$trip_hour >= DAYTIME & trip_data$trip_hour < NIGHTTIME, "daytime", "nighttime")

daytime <- subset(trip_data, trip_data$trip_hour >= DAYTIME & trip_data$trip_hour < NIGHTTIME)
nighttime <- subset(trip_data, trip_data$trip_hour < DAYTIME | trip_data$trip_hour >= NIGHTTIME)
  
mean(daytime$score, na.rm = T)
mean(nighttime$score, na.rm = T)

rm(daytime, nighttime)

'/Score for night time driving is a little bit smaller than that during day time. This shows that the driving behaviour
is not affected much by the driving time'

createDataForClass <- function(trip_data){
  
  # Distance_score per trip is calculated by taking trip distance multiplying with trip score
  trip_data$distance_score <- trip_data$distance * trip_data$score
  t1 = aggregate(distance ~ driver_id, data = trip_data, FUN = sum)
  t2 = aggregate(distance_score ~ driver_id, data = trip_data, FUN = sum)
  t <- merge(x = t1, y = t2, by.x = "driver_id", by.y = "driver_id", all = TRUE)
  
  # Final score for each driver. Use it for category
  t$final_score = t$distance_score/t$distance
  
  t3 <- as.data.frame(table(trip_data$driver_id, trip_data$time_of_day))
  daytime <- subset(t3, t3$Var2 == "daytime", select = c("Var1", "Freq"))
  names(daytime) <- c("driver_id", "day")
  nighttime <- subset(t3, t3$Var2 == "nighttime", select = c("Var1", "Freq"))
  names(nighttime) <- c("driver_id", "night")
  
  t4 <- merge(x = daytime, y = nighttime, by.x = "driver_id", by.y = "driver_id")
  
  # Ratio of day over night
  t4$ratio <- t4$day/(t4$night+1) # to avoid divide by 0
  
  t5 <- merge(x = t, y = t4, by.x = "driver_id", by.y = "driver_id")
  
  t5$total_trip = t5$day + t5$night
  
  return(t5)
}

t <- createDataForClass(trip_data)

save(t, file = "data_for_classification.Rdata")

quantile(t$total_trip, seq(0.1, 1, 0.1))

# Remove any customers who have less than 20 trips in total

sql = "SELECT profile_id, driver_id, policy_distance, score, acceleration, braking, cornering, speeding, date, aggregated_distance
FROM profiles
INNER JOIN profile_sub_scores on profiles.sub_scores_id = profile_sub_scores.id"

all_profile_scores <- getDataFromTelematics(sql)

#to be deleted
all_profile_scores$date <- as.Date(all_profile_scores$date, format = '%Y-%m-%d')
all_profile_scores <- subset(all_profile_scores, all_profile_scores$date < TODAY)

all_profile_scores <- subset(all_profile_scores, all_profile_scores$profile_id %in% 
                               aggregate(profile_id ~ driver_id, all_profile_scores, FUN = max)$profile_id &
                               all_profile_scores$aggregated_distance >= MIN.AGG.DST)

save(all_profile_scores, file = "all_profile_scores.Rdata")

#--------------

'/RUN THE PROGRAM FROM HERE'

library(lubridate)
library(rpart)
source("P:/Duy/R/Chillidrive Analysis/connect_to_MySQL.R")
setwd("C:/Users/duy.bui/Documents/GitHub/driver-classification/")

load("all_profile_scores.Rdata")
load("data_for_classification.Rdata")

library(rpart)

'/USE CART from rpart to find the relavant features'

m <- merge(x = all_profile_scores, y = t, by.x = "driver_id", by.y = "driver_id")

# Take a quantile
quantile(m$total_trip, seq(0.1, 1, 0.1))
# 10% is equal to 24 trips

m <- subset(m, m$total_trip > 20)

'/Categorise the fields
- Final score: over 60 as good, 30 to 60 as average, lower than 30 as bad'

m$driver_type = ""
m[m$final_score > 70, "driver_type"] = "good"
m[m$final_score > 50 & m$final_score <=70, "driver_type"] = "average"
m[m$final_score <= 50, "driver_type"] = "bad"

table(m$driver_type)

#-----------------
# Regression and classification

# 1. Classification
fit <- rpart(driver_type ~ ratio, method="class", data=m)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

rsq.rpart(fit)

# 2. Regression
fit <- rpart(final_score ~ ratio + total_trip, method = "anova", data=m)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
