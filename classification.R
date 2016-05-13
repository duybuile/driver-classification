
setwd("P:/Duy/R/Chillidrive Analysis/")

source("P:/Duy/R/Chillidrive Analysis/connect_to_MySQL.R")

sql = "select * from trips
inner join user_profiles
on trips.driver_id = user_profiles.id
AND trips.status = 'OK'"

trip_data <- getDataFromTelematics(sql)

strptime(trip_data$date[1], format = "%Y-%m-%d %H:%M%S")

t = format(trip_data$date[1], format = "%Y-%m-%d %H:%M%S")
class(t)

library(lubridate)

hour(trip_data$date[1])

day(trip_data$date[1])

trip_data$time_of_day = ""
trip_data$trip_hour = hour(trip_data$date)
trip_data$time_of_day = ifelse(trip_data$trip_hour >=6 & trip_data$trip_hour < 18, "daytime", "nighttime")

daytime <- subset(trip_data, trip_data$trip_hour >=6 & trip_data$trip_hour < 21)
nighttime <- subset(trip_data, trip_data$trip_hour < 6 | trip_data$trip_hour >= 21)
  
mean(daytime$score, na.rm = T)
mean(nighttime$score, na.rm = T)

createDataForClass <- function(trip_data){
  
  trip_data$distance_score <- trip_data$distance * trip_data$score
  t1 = aggregate(distance ~ driver_id, data = trip_data, FUN = sum)
  t2 = aggregate(distance_score ~ driver_id, data = trip_data, FUN = sum)
  t <- merge(x = t1, y = t2, by.x = "driver_id", by.y = "driver_id", all = TRUE)
  t$final_score = t$distance_score/t$distance
  
  t3 <- as.data.frame(table(trip_data$driver_id, trip_data$time_of_day))
  daytime <- subset(t3, t3$Var2 == "daytime", select = c("Var1", "Freq"))
  names(daytime) <- c("driver_id", "day")
  nighttime <- subset(t3, t3$Var2 == "nighttime", select = c("Var1", "Freq"))
  names(nighttime) <- c("driver_id", "night")
  
  t4 <- merge(x = daytime, y = nighttime, by.x = "driver_id", by.y = "driver_id")
  t4$ratio <- t4$day/(t4$night+1) # to avoid divide by 0
  
  t5 <- merge(x = t, y = t4, by.x = "driver_id", by.y = "driver_id")
  
  t5$total_trip = t5$day + t5$night
  
  return(t5)
}

t <- createDataForClass(trip_data)

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


