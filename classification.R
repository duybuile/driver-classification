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

sql = "SELECT profile_id, policy_id, driver_id, policy_distance, score, acceleration, braking, cornering, speeding, date, aggregated_distance, dob, gender
FROM profiles
INNER JOIN profile_sub_scores on profiles.sub_scores_id = profile_sub_scores.id
INNER JOIN user_profiles ON profiles.driver_id = user_profiles.id;"

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
library(randomForest)
library(VSURF)
library(caret)
source("P:/Duy/R/Chillidrive Analysis/connect_to_MySQL.R")
setwd("C:/Users/duy.bui/Documents/GitHub/driver-classification/")

load("all_profile_scores.Rdata")
load("data_for_classification.Rdata")

library(rpart)

'/USE CART from rpart to find the relavant features'

m <- merge(x = all_profile_scores, y = t, by.x = "driver_id", by.y = "driver_id")
m <- subset(m, !is.na(m$dob) & !is.na(m$gender))

age = function(from, to = Sys.Date()) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

m$age <- age(m$dob)
m$gender <- as.factor(m$gender)

# Take a quantile
quantile(m$total_trip, seq(0.1, 1, 0.1))
# 10% is equal to 24 trips

m <- subset(m, m$total_trip > 20)

'/Categorise the fields
- Final score: over 60 as good, 30 to 60 as average, lower than 30 as bad'

m$driver_type = ""
m[m$final_score >= 80, "driver_type"] = "excellent"
m[m$final_score >= 50 & m$final_score <80, "driver_type"] = "good"
m[m$final_score >= 30 & m$final_score <50, "driver_type"] = "average"
m[m$final_score < 30, "driver_type"] = "bad"

m$driver_type <- as.factor(m$driver_type) # convert to factor

table(m$driver_type)

par(mfrow= c(1, 2))
plot(m$total_trip, xlab = "", ylab = "Total trips")
plot(m$ratio, xlab = "", ylab = "Ratio of daytime over night time driving", ylim = c(0, 50))

#-----------------
# Regression and classification - NOT USING THIS PART ANYMORE

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

# 3. Random forests
set.seed(10)
fit <- randomForest(driver_type ~ acceleration + braking + cornering + speeding, data=m, importance=TRUE, proximity=TRUE)
fit <- randomForest(driver_type ~ ratio + total_trip, data=m, importance=TRUE, proximity=TRUE)
fit <- randomForest(driver_type ~ ratio + total_trip +  age, data=m, importance=TRUE, proximity=TRUE)
round(importance(fit), 2)
print(fit)

#-------------------
set.seed(10)
inTrain <- createDataPartition(y = m$driver_type, p = 0.7, list = FALSE)
training <- m[inTrain,]
testing <- m[-inTrain,]

modFit <- train(driver_type ~ ratio + total_trip +  age, data = m, importance = T, proximity = T, method = "rf")
modFit
modFit2 <- train(driver_type ~ ratio + total_trip, data = m, importance = T, proximity = T, method = "rf")
modFit2

predicted <- predict(modFit, newdata = training)
table(predicted == training$driver_type)

predicted2 <- predict(modFit, newdata = testing)
table(predicted2 == testing$driver_type)

confusionMatrix(table(predicted, training$driver_type))$overall
confusionMatrix(table(predicted2, testing$driver_type))$overall
#--------
predicted <- predict(modFit2, newdata = training)
table(predicted == training$driver_type)

predicted2 <- predict(modFit2, newdata = testing)
table(predicted2 == testing$driver_type)

confusionMatrix(table(predicted, training$driver_type))$overall
confusionMatrix(table(predicted2, testing$driver_type))$overall

#-----------
load("employment.Rdata")
t <- merge(x = m, y = employment_status, by.x = "policy_id", by.y = "policy_id", all.x = T)
t$employment_status_ft <- as.factor(t$employment_status_ft)
t <- subset(t, !is.na(t$employment_status_ft))
set.seed(10)
inTrain <- createDataPartition(y = t$driver_type, p = 0.7, list = FALSE)
training <- t[inTrain,]
testing <- t[-inTrain,]
modFit3 <- train(driver_type ~ age + gender + employment_status_ft, data = t, importance = T, proximity = T, method = "rf")
modFit3
predicted <- predict(modFit3, newdata = training)
table(predicted == training$driver_type)

predicted2 <- predict(modFit3, newdata = testing)
table(predicted2 == testing$driver_type)

confusionMatrix(table(predicted, training$driver_type))$overall
confusionMatrix(table(predicted2, testing$driver_type))$overall

varImp(modFit)
importance(modFit$finalModel)
print(modFit$finalModel)

varImp(modFit2)
importance(modFit2$finalModel)
print(modFit2$finalModel)

varImp(modFit3)
importance(modFit3$finalModel)
print(modFit3$finalModel)

#importance(predicted)
#class(modFit$)

# Data visualisation
#### PERSONAL INFORMATION DATASET
table(t$age)
table(t$gender)
table(t$employment_status_ft)
table(t$driver_type)

#### TELEMATICS INFORMATION DATASET


#### HYBRID INFORMATION DATASET
library(graphics)

par(mfrow= c(1, 2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(t$age, ylim = c(16,50), main = "Age distribution", ylab = "Age")
plot(t$gender, ylim = c(0, 800), main = "Gender distribution")
plot(t$employment_status_ft, ylim = c(0, 1000), main = "Employment distribution")
piedat <- as.data.frame(table(t$driver_type))
pie(piedat$Freq, labels = piedat$Var1, main="Driver type distribution")

par(mfrow= c(1, 2))
plot(modFit, log = "y")
plot(modFit2, log = "y")
plot(modFit3, log = "y")


ggplot(t, aes(x=age)) +
  geom_line(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(data=t, aes(x=gender, y=driver_type, fill=gender)) +
  geom_bar(colour="black", stat="identity")

# 4. Gradient boosting tree
library(gbm)
set.seed(10)
fit <- gbm(driver_type ~ ratio + total_trip +  age, data=m, distribution = "multinomial")
print(fit)
gbm.perf(fit)

# Variable Selection for Random Forest
set.seed(10)
var.selection <- VSURF(x = subset(m, select = c("policy_distance", "gender", "distance", "ratio", "total_trip", "age")),
                       y = m$driver_type, mtry = 100)
names(var.selection)
summary(var.selection)
plot(var.selection)

var.selection$varselect.thres
var.selection$varselect.pred
var.selection$varselect.interp
