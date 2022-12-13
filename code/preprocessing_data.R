library(tidymodels)
library(data.table)
library(nycflights13)
library(skimr)

set.seed(123)

flightData = data.table(flights)
flightData[, `:=`(
  #convert arrival delay to factor
  arr_delay = factor(ifelse(arr_delay >= 30, 'late', 'on_time')),
  #create date for recipe
  date = lubridate::as_date(time_hour))]
#including weather data
flightData = merge(flightData, weather, by = c('origin', 'time_hour'))
#retaining only specific columns
flightData = flightData[, .(
  dep_time, flight, origin, dest, air_time, distance, carrier, date, arr_delay, 
  time_hour)]
#exclude missing data
flightData = na.omit(flightData)
#convert character columns to factor
charCols = names(Filter(is.character, flightData))
flightData[, (charCols) := lapply(.SD, as.factor), .SDcols = charCols]

#checking proportion of flights that arrived > 30 minutes late
flightData[, .N, .(arr_delay)][, .(N, prop = N/sum(N))]

#check classes of columns
glimpse(flightData)
skimr::skim(flightData, dest, carrier)

#split the data into train and test
set.seed(222)
#put 3/4 of the data into the training set
dataSplit = initial_split(flightData, prop = 3/4)

#create data.tables for the two sets
trainData = training(dataSplit)
trainData

testData = testing(dataSplit)
testData

#creating basic recipe with arr_delay as outcome and all other variables as predictors
flightsRec = recipe(arr_delay ~ ., data = trainData)
flightsRec

#retain flight and time_hour, but don't include them in the model
flightsRec = update_role(flightsRec, flight, time_hour, new_role = 'ID')
flightsRec

#look at current set of variables and roles
data.table(summary(flightsRec))

#create features
