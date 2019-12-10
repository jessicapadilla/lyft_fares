## load libraries
library(tidyverse)
library(rpart)
library(rpart.plot)

## assign the link for the lyft zip folder
lyft_url <- "https://github.com/jessicapadilla/uber_and_lyft/blob/master/lyft.csv.zip?raw=true"

## create a temporary file so the data can be downloaded into it
lyft_temp <- tempfile()

## download the zip folder containing the data
download.file(lyft_url, lyft_temp, mode = "wb")

## unzip the zip folder
unzip(lyft_temp, "lyft.csv")

## read the csv file within the zip folder
lyft <- read.csv("lyft.csv", sep = ",", header = TRUE)

## check the structure of the data
str(lyft)

## assign the link for the uber zip folder
uber_url <- "https://github.com/jessicapadilla/uber_and_lyft/blob/master/uber.csv.zip?raw=true"

## create a temporary file so the data can be downloaded into it
uber_temp <- tempfile()

## download the zip folder containing the data
download.file(uber_url, uber_temp, mode = "wb")

## unzip the zip folder
unzip(uber_temp, "uber.csv")

## read the csv file within the zip folder
uber <- read.csv("uber.csv", sep = ",", header = TRUE)

## check the structure of the data
str(uber)

## combine the lyft and uber data tables
rideshare <- rbind(lyft, uber)

## check the structure of the data
str(rideshare)

## convert the data into a tibble
rideshare <- as_tibble(rideshare)

## check if the visibility and visibility.1 columns are the same
identical(rideshare$visibility, rideshare$visibility.1)

## remove the visibility.1 and id columns
## rename some of the other columns
rideshare <- rideshare %>% select(-c(id, visibility.1)) %>%
  rename(company = cab_type,
         car_type = name,
         apparent_temperature = apparentTemperature,
         weather_summary = short_summary,
         precip_intensity = precipIntensity,
         precip_probability = precipProbability,
         wind_speed = windSpeed,
         temp_high = temperatureHigh,
         temp_low = temperatureLow,
         dew_point = dewPoint,
         wind_bearing = windBearing,
         cloud_cover = cloudCover,
         uv_index = uvIndex,
         moonphase = moonPhase)

## remove rows that do not have prices listed
rideshare <- na.omit(rideshare, cols = price)

## list the names of all the columns
names(rideshare)

## check the distribution of the prices
summary(rideshare$price)

## create a histogram for the distribution of prices
rideshare %>% ggplot(aes(price)) + 
  geom_histogram(binwidth = 5, fill = "purple", col = "black") +
  xlab("Price (Dollars)") + ylab("Count") + 
  ggtitle("Distribution of Rideshare Prices") +
  theme_bw()

## check the correlation between price and several features
cor(rideshare[c("hour", "day", "month", 
                "distance", "temperature", "apparent_temperature",
                "precip_intensity", "precip_probability", 
                "humidity", "wind_speed", "visibility",
                "price")])

## check the correlation between distance and price
cor(rideshare[c("distance", "price")])

## create a graph showing the relationship between distance and price
rideshare %>% ggplot(aes(distance, price, col = company)) +
  geom_point()

## create a graph showing the relationship between distance and price for uber
rideshare %>% filter(company == "Uber") %>%
  ggplot(aes(distance, price, col = car_type)) +
  geom_point()

## create a graph showing the relationship between distance and price for lyft
rideshare %>% filter(company == "Lyft") %>%
  ggplot(aes(distance, price, col = car_type)) +
  geom_point()

## set the seed
set.seed(123, sample.kind = "Rounding")

## there are 637976 observations
## split the data into training and testing sets
## set 90% of the data for the training set (574178 observations)
## set the remaining 10% of the data for the test set (63798 observations)
## randomize the data by first creating a vector of random integers
train_sample <- sample(637976, 574178)

## use the vector of random integers to randomly select observations from the data
## create the training set
rideshare_train <- rideshare[train_sample, ]

## create the test set
rideshare_test <- rideshare[-train_sample, ]

## create a regression tree model using the training set
rideshare_model <- rpart(price ~ temperature + precip_intensity + distance, data = rideshare)

rideshare_model
rpart.plot(rideshare_model, digits = 3)


rideshare <- rideshare %>% filter(company == "Uber" & car_type == "UberX")
