## load libraries
library(tidyverse)

## assign the link for the lyft data
lyft_url <- "https://github.com/jessicapadilla/uber_lyft/blob/master/lyft.csv.zip?raw=true"

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

## assign the link for the uber data
uber_url <- "https://github.com/jessicapadilla/uber_lyft/blob/master/uber.csv.zip?raw=true"

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
  rename(apparent_temperature = apparentTemperature,
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

rideshare %>% ggplot(aes(weekday, price)) + geom_jitter()
