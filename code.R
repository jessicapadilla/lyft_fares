## load libraries
library(tidyverse)

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

names(rideshare)

rideshare %>% 
  ggplot(aes(weather_summary, price)) +
  geom_bar(stat = "identity")

Black, Black SUV, UberPool, UberX, UberXL, WAV

unique(rideshare$weather_summary)

## check price ranges between companies and their different services
## for uber
unique(rideshare$car_type)

rideshare %>% filter(company == "Uber") %>%
  ggplot(aes(car_type, price)) + geom_jitter()
## UberPool is the least and Black SUV is the most

## for lyft
rideshare %>% filter(company == "Lyft") %>%
  ggplot(aes(car_type, price)) + geom_jitter()
## Shared is least and Lux Black XL is most



## check price for hours, days, and months
## for uber
rideshare %>% filter(company == "Uber") %>%
  ggplot(aes(hour, price, col = car_type)) + geom_jitter()

ggplot(aes(fertility, life_expectancy, col = continent)) + geom_point() + facet_grid(. ~ year)




## create bar plots for destination
## create bar plots for source
## create bar plots for weather summary
## check how distance affects price
## check how temperature affects surge multiplier, price
## check how precipitation affects surge multiplier, price
## check how humidity affects surge multiplier, price
## check how wind speed affects surge multiplier, price
## check how visibility affects surge multiplier, price
## check how wind_bearing affects surge multiplier, price
## check how cloud_cover affects surge multiplier, price
## check how weekday affects surge multiplier, price
## compare prices between companies
## correlation
## regression
## check decision trees

