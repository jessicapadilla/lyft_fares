## load libraries
library(tidyverse)
library(psych)
library(ggcorrplot)

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

## check if the visibility and visibility.1 columns are the same
identical(lyft$visibility, lyft$visibility.1)

## convert the data into a tibble
## remove unnecessary columns
## rename the remaining columns
lyft <- as_tibble(lyft) %>% select(-c(id, visibility.1)) %>%
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

## convert the integer columns to numeric
lyft[, c(1:3, 22, 24)] <- sapply(lyft[, c(1:3, 22, 24)], as.numeric)

## filter the data to only include the basic lyft services
lyft <- lyft %>% filter(car_type == "Lyft")

## condense the categories in the weather summary column
lyft <- lyft %>% mutate(weather_summary = case_when(
  weather_summary == " Clear " ~ "Clear",
  weather_summary %in% c(" Overcast ", " Mostly Cloudy ", " Partly Cloudy ", " Possible Drizzle ") ~ "Cloudy",
  weather_summary %in% c(" Light Rain ", " Rain ", " Drizzle ") ~ "Rainy",
  weather_summary == " Foggy " ~ "Foggy"))

## reorder the types of weather conditions
lyft$weather_summary <- factor(lyft$weather_summary,
                                         levels = c("Cloudy", "Clear", "Rainy", "Foggy"))

## reorder the weekdays
lyft$weekday <- factor(lyft$weekday,
                               levels = c("Sun", "Mon", "Tue",
                                          "Wed", "Thu",
                                          "Fri", "Sat"))

## create a graph to show how frequent price surging occurs
## exclude surge_multiplier of 1 since it doesn't cause fare to increase
lyft %>% filter(surge_multiplier > 1) %>%
  ggplot(aes(surge_multiplier)) + 
  geom_histogram(binwidth = 0.25, col = "black", fill = "purple") +
  scale_x_continuous(breaks = seq(1.25, 3.00, 0.25)) +
  coord_cartesian(ylim = c(0, 2250)) +
  xlab("Surge Multiplier") + ylab("Count") +
  ggtitle("Frequency of Price Surging") +
  theme_bw()

## create a graph to show how surge multiplier varies by source location
## exclude surge_multiplier of 1 since it doesn't cause fare to increase
lyft %>% filter(surge_multiplier > 1) %>%
  mutate(source = reorder(source, -surge_multiplier)) %>%
  ggplot(aes(source, surge_multiplier, col = source)) + 
  geom_jitter(alpha = 0.2, size = 2, width = 0.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(1.25, 3.00, 0.25)) +
  xlab("Starting Location") + ylab("Surge Multiplier") +
  ggtitle("Surge Multiplier Based On Starting Location") +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))

## create a graph to show how the surge_multiplier changes with weather
## exclude surge_multiplier of 1 since it doesn't cause fare to increase
lyft %>% filter(surge_multiplier > 1) %>% 
  group_by(weather_summary) %>% count(surge_multiplier) %>%
  ggplot(aes(surge_multiplier, n, col = purple)) + 
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  facet_wrap(. ~ weather_summary) + 
  scale_x_continuous(breaks = seq(1.25, 3.00, 0.25)) +
  xlab("Surge Multiplier") + ylab("Count") + 
  ggtitle("Surge Pricing During Different Weather Conditions") +
  theme_bw()

## revise the data frame to only include numeric values of interest
lyft_num <- lyft %>% select(c(source, destination, company, car_type,
                               weather_summary, weekday))

## round the data to one decimal place
corr <- round(cor(lyft_num), 1)

## create a correlogram
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlogram Of Lyft Data", 
           ggtheme = theme_bw)



lyft %>% 
  filter(surge_multiplier > 1) %>% 
  mutate(surge_multiplier = factor(surge_multiplier)) %>%
  group_by(hour) %>% count(surge_multiplier) %>% 
  ggplot(aes(hour, n, col = surge_multiplier)) + geom_line() +
  geom_point()
  



lyft %>% filter(surge_multiplier > 1) %>% 
  group_by(weekday) %>% count(surge_multiplier)


lyft %>% filter(surge_multiplier > 1) %>% 
  group_by(hour) %>% 
  summarize(avg_surge = mean(surge_multiplier)) %>%
  ggplot(aes(hour, avg_surge)) + geom_point()

lyft %>% filter(surge_multiplier > 1) %>% 
  group_by(weekday) %>% 
  summarize(avg_surge = mean(surge_multiplier)) %>%
  ggplot(aes(weekday, avg_surge)) + geom_point()


lyft %>% ggplot(aes(hour, surge_multiplier)) + geom_point()

cor(lyft[c("surge_multiplier", 
           "hour", "day", "month",
           "price", "distance",
           "temperature",
           "precip_intensity", 
           "humidity", "wind_speed", "visibility")])



lyft %>% 
  ggplot(aes(source, surge_multiplier)) + 
  geom_jitter(alpha = 0.2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

names(lyft)




## create a scatterplot matrix
## show how the price correlates to certain variables
pairs.panels(lyft[c("price", "hour", "day", "month")], 
             density = FALSE, ellipses = FALSE)
             
             
             
             "distance", "temperature", "precip_intensity")])








## check the correlation between price and all the other variables
cor(lyft[c("price", "hour", "day", "month", 
           "distance", "temperature", "apparent_temperature", 
           "precip_intensity", "precip_probability",
           "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
           "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
           "moonphase")])

unique(lyft$car_type)

lyft_shared <- lyft %>% filter(car_type == "Shared")

unique(lyft_shared$surge_multiplier)

cor(lyft_shared[c("price", "hour", "day", "month", 
           "distance", "temperature", "apparent_temperature", 
           "precip_intensity", "precip_probability",
           "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
           "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
           "moonphase")])

unique(lyft$car_type)

lyft_lux <- lyft %>% filter(car_type == "Lux")

unique(lyft_lux$surge_multiplier)

cor(lyft_lux[c("price", "hour", "day", "month", 
                  "distance", "surge_multiplier", "temperature", "apparent_temperature", 
                  "precip_intensity", "precip_probability",
                  "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
                  "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
                  "moonphase")])

unique(lyft$car_type)

lyft_lyft <- lyft %>% filter(car_type == "Lyft")

unique(lyft_lyft$surge_multiplier)

cor(lyft_lyft[c("price", "hour", "day", "month", 
               "distance", "surge_multiplier", "temperature", "apparent_temperature", 
               "precip_intensity", "precip_probability",
               "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
               "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
               "moonphase")])

unique(lyft$car_type)

lyft_xl <- lyft %>% filter(car_type == "Lyft XL")

unique(lyft_xl$surge_multiplier)

cor(lyft_xl[c("price", "hour", "day", "month", 
                "distance", "surge_multiplier", "temperature", "apparent_temperature", 
                "precip_intensity", "precip_probability",
                "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
                "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
                "moonphase")])

test <- lyft %>% filter(car_type == "Lyft") %>% group_by(weekday) %>% tally()
  
  




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

## check the first few rows of data
head(uber)

## check the structure of the data
str(uber)

## check if the visibility and visibility.1 columns are the same
identical(rideshare$visibility, rideshare$visibility.1)

## convert the data into a tibble
## remove unnecessary columns
## rename the remaining columns
uber <- as_tibble(uber) %>% select(-c(id, visibility.1)) %>%
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

## filter the data to focus only on UberX services
uber <- uber %>% filter(car_type == "UberX")

## convert integer columns to numeric
uber[, c(1:3, 10, 22, 24)] <- sapply(uber[, c(1:3, 10, 22, 24)], as.numeric)

## check the correlation between price and all the other variables
cor(uber[c("price", "hour", "day", "month", 
           "distance", "temperature", "apparent_temperature", 
           "precip_intensity", "precip_probability",
           "humidity", "wind_speed", "visibility", "temp_high", "temp_low",
           "dew_point", "wind_bearing", "cloud_cover", "uv_index", 
           "moonphase")])

## create a correlation graph
pairs(uber[c("price", "distance", "temperature", "precip_intensity")])

## set the seed
set.seed(123, sample.kind = "Rounding")

## there are 55094 observations
## split the data into training and testing sets
## set 90% of the data for the training set (49585 observations)
## set the remaining 10% of the data for the test set (5509 observations)
## randomize the data by first creating a vector of random integers
train_sample <- sample(55094, 49585)

## use the vector of random integers to randomly select observations from the data
## create the training set
uber_train <- uber[train_sample, ]

## create the test set
uber_test <- uber[-train_sample, ]

## create a model for forecasting
uber_prices <- lm(price ~ hour + day + month + 
distance + temperature + apparent_temperature + precip_intensity + 
precip_probability + humidity + wind_speed + visibility + temp_high + temp_low + 
dew_point + wind_bearing + cloud_cover + uv_index + moonphase, data = uber_train)

uber_prices <- lm(price ~ distance, data = uber_train)

## get a summary of the model
summary(uber_prices)

## test the model
uber_pred <- predict(uber_prices, data = uber_test)

## create a correlation graph
uber %>% ggplot(aes(distance, price, col = weekday)) + 
  geom_point() + stat_smooth(method = "lm")

qplot(uber$distance, uber$price)

qplot(uber$weather_summary, uber$price)

uber %>% ggplot(aes(weather_summary, price)) + geom_jitter()

uber %>% ggplot(aes(hour, price)) + geom_jitter()







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
rideshare_model <- rpart(price ~ ., data = rideshare)

rideshare_model



rideshare <- rideshare %>% filter(company == "Uber")

unique(rideshare$surge_multiplier)
