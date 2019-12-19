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
lyft_num <- lyft %>% select(c(surge_multiplier, hour, day,
                              price, distance,
                              temperature, precip_intensity, 
                              humidity, wind_speed, 
                              visibility, cloud_cover))

## round the data to one decimal place
corr <- round(cor(lyft_num), 1)

## create a correlogram
ggcorrplot(corr, type = "lower",
           lab = TRUE, lab_size = 3.5,
           title = "Correlogram Of Variables",
           ggtheme = theme_bw())

## set the seed
set.seed(123, sample.kind = "Rounding")

## there are 51235 observations
## split the data into training and testing sets
## set 90% of the data for the training set (46111 observations)
## set the remaining 10% of the data for the test set (5124 observations)
## randomize the data by first creating a vector of random integers
train_sample <- sample(51235, 46111)

## use the vector of random integers to randomly select observations from the data
## create the training set
lyft_train <- lyft[train_sample, ]

## create the test set
lyft_test <- lyft[-train_sample, ]


lyft_model <- lm(price ~ surge_multiplier + distance + source + weather_summary, data = lyft_train)
summary(lyft_model)

lyft_pred <- predict(lyft_model, lyft_test)

actuals_preds <- data.frame(cbind(actuals=lyft_test$price, predicteds=lyft_pred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)
