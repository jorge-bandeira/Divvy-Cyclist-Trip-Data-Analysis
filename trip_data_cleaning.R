library(tidyverse)
library(lubridate)

# load data
Jun_21 <- read.csv("data/trip_data/202106-divvy-tripdata.csv")
Jul_21 <- read.csv("data/trip_data/202107-divvy-tripdata.csv")
Aug_21 <- read.csv("data/trip_data/202108-divvy-tripdata.csv")
Sep_21 <- read.csv("data/trip_data/202109-divvy-tripdata.csv")
Oct_21 <- read.csv("data/trip_data/202110-divvy-tripdata.csv")
Nov_21 <- read.csv("data/trip_data/202111-divvy-tripdata.csv")
Dec_21 <- read.csv("data/trip_data/202112-divvy-tripdata.csv")
Jan_22 <- read.csv("data/trip_data/202201-divvy-tripdata.csv")
Feb_22 <- read.csv("data/trip_data/202202-divvy-tripdata.csv")
Mar_22 <- read.csv("data/trip_data/202203-divvy-tripdata.csv")
Apr_22 <- read.csv("data/trip_data/202204-divvy-tripdata.csv")
May_22 <- read.csv("data/trip_data/202205-divvy-tripdata.csv")

# check tables structure
str(Jun_21)
str(Jul_21)
str(Aug_21)
str(Sep_21)
str(Oct_21)
str(Nov_21)
str(Dec_21)
str(Jan_22)
str(Feb_22)
str(Mar_22)
str(Apr_22)

# combine data into a single table
trip_data <- bind_rows(
  Jun_21,
  Jul_21,
  Aug_21,
  Sep_21,
  Oct_21,
  Nov_21,
  Dec_21,
  Jan_22,
  Feb_22,
  Mar_22,
  Apr_22,
  May_22
)

# convert started_at and ended_at from char to date
trip_data$started_at <- ymd_hms(trip_data$started_at)
trip_data$ended_at <- ymd_hms(trip_data$ended_at)

# sort data by started_at
trip_data <- trip_data %>% 
  arrange(started_at)

# create day, month, year and day_of_week columns
trip_data$year <- year(trip_data$started_at)
trip_data$month <- month(trip_data$started_at)
trip_data$day <- day(trip_data$started_at)
trip_data$day_of_week <- wday(trip_data$started_at)

# calculate ride_length
trip_data$ride_length <- difftime(trip_data$ended_at, trip_data$started_at, units = "secs")
trip_data$ride_length <- as.numeric(as.character(trip_data$ride_length))

# remove rows with negative ride length
trip_data_v2 <- trip_data %>% 
  filter(trip_data$ride_length > 0)

# remove empty station ids
trip_data_v2 <- trip_data_v2 %>% 
  filter(start_station_id != "") %>% 
  filter(end_station_id != "")

# create station df
start_station_df <- trip_data_v2[c(5, 6)]
end_station_df <- trip_data_v2[c(7, 8)]
colnames(start_station_df) <- c("station_name", "station_id")
colnames(end_station_df) <- c("station_name", "station_id")
station_df <- bind_rows(start_station_df, end_station_df) %>% 
  distinct(station_id, .keep_all = TRUE) %>% 
  arrange(station_id)

# remove test and repair trips
trip_data_v2 <- trip_data_v2 %>% 
  filter(!str_detect(start_station_id, "test")) %>% 
  filter(!str_detect(start_station_id, "TEST")) %>%
  filter(!str_detect(start_station_id, "REPAIR")) %>%
  filter(!str_detect(start_station_id, "checking"))
  
# check for duplicates
sum(duplicated(trip_data_v2$ride_id))

# save cleaned dataset
write_csv(trip_data_v2,"trip_data_cleaned.csv")
