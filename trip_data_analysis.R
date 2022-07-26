library(tidyverse)
library(lubridate)
library(ggplot2)

trip_data <- read_csv('trip_data_cleaned.csv')

# ride length summary
summary(trip_data$ride_length)

# set day of week labels, correct day column to ymd, add week_num column
trip_data <- trip_data %>% 
  mutate(day_of_week=wday(started_at,label=TRUE, abbr=FALSE)) %>% 
  mutate(day = date(started_at)) %>% 
  mutate(week = isoweek(started_at))

# check ride length average per user type
ride_length_df <- trip_data %>% 
  group_by(member_casual, day) %>% 
  summarise(num_trips = n(), avg_trip_length = mean(ride_length), median_trip_length = median(ride_length))

# plot ride length chart
chart_1 <- ggplot(data = ride_length_df) +
  geom_point(mapping = aes(x = avg_trip_length, y = num_trips, colour = member_casual)) +
  scale_x_continuous(name = "Average trip length") +
  scale_y_continuous(name = "Number of trips") +
  scale_color_discrete(name = "") +
  labs(title = "Number of trips & trip length per user type")


# create df that summarises the number of trips by date and the rider membership
hm_data <- trip_data %>% 
  select(day, day_of_week, week, month, year, member_casual) %>% 
  group_by(member_casual, day) %>% 
  mutate(num_trips = n()) %>% 
  distinct(day, member_casual, .keep_all = TRUE)

# create separate df for casual and member riders
hm_member <- hm_data %>% 
  filter(member_casual == "member")

hm_casual <- hm_data %>% 
  filter(member_casual == "casual")

# plot heatmap for member riders
chart_2 <- ggplot(data = hm_member) + 
  geom_tile(mapping = aes(x = week, y = day_of_week, fill = num_trips),colour = "white") +
  scale_fill_viridis_b(name = "Number of Trips") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.title = element_blank()) +
  labs(title = "Member Riders")

# plot heatmap for casual riders
chart_3 <- ggplot(data = hm_casual) + 
  geom_tile(mapping = aes(x = week, y = day_of_week, fill = num_trips),colour = "white") +
  scale_fill_viridis_b(name = "Number of Trips") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.title = element_blank()) +
  labs(title = "Casual Riders")

# create df grouping number of trips per time of day
tod_df <- trip_data %>% 
  mutate(hour = hour(started_at)) %>%
  group_by(hour, member_casual) %>% 
  summarise(num_trips = n()/1000)

# change hour order 
level_order <- c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0, 1 ,2, 3, 4, 5)

# plot stacked bar chart displaying the number of trips per member type and time of day
chart_4 <- ggplot(data = tod_df) +
  geom_bar(mapping = aes(x = factor(tod_df$hour, level = level_order), y = tod_df$num_trips, fill=tod_df$member_casual),
           position = "stack",
           stat = "identity") +
  scale_x_discrete(name = "Time of day (hour)") + 
  scale_y_continuous(name = "Number of trips (k)") +
  scale_fill_discrete(name = "") +
  labs(title = "Most popular time of day")
           
