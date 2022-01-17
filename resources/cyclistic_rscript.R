# -------------------------
# Author: Ahmad Ridho
# Email: ahmdrdo@gmail.com
# -------------------------

setwd("~/Collection/Portfolio/Cyclistic Bike-sharing")

# Load packages.
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(skimr)

# =====================================================
# IMPORT DATA
# =====================================================

data_dir <- "~/Collection/Portfolio/Cyclistic Bike-sharing/Used Dataset"

# Stack individual files as one.
trips_raw <- list.files(path = data_dir, pattern = ".csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

# =====================================================
# COMPLETE MISSING DATA
# =====================================================

trips_clean <- trips_raw
trips_clean <- trips_clean %>% # Fill missing station name.
  mutate(start_station_name = coalesce(start_station_name, end_station_name),
         end_station_name = coalesce(end_station_name, start_station_name))

# =====================================================
# TIDYING
# =====================================================

# Remove unnecessary columns.
trips_clean <- select(trips_clean, -c(start_station_id, end_station_id,
                                      start_lat, start_lng,
                                      end_lat, end_lng))
# Rename columns.
trips_clean <- rename(trips_clean, trip_id = ride_id,
                      bike_type = rideable_type,
                      membership = member_casual)

# =====================================================
# TRANSFORM DATA
# =====================================================

# Calculate trip duration (result in minutes).
trips_clean$trip_duration <- difftime(trips_clean$ended_at, trips_clean$started_at, units = "mins")
# Reformat trip duration.
trips_clean$trip_duration <- as.numeric(as.character(trips_clean$trip_duration))

# Generate Date, Month, Day of Week, and Hour.
trips_clean$date <- as.Date(trips_clean$started_at)
trips_clean$month <- format(trips_clean$date, "%B")
trips_clean$day_of_week <- format(trips_clean$date, "%A")

# Fix the order of ordinal data.
trips_clean$month <- ordered(trips_clean$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
trips_clean$day_of_week <- ordered(trips_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# =====================================================
# CLEANING
# =====================================================

# Remove NA and trip duration < 1 min (inc. minus duration).
trips_clean <- trips_clean %>%  
  na.omit() %>% 
  filter(trip_duration >= 1)

# Filter data only for 2021.
trips_clean <- filter(trips_clean, date >= "2021-01-01" & date <= "2021-12-31")

# =====================================================
# VALIDATION
# =====================================================

skim_without_charts(trips_clean)

# =====================================================
# DATA VISUALIZATION
# =====================================================

# Bike used by rider type.
ggplot(data = trips_clean, aes(x = bike_type, fill = membership)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Most Frequent Bike to Ride", caption = "*Data from Jan-Dec 2021", x = "Bike Type", y = "Num. of Trips") +
  scale_x_discrete(labels = c("Classic Bike", "Docked Bike", "Electric Bike"))
  
# Number of trips by month.
ggplot(data = trips_clean, aes(x = month, fill = membership)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Trips by Month", caption = "*Data from Jan-Dec 2021", y = "Num. of Trips") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(labels = scales::comma)

# Number of trips by day.
ggplot(data = trips_clean, aes(x = day_of_week, fill = membership)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Trips by Day", caption = "*Data from Jan-Dec 2021", y = "Num. of Trips") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(labels = scales::comma)

# Heatmap number of Trips
trips_clean %>% 
  count(membership, month, day_of_week, name = "count") %>% 
  ggplot(aes(x = day_of_week, y = month, fill = count)) +
  geom_tile() +
  facet_wrap(~membership) +
  scale_fill_gradientn(name = "Num. of Trips", colors = rainbow(3)) +
  labs(title = "Heatmap of Number of Trips", caption = "*Data from Jan-Dec 2021",
       x = "Day of Week", y = "Month") +
  theme(axis.text.x = element_text(angle = 45))

# Average trip duration.
trips_clean %>% 
  group_by(membership, day_of_week) %>% 
  summarise(avg_duration = mean(trip_duration)) %>% 
  ggplot(aes(x = day_of_week, y = avg_duration, fill = membership)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Average Trip Duration by Day", caption = "*Data from Jan-Dec 2021",
       x = "Day of week", y = "Trip Duration (mins)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45))

# Top stations of casual riders.
trips_clean %>% 
  filter(membership == "casual") %>% 
  count(start_station_name, name = "count") %>% 
  top_n(n = 10, wt = count) %>% 
  ggplot(aes(x = count, y = start_station_name, fill = count)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Popular Stations of Casual Riders", subtitle = "Measured by total trips started at the stations.", caption = "*Data from Jan-Dec 2021",
       x = "Num. of Trips", y = "Station Name") +
  theme(axis.text.x = element_blank(), axis.title = element_blank()) +
  geom_text(aes(label = count))



  
  
  
  