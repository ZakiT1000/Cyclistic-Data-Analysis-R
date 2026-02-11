# Cyclistic-Data-Analysis-R
Code for the whole data life cycle
# --- PART 1: LOAD TOOLS ---
library(tidyverse)
library(lubridate)
library(janitor)

# --- PART 2: IMPORT ---
# We are looking inside the 'csv_data' folder
file_list <- list.files(path = ".", pattern = "*.csv", full.names = TRUE)
all_trips_raw <- file_list %>% map_df(~read_csv(.))

# --- PART 3: CLEANING ---
# This does every cleaning step at once to ensure nothing is missed
all_trips_final <- all_trips_raw %>%
  # 1. Remove rows where station name is "HQ QR" (Test rides)
  filter(start_station_name != "HQ QR" | is.na(start_station_name)) %>%
  # 2. Create the ride_length column (Duration in seconds)
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "secs"))) %>%
  # 3. Remove any ride that is 0 seconds or negative (Errors)
  filter(ride_length > 0) %>%
  # 4. Add the Day of the Week column
  mutate(day_of_week = format(as.Date(started_at), "%A"))

# --- PART 4: VERIFICATION ---
# How many rows did we start with?
nrow(all_trips_raw)
# How many rows do we have now?
nrow(all_trips_final)
# Are there any "HQ QR" left? (This should be 0)
nrow(all_trips_final[all_trips_final$start_station_name == "HQ QR",])

# FIX: Forcefully removing the 1.1 million HQ QR rows
all_trips_final <- all_trips_final[all_trips_final$start_station_name != "HQ QR", ]

# REMOVE any remaining NAs in the station name just to be safe
all_trips_final <- drop_na(all_trips_final, start_station_name)

# FINAL VERIFICATION: This should definitely be 0 now
nrow(all_trips_final[all_trips_final$start_station_name == "HQ QR",])

# Check the new total row count
nrow(all_trips_final)

# --- PART 4: ANALYZE ---

# 1. Who rides longer on average? (Results in secs)
aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual, FUN = mean)

# 2. Who takes more rides on each day?
all_trips_final %>%
  group_by(member_casual, day_of_week) %>%
  arrange(member_casual, day_of_week)

# Count rides per day for each group
all_trips_final %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

# --- PART 5: VISUALIZATIONS ---

# Changing the order of days of the week to chronological
all_trips_final$day_of_week <- factor(all_trips_final$day_of_week, 
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Chart 1 — Total Rides per Day
all_trips_final %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Total Rides by User Type and Day", 
       # ADDED \n BELOW TO CREATE A NEW LINE
       subtitle = "Members dominate the work week;\nCasual riders spike on weekends", 
       x = "Day of the Week", y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  scale_fill_manual(values = c("casual" = "#ffa600", "member" = "#003f5c")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic") # Makes the subtitle look cleaner
  )

# Chart 2 — Average Duration per Day
all_trips_final %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) + # Keeps Y-axis labels consistent with Chart 1
  labs(title = "Average Ride Duration by User Type and Day", 
       # ADDED \n for the line break here:
       subtitle = "Casual riders consistently ride\n2x longer than annual members", 
       x = "Day of the Week", y = "Average Duration (Seconds)",
       fill = "User Type") +
  theme_minimal() +
  scale_fill_manual(values = c("casual" = "#ffa600", "member" = "#003f5c")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic") # Matches Chart 1 styling
  )
