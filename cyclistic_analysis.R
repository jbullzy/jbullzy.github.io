# This is the script I'm working with to clean/analyze the cyclistic data

# Loading tidyverse

install.packages('tidyverse')
library(tidyverse)

# Loading trip data as variables

tripdata_2020_11 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202011-divvy-tripdata.csv")
tripdata_2020_12 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202012-divvy-tripdata.csv")
tripdata_2021_01 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202101-divvy-tripdata.csv")
tripdata_2021_02 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202102-divvy-tripdata.csv")
tripdata_2021_03 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202103-divvy-tripdata.csv")
tripdata_2021_04 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202104-divvy-tripdata.csv")
tripdata_2021_05 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202105-divvy-tripdata.csv")
tripdata_2021_06 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202106-divvy-tripdata.csv")
tripdata_2021_07 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202107-divvy-tripdata.csv")
tripdata_2021_08 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202108-divvy-tripdata.csv")
tripdata_2021_09 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202109-divvy-tripdata.csv")
tripdata_2021_10 <- read.csv("/Users/jackbullington/Documents/Google_Analytics_Certificate/12Mos_TripData/202110-divvy-tripdata.csv")

#=========================
# CLEANING DATA
#=========================

# Data frame from November 2020 doesn't match the rest of the data, fixing this

old_framework <- mutate(tripdata_2020_11, start_station_id = as.character(start_station_id),
                        end_station_id = as.character(end_station_id))

# Combining data with updated framework

new_framework <- bind_rows(tripdata_2020_12, tripdata_2021_01, tripdata_2021_02, tripdata_2021_03,
                           tripdata_2021_04, tripdata_2021_05, tripdata_2021_06, tripdata_2021_07,
                           tripdata_2021_08, tripdata_2021_09, tripdata_2021_10)

# Combining all data

all_trips <- bind_rows(old_framework, new_framework)

# Dropping null values

all_trips_clean <- drop_na(all_trips)

# Standardizing membership status to 'member' or 'casual'

all_trips_clean <- all_trips_clean %>% mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))

# Creating a column with a date format for start time and end time

library(lubridate)
all_trips_clean$date_start <- ymd_hms(all_trips_clean$started_at)
all_trips_clean$date_end <- ymd_hms(all_trips_clean$ended_at)

# Creating a column for the ride length

all_trips_clean$ride_length <- difftime(all_trips_clean$date_end, all_trips_clean$date_start)

# Creating a column for the day of the week

all_trips_clean$day_of_week <- weekdays(all_trips_clean$date_start)

# Creating additional columns for month, day, and year

all_trips_clean$month <- format(as.Date(all_trips_clean$date_start), "%m")
all_trips_clean$day <- format(as.Date(all_trips_clean$date_start), "%d")
all_trips_clean$year <- format(as.Date(all_trips_clean$date_start), "%Y")

# Converting ride_length to numeric

is.factor(all_trips_clean$ride_length)
all_trips_clean$ride_length <- as.numeric(as.character(all_trips_clean$ride_length))
is.numeric(all_trips$ride_length)

# Deleting rows with bad data. Bike was either taken back to HQ for maintenance or the trip length had a negative value

all_trips_final <- all_trips_clean[!(all_trips_clean$start_station_id == "HQ QR" | all_trips_clean$ride_length<0),]

#=========================
# DATA ANALYSIS
#=========================

# Summary of the data

summary(all_trips_final)

# Compare casual riders to members with mean, median, max, min

aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual, FUN = mean)
aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual, FUN = median)
aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual, FUN = max)
aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual, FUN = min)

# Average ride time for casual users compared to members

aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual + all_trips_final$day_of_week, FUN = mean)

# Ordering rows by the days of the week

all_trips_final$day_of_week <- ordered(all_trips_final$day_of_week, levels = c("Sunday", "Monday",
                                "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Redoing average ride time for casual users / members with updated ordering

aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual + all_trips_final$day_of_week, FUN = mean)

# Analyze rider data by membership status and weekday

all_trips_final %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% # Creates weekday field with wday
  group_by(member_casual, weekday) %>% # Groups by user type and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Visualize the number of rides by rider type

all_trips_final %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge")

# Creating a visualization for average duration

all_trips_final %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge")

#=========================
# EXPORTING DATA
#=========================

counts <- aggregate(all_trips_final$ride_length ~ all_trips_final$member_casual + all_trips_final$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/avg_ride_length.csv')


