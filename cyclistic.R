# Persiapan
# Import library
library(readr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(geosphere)
library(leaflet)

# Import dataset
cyc_2023_07 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202307-divvy-tripdata/202307-divvy-tripdata.csv")
cyc_2023_08 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202308-divvy-tripdata/202308-divvy-tripdata.csv")
cyc_2023_09 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202309-divvy-tripdata/202309-divvy-tripdata.csv")
cyc_2023_10 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202310-divvy-tripdata/202310-divvy-tripdata.csv")
cyc_2023_11 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202311-divvy-tripdata/202311-divvy-tripdata.csv")
cyc_2023_12 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202312-divvy-tripdata/202312-divvy-tripdata.csv")
cyc_2024_01 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202401-divvy-tripdata/202401-divvy-tripdata.csv")
cyc_2024_02 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202402-divvy-tripdata/202402-divvy-tripdata.csv")
cyc_2024_03 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202403-divvy-tripdata/202403-divvy-tripdata.csv")
cyc_2024_04 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202404-divvy-tripdata/202404-divvy-tripdata.csv")
cyc_2024_05 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202405-divvy-tripdata/202405-divvy-tripdata.csv")
cyc_2024_06 <- read.csv("E:/Bootcamp/Digitalent/Google/capstone/Kasus 1 Cyclistic/Data/202406-divvy-tripdata/202406-divvy-tripdata.csv")

# Menggabungkan Dataset
data_concat <- rbindlist(list(cyc_2023_07, cyc_2023_08, cyc_2023_09,cyc_2023_10, cyc_2023_11, cyc_2023_12,
                         cyc_2024_01, cyc_2024_02, cyc_2024_03, cyc_2024_04, cyc_2024_05, cyc_2024_06))
head(data_concat)
glimpse(data_concat)

# Proses
# cleaning data
sum(duplicated(data_concat))
missing_value <- sum(is.na(data_concat))
percent <- (missing_value/nrow(data_concat))
percent
colSums(is.na(data_concat))
data_cleaned <- na.omit(data_concat)
sum(is.na(data_cleaned))
nrow(data_cleaned)

# Transformasi data
data_cleaned$date <- as.Date(data_cleaned$started_at) 
data_cleaned$date_end <- as.Date(data_cleaned$ended_at)
data_cleaned$month <- format(as.Date(data_cleaned$date), "%m")
data_cleaned$day <- format(as.Date(data_cleaned$date), "%d")
data_cleaned$year <- format(as.Date(data_cleaned$date), "%Y")
data_cleaned$day_of_week <- format(as.Date(data_cleaned$date), "%A")
data_cleaned$hour <- format(as.Date(data_cleaned$date), "%H")

# Analisis
# Bike Type
count_bike <- count(data_cleaned, rideable_type)
ggplot(data = count_bike) +
  geom_bar(mapping = aes(x = rideable_type, y = n), stat = "identity", fill = "blue") +
  geom_text(mapping = aes(x = rideable_type, y = n, label = n), vjust = -0.5) +
  labs(x="Rideable Type", y="Number of Rider")
  ylim(0, max(count_bike$n) * 1.1)

# Rider Type
count_cust <- count(data_cleaned, member_casual)
ggplot(data = count_cust)+
  geom_bar(mapping = aes(x = member_casual, y = n), stat = "identity", fill = "red") +
  geom_text(mapping = aes(x = member_casual, y = n, label = n), vjust = -0.5) +
  labs(x = "Member Type", y = "Number of Rider", fill = "Member Type") +
  ylim(0, max(count_cust$n) * 1.1) 

# Bike Type and Rider Type
count_bike_cust <- data_cleaned %>%
  count(rideable_type, member_casual)

ggplot(data = count_bike_cust) +
  geom_bar(mapping = aes(x = rideable_type, y = n, fill = member_casual), stat = "identity", position = "dodge") +
  geom_text(mapping = aes(x = rideable_type, y = n, label = n, group = member_casual), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Rideable Type", y = "Number of Rider", fill = "Member Type") +
  ylim(0, max(count_bike_cust$n) * 1.1)

# Number of Rider in Month
count_month_cust <- data_cleaned %>%
  count(month, member_casual)

ggplot(data = count_month_cust) + 
  geom_bar(mapping = aes(x = month, y = n, fill = member_casual), stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Rider", fill = "Member Type") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Number of Rider per day
count_day_cust <- data_cleaned %>%
  count(day_of_week, member_casual) %>%
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

ggplot(data = count_day_cust) + 
  geom_bar(mapping = aes(x = day_of_week, y = n, fill = member_casual), stat = "identity", position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rider", fill = "Member Type")

# the most popular times of day for casual riders to start their rides
data_cleaned$ride_minutes <-  as.numeric(difftime(data_cleaned$ended_at, data_cleaned$started_at, units = "mins"))

casual <- data_cleaned %>%
  filter(member_casual == "casual")

casual_hour <- casual %>%
  mutate(hour = hour(started_at))

count_hour_cus <- casual_hour %>%
  count(hour) %>%
  arrange(hour)

ggplot(data = count_hour_cus) + 
  geom_line(mapping = aes(x = hour, y = n), stat = "identity") +
  labs(x = "Hour of the Day", y = "Number of Casual Riders") +
  theme_minimal() +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = 0:23)

# the most popular times of day for member riders to start their rides
member <- data_cleaned %>%
  filter(member_casual == "member")

member_hour <- member %>%
  mutate(hour = hour(started_at))

count_hour_member <- member_hour %>%
  count(hour) %>%
  arrange(hour)

ggplot(data = count_hour_member) + 
  geom_line(mapping = aes(x = hour, y = n), stat = "identity") +
  labs(x = "Hour of the Day", y = "Number of Member Riders") +
  theme_minimal() +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = 0:23)

# the top 5 most popular starting stations
# the top 5 most popular starting stations among casual riders
top5_start_station_casual <- casual %>%
  filter(start_station_name != "" & str_trim(start_station_name) != "") %>%
  count(start_station_name, member_casual) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 5)

ggplot(data = top5_start_station_casual) +
  geom_bar(mapping = aes(x = reorder(start_station_name, -n), 
                         y = n, fill = member_casual), stat = "identity") +
  geom_text(mapping = aes(x = reorder(start_station_name, -n), 
                          y = n, label = n), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Start Station Name", y = "Number of Riders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# the top 5 most popular starting stations among member riders
top5_start_station_member <- member %>%
  filter(start_station_name != "" & str_trim(start_station_name) != "") %>%
  count(start_station_name, member_casual) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 5)

ggplot(data = top5_start_station_member) +
  geom_bar(mapping = aes(x = reorder(start_station_name, -n), 
                         y = n, fill = member_casual), stat = "identity") +
  geom_text(mapping = aes(x = reorder(start_station_name, -n), 
                          y = n, label = n), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Start Station Name", y = "Number of Riders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Frequently traveled routes
data_filter <- data_cleaned %>%
  filter(start_lat != end_lat & start_lng != end_lng) %>%
  group_by(start_lat, start_lng, end_lat, end_lng, member_casual, rideable_type) %>%
  summarise(total = n(), .groups="drop") %>%
  filter(total > 250)

sample_size <- min(nrow(data_filter %>% filter(member_casual == "casual")),
                   nrow(data_filter %>% filter(member_casual == "member")))

casual <- data_filter %>%
  filter(member_casual == "casual") %>%
  slice_sample(n = sample_size)

member <- data_filter %>%
  filter(member_casual == "member") %>%
  slice_sample(n = sample_size)

# Casual
leaflet() %>%
  addTiles() %>%
  fitBounds(lng1 = -87.7, lat1 = 41.79, lng2 = -87.55, lat2 = 41.99) %>%
  addPolylines(
    lng = c(casual$start_lng, casual$end_lng),
    lat = c(casual$start_lat, casual$end_lat),
    color = "blue",
    weight = 1
  )

# Member
leaflet() %>%
  addTiles() %>%
  fitBounds(lng1 = -87.7, lat1 = 41.79, lng2 = -87.55, lat2 = 41.99) %>%
  addPolylines(
    lng = c(member$start_lng, member$end_lng),
    lat = c(member$start_lat, member$end_lat),
    color = "red",
    weight = 1
  )