#===============================
# Installing packages necessary
#===============================
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)

#=====================
# COLLECTING DATA
#=====================
#Link to the dataset https://divvy-tripdata.s3.amazonaws.com/index.html

q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")


#======================================
# Checking col names of the datasets 
#======================================

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

#===================================================
# Renaming col names (so it's easier when stacking)
#===================================================

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"
                   ,rideable_type = "X01...Rental.Details.Bike.ID" 
                   ,started_at = "X01...Rental.Details.Local.Start.Time"  
                   ,ended_at = "X01...Rental.Details.Local.End.Time"  
                   ,start_station_name = "X03...Rental.Start.Station.Name" 
                   ,start_station_id = "X03...Rental.Start.Station.ID"
                   ,end_station_name = "X02...Rental.End.Station.Name" 
                   ,end_station_id = "X02...Rental.End.Station.ID"
                   ,member_casual = "User.Type"))

#=================================================
# Converting col datatype (easier when stacking)
#=================================================

q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))


#======================================================
# combining 4 smaller datasets into one big data frame
#======================================================

all_trips <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)


#=====================================================================
# remove unwanted columns (some data collecting was dropped in 2020)
#=====================================================================

all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,"X01...Rental.Details.Duration.In.Seconds.Uncapped","X05...Member.Details.Member.Birthday.Year","Member.Gender","tripduration"))

#=========================================
# CLEAN UP DATA TO PREPATE FOR ANALYSIS
#=========================================

#in 'member_casual', Subscriber=Member and Customer=casual (keeping only 2 instead of 4)
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber"="member","Customer"="casual"))


#convert the date ---> day, month, year for more data to work with later
#add a new column that is the date format of 'started_at' column
all_trips$date <- as.Date(all_trips$started_at)

#extract month
all_trips$month <- format(as.Date(all_trips$date),"%m")
#day
all_trips$day <- format(as.Date(all_trips$date),"%d")
#year
all_trips$year <- format(as.Date(all_trips$date),"%Y")
#day_of_week
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

#add 'ride length', a calculated column to 'all_trips' in secs
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#convert the datatype of 'ride_length'
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#some entries in 'ride_length' contains 0 and negative values,
#hence, creating a new version of dataset with these rows removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]


#=============================================
# DESCRIPTIVE ANALYSIS on all_trips_v2 dataset
#=============================================

# descriptive analysis on ride_length
#calculate avg,median,max,min (in categories members and casual) using aggregate function

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# aggregate it further to include day of week: to see avg ride time by each day for members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# using ordered function to order it from SUN-SAT for analysis purposes
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))


#analyzing ridership data by type and week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)

#GGPLOT- Visualize the above by rider type for TOTAL NO. OF RIDES
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  #label=TRUE makes returns the weekday in characters
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) + geom_col(position = "dodge")

#GGPLOT- Visualize the above by rider type for AVERAGE DURATION OF RIDE
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual)) + geom_col(position = "dodge")


#assigning a new name and exporting the dataframe we generated earlier for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write_csv(counts, file = '/Users/namrathasmacbookpro/Desktop/JOB 2023/Case Study/dataset/avg_ride_length.csv')

