### Cyclistic_Exercise_Full_Year_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It's originally based on the case study "'Sophisticated, Clear, and Polished': Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for the case study. The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: "In what ways do members and casual riders use Divvy bikes differently?"

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# libridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  
library(rmarkdown)
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(chron)
getwd() #displays your working directory
setwd("Z:/School stuff/Google Data Analytic Certificate/Case study/DATA")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here


apr_2020<-read.csv("202004_tripdata.csv")
may_2020<-read.csv("202004_tripdata.csv")
jun_2020<-read.csv("202006_tripdata.csv")
jul_2020<-read.csv("202007_tripdata.csv")
aug_2020<-read.csv("202008_tripdata.csv")
sep_2020<-read.csv("202009_tripdata.csv")
oct_2020<-read.csv("202010_tripdata.csv")
nov_2020<-read.csv("202011_tripdata.csv")
dec_2020<-read.csv("202012_tripdata.csv")
jan_2021<-read.csv("202101_tripdata.csv")
feb_2021<-read.csv("202102_tripdata.csv")
mar_2021<-read.csv("202103_tripdata.csv")
apr_2021<-read.csv("202104_tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

#Creating a list to contain the name of each month 

month_2021<- list(apr_2020,may_2020,jun_2020,jul_2020,aug_2020,sep_2020,oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021)

# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

for (m in month_2021) {
  print(colnames(m))
}


# Rename columns  to make them consisent 
iterator = 1
while (iterator < 14) {
  (month_2021[[iterator]] <- rename(month_2021[[iterator]]
               ,trip_id = ride_id
               ,bike_id = rideable_type 
               ,start_time = started_at
               ,end_time = ended_at  
               ,from_station_name = start_station_name 
               ,from_station_id = start_station_id 
               ,to_station_name = end_station_name 
               ,to_station_id = end_station_id 
               ,usertype = member_casual))
  iterator = iterator + 1
}

#Confirmation of rename
print(colnames(month_2021[[1]]))
print(colnames(month_2021[[13]]))


# Inspect the dataframes and look for inconguencies

for (m in month_2021) {
  str(m)
}

# Convert ride_id and rideable_type to character so that they can stack correctly

iterator = 1
while (iterator < 14) {
  month_2021[[iterator]] <- mutate(month_2021[[iterator]]
                                              ,trip_id = as.character(trip_id)
                                              ,bike_id = as.character(bike_id)
                                              ,from_station_id = as.character(from_station_id)
                                              ,to_station_id = as.character(to_station_id)) 
                                  
  iterator = iterator + 1
}
#Confirm the colnames match 
#colnames(apr_2020)
#print(colnames(month_2021[[1]]))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(month_2021[[1]]
                       ,month_2021[[2]]
                       ,month_2021[[3]]
                       ,month_2021[[4]]
                       ,month_2021[[5]]
                       ,month_2021[[6]]
                       ,month_2021[[7]]
                       ,month_2021[[8]]
                       ,month_2021[[9]]
                       ,month_2021[[10]]
                       ,month_2021[[11]]
                       ,month_2021[[12]]
                       ,month_2021[[13]])

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

all_trips <- na.omit(all_trips) #Remove data that has missing information.


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$start_time,format="%m/%d/%Y") #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
print(all_trips$ride_length)
converted_time <- as.numeric(times(all_trips$ride_length))
print(converted_time)
all_trips$ride_length<-60* 24 * converted_time
print(all_trips$ride_length)

all_trips <- na.omit(all_trips) #filter out dates with incorrect format, blank etc.



#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride


summary(all_trips$ride_length)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = min)


# reorder day of week.
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)

print(all_trips$ride_length)

# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = day_of_week) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, weekday)								# sorts

summary(all_trips)
# visualize the number of rides by rider type
all_trips %>% 
  mutate(weekday = day_of_week) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
    geom_col(position = "dodge")

# create a visualization for average duration

all_trips %>% 
  mutate(weekday = day_of_week) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'Z:/School stuff/Google Data Analytic Certificate/Case study/avg_ride_length.csv')

