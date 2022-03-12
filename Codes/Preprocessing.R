# ============================================================================
# Date:    March 12, 2022
# Course:  Networks: Concepts and Algorithms
# Master:  Data Science for Decision Making 
#
# Term Paper
#          
# Group: - Simón Caicedo
#        - Diego Eslava  
#        - Philine Meyjohann 
#        - Eszter Pazmandi
# ============================================================================

setwd("G:/Mi unidad/Classroom/21D009 Networks  Concepts and Algorithms 21-22 DS T2/Paper")

# Packages to be used ----
library(tidyverse)
library(igraph)
library(readxl)
library(writexl)

# Reading stations (nodes) data -----

# Reading data with the number of passengers leaving each Transmilenio station
leave<-read_xlsx("Data/Raw Data/Salidas Febrero 2022.xlsx")[,c(2,36)]
leave<-na.omit(leave)
colnames(leave)<-c("station", "total_passangers")

# Reading data with the number of passangers entering each Transmilenio station
enter<-read_xlsx("Data/Raw Data/Entradas Febrero 2022.xlsx")[,c(2,36)]
enter<-na.omit(enter)
colnames(enter)<-c("station", "total_passangers")


# Summarizing data ----

# Getting the average number of passengers per day leaving each station
leave_avg<-leave %>% 
  group_by(station) %>% 
  summarize(mean_passangers_leave = round(sum(total_passangers)/28, 2))  # 28 days


# Getting the average number of passangers per day leaving each station
enter_avg<-enter %>% 
  group_by(station) %>% 
  summarize(mean_passangers_enter = round(sum(total_passangers)/28, 2))  # 28 days


# Getting the codes of each station ----
enter_avg$station_code<-regmatches(enter_avg$station, gregexpr("(?<=\\().*?(?=\\))", enter_avg$station, perl=T))
leave_avg$station_code<-regmatches(leave_avg$station, gregexpr("(?<=\\().*?(?=\\))", leave_avg$station, perl=T))

enter_avg$station_code<-sapply(enter_avg$station_code, toString)
leave_avg$station_code<-sapply(leave_avg$station_code, toString)

# Merging enter/leave data ----
stations_data<-inner_join(enter_avg, leave_avg, by=c("station_code"))[,c(1,3, 2, 5)]
colnames(stations_data)[c(1,2)]<-c("station", "station_code")


# Reading routes (edges) data -----
stop_times<-read.delim("Data/Raw Data/stop_times.txt", sep = ",")


stop_times_df<-
  stop_times[,c(1,4,5)] %>% 
  distinct() %>% 
  group_by(trip_id) %>% 
  mutate(previous_station = lag(stop_id), 
         current_station = stop_id) %>% 
  na.omit()


# G45


num_buses<-
  stop_times[stop_times$stop_sequence==1,] %>% 
  group_by(trip_id) %>% 
  count(trip_id) %>% 
  rename(number_buses_trip= n)


# Keep only stations for which there is also entering/leaving information available
stop_times_df<-stop_times_df[stop_times_df$stop_id %in% stations_data$station_code,]

# Drop self-edges
stop_times_df<-stop_times_df[stop_times_df$previous_station!=stop_times_df$current_station,]


# Join all data
stop_times_df<-left_join(stop_times_df, stations_data, by=c("stop_id"="station_code"))

stop_times_df$edge<-paste(stop_times_df$previous_station, "-" ,stop_times_df$current_station)


stop_times_df<-
  stop_times_df %>% 
  distinct(trip_id, stop_id, previous_station, current_station, .keep_all = TRUE)


# Number of routes joining two stations
count_routes<-
  stop_times_df %>% 
  group_by(edge) %>% 
  count(edge) %>% 
  rename(number_routes = n)
  
# Join number of routes joining two stations with stop_times_df
final_df<-left_join(stop_times_df, count_routes, by=c("edge"))

# Join number of buses per route with final data
final_df<-left_join(final_df, num_buses, by="trip_id")

# Count the number of buses per edge
num_buses_edge<-final_df %>% 
  group_by(edge) %>% 
  summarize(number_buses_route = sum(number_buses_trip))
  

final_df<-left_join(final_df, num_buses_edge, by="edge")
  
  
# Remove datasets that are not longer needed
rm(count_routes, enter, enter_avg, leave, leave_avg, stations_data,
   stop_times_df, stop_times, num_buses, num_buses_edge)


# Join with spatial data (longitude and latitude of each station) -----
stops<-read.delim("Data/Raw Data/stops.txt", sep = ",")
final_df<-left_join(final_df, stops, by=c("stop_id"))


# Export data to excel ----
write_xlsx(final_df, "Data/Data Created/Data Final Transmilenio.xlsx")









