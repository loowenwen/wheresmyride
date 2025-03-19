library(sf)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(dpylr)

bus_stops <- st_read("BusStopLocation_Nov2024/BusStop.shp")

#initial exploration
str(bus_stops)
dim(bus_stops) #5166 rows, 3 columns
sum(is.na(bus_stops))
bus_stops[apply(is.na(bus_stops), 1, any), ] #missing values mostly NAs in loc_desc (no description), UNK represents likely unknown bus stop number but still exists


#modifying the dataset 
bus_stops1 <- bus_stops %>% 
  rename(bus_stop_number = BUS_STOP_N, bus_stop_name = LOC_DESC) %>%
  mutate(bus_stop_name = replace_na(bus_stop_name, "Unknown Location")) %>%
  mutate(bus_stop_number = replace_na(bus_stop_number, "0")) %>%
  mutate(bus_stop_number = ifelse(bus_stop_number == "UNK", "0", bus_stop_number)) %>%
  mutate(bus_stop_number = as.numeric(bus_stop_number)) %>%
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2]
  ) 

bus_stops_final <- bus_stops1 %>% st_drop_geometry()

  
