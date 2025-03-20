library(sf)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(readxl)


##BUS STOP LOCATIONS dataset

bus_stops <- st_read("back_end_data/BusStopLocation_Nov2024/BusStop.shp")

#initial exploration
str(bus_stops) #5166 rows, 3 columns
sum(is.na(bus_stops))
bus_stops[apply(is.na(bus_stops), 1, any), ] #missing values mostly NAs in loc_desc (no description), UNK represents likely unknown bus stop number but still exists


#modifying the dataset for bus_stops_final
bus_stops_final <- bus_stops %>%
  rename(
    bus_stop_number = BUS_STOP_N, 
    bus_stop_name = LOC_DESC
  ) %>%
  st_transform(crs = 4326) %>% # Transform to WGS84 (EPSG:4326)
  mutate(
    bus_stop_name = replace_na(bus_stop_name, "Unknown Location"),
    bus_stop_number = replace_na(bus_stop_number, "0"),
    bus_stop_number = ifelse(bus_stop_number == "UNK", "0", bus_stop_number) %>% as.numeric(),
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()




#bus stop data
json <- fromJSON("back_end_data/BusStops/BusStops.json")
bus_stops_df <- json$value

#checking if any of the bus stops in this new dataset are not in our final cleaned bus stop dataset
c(1012, 1013, 1019, 1029) %in% bus_stops_final$bus_stop_number





##MRT STATION LOCATIONS dataset

mrt_station <- st_read("back_end_data/TrainStation_Nov2024/RapidTransitSystemStation.shp")
invalid_geometries <- which(!st_is_valid(mrt_station$geometry))
length(invalid_geometries)  #there are two invalid
st_is_valid(mrt_station$geometry[invalid_geometries], reason = TRUE) #find out reason why invalid: self-intersection
##check which mrt stations are these
mrt_station[invalid_geometries, ] #harbourfront and upper thomson
mrt_station_removed <- mrt_station[-invalid_geometries, ]


#remove the invalid geometries and work on the rest to get lat and long
mrt_station_partialfixed <- mrt_station_removed %>%
  st_transform(crs = 4326) %>%
  mutate(geometry = st_make_valid(geometry),
         centroid = st_centroid(geometry), 
         latitude = st_coordinates(centroid)[, 2], 
         longitude = st_coordinates(centroid)[, 1]) 

##fixing just the invalid entries and getting the lat and long
mrt_station <- st_read("back_end_data/TrainStation_Nov2024/RapidTransitSystemStation.shp")
invalid_geometries <- which(!st_is_valid(mrt_station$geometry))
mrt_invalid_entries <- mrt_station[invalid_geometries, ]
mrt_invalid_entries_fixed <- mrt_invalid_entries %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_transform(crs = 4326) %>% # Transform to WGS84 (EPSG:4326)
  mutate(
    # Ensure to calculate centroid for MULTIPOLYGON correctly by first casting to POLYGON
    centroid = st_centroid(st_union(st_cast(geometry, "POLYGON"))), # Union and then centroid
    latitude = st_coordinates(centroid)[, 2],
    longitude = st_coordinates(centroid)[, 1]
  )


#combining both the partial fixed and the invalid entries fixed datasets 
mrt_station_finalfixed <- bind_rows(mrt_station_partialfixed, mrt_invalid_entries_fixed) %>% 
  select(TYP_CD_DES, STN_NAM_DE, longitude,latitude) %>%
  st_drop_geometry() %>%
  rename(station_name = STN_NAM_DE,
         type = TYP_CD_DES) %>%
  mutate(type = factor(type, levels = c("MRT", "LRT")))
         
  

##BUS ROUTES
json <- fromJSON("back_end_data/BusRoutes/BusRoutes.json")
bus_routes <- json[2]$value


