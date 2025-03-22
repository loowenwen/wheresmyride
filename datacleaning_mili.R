library(sf)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(readxl)


##BUS STOP LOCATIONS dataset

bus_stops <- st_read("data/BusStopLocation_Nov2024/BusStop.shp")

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
json <- fromJSON("data/BusStops/BusStops.json")
bus_stops_df <- json$value

#checking if any of the bus stops in this new dataset are not in our final cleaned bus stop dataset
c(1012, 1013, 1019, 1029) %in% bus_stops_final$bus_stop_number





##MRT STATION LOCATIONS dataset

mrt_station <- st_read("data/TrainStation_Nov2024/RapidTransitSystemStation.shp")
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

# Install and load necessary packages


library(httr)
library(jsonlite)

# Set your account key
account_key <- "o6OuJxI3Re+qYgFQzb+4+w=="

# API URL
url <- "https://datamall2.mytransport.sg/ltaodataservice/BusRoutes"

# Function to get data with pagination handling
get_all_bus_routes <- function(url, account_key, max_pages = 10) {
  bus_routes_df <- data.frame()  # Empty dataframe to store all results
  skip <- 0  # Initial skip value
  page_counter <- 1
  
  repeat {
    # Make the GET request with pagination
    response <- GET(
      url,
      query = list(skip = skip),  # Adding the skip parameter to fetch the next page
      add_headers(
        AccountKey = account_key,
        Accept = "application/json"  # Accept JSON response format
      )
    )
    
    # Check if the request was successful
    if(status_code(response) == 200) {
      # Parse the JSON response
      data <- content(response, "text")
      parsed_data <- fromJSON(data, flatten = TRUE)
      
      # Append the current page to the dataframe
      current_page <- as.data.frame(parsed_data$value)
      bus_routes_df <- rbind(bus_routes_df, current_page)
      
      # Print progress
      print(paste("Page", page_counter, "fetched:", nrow(current_page), "records"))
      
      # If less than 500 rows returned, assume we have reached the last page
      if (nrow(current_page) == 0) {
        break  # Exit the loop if there are no more pages
      }
      
      # Update the skip value to fetch the next page
      skip <- skip + 500
      page_counter <- page_counter + 1
      
      # Introduce a small delay (2 seconds) between requests to avoid rate limiting
      Sys.sleep(2)
      
      # Stop after max_pages (optional, to prevent too many pages)
      if (page_counter > max_pages) {
        break
      }
    } else {
      stop(paste("API call failed with status code:", status_code(response)))
    }
  }
  return(bus_routes_df)
}

# Fetch all bus routes
all_bus_routes <- get_all_bus_routes(url, account_key)

# View the first few rows
head(all_bus_routes)


