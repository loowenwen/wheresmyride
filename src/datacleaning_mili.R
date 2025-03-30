library(sf)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(readxl)


##BUS STOP LOCATIONS dataset

bus_stops <- st_read("../data/BusStopLocation_Nov2024/BusStop.shp")



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





##MRT STATION LOCATIONS dataset
mrt_station <- st_read("../data/TrainStation_Nov2024/RapidTransitSystemStation.shp")

# Identify and remove invalid geometries
invalid_geometries <- which(!st_is_valid(mrt_station$geometry))
valid_mrt <- mrt_station[-invalid_geometries, ] %>%
  st_transform(crs = 4326) %>%
  mutate(geometry = st_make_valid(geometry),
         centroid = st_centroid(geometry), 
         latitude = st_coordinates(centroid)[, 2], 
         longitude = st_coordinates(centroid)[, 1]) 

# Fix and transform invalid geometries
fixed_invalid_mrt <- mrt_station[invalid_geometries, ] %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_transform(crs = 4326) %>%
  mutate(centroid = st_centroid(st_union(st_cast(geometry, "POLYGON"))),
         latitude = st_coordinates(centroid)[, 2],
         longitude = st_coordinates(centroid)[, 1])

# Combine fixed datasets
mrt_station_finalfixed <- bind_rows(valid_mrt, fixed_invalid_mrt) %>% 
  select(TYP_CD_DES, STN_NAM_DE, longitude, latitude) %>%
  st_drop_geometry() %>%
  rename(station_name = STN_NAM_DE, type = TYP_CD_DES) %>%
  mutate(type = factor(type, levels = c("MRT", "LRT"))) %>%
  mutate(station_name = str_squish(str_to_lower(station_name))) %>%
  mutate(station_name = str_remove(station_name, "\\s*mrt\\s*station$"))



##BUS ROUTES (no need)

library(httr)
library(jsonlite)
library(dplyr)

# Function to get bus routes with pagination handling
get_bus_routes <- function() {
  url <- "http://datamall2.mytransport.sg/ltaodataservice/BusRoutes"
  api_key <- "o6OuJxI3Re+qYgFQzb+4+w=="  # Replace with your API key
  
  offset <- 0
  all_data <- list()
  
  repeat {
    # Make the API request
    response <- GET(url, add_headers(AccountKey = api_key), query = list("$skip" = offset))
    
    # Parse the response
    data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(data, flatten = TRUE)
    
    # Check if data is empty
    if (length(parsed_data$value) == 0) {
      break
    }
    
    # Store retrieved data
    all_data <- append(all_data, list(parsed_data$value))
    
    # Increment offset (LTA DataMall returns 500 records per request)
    offset <- offset + 500
  }
  
  # Combine all data into a single dataframe
  final_data <- bind_rows(all_data)
  return(final_data)
}

# Fetch all bus routes
bus_routes <- get_bus_routes()

# Print the first few rows to check output
head(bus_routes)

bus_routes %>% filter(ServiceNo == "291")



##CODE TO FIND MRT ROUTES

train_station_codes <- read_excel("../data/TrainStationCodes_Chinese Names.xls") %>% select(-c(mrt_station_chinese, mrt_line_chinese)) %>%
  mutate(mrt_station_english = str_squish(str_to_lower(mrt_station_english)))
train_line_df <- read_excel("../data/TrainLineCodes.xlsx") %>% 
  select(-'Shuttle Direction') %>%
  separate_rows(`MRT/LRT Direction`, sep = "\\s*\\r\\n\\s*") %>%
  mutate(`MRT/LRT Direction` = str_remove(`MRT/LRT Direction`, "^\\d+\\)\\s*")) %>%  # Remove "1)", "2)", etc.
  mutate(`MRT/LRT Direction` = str_trim(`MRT/LRT Direction`)) 

  


#combines train_station_codes and mrt_station_finalfixed to get codes + mrt station names in one df

mrt_station_codes_locations <- mrt_station_finalfixed %>% 
  filter(type == "MRT") %>%
  left_join(train_station_codes, by = c("station_name" = "mrt_station_english"), relationship = "many-to-many") %>%
  drop_na() #all the depots 







#install and load
library(httr)
library(jsonlite)
library(igraph)
library(sf)

# Replace with your actual token
auth_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiI0Mzc4NGMyOTcwMzdjMjQ1MWU5YTk2NmM0YjJlYjAxNyIsImlzcyI6Imh0dHA6Ly9pbnRlcm5hbC1hbGItb20tcHJkZXppdC1pdC1uZXctMTYzMzc5OTU0Mi5hcC1zb3V0aGVhc3QtMS5lbGIuYW1hem9uYXdzLmNvbS9hcGkvdjIvdXNlci9wYXNzd29yZCIsImlhdCI6MTc0Mjc4NTQ2MywiZXhwIjoxNzQzMDQ0NjYzLCJuYmYiOjE3NDI3ODU0NjMsImp0aSI6IjJWWTdRcjI1WTFmT3RReEIiLCJ1c2VyX2lkIjo2NDYyLCJmb3JldmVyIjpmYWxzZX0.1RWOxOmv6qLemIyAlgPyTOPzSXVgelw-PHAT6VofVao"

base_url <- "https://www.onemap.gov.sg/api/public/routingsvc/route"
query_params <- list(
  start = "1.298921,103.7744", #YUSOF ISHAK HSE
  end = "1.355948,103.9372",  #BELOW BLOCK 702
  routeType = "walk",
  date = "08-13-2023",
  time = "13:00:00",
  mode = "WALKING",
  maxWalkDistance = "10000",
  numItineraries = "1"
)

# Make the GET request
response <- GET(
  url = base_url,
  query = query_params,
  add_headers(Authorization = paste("Bearer", auth_token))
)

# Check status
if (status_code(response) == 200) {
  result <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(result)
  print(parsed_data)
} else {
  print(paste("Error:", status_code(response)))
}


