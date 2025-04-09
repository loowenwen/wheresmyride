# Necessary Libraries
library(httr)
library(jsonlite)
library(leaflet)
library(lwgeom)
library(sf)
library(tidyverse)
library(geosphere)
library(rjson)

# LTA DataMall Bus Stops API
headers = c(
  'AccountKey' = 'gvoVWhshRm+rrWjscjcy+A==',
  'Accept' = 'application/json'
)
base_url <- "https://datamall2.mytransport.sg/ltaodataservice/BusStops"

# empty list to store data
all_bus_stops <- list()
skip <- 0
batch <- 1

repeat {
  url <- paste0(base_url, "?$skip=", skip)
  
  res <- VERB("GET", url = url, add_headers(.headers = headers))
  
  # convert to text and parse
  res_text <- content(res, "text", encoding = "UTF-8")
  res_json <- fromJSON(res_text)
  
  # extract the data
  data <- res_json$value
  
  # stop if no more data
  if (length(data) == 0) {
    break
  }
  
  # store this batch
  all_bus_stops[[batch]] <- data
  
  # update counters
  skip <- skip + 500
  batch <- batch + 1
  
}

# combine all data frames
bus_stops_df <- bind_rows(all_bus_stops)
head(bus_stops_df)

# save as RDS
saveRDS(bus_stops_df, file = "data/RDS Files/bus_stops.rds")


# MRT Stations Dataset
getwd()
mrt_stations_df <- st_read("data/TrainStation_Nov2024/RapidTransitSystemStation.shp") %>%
  select(-c(TYP_CD, STN_NAM, ATTACHEMEN)) %>%
  mutate(
    station_type = str_extract(STN_NAM_DE, "MRT STATION|LRT STATION|DEPOT|FACILITY BUILDING|SUB STATION"),
    station_name = str_remove(STN_NAM_DE, "MRT STATION|LRT STATION|DEPOT|FACILITY BUILDING|SUB STATION") %>%
      str_trim()
  ) %>%
  rename(shape_area = SHAPE_AREA, shape_length = SHAPE_LEN) %>%
  select(station_type, station_name, shape_area, shape_length, geometry)

mrt_stations_df <- mrt_stations_df %>%
  filter(station_type == "MRT STATION" | station_type == "LRT STATION")

mrt_stations_df %>%
  group_by(station_name) %>%
  summarise(n = n()) %>%
  filter(n > 1)

mrt_stations_merged <- mrt_stations_df %>%
  group_by(station_name, station_type) %>%
  summarise(
    geometry = st_union(geometry),
    shape_area = sum(shape_area),
    shape_length = sum(shape_length),
    .groups = "drop"
  )

mrt_stations_merged %>%
  group_by(station_name, station_type) %>%
  summarise(n = n()) %>%
  filter(n > 1)

mrt_stations_merged <- mrt_stations_merged %>%
  mutate(
    centroid = st_centroid(geometry),
    centroid_lon = st_coordinates(st_transform(centroid, 4326))[, 1],
    centroid_lat = st_coordinates(st_transform(centroid, 4326))[, 2]
  )
  
train_station_codes <- read_excel("data/TrainStationCodes_Chinese Names.xls") %>% 
  select(c(stn_code, mrt_station_english, mrt_line_english)) %>%
  mutate(mrt_station_english = str_to_upper(mrt_station_english),
         mrt_line_english = str_to_upper(mrt_line_english)) 


# combine mrt_stations_df and train_station_codes
mrt_stations_final <- train_station_codes %>% 
  left_join(mrt_stations_merged, by = c("mrt_station_english" = "station_name")) %>%
  rename(mrt_station = mrt_station_english, 
         mrt_line = mrt_line_english)

duplicated_codes <- mrt_stations_final$stn_code[duplicated(mrt_stations_final$stn_code)]
unique(duplicated_codes)

# remove duplicated rows
mrt_stations_final <- mrt_stations_final[-c(4, 78, 80, 113, 150, 156, 165, 180),]

duplicated_codes <- mrt_stations_final$stn_code[duplicated(mrt_stations_final$stn_code)]
unique(duplicated_codes)

mrt_stations_final %>%
  group_by(mrt_station) %>%
  summarise(
    stn_code_combined = paste(unique(stn_code), collapse = ", "),
    mrt_line_combined = paste(unique(mrt_line), collapse = ", "),
    n = n()
  ) %>%
  filter(n > 1)

combined_codes <- mrt_stations_final %>%
  group_by(mrt_station) %>%
  summarise(stn_code_combined = paste(unique(stn_code), collapse = ", "),
            mrt_line_combined = paste(unique(mrt_line), collapse = ", "))

mrt_stations_final <- mrt_stations_final %>%
  left_join(combined_codes, by = "mrt_station")

# save as RDS
saveRDS(mrt_stations_final, file = "data/RDS Files/mrt_stations.rds")


# Import Previously Saved RDS Files
planning_areas <- 
  readRDS("data/RDS Files/planning_area_polygons.rds")
bus_stops <- 
  readRDS("data/RDS Files/bus_stops.rds")
mrt_stations <-
  readRDS("data/RDS Files/mrt_stations.rds")

# mrt stations
mrt_sf <- st_sf(mrt_stations, geometry = mrt_stations$centroid)
mrt_sf <- st_transform(mrt_sf, 4326)

# join each MRT station to the planning area it falls within
planning_areas <- st_make_valid(planning_areas)
mrt_with_planning <- st_join(mrt_sf, planning_areas, join = st_within, largest = TRUE)

mrt_with_planning %>%
  filter(is.na(pln_area_n))

mrt_final <- mrt_with_planning %>%
  st_drop_geometry() %>%
  left_join(mrt_stations)

mrt_final <- mrt_final %>%
  mutate(station_type = case_when(
    station_type == "MRT STATION" ~ "MRT Station",
    station_type == "LRT STATION" ~ "LRT Station",
    TRUE ~ station_type
  ))

# save as RDS
saveRDS(mrt_final, file = "data/RDS Files/mrt_with_planning.rds")

# bus stops
bus_sf <- st_as_sf(bus_stops,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE  # keep original lon/lat columns
)

# join each bus stop to the planning area it falls within
bus_with_planning <- st_join(bus_sf, planning_areas, join = st_within)

bus_with_planning %>%
  filter(is.na(pln_area_n))

bus_with_planning <- bus_with_planning %>%
  filter(!is.na(pln_area_n))

# save as RDS
saveRDS(bus_with_planning, file = "data/RDS Files/bus_with_planning.rds")




