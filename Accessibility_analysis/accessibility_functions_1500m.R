params <-
list(location_input = "", weight_mrt = 25L, weight_bus = 25L, 
    weight_walk = 25L, weight_congestion = 25L)

## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(rjson)
library(httr)
library(jsonlite)
library(geosphere)
library(sf)
library(purrr)
library(units)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
email <- "loowenwen1314@gmail.com"
password <- "sochex-6jobge-fomsYb"
auth_body <- list(email = email, password = password)
response <- POST(url = auth_url, body = auth_body, encode = "json")

if (status_code(response) == 200) {
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  token <- data$access_token
  Sys.setenv(ONEMAP_TOKEN = token)
} else {
  print(paste("Error:", status_code(response)))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Example input: Postal code or Coordinates (lat, lon)
location_input <- "543305" # Replace this with the postal code or lat, lon pair

# Check if the input is a valid postal code or coordinate pair
if (grepl(",", location_input)) {
  # If coordinates (lat, lon) are entered
  latlon <- as.numeric(strsplit(location_input, ",")[[1]])
  if (length(latlon) == 2) {
    lat <- latlon[1]
    lon <- latlon[2]
  } else {
    stop("Invalid coordinate format. Please enter as 'lat, lon'.")
  }
} else {
  # If postal code is entered, use OneMap API to get lat, lon
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  token <- Sys.getenv("ONEMAP_TOKEN")
  
  request_url <- paste0(base_url, "?searchVal=", location_input, "&returnGeom=Y&getAddrDetails=Y")
  res <- GET(request_url, add_headers(Authorization = token))
  parsed <- content(res, as = "parsed", type = "application/json")
  
  if (length(parsed$results) == 0) {
    stop("No results found for this postal code.")
  }
  
  lat <- as.numeric(parsed$results[[1]]$LATITUDE)
  lon <- as.numeric(parsed$results[[1]]$LONGITUDE)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

parse_crowd_json <- function(filepath) {
  options(lubridate.verbose = FALSE)
  # Read and preprocess JSON content 
  json_content <- paste0(readLines(filepath, warn = FALSE, encoding = "UTF-8"), collapse = "")
  json_content <- trimws(json_content)
  json_content <- sub(",\\s*$", "", json_content)  # Remove trailing comma
  
  # Validate JSON
  if (!jsonlite::validate(json_content)) {
    stop("Invalid JSON in file: ", filepath)
  }
  
  # Parse JSON with error handling
  raw <- tryCatch({
    jsonlite::fromJSON(json_content)
  }, error = function(e) {
    message("Error parsing JSON from: ", filepath)
    message("Error details: ", e$message)
    message("First 200 chars: ", substr(json_content, 1, 200))
    stop("Failed to parse JSON")
  })
  
  stations_df <- raw$value$Stations[[1]]  # <-- the [1] is KEY
  
  all_intervals <- purrr::map2_dfr(
    stations_df$Station,
    stations_df$Interval,
    function(station_code, interval_df) {
      interval_df %>%
        mutate(
          Station = station_code,
          datetime = suppressMessages(ymd_hms(Start, tz = "Asia/Singapore")),
          hour = hour(datetime),
          crowd_score = case_when(
            CrowdLevel == "l" ~ 1,
            CrowdLevel == "m" ~ 2,
            CrowdLevel == "h" ~ 3,
            TRUE ~ NA_real_
          ),
          time_slot = case_when(
            hour >= 6 & hour < 9 ~ "AM_peak",
            hour == 5 | (hour >= 9 & hour < 17) ~ "AM_offpeak",
            hour >= 17 & hour < 19 ~ "PM_peak",
            (hour >= 19 & hour <= 23) | (hour >= 0 & hour < 2) ~ "PM_offpeak",
            TRUE ~ "Other" # as the dataset is a 24hr forecast, it includes all timings. They will just be categorised as other since the MRT does not run from 1am - 4.59am. MRT generally starts at ~5.30am so timing from 5am onwards will be taken
          )
        )
    }
  )
  
  return(all_intervals)
}
# Now use the modified function to parse your MRT crowd data

mrt_crowdDensity <- bind_rows(
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_CCL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_DTL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_EWL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_CEL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_CGL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_NEL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_NSL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_BPL.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_SLRT.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_PLRT.json"),
  parse_crowd_json("../data/MRT_CrowdDensity/CrowdDensity_TEL.json")
)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
readRDS("../data/RDS Files/planning_area_polygons.rds")
bus_stops <- readRDS("../data/RDS Files/bus_with_planning.rds")
mrt_lrt <- readRDS("../data/RDS Files/mrt_stations.rds")

mrt_lrt <- mrt_lrt %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

bus_stops <- bus_stops %>%
  st_as_sf() %>%
  st_transform(crs = 4326)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
get_bus_routes <- function() {
  url <- "http://datamall2.mytransport.sg/ltaodataservice/BusRoutes"
  api_key <- "o6OuJxI3Re+qYgFQzb+4+w=="  # Replace with your own API key
  
  offset <- 0
  all_data <- list()
  
  repeat {
    response <- GET(url, add_headers(AccountKey = api_key), query = list("$skip" = offset))
    data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(data)
    
    if (length(parsed_data$value) == 0) break
    all_data <- append(all_data, list(parsed_data$value))
    offset <- offset + 500
  }
  
  bind_rows(all_data)
}

# Fetch all bus routes from API
bus_routes_raw <- get_bus_routes()

# Create lookup table: BusStopCode -> ServiceNo
bus_services_lookup <- bus_routes_raw %>%
  select(ServiceNo, BusStopCode) %>%
  distinct() %>%
  mutate(BusStopCode = str_pad(as.character(BusStopCode), 5, side = "left", pad = "0"))



## ----feature-engineering---------------------------------------------------------------------------------------------------------------------------------------------------------

engineer_features <- function(lat, lon, distance = 1500) {
  
  #Time based Bus passenger volume
  busstops_passengerVolume <- read_csv("../data/PV_busstops.csv", show_col_types = FALSE) %>%
    filter(DAY_TYPE == "WEEKDAY") %>%
    mutate(
      PT_CODE = str_pad(as.character(PT_CODE), 5, "left", "0"),
      BusStop_code = PT_CODE,
      time_slot = case_when(
        TIME_PER_HOUR >= 6 & TIME_PER_HOUR < 9 ~ "AM_peak",
        TIME_PER_HOUR == 5 | (TIME_PER_HOUR >= 9 & TIME_PER_HOUR < 17) ~ "AM_offpeak",
        TIME_PER_HOUR >= 17 & TIME_PER_HOUR < 19 ~ "PM_peak",
        (TIME_PER_HOUR >= 19 & TIME_PER_HOUR <= 23) | (TIME_PER_HOUR >= 0 & TIME_PER_HOUR < 2) ~ "PM_offpeak",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(BusStop_code, time_slot) %>%
    summarise(
      daily_avg_vol = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE) / 20,
      .groups = 'drop'
    )
  '  
  # Finding coordinates of all bus stops
  bus_coords <- st_read("../data/BusStopLocation_Nov2024/BusStop.shp", quiet = TRUE) %>%
    rename(BusStop_code = BUS_STOP_N) %>%
    st_transform(crs = 4326) %>%
    mutate(
      BusStop_code = str_pad(as.character(BusStop_code), 5, "left", "0"),
      Longitude = st_coordinates(geometry)[,1],
      Latitude = st_coordinates(geometry)[,2]
    ) %>%
    st_drop_geometry()
'
  
  bus_coords <- bus_stops %>%
    mutate(BusStop_code = as.character(BusStopCode)) %>%
    st_transform(crs = 4326)
  
  
  # Convert user location to sf POINT
  location_point <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  
  ### MRT ANALYSIS
  mrt_coords <- mrt_lrt %>%
    st_make_valid() %>%
    mutate(dist = as.numeric(st_distance(geometry, location_point))) %>%
    filter(dist < distance)
  
  num_mrt_stations <- nrow(mrt_coords)
  avg_dist_mrt <- ifelse(num_mrt_stations > 0, mean(mrt_coords$dist), NA)
  
  if (num_mrt_stations == 0) {
    message("No MRT stations found within 1500m radius. MRT score may be low or NA.")
  }
  
  mrt_station_names <- mrt_coords$mrt_station
  
  # Extract MRT line codes (e.g., NS1 -> NS)
  mrt_lines <- mrt_coords %>%
    pull(mrt_line) %>%
    unique()
  
  mrt_station_distances <- mrt_coords %>%
    st_drop_geometry() %>%
    select(mrt_station, dist) %>%
    arrange(dist)
  
  num_unique_mrt_lines <- length(mrt_lines)
  
  ### BUS ANALYSIS
  bus_nearby <- bus_coords %>%
    st_make_valid() %>%
    mutate(dist = as.numeric(st_distance(geometry, location_point))) %>%
    filter(dist < distance)
  
  num_bus_stops <- nrow(bus_nearby)
  avg_dist_bus <- ifelse(num_bus_stops > 0, mean(bus_nearby$dist), NA)

  bus_stop_distances <- bus_nearby %>%
    st_drop_geometry() %>%
    select(BusStop_code, Description, dist) %>%
    arrange(dist)
  
  if (num_bus_stops == 0) {
    message("No bus stops found within 1500m radius. Bus score may be low or NA.")
  }
  
  nearby_bus_codes <- bus_nearby$BusStop_code
  
  # Join to busRoutes lookup to get services at nearby stops
  bus_services_list <- bus_services_lookup %>%
    filter(BusStopCode %in% nearby_bus_codes) %>%
    pull(ServiceNo) %>%
    unique()
  
  num_unique_bus_services <- length(bus_services_list)
  
  ### CROWD / VOLUME ANALYSIS
  mrtNames_crowdDensity <- mrt_crowdDensity %>%
    rowwise() %>%
    mutate(STN_NAME = mrt_lrt$mrt_station[
      which(str_detect(mrt_lrt$stn_code, Station))[1]
    ])
  
  mrt_crowd_scores <- mrtNames_crowdDensity %>%
    filter(STN_NAME %in% mrt_station_names) %>%
    group_by(time_slot) %>%
    summarise(
      avg_crowd = mean(crowd_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = time_slot, values_from = avg_crowd, names_prefix = "mrt_")
  
  
  bus_volume_by_slot <- bus_nearby %>%
    left_join(busstops_passengerVolume, by = "BusStop_code") %>%
    group_by(time_slot) %>%
    summarise(avg_vol = mean(daily_avg_vol, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = time_slot, values_from = avg_vol, names_prefix = "bus_")
  
  ### RETURN
  return(list(
    # Quantitative features
    num_mrt_stations = num_mrt_stations,
    avg_dist_mrt = avg_dist_mrt,
    num_unique_mrt_lines = num_unique_mrt_lines,
    num_bus_stops = num_bus_stops,
    avg_dist_bus = avg_dist_bus,
    num_unique_bus_services = num_unique_bus_services,
    mrt_crowd_AM_peak = mrt_crowd_scores$mrt_AM_peak,
    mrt_crowd_AM_offpeak = mrt_crowd_scores$mrt_AM_offpeak,
    mrt_crowd_PM_peak = mrt_crowd_scores$mrt_PM_peak,
    mrt_crowd_PM_offpeak = mrt_crowd_scores$mrt_PM_offpeak,
    bus_volume_AM_peak = bus_volume_by_slot$bus_AM_peak,
    bus_volume_AM_offpeak = bus_volume_by_slot$bus_AM_offpeak,
    bus_volume_PM_peak = bus_volume_by_slot$bus_PM_peak,
    bus_volume_PM_offpeak = bus_volume_by_slot$bus_PM_offpeak,
    
    # Detailed lists
    nearby_mrt_stations = mrt_station_names,
    nearby_mrt_lines = mrt_lines,
    nearby_bus_stops = nearby_bus_codes,
    bus_services_available = bus_services_list,
    distance_radius = distance,
    mrt_stop_distances = mrt_station_distances,
    bus_stop_distances = bus_stop_distances
  ))
}



## ----scoring-function------------------------------------------------------------------------------------------------------------------------------------------------------------
normalised_score <- function(features, 
                             weight_mrt = 1/4 * 100, 
                             weight_bus = 1/4 * 100, 
                             weight_walk = 1/4 * 100,
                             weight_congestion = 1/4 * 100,
                             selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak")) {
  
  # --- Normalise weights ---
  sum_weights <- weight_mrt + weight_bus + weight_walk + weight_congestion
  norm_mrt <- weight_mrt / sum_weights
  norm_bus <- weight_bus / sum_weights
  norm_walk <- weight_walk / sum_weights
  norm_congestion <- weight_congestion / sum_weights
  
  # ---- Normalise/scale scores ----
  # MRT score (distance excluded)
  score_mrt <- min(1, mean(c(
    ifelse(is.na(features$num_mrt_stations), 0, min(features$num_mrt_stations / 3, 1)),
    ifelse(is.na(features$num_unique_mrt_lines), 0, min(features$num_unique_mrt_lines / 3, 1))
    ))) * 100
  
  # Bus score (distance excluded)
  score_bus <- min(1, mean(c(
    ifelse(is.na(features$num_bus_stops), 0, min(features$num_bus_stops / 20, 1)),
    ifelse(is.na(features$num_unique_bus_services), 0, min(features$num_unique_bus_services / 30, 1))
    ))) * 100
  
  # Walkability (distance included)
  norm_mrt_dist <- max(0, 1 - (ifelse(is.na(features$avg_dist_mrt), features$distance_radius, features$avg_dist_mrt) / features$distance_radius))
  norm_bus_dist <- max(0, 1 - (ifelse(is.na(features$avg_dist_bus), features$distance_radius, features$avg_dist_bus) / features$distance_radius))
  
  walkability_score <- min(1, mean(c(norm_mrt_dist, norm_bus_dist))) * 100
  
  # Penalty
  calc_penalty <- function(mrt_crowd, bus_volume) {
    mean(c(
      ifelse(is.na(mrt_crowd), 0, (mrt_crowd - 1) / 2),
      ifelse(is.na(bus_volume), 0, bus_volume / 500)
    ))
  }
  
  # Compute congestion per time slot
  congestion_list <- list(
    AM_peak = calc_penalty(features$mrt_crowd_AM_peak, features$bus_volume_AM_peak),
    AM_offpeak = calc_penalty(features$mrt_crowd_AM_offpeak, features$bus_volume_AM_offpeak),
    PM_peak = calc_penalty(features$mrt_crowd_PM_peak, features$bus_volume_PM_peak),
    PM_offpeak = calc_penalty(features$mrt_crowd_PM_offpeak, features$bus_volume_PM_offpeak)
  )
  
  # Filter congestion by selected time slots
  filtered_congestion <- congestion_list[selected_time_slots]
  congestion_penalty <- mean(unlist(filtered_congestion), na.rm = TRUE)
  
  # Flip to congestion score (higher = better), scale to 0–100
  congestion_score <- max(0, min(1, 1 - congestion_penalty)) * 100
  
  # Weighted average of all components
  final_score <- (
    score_mrt * norm_mrt +
      score_bus * norm_bus +
      walkability_score * norm_walk +
      congestion_score * norm_congestion
  )
  
  # ---- Create breakdown ----
  breakdown <- list(
    total_score = final_score,
    
    score_mrt = score_mrt,
    score_bus = score_bus,
    walkability_score = walkability_score,
    congestion_penalty = congestion_penalty,
    congestion_score = congestion_score,
    time_slot_congestion = congestion_list[selected_time_slots],
    
    mrt_score_components = list(
      num_mrt_stations = features$num_mrt_stations,
      avg_dist_mrt = features$avg_dist_mrt,
      num_unique_mrt_lines = features$num_unique_mrt_lines,
      nearby_mrt_stations = features$nearby_mrt_stations,
      nearby_mrt_lines = features$nearby_mrt_lines
    ),
    
    bus_score_components = list(
      num_bus_stops = features$num_bus_stops,
      avg_dist_bus = features$avg_dist_bus,
      num_unique_bus_services = features$num_unique_bus_services,
      nearby_bus_stops = features$nearby_bus_stops,
      bus_services_available = features$bus_services_available
    ),
    
    congestion_components = list(
      bus_volume_AM_peak = features$bus_volume_AM_peak,
      bus_volume_AM_offpeak = features$bus_volume_AM_offpeak,
      bus_volume_PM_peak = features$bus_volume_PM_peak,
      bus_volume_PM_offpeak = features$bus_volume_PM_offpeak,
      
      mrt_crowd_AM_peak = features$mrt_crowd_AM_peak,
      mrt_crowd_AM_offpeak = features$mrt_crowd_AM_offpeak,
      mrt_crowd_PM_peak = features$mrt_crowd_PM_peak,
      mrt_crowd_PM_offpeak = features$mrt_crowd_PM_offpeak
    ),
    
    time_slot_congestion = congestion_list,
    
    walkability = list(
      avg_mrt_distance = features$avg_dist_mrt,
      avg_bus_distance = features$avg_dist_bus,
      walkability_score = walkability_score
    )
  )
  
  return(breakdown)
  
}




## ----main-function---------------------------------------------------------------------------------------------------------------------------------------------------------------
predict_accessibility <- function(location_input, 
                                  weight_mrt = 1/4 * 100, 
                                  weight_bus = 1/4 * 100, 
                                  weight_walk = 1/4 * 100, 
                                  weight_congestion = 1/4 * 100,
                                  selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak"),
                                  distance = 1500) {
  
  # Determine if input is postal code or coordinate pair
  if (is.character(location_input)) {
    base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
    token <- Sys.getenv("ONEMAP_TOKEN")
    
    request_url <- paste0(base_url, "?searchVal=", location_input, "&returnGeom=Y&getAddrDetails=Y")
    res <- GET(request_url, add_headers(Authorization = token))
    parsed <- content(res, as = "parsed", type = "application/json")
    
    if (length(parsed$results) == 0) {
      stop("No results found for this postal code.")
    }
    
    lat <- as.numeric(parsed$results[[1]]$LATITUDE)
    lon <- as.numeric(parsed$results[[1]]$LONGITUDE)
    
  } else if (is.numeric(location_input) && length(location_input) == 2) {
    
    latlon <- location_input
    if (latlon[1] > 90) {
      # (lon, lat) mistakenly passed, flip
      lon <- latlon[1]
      lat <- latlon[2]
      message("INFO: Flipped (lon, lat) to (lat, lon)")
    } else {
      lat <- latlon[1]
      lon <- latlon[2]
    }
    
  } else {
    stop("Invalid input: please provide either a postal code (character) or coordinates (numeric vector of length 2).")
  }
  
  
  features <- engineer_features(lat, lon, distance = distance)
  
  score_breakdown <- normalised_score(
    features, 
    weight_mrt = weight_mrt, 
    weight_bus = weight_bus, 
    weight_walk = weight_walk, 
    weight_congestion = weight_congestion,
    selected_time_slots = selected_time_slots
  )
  
  return(list(
    location_input = location_input,
    latitude = lat,
    longitude = lon,
    score = score_breakdown$total_score,
    score_breakdown = score_breakdown,
    features = features
  ))
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# result <- predict_accessibility(location_input = "543305", distance = 500)
# print(result)
# 
# # Define the location directly as the coordinates for Clementi
# lat <- 1.3169289
# lon <- 103.76270196
# 
# # Run your function to process the features based on these coordinates
# features <- engineer_features(lat, lon, distance = 500)
# 
# # Assuming your normalised_score function is loaded
# score_breakdown <- normalised_score(
#   features, 
#   weight_mrt = 25, 
#   weight_bus = 25, 
#   weight_walk = 25, 
#   weight_congestion = 25,
#   selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak")
# )
# 
# # Output the score breakdown
# score_breakdown
# 
# # Final Accessibility Score
# final_score <- score_breakdown$total_score
# final_score


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --- Step 1: Extract & Source the Rmd code ---
# Convert the Rmd into an R script so that its functions become accessible.
# (This step uses knitr::purl; you only need to run it once.)
knitr::purl("predict_accessibility_test.Rmd", output = "accessibility_functions.R")

