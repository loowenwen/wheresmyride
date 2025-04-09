library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)

# ---- Load Required Data ----
data_dir <- "data/RDS Files"

planning_areas <- readRDS(file.path(data_dir, "planning_area_polygons.rds"))
bus_with_planning <- readRDS(file.path(data_dir, "bus_with_planning.rds"))
mrt_with_planning <- readRDS(file.path(data_dir, "mrt_with_planning.rds"))
upcoming_bto <- readRDS(file.path(data_dir, "upcoming_bto.rds"))

#read in bto data
upcoming_bto$Start <- sprintf("%.7f,%.8f", upcoming_bto$lat, upcoming_bto$lng)

source("src/RouteQualityScore.R")
 #if (exists("results") && !is.null(results$summary)) {
#rqs_summary <- results$summary %>%
#     mutate(
#       lat = as.numeric(sub(",.*", "", Start)),
#       lng = as.numeric(sub(".*,", "", Start)),
#       Start = sprintf("%.7f,%.8f", lat, lng)
#     )
# } else {
#   stop("Route Quality Score (results$summary) data is missing or not loaded.")
# }


# ==== Use of OneMap API (REUSABLE) ====
# --- OneMap Authentication ---
get_onemap_token <- function(force_refresh = FALSE) {
  current_token <- Sys.getenv("ONEMAP_TOKEN")
  
  if (!nzchar(current_token) || force_refresh) {
    auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
    auth_body <- list(
      email = "loowenwen1314@gmail.com",
      password = "sochex-6jobge-fomsYb"
    )
    
    response <- POST(url = auth_url, body = auth_body, encode = "json")
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      Sys.setenv(ONEMAP_TOKEN = data$access_token)
      return(data$access_token)
    } else {
      stop(paste("Failed to get token. Status code:", status_code(response)))
    }
  }
  
  return(current_token)
}

# Get token once (only if not already present)
get_onemap_token()


# --- Function: Get Coordinates from Postal Code ---
get_coords_from_postal <- function(postal_code) {
  token <- get_onemap_token(force_refresh = TRUE) 
  
  url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  
  response <- GET(
    url,
    query = list(
      searchVal = postal_code,
      returnGeom = "Y",
      getAddrDetails = "Y"
    ),
    add_headers(Authorization = token)
  )
  
  if (status_code(response) != 200) {
    message("❌ OneMap lookup failed with status: ", status_code(response))
    return(NULL)
  }
  
  result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  if (length(result$results) == 0) {
    message("⚠️ No results found for postal code.")
    return(NULL)
  }
  
  lng <- as.numeric(result$results$LONGITUDE[1])
  lat <- as.numeric(result$results$LATITUDE[1])
  return(c(lng, lat))
}



# ==== Tab One ====
# --- Bus Stop Density ---
bus_stop_density <- bus_with_planning %>% 
  st_drop_geometry() %>%
  count(pln_area_n)

# fill in planning areas that are missing bus stops with zero counts
missing_pln_areas <- setdiff(planning_areas$pln_area_n, bus_stop_density$pln_area_n)

bus_stop_density <- bind_rows(
  bus_stop_density,
  data.frame(pln_area_n = missing_pln_areas, n = 0)
)

bus_stop_density <- bus_stop_density %>%
  mutate(rank = min_rank(desc(n)))

# --- MRT Station Density ---
mrt_station_density <- mrt_with_planning %>% 
  distinct(mrt_station, .keep_all = TRUE) %>%
  st_drop_geometry() %>%
  count(pln_area_n)

# fill in planning areas that are missing MRT stations with zero counts
missing_pln_areas <- setdiff(planning_areas$pln_area_n, mrt_station_density$pln_area_n)

mrt_station_density <- bind_rows(
  mrt_station_density,
  data.frame(pln_area_n = missing_pln_areas, n = 0)
)

mrt_station_density <- mrt_station_density %>%
  mutate(rank = min_rank(desc(n)))


# ==== Tab Two ====
# --- Function: Generate Isochrone (Fast Buffer Approximation) ---
generate_fast_isochrone <- function(center_lng, center_lat, duration_mins) {
  radius_m <- duration_mins * 333
  center <- c(center_lng, center_lat)
  circle_coords <- geosphere::destPoint(center, b = seq(0, 360, length.out = 100), d = radius_m)
  return(as.data.frame(circle_coords))
}



# ==== Tab Four Preload ====

options(lubridate.verbose = FALSE)

### MRT Crowd Density by Station

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
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_CCL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_DTL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_EWL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_CEL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_CGL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_NEL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_NSL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_BPL.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_SLRT.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_PLRT.json"),
  parse_crowd_json("data/MRT_CrowdDensity/CrowdDensity_TEL.json")
)


### Importing Bus and MRT location information from "planningareapolygons.R"

readRDS("data/RDS Files/planning_area_polygons.rds")
bus_stops <- readRDS("data/RDS Files/bus_with_planning.rds")
mrt_lrt <- readRDS("data/RDS Files/mrt_stations.rds")


mrt_lrt <- mrt_lrt %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

bus_stops <- bus_stops %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

### Extracting Bus Services Function

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
