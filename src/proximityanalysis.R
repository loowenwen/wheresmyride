# Necessary Libraries
library(httr)
library(jsonlite)
library(tidyverse)


# OneMap Authentication API

# define API endpoint for authentication
auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"

# define email and password 
email <- "loowenwen1314@gmail.com"
password <- "sochex-6jobge-fomsYb"

# create JSON payload
auth_body <- list(
  email = email,
  password = password
)

# make API request
response <- POST(
  url = auth_url,
  body = auth_body,
  encode = "json"
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  
  # extract token and store it as an environment variable
  token <- data$access_token
  
} else {
  print(paste("error:", status_code(response)))
}




# Define function to count nearby bus stops/nearby mrt stops (used bus_stops_df label but can be used for mrt stops as well), takes in coordinate dataframe of planning region, coordinates dataframe of bus stops/mrt stops 
count_nearby_transit_stops <- function(planning_region_df, bus_stops_df, buffer_distance = 3000, crs_from = 4326, crs_to = 3414) {
  if (is.null(planning_region_df) || nrow(planning_region_df) == 0) return(0)  # Handle empty data
  
  planning_region_sf <- st_as_sf(planning_region_df, coords = c("longitude", "latitude"), crs = crs_from)
  region_centroid <- st_centroid(st_union(planning_region_sf))
  region_buffer <- st_buffer(region_centroid, dist = buffer_distance)
  bus_stops_sf <- st_as_sf(bus_stops_df, coords = c("longitude", "latitude"), crs = crs_from)
  bus_stops_proj <- st_transform(bus_stops_sf, crs_to)
  region_buffer_proj <- st_transform(region_buffer, crs_to)
  count_nearby <- sum(st_within(bus_stops_proj, region_buffer_proj, sparse = FALSE))
  return(as.numeric(count_nearby))  # Ensure numeric output
}


##count_bus_stops_all_regions (can also be used for mrt_stations just swap the bus_stops_df to mrt_stations_final dataset)
count_bus_stops_all_regions <- function(data, bus_stops_df) {
  # Initialize empty list to store results
  results <- list()
  
  for (i in 1:55) {
    region_name <- data$SearchResults$pln_area_n[i]
    geojson_string <- data$SearchResults$geojson[i]
    
    # Skip if no data available for this region
    if (is.null(region_name) || is.null(geojson_string)) next
    
    # Choose the correct extract function based on the region index (i)
    if (i == 26) {
      planning_region_df <- tryCatch({
        extract_coordinates_26(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 27) {
      planning_region_df <- tryCatch({
        extract_coordinates_27(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 33) {
      planning_region_df <- tryCatch({
        extract_coordinates_33(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 48) {
      planning_region_df <- tryCatch({
        extract_coordinates_48(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
      } else {
      # Use the default extract_coordinates_2 for all other cases
      planning_region_df <- tryCatch({
        extract_coordinates_general(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    }
    
    # Handle invalid coordinates
    if (is.null(planning_region_df) || nrow(planning_region_df) == 0 || 
        any(is.na(planning_region_df$longitude) | is.na(planning_region_df$latitude))) {
      # Handle missing or invalid coordinates
      message("Handling invalid coordinates for region ", region_name)
      
      # Option 1: Apply default coordinates (e.g., coordinates for the region's centroid or another default)
      default_coords <- data.frame(longitude = 0, latitude = 0)  # Use 0,0 or other default values
      planning_region_df <- default_coords  # Apply default coordinates to this region
    
    }
    
    # Now proceed with counting nearby bus stops
    count_nearby <- tryCatch({
      count_nearby_transit_stops(planning_region_df, bus_stops_df)
    }, error = function(e) {
      message("Error counting stops for region ", region_name, ": ", e$message)
      return(NA)
    })
    
    # Append the result for this region
    results[[i]] <- data.frame("Planning Region" = region_name, 
                               "Nearby Bus Stops" = count_nearby, 
                               stringsAsFactors = FALSE)
  }
  
  # Combine all results and remove NULL entries
  final_result <- do.call(rbind, results)
  return(final_result)
}


##FINAL DATASETS (remember to load bus_stops_final and mrt_station_finalfixed datasets from datacleaning script)
bus_stop_proximity_numbers <- count_bus_stops_all_regions(data, bus_stops_final) 
#tidy up
bus_stop_rankings <- bus_stop_proximity_numbers %>% group_by(Planning.Region) %>% arrange(desc(Nearby.Bus.Stops))

mrt_stop_proximity_numbers <- count_bus_stops_all_regions(data, mrt_station_finalfixed) 

mrt_stop_proximity_rankings <- mrt_stop_proximity_numbers %>% group_by(Planning.Region) %>% 
  rename(Nearby.Mrt.Stops = Nearby.Bus.Stops) %>%
  arrange(desc(Nearby.Mrt.Stops))

































## PLANNING AREA QUERY FUNCTION (this is the API that gives only ONE planning area, their output is different from the planning area polygon)
library(httr)

get_planning_area <- function(latitude, longitude, year = "2019") {
  # define the URL and headers
  url <- "https://www.onemap.gov.sg/api/public/popapi/getPlanningarea"
  # replace with your actual API token
  authToken <- token
  
  # construct full URL with parameters
  request_url <- paste0(url, 
                        "?latitude=", latitude, 
                        "&longitude=", longitude,
                        "&year=", year
  )
  # make API request
  response <- GET(
    url = request_url,
    add_headers(Authorization = authToken)
  )
  
  # check response status
  if (status_code(response) == 200) {
    # parse JSON response
    result <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(result)
    print(result)  # print response data
  } else {
    print(paste("Error:", status_code(response)))
  }
  
}



##EXTRACT COORDINATES from PLANNING AREA QUERY FUNCTION (output different so needs another extract coordinates function)
extract_coordinates <- function(output) {
  # Parse the JSON response
  response <- fromJSON(output)
  
  # Extract and parse the nested GeoJSON string
  coordinates <- fromJSON(response$geojson, simplifyVector = FALSE)
  
  # Extract coordinates array (handles MultiPolygon structure)
  coords_list <- coordinates$coordinates[[1]][[1]]  # Assumes first polygon
  
  # Convert to data frame with proper column names
  coords_df <- as.data.frame(do.call(rbind, coords_list))
  colnames(coords_df) <- c("longitude", "latitude")
  
  coords_df$longitude <- format(coords_df$longitude, scientific = FALSE, digits = 15)
  coords_df$latitude <- format(coords_df$latitude, scientific = FALSE, digits = 15)
  
  # Return the data frame (full precision retained)
  return(coords_df)
}





# LTA DataMall Bus Stops API
library(httr)
library(jsonlite)
library(tidyverse)

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






