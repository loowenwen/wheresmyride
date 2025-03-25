# AUTHENTICATION
library(httr)
library(jsonlite)

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
  Sys.setenv(ONEMAP_TOKEN = token)
  
} else {
  print(paste("Error:", status_code(response)))
}

## PLANNING AREA QUERY FUNCTION
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



##EXTRACT COORDINATES from PLANNING AREA FUNCTION
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

coord_df <- extract_coordinates(output = get_planning_area("1.3", "103.8"))


##FUNCTION TO COUNT NEARBY BUS STOPS/NEARBY MRT STATIONS
# Define the function to count nearby bus stops/mrt stations

count_nearby_transit_stops <- function(planning_region_df, bus_stops_df, buffer_distance = 45000, crs_from = 4326, crs_to = 3414) {
  
  #ensures precision
  planning_region_df$longitude <- as.numeric(format(planning_region_df$longitude, scientific = FALSE, digits = 15))
  planning_region_df$latitude <- as.numeric(format(planning_region_df$latitude, scientific = FALSE, digits = 15))
  
  # Convert planning region to a spatial object (SF)
  planning_region_sf <- st_as_sf(planning_region_df, coords = c("longitude", "latitude"), crs = crs_from)
  
  # Compute the centroid of the planning region (centre of planning region)
  region_centroid <- st_centroid(st_union(planning_region_sf))
  
  # Create a buffer around the centroid of the planning region (radius of search)
  region_buffer <- st_buffer(region_centroid, dist = buffer_distance)
  
  # Convert bus stops into spatial points (SF) and transform to correct crs if needed
  bus_stops_sf <- st_as_sf(bus_stops_df, coords = c("longitude", "latitude"), crs = crs_from)
  bus_stops_proj <- st_transform(bus_stops_sf, crs_to)
  
  # Transform planning region to the same CRS as bus stops
  planning_region_sf_proj <- st_transform(planning_region_sf, crs_to)
  
  # Create the buffer on the same CRS as bus stops and planning region
  region_buffer_proj <- st_transform(region_buffer, crs_to)
  
  # Find bus stops within the buffer
  bus_stops_nearby <- st_within(bus_stops_proj, region_buffer_proj, sparse = FALSE)
  
  # Count the number of bus stops within the buffer
  count_nearby <- sum(bus_stops_nearby)
  
  # Return the count
  return(count_nearby)
}



count_nearby_transit_stops(coord_df, mrt_station_finalfixed)




##plotting (wanted to quickly see a visual)
bedok_sf <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)
plot(st_geometry(bedok_sf), col = "lightblue", main = "Bedok Planning Area Polygon")
bedok_centroid <- st_centroid(st_union(bedok_sf))
plot(st_geometry(bedok_sf), col = "lightblue", main = "Bedok Planning Area with Centroid")
plot(st_geometry(bedok_centroid), col = "red", pch = 19, add = TRUE)



