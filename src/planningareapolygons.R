library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(sf)


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


# OneMap Planning Area Polygons API

# define the URL and headers
url <- "https://www.onemap.gov.sg/api/public/popapi/getAllPlanningarea"

# replace with actual API token
authToken <- token

# make API request
response <- GET(
  url = url,
  add_headers(Authorization = authToken)
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
} else {
  print(paste("error:", status_code(response)))
}


# Extract Planning Area Polygons 
data <- data[["SearchResults"]]

# initialize an empty sf dataframe
spatial_polygon <- st_read(data$geojson[1], quiet = TRUE)
combined_sf <- st_sf(geometry = st_sfc(), crs = st_crs(spatial_polygon))
combined_sf$pln_area_n <- character(0)

for (i in 1:55) {
  spatial_polygon <- st_read(data$geojson[i], quiet = TRUE)
  spatial_polygon$pln_area_n <- data$pln_area_n[i]
  combined_sf <- rbind(combined_sf, spatial_polygon)
}

# manually rerun for all 55 planning areas
i <- 55
spatial_polygon <- st_read(data$geojson[i], quiet = TRUE)
spatial_polygon$pln_area_n <- data$pln_area_n[i]
plot(spatial_polygon$geometry)

# save separately
file_name <- paste0(data$pln_area_n[i], ".geojson")
full_path <- file.path("data/PlanningAreaPolygons", file_name)
st_write(spatial_polygon, full_path)

# save combined dataframe
file_name <-"ALL PLANNING AREAS.geojson"
full_path <- file.path("data/PlanningAreaPolygons", file_name)
st_write(combined_sf, full_path)

# save combined dataframe as RDS
saveRDS(combined_sf, file = "data/RDS Files/planning_area_polygons.rds")

# troubleshoot missing planning area 
leaflet(data = planning_areas) %>%
  addTiles() %>%
  addPolygons(
    color = "blue",
    weight = 1,
    fillOpacity = 0.4,
    label = ~pln_area_n
  )

readRDS("data/RDS Files/planning_area_polygons.rds")
bus_stops <- readRDS("data/RDS Files/bus_with_planning.rds")
mrt_lrt <- readRDS("data/RDS Files/mrt_stations.rds")

