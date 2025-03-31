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



# WEN WEN'S CODE #
data <- data[["SearchResults"]]

geojson_string <- data$geojson[1]

spatial_polygon <- st_read(geojson_string, quiet = TRUE)

print(spatial_polygon)

print(st_crs(spatial_polygon))

plot(spatial_polygon$geometry)

spatial_polygon$pln_area_n <- data$pln_area_n[1]
names(spatial_polygon)

file_name <- paste0(data$pln_area_n[1], ".geojson")
full_path <- file.path("data/PlanningAreaPolygons", file_name)
st_write(spatial_polygon, full_path)