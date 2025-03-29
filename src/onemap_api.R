# MAP SERVICES

library(leaflet)
# create a leaflet map with OneMap basemap
leaflet() %>%
  addTiles(
    urlTemplate = "https://www.onemap.gov.sg/maps/tiles/Default/{z}/{x}/{y}.png",
    attribution = '<img src="https://www.onemap.gov.sg/web-assets/images/logo/om_logo.png" style="height:20px;width:20px;"/>&nbsp;<a href="https://www.onemap.gov.sg/" target="_blank" rel="noopener noreferrer">OneMap</a>&nbsp;&copy;&nbsp;contributors&nbsp;&#124;&nbsp;<a href="https://www.sla.gov.sg/" target="_blank" rel="noopener noreferrer">Singapore Land Authority</a>',
    options = tileOptions(
      detectRetina = TRUE,
      maxZoom = 19,
      minZoom = 11
    )
  ) %>%
  setView(lng = 103.8198, lat = 1.3521, zoom = 12)


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


# SEARCH: This API takes a text input which can be a building name, road name, bus stop number, or postal code; for example: "Revenue House" or "307987". 
#It returns address information including both latitude, longitude, and x, y coordinates, based on the text input and sorts the results according to their estimated relevance.

library(httr)
library(jsonlite)

# define API endpoint
base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"

# define parameters
searchVal <- "460116"  # example search value (postal code, road name, etc.)
returnGeom <- "Y"  # get geometry values
getAddrDetails <- "Y"  # get address details

# replace with your actual API token
authToken <- token

# construct full URL with parameters
request_url <- paste0(base_url, "?searchVal=", searchVal, "&returnGeom=", returnGeom, "&getAddrDetails=", getAddrDetails)

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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


# ROUTING 2
## PUBLIC TRANSPORT

library(httr)
library(jsonlite)

# Function to extract itineraries and legs from API response
extract_itineraries <- function(data) {
  # Check if data contains itineraries
  if (!"plan" %in% names(data) || !"itineraries" %in% names(data$plan)) {
    stop("No itineraries found in the response")
  }
  
  itineraries <- data$plan$itineraries
  result <- list()
  
  # Iterate through each itinerary
  for (i in seq_along(itineraries)) {
    itinerary_name <- paste0("itinerary_", i)
    legs_name <- paste0("legs_", i)
    
    # Extract the entire itinerary (excluding "Too sloped" and "Fare")
    itinerary <- itineraries[[i]]
    itinerary$fare <- NULL
    itinerary$tooSloped <- NULL
    
    # Store the itinerary
    result[[itinerary_name]] <- itinerary
    
    # Extract the legs separately
    legs <- itinerary$legs
    result[[legs_name]] <- legs
  }
  
  return(result)
}

# define API endpoint
base_url <- "https://www.onemap.gov.sg/api/public/routingsvc/route"

# define parameters
start = "1.35617,103.93748" # in WGS84 latitude, longitude format  (my house)
end = "1.294178,103.7698" # in WGS84 latitude, longitude format (white sands)
routeType = "pt" # route types available: walk, drive, pt, and cycle
date = "03-24-2025" # date of the selected start point in MM-DD-YYYY
time = "09:35:00" # time of the selected start point in [HH][MM][SS], using the 24-hour clock system
mode = "TRANSIT"  # mode of public transport: TRANSIT, BUS, RAIL
maxWalkDistance = 5000
numItineraries = 3

# replace with your actual API token
authToken <- token

# construct full URL with parameters
request_url <- paste0(base_url, 
                      "?start=", start, 
                      "&end=", end,
                      "&routeType=", routeType,
                      "&date=", date,
                      "&time=", time,
                      "&mode=", mode,
                      "&maxWalkDistance=", maxWalkDistance,
                      "&numItineraries=", numItineraries)

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
  # Extract itineraries and legs
  itineraries_list <- extract_itineraries(data)
  print(itineraries_list)  # Print the extracted data
} else {
  print(paste("Error:", status_code(response)))
}



# ROUTING
## PUBLIC TRANSPORT

library(httr)

# define API endpoint
base_url <- "https://www.onemap.gov.sg/api/public/routingsvc/route"

# define parameters
start = "1.393027,103.9013" # in WGS84 latitude, longitude format
end = "1.370669,103.8824" # in WGS84 latitude, longitude format
routeType = "pt" # route types available: walk, drive, pt, and cycle
date = "03-24-2025" # date of the selected start point in MM-DD-YYYY
time = "07:35:00" # time of the selected start point in [HH][MM][SS], using the 24-hour clock system
mode = "TRANSIT"  # mode of public transport: TRANSIT, BUS, RAIL
maxWalkDistance = 1000
numItineraries = 3

# replace with your actual API token
authToken <- token

# construct full URL with parameters
request_url <- paste0(base_url, 
                      "?start=", start, 
                      "&end=", end,
                      "&routeType=", routeType,
                      "&date=", date,
                      "&time=", time,
                      "&mode=", mode,
                      "&maxWalkDistance=", maxWalkDistance,
                      "&numItineraries=", numItineraries)

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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## WALK/DRIVE/CYCLE

library(httr)

# define API endpoint
base_url <- "https://www.onemap.gov.sg/api/public/routingsvc/route"

# define parameters
start = "1.29779464772932,103.780638335602" # in WGS84 latitude, longitude format
end = "1.33100823526591,103.938066838998" # in WGS84 latitude, longitude format
routeType = "drive" # route types available: walk, drive, pt, and cycle

# replace with your actual API token
authToken <- token

# construct full URL with parameters
request_url <- paste0(base_url, 
                      "?start=", start, 
                      "&end=", end,
                      "&routeType=", routeType
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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


# PLANNING AREA

## PLANNING AREA POLYGONS
library(httr)

# define the URL and headers
url <- "https://www.onemap.gov.sg/api/public/popapi/getAllPlanningarea"

# replace with your actual API token
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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}

data$SearchResults$geojson[2]


## NAMES OF PLANNING AREA
library(httr)

# define the URL and headers
url <- "https://www.onemap.gov.sg/api/public/popapi/getPlanningareaNames"

# replace with your actual API token
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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## PLANNING AREA QUERY
library(httr)

# define the URL and headers
url <- "https://www.onemap.gov.sg/api/public/popapi/getPlanningarea"

# define parameters
latitude = "1.3" 
longitude = "103.8"
year = "2019"

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
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


# THEMES

## GET ALL THEMES INFO
library(httr)

# define the URL and headers
url <- "https://www.onemap.gov.sg/api/public/themesvc/getAllThemesInfo?moreInfo=Y"

# replace with your actual API token
authToken <- token

# make API request
response <- GET(
  url,
  add_headers(Authorization = paste("Bearer", authToken))
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## RETRIEVE THEME
library(httr)

# define the URL with query parameters
url <- "https://www.onemap.gov.sg/api/public/themesvc/retrieveTheme"
queryName <- "tourism"

# replace with your actual API token
authToken <- token

# construct the request URL
request_url <- paste0(url, "?queryName=", queryName)

# make API request
response <- GET(
  request_url,
  add_headers(Authorization = paste("Bearer", authToken))
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## RETRIEVE THEME - WITH EXTENTS
library(httr)

# define the base URL
url <- "https://www.onemap.gov.sg/api/public/themesvc/retrieveTheme"

# define query parameters
queryName <- "tourism"
extents <- "1.291789,103.7796402,1.3290461,103.8726032"  # boundary coordinates provided by user

# replace with your actual API token
authToken <- token

# construct the request URL with encoded parameters
request_url <- paste0(url, "?queryName=", queryName, "&extents=", URLencode(extents, reserved = TRUE))

# make API request
response <- GET(
  request_url,
  add_headers(Authorization = paste("Bearer", authToken))
)

if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## GET THEMES INFO
library(httr)

# define the URL with query parameters
url <- "https://www.onemap.gov.sg/api/public/themesvc/getThemeInfo"
queryName <- "tourism"

# replace with your actual API token
authToken <- token

# construct the request URL
request_url <- paste0(url, "?queryName=", queryName)

# make API request
response <- GET(
  request_url,
  add_headers(Authorization = paste("Bearer", authToken))
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}


## CHECK THEME STATUS
library(httr)

# define the URL with query parameters
url <- "https://www.onemap.gov.sg/api/public/themesvc/checkThemeStatus"
queryName <- "tourism"
dateTime <- "2023-06-15T16:00:00.000Z"  # ensure correct formatting

# replace with your actual API token
authToken <- token

# construct the request URL
request_url <- paste0(url, "?queryName=", queryName, "&dateTime=", URLencode(dateTime, reserved = TRUE))

# make API request
response <- GET(
  request_url,
  add_headers(Authorization = paste("Bearer", authToken))
)

# check response status
if (status_code(response) == 200) {
  # parse JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  print(data)  # print response data
} else {
  print(paste("Error:", status_code(response)))
}