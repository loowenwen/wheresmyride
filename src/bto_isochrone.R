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

# BTO OVERVIEW

# LOADING BTO DATASET
library(sf)
library(dplyr)
library(tidyr)

# Read the CSV file
bto_data <- read.csv("../data/BTO_projects.csv")

# Remove brackets and extra spaces from 'Coordinates' column
bto_data$Coordinates <- gsub("\\[|\\]|,", "", bto_data$Coordinates)  # Remove square brackets and commas
bto_data$Coordinates <- gsub("\\s+", " ", bto_data$Coordinates)  # Remove extra spaces

# Split the 'Coordinates' column into latitude and longitude
bto_data <- separate(bto_data, Coordinates, into = c("Latitude", "Longitude"), sep = " ")

# Convert Latitude and Longitude columns to numeric
bto_data$Latitude <- as.numeric(bto_data$Latitude)
bto_data$Longitude <- as.numeric(bto_data$Longitude)

# Now convert to a spatial object
bto_data_sf <- st_as_sf(bto_data, coords = c("Longitude", "Latitude"), crs = 4326)

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
  setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
  addCircleMarkers(
    data = bto_data_sf,
    color = "blue",
    radius = 6,
    fillOpacity = 0.5,
    popup = ~paste("Town: ", Town, "<br>", "Region: ", Region)
  )

# ISOCHRONE MAP

library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(geosphere)

# Function to get coordinates from postal code
get_coords_from_postal <- function(postal_code) {
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  token <- Sys.getenv("ONEMAP_TOKEN")  # Retrieve stored token
  
  # API Request with authentication
  response <- GET(
    url = base_url,
    query = list(
      searchVal = postal_code,
      returnGeom = "Y",
      getAddrDetails = "Y"
    ),
    add_headers(Authorization = token)  # Token should be passed here
  )
  
  # Check response status
  if (status_code(response) != 200) {
    stop(paste("API Error:", status_code(response)))
  }
  
  # Parse response
  result_text <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result_text, flatten = TRUE)
  
  # Debugging: Print API response
  print(data)
  
  # Ensure data$results exists and is not empty
  if (!("results" %in% names(data)) || nrow(data$results) == 0) {
    stop("No results found for the given postal code.")
  }
  
  # Extract latitude and longitude
  lng <- as.numeric(data$results$LONGITUDE[1])
  lat <- as.numeric(data$results$LATITUDE[1])
  
  return(c(lng, lat))
}

# Generate circular buffer using average speed (20km/h = ~333 m/min)
generate_fast_isochrone <- function(center_lng, center_lat, duration_mins) {
  radius_m <- duration_mins * 333
  center <- c(center_lng, center_lat)
  
  circle_coords <- geosphere::destPoint(p = center, b = seq(0, 360, length.out = 100), d = radius_m)
  coords_df <- as.data.frame(circle_coords)
  colnames(coords_df) <- c("lon", "lat")
  return(coords_df)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Public Transport Isochrone Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("postal", "Enter Postal Code:", "018989"),
      selectInput("time", "Travel Time (minutes):", choices = c(15, 30, 45, 60), selected = 30),
      actionButton("submit", "Generate Isochrone")
    ),
    mainPanel(
      leafletOutput("isochrone_map")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  # Initial map of Singapore
  output$isochrone_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)  # Default view of Singapore
  })
  
  observeEvent(input$submit, {
    coords <- tryCatch(get_coords_from_postal(input$postal), error = function(e) NA)

    
    # Check if coords is valid
    if (length(coords) != 2 || is.na(coords[1]) || is.na(coords[2])) {
      showNotification("Invalid postal code. Please try again.", type = "error")
      return()
    }
    
    lng <- coords[1]
    lat <- coords[2]
    
    # Generate isochrone-like points using OneMap API
    isochrone_pts <- generate_fast_isochrone(
      center_lng = lng,
      center_lat = lat,
      duration_mins = as.numeric(input$time)
    )
    
    # Render Leaflet map with isochrone data and markers
    output$isochrone_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = lng, lat = lat, zoom = 13) %>%
        addMarkers(lng = lng, lat = lat, popup = paste("Postal Code:", input$postal)) %>%
        addPolygons(
          lng = isochrone_pts$lon,
          lat = isochrone_pts$lat,
          fillColor = "blue",
          fillOpacity = 0.3,
          color = "blue",
          weight = 2,
          popup = "Approximate isochrone area"
        ) %>%
        fitBounds(
          lng1 = min(isochrone_pts$lon),
          lat1 = min(isochrone_pts$lat),
          lng2 = max(isochrone_pts$lon),
          lat2 = max(isochrone_pts$lat)
        )
    })
  })
}

# Run the application
shinyApp(ui, server)
