library(httr)
library(jsonlite)

route_analyzer <- RouteAnalyzer$new(
  email = "loowenwen1314@gmail.com",
  password = "sochex-6jobge-fomsYb"
)

get_coordinates_from_postal <- function(postal_code) {
  # Authenticate with OneMap API
  auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
  email <- "loowenwen1314@gmail.com"
  password <- "sochex-6jobge-fomsYb"
  
  auth_body <- list(email = email, password = password)
  
  auth_response <- POST(
    url = auth_url,
    body = auth_body,
    encode = "json"
  )
  
  if (status_code(auth_response) != 200) {
    stop(paste("Authentication failed with status:", status_code(auth_response)))
  }
  
  # Get token from response
  token <- content(auth_response, as = "parsed")$access_token
  
  # Search API endpoint
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  
  # Construct request URL
  request_url <- paste0(base_url, 
                        "?searchVal=", postal_code,
                        "&returnGeom=Y",
                        "&getAddrDetails=Y")
  
  # Make API request
  search_response <- GET(
    url = request_url,
    add_headers(Authorization = token)
  )
  
  if (status_code(search_response) != 200) {
    stop(paste("Search failed with status:", status_code(search_response)))
  }
  
  # Parse response
  result <- content(search_response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result)
  
  if (data$found == 0) {
    stop("No results found for this postal code")
  }
  
  # Extract first result (most relevant)
  first_result <- data$results[1, ]
  
  # Format as "lat,long" string
  coords_string <- paste0(first_result$LATITUDE, ",", first_result$LONGITUDE)
  
  return(coords_string)
}



#use this vector so don't have to use api to get the keylocation coordinates(since its standard)
keylocation_coordinates <- c("1.28433,103.85138", "1.29939,103.78728", "1.30108,103.83899", "1.33310,103.74253", "1.36132,103.99016", "1.28036,103.83474")
names <- c("Raffles Place", "One North", "Orchard Gateway", "Jurong East", "Changi Airport Terminal", "Singapore General Hospital")
build_travel_time_df <- function(start_postalcode, 
                                 time_period = "Morning Peak (6:30-8:30am)",
                                 names = c("Raffles Place", "One North", "Orchard Gateway", "Jurong East", "Changi Airport Terminal", "Singapore General Hospital"),
                                 location_coords = c("1.28433,103.85138", "1.29939,103.78728", "1.30108,103.83899", "1.33310,103.74253", "1.36132,103.99016", "1.28036,103.83474"), 
                                 date = "03-24-2025", maxWalkDistance = 1000) {
  

  start_coord <- get_coordinates_from_postal(start_postalcode)
  travel_times <- numeric(length(location_coords))
  
  #loop through location coordinate
  for (j in seq_along(location_coords)) {
    end_coord <- location_coords[j] 
    
    # Get route data
    routes <- route_analyzer$get_route_data(
      start = start_coord, 
      end = end_coord,
      date = date,
      time_period = time_period,
      maxWalkDistance = maxWalkDistance
    )
    
    travel_times[j] <- round(mean(routes$duration)/ 60)  
  }
  
  # Combine results into a dataframe
  travel_df <- data.frame(
    Name = names,
    EstimatedTimeMin = travel_times
  )
  
  return(travel_df)
}

#usage to get the results - input a start postal code (default time period is morning peak)
build_travel_time_df(520702)



