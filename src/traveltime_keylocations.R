library(httr)
library(jsonlite)

route_analyzer <- RouteAnalyzer$new(
  email = "loowenwen1314@gmail.com",
  password = "sochex-6jobge-fomsYb"
)


#raffles place, one north, orchard road (orchard gateway), jurong east, changi airport (terminal 1), singapore general hospital
#048618, 138647, 238858, 609690, 819642, 169608


postal_codes <- c(048618, 138647, 238858, 609690, 819642, 169608)

build_travel_time_matrix <- function(town, bto_coord, postal_codes, date = "03-24-2025", time_period = "Morning Peak (6:30-8:30am)", maxWalkDistance = 1000) {
  
  # Helper: Clean coordinate string
  clean_coord <- function(coord_str) {
    gsub("\\s+", "", gsub("\\[|\\]", "", coord_str))
  }
  
  # Clean the BTO coordinate
  start_coord <- clean_coord(bto_coord)
  
  # Initialize vector to store results
  travel_times <- numeric(length(postal_codes))
  
  # Loop through postal codes
  for (j in seq_along(postal_codes)) {
    end_coord <- get_coordinates_from_postal(postal_codes[j])
    
    routes <- route_analyzer$get_route_data(
      start = start_coord,
      end = end_coord,
      date = date,
      time_period = time_period,
      maxWalkDistance = maxWalkDistance
    )
    
    travel_times[j] <- round(min(routes$duration) / 60)
  }
  
  # Combine into a dataframe
  travel_df <- data.frame(
    Town = town,
    Name = names,
    EstimatedTimeMin = travel_times
  )
  
  return(travel_df)
}

names <- c("Raffles Place", "One North", "Orchard Gateway", "Jurong East", "Changi Airport Terminal", "Singapore General Hospital")
postal_codes <- c(048618, 138647, 238858, 609690, 819642, 169608)

# Let's say you're using Bukit Merah (row 1 of your dataset)
bukit_merah1 <- build_travel_time_matrix(
  town = "Bukit Merah", 
  bto_coord = bto_data$Coordinates[1], 
  postal_codes = postal_codes
)
 



travel_times_df <- build_travel_time_matrix(bto_data, postal_codes)

get_travel_times_to_postal <- function(bto_data, postal_code, date = "03-24-2025", time_period = "Morning Peak (6:30-8:30am)", maxWalkDistance = 1000) {
  
  # Helper: Clean coordinate string
  clean_coord <- function(coord_str) {
    gsub("\\s+", "", gsub("\\[|\\]", "", coord_str))
  }
  
  # Initialize vector to store travel times
  travel_times <- numeric(nrow(bto_data))
  
  for (i in seq_len(nrow(bto_data))) {
    start_coord <- clean_coord(bto_data$Coordinates[i])
    
    # Get route data
    routes <- route_analyzer$get_route_data(
      start = start_coord,
      end = get_coordinates_from_postal(postal_code),
      date = date,
      time_period = time_period,
      maxWalkDistance = maxWalkDistance
    )
    
    # Compute mean travel time in minutes
    travel_times[i] <- round(mean(routes$duration) / 60)
  }
  
  # Return new dataframe
  result <- data.frame(
    Town = bto_data$Town,
    Region = bto_data$Region,
    Coordinates = sapply(bto_data$Coordinates, clean_coord),
    AvgTravelTimeMins = travel_times
  )
  
  return(result)
}



travel_df <- get_travel_times_to_postal(bto_data, postal_code = 819642)













find_closest_key_location <- function(postal_code, building_type = "Mall") {
  # Authentication with OneMap API
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
  
  # Get coordinates from postal code
  postal_coords <- get_coordinates_from_postal(postal_code)
  start_coord <- as.numeric(unlist(strsplit(postal_coords, ",")))
  
  # Search for specified building type in Singapore
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  request_url <- paste0(base_url, 
                        "?searchVal=", URLencode(building_type),
                        "&returnGeom=Y",
                        "&getAddrDetails=Y")
  
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
  
  # Check if results exist
  if (is.null(data$results) || nrow(data$results) == 0) {
    stop(paste("No results found for building type:", building_type))
  }
  
  results_df <- data$results
  
  # Convert coordinates to numeric
  results_df$LATITUDE <- as.numeric(results_df$LATITUDE)
  results_df$LONGITUDE <- as.numeric(results_df$LONGITUDE)
  
  # Calculate distances (euclidean)
  distances <- sqrt(
    (results_df$LATITUDE - start_coord[1])^2 + 
      (results_df$LONGITUDE - start_coord[2])^2
  )
  
  # Find the closest location
  min_index <- which.min(distances)
  
  # Return results
  closest <- list(
    search_coordinate = start_coord,
    search_postal = postal_code,
    building_type = building_type,
    building_name = results_df$BUILDING[min_index],
    building_address = results_df$ADDRESS[min_index],
    building_postal = results_df$POSTAL[min_index],
    building_coordinate = c(results_df$LATITUDE[min_index], results_df$LONGITUDE[min_index]),
    distance = distances[min_index],
    total_results = nrow(results_df)
  )
  
  return(closest)
}




closest_hospital <- find_closest_key_location("520702", "Hospital")

closest_mall <- find_closest_key_location(520702)

routes <- route_analyzer$get_route_data(
  start = paste(closest_mall$search_coordinate, collapse = ","),  
  end = paste(closest_mall$building_coordinate, collapse = ","),    
  date = "03-24-2025",                        
  time_period = "Morning Peak (6:30-8:30am)", 
  maxWalkDistance = 1000                     
)

duration <- mean(routes$duration)/60
distance <- sum(routes$legs[[1]]$distance)




find_most_accessible_location <- function(start_postal, building_type = "Hospital", 
                                          date = Sys.Date(), 
                                          time_period = "Morning Peak (6:30-8:30am)",
                                          maxWalkDistance = 500) {
  
  # 1. Authenticate with OneMap
  auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
  auth_response <- POST(
    url = auth_url,
    body = list(email = "loowenwen1314@gmail.com", password = "sochex-6jobge-fomsYb"),
    encode = "json"
  )
  
  if (status_code(auth_response) != 200) {
    stop("Authentication failed")
  }
  token <- content(auth_response)$access_token
  
  # 2. Get start coordinates
  start_coord_str <- get_coordinates_from_postal(start_postal)
  start_coord <- as.numeric(unlist(strsplit(start_coord_str, ",")))
  
  # 3. Search for all buildings of specified type
  search_url <- paste0(
    "https://www.onemap.gov.sg/api/common/elastic/search",
    "?searchVal=", URLencode(building_type),
    "&returnGeom=Y",
    "&getAddrDetails=Y"
  )
  
  search_response <- GET(search_url, add_headers(Authorization = token))
  if (status_code(search_response) != 200) {
    stop("Building search failed")
  }
  
  buildings <- content(search_response, "parsed")$results
  if (length(buildings) == 0) {
    stop(paste("No", building_type, "found"))
  }
  
  # 4. Initialize variables to track best option
  best_duration <- Inf
  best_hospital <- NULL
  best_route <- NULL
  
  # 5. Check each building's travel time
  for (i in 1:nrow(buildings)) {
    end_coord <- paste(buildings$LATITUDE[i], buildings$LONGITUDE[i], sep = ",")
    
    route_url <- paste0(
      "https://www.onemap.gov.sg/api/public/routingsvc/route",
      "?start=", paste(start_coord[2], start_coord[1], sep = ","), # Note: lon,lat order
      "&end=", end_coord,
      "&routeType=pt",
      "&date=", date,
      "&time=", switch(time_period,
                       "Morning Peak (6:30-8:30am)" = "07:30:00",
                       "Evening Peak (5-7pm)" = "18:00:00",
                       "Daytime Off-Peak (8:30am-5pm)" = "12:00:00",
                       "Nighttime Off-Peak (7pm-6:30am)" = "20:00:00"),
      "&mode=TRANSIT",
      "&maxWalkDistance=", maxWalkDistance,
      "&numItineraries=1"
    )
    
    route_response <- GET(route_url, add_headers(Authorization = token))
    
    if (status_code(route_response) == 200) {
      route_data <- content(route_response, "parsed")
      if (!is.null(route_data$plan$itineraries)) {
        duration <- route_data$plan$itineraries[[1]]$duration / 60 # Convert to minutes
        
        if (duration < best_duration) {
          best_duration <- duration
          best_hospital <- buildings[i,]
          best_route <- route_data$plan$itineraries[[1]]
        }
      }
    }
  }
  
  if (is.null(best_hospital)) {
    stop("No accessible routes found to any locations")
  }
  
  # 6. Return comprehensive results
  list(
    search_postal = start_postal,
    building_type = building_type,
    best_building = list(
      name = best_hospital$BUILDING,
      address = best_hospital$ADDRESS,
      postal = best_hospital$POSTAL,
      coordinates = c(as.numeric(best_hospital$LATITUDE), 
                      as.numeric(best_hospital$LONGITUDE)),
      straight_line_distance = sqrt(
        (as.numeric(best_hospital$LATITUDE) - start_coord[1])^2 +
          (as.numeric(best_hospital$LONGITUDE) - start_coord[2])^2
      )
    ),
    travel_info = list(
      duration_minutes = best_duration,
      departure_time = best_route$startTime,
      arrival_time = best_route$endTime,
      walk_time = best_route$walkTime / 60,
      transit_time = best_route$transitTime / 60,
      transfers = best_route$transfers
    ),
    route_details = best_route$legs,
    alternatives_tested = nrow(buildings)
  )
}
result <- find_most_accessible_location(
  start_postal = "520702",
  building_type = "Hospital",
  time_period = "Morning Peak (6:30-8:30am)"
)



