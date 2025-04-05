library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)


#conver postal to lat and long
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


RouteAnalyzer <- R6::R6Class("RouteAnalyzer",
                             public = list(
                               email = NULL,
                               password = NULL,
                               token = NULL,
                               analyzer = bus_analyzer,
                               
                               initialize = function(email, password, analyzer = NULL) {
                                 self$email <- email
                                 self$password <- password
                                 self$token <- private$get_auth_token()
                                 self$analyzer <- analyzer
                               },
                               
                               get_route_data = function(start, end, routeType = "pt", date, time, 
                                                         mode = "TRANSIT", maxWalkDistance, numItineraries = 3) {
                                 # Define API endpoint
                                 base_url <- "https://www.onemap.gov.sg/api/public/routingsvc/route"
                                 
                                 # Construct full URL with parameters
                                 request_url <- paste0(
                                   base_url,
                                   "?start=", start,
                                   "&end=", end,
                                   "&routeType=", routeType,
                                   "&date=", date,
                                   "&time=", time,
                                   "&mode=", mode,
                                   "&maxWalkDistance=", maxWalkDistance,
                                   "&numItineraries=", numItineraries
                                 )
                                 
                                 # Make API request
                                 response <- GET(
                                   url = request_url,
                                   add_headers(Authorization = self$token)
                                 )
                                 
                                 # Check response status
                                 if (status_code(response) == 200) {
                                   # Parse JSON response
                                   result <- content(response, as = "text", encoding = "UTF-8")
                                   data <- fromJSON(result)
                                   
                                   # Extract and print itineraries
                                   if ("plan" %in% names(data) && "itineraries" %in% names(data$plan)) {
                                     itineraries <- data$plan$itineraries
                                     return(itineraries)
                                   } else {
                                     print("Itineraries not found in the response.")
                                     return(NULL)
                                   }
                                 } else {
                                   print(paste("Error:", status_code(response)))
                                   return(NULL)
                                 }
                               },
                               
                               
                               generate_all_route_sequences = function(api_response) {
                                 
                                 all_sequences <- list()
                                 
                                 # Process each route in the api_response
                                 for (route_num in seq_along(api_response)) {
                                   current_route <- api_response[[route_num]]
                                   mode <- as.list(current_route$mode)
                                   from_to_stops <- current_route$from$stopCode[!is.na(current_route$from$stopCode)]
                                   
                                   # Generate sequence for this route
                                   route_sequence <- c("Origin")
                                   from_to_index <- 1
                                   intermediate_index <- 1
                                   
                                   for (i in seq_along(mode)) {
                                     current_mode <- mode[[i]]
                                     
                                     # Skip pure walking segments (except first/last)
                                     if (current_mode == "WALK" && i != 1 && i != length(mode)) next
                                     
                                     # Add starting point of this leg
                                     if (from_to_index <= length(from_to_stops)) {
                                       route_sequence <- c(route_sequence, from_to_stops[from_to_index])
                                       from_to_index <- from_to_index + 1
                                     }
                                     
                                     # Check for valid intermediate stops
                                     if (current_mode != "WALK") {
                                       max_checks <- 3  
                                       checks_done <- 0
                                       
                                       while (checks_done < max_checks) {
                                         # Check if intermediateStops exists and has enough elements
                                         if (!is.null(current_route$intermediateStops) && 
                                             length(current_route$intermediateStops) >= intermediate_index) {
                                           
                                           current_intermediate <- current_route$intermediateStops[[intermediate_index]]
                                           
                                           # Check if valid and not empty
                                           if (!is.null(current_intermediate) && 
                                               length(current_intermediate) > 0 && 
                                               !is.null(current_intermediate$stopCode) &&
                                               length(current_intermediate$stopCode) > 0) {
                                             
                                             intermediates <- current_intermediate$stopCode
                                             route_sequence <- c(route_sequence, intermediates)
                                             intermediate_index <- intermediate_index + 1
                                             break
                                           }
                                         }
                                         
                                         
                                         intermediate_index <- intermediate_index + 1
                                         checks_done <- checks_done + 1
                                       }
                                     }
                                     
                                     # Add ending point of this leg if it's not the last transportation segment
                                     if (current_mode != "WALK" && i < length(mode) - 1 && from_to_index <= length(from_to_stops)) {
                                       route_sequence <- c(route_sequence, from_to_stops[from_to_index])
                                       from_to_index <- from_to_index + 1
                                     }
                                   }
                                   
                                   # Add final destination
                                   route_sequence <- c(route_sequence, "Destination")
                                   
                                   # Store in list with route number
                                   all_sequences[[paste0("route_", route_num)]] <- route_sequence
                                 }
                                 
                                 return(all_sequences)
                               },
                               
                               fix_mrt_sequences = function(route_sequences) {
                                 # Function to process each route
                                 fix_single_route <- function(route) {
                                   # Identify MRT segments and their directions
                                   mrt_segments <- list()
                                   current_segment <- NULL
                                   current_line <- NULL
                                   
                                   for (stop in route) {
                                     # Skip non-MRT stops
                                     if (!grepl("^(EW|NS|NE|CC|DT|TE|CG)\\d+$", stop)) {
                                       if (!is.null(current_segment)) {
                                         mrt_segments[[length(mrt_segments) + 1]] <- current_segment
                                         current_segment <- NULL
                                       }
                                       next
                                     }
                                     
                                     # Extract line and number
                                     line <- gsub("\\d+", "", stop)
                                     number <- as.numeric(gsub("\\D", "", stop))
                                     
                                     # Start new segment if needed
                                     if (is.null(current_segment) || line != current_line) {
                                       if (!is.null(current_segment)) {
                                         mrt_segments[[length(mrt_segments) + 1]] <- current_segment
                                       }
                                       current_segment <- list(line = line, stops = stop)
                                       current_line <- line
                                     } else {
                                       current_segment$stops <- c(current_segment$stops, stop)
                                     }
                                   }
                                   
                                   # Add the last segment if it exists
                                   if (!is.null(current_segment)) {
                                     mrt_segments[[length(mrt_segments) + 1]] <- current_segment
                                   }
                                   
                                   # Fix each MRT segment
                                   for (seg in mrt_segments) {
                                     # Extract numbers and determine direction
                                     numbers <- as.numeric(gsub("\\D", "", seg$stops))
                                     
                                     if (all(diff(numbers) > 0)) next  # Already increasing
                                     if (all(diff(numbers) < 0)) next  # Already decreasing
                                     
                                     # Determine proper order (increasing or decreasing)
                                     if (numbers[1] < numbers[length(numbers)]) {
                                       # Should be increasing
                                       seg$stops <- seg$stops[order(numbers)]
                                     } else {
                                       # Should be decreasing
                                       seg$stops <- seg$stops[order(-numbers)]
                                     }
                                     
                                     # Replace in original route
                                     route[route %in% seg$stops] <- seg$stops
                                   }
                                   
                                   return(route)
                                 }
                                 
                                 # Apply to all routes
                                 lapply(route_sequences, fix_single_route)
                               },
                               
                               extract_route_metrics = function(itinerary) {
                                 tibble(
                                   duration = itinerary$duration / 60, # Convert to minutes
                                   walkTime = itinerary$walkTime / 60,
                                   transitTime = itinerary$transitTime / 60,
                                   waitingTime = itinerary$waitingTime / 60,
                                   transfers = itinerary$transfers
                                 )
                               },
                               
                               combined_transport_efficiency = function(routes) {
                                 
                                 transport_scores <- numeric(length(routes))
                                 
                                 # Loop through all routes
                                 for (i in 1:length(routes$duration)) {
                                   
                                   duration_hours <- routes$duration[[i]] / 3600
                                   total_distance <- sum(routes$legs[[i]]$distance)
                                   speed <- (total_distance / 1000) / duration_hours #km/h 
                                   
                                   # Normalize speed score (0-100)
                                   max_speed <- 50  # km/h 
                                   min_speed <- 3    # km/h
                                   
                                   transport_scores[i] <- 100 * (speed - min_speed) / (max_speed - min_speed)
                                   transport_scores[i] <- max(0, min(100, transport_scores[i]))  # Clamp between 0-100
                                 }
                                 
                                 # Handle case where no valid scores were calculated
                                 if (all(is.na(transport_scores))) return(0)
                                 
                                 # Combine scores with weighting
                                 sorted_scores <- sort(transport_scores, decreasing = TRUE)
                                 
                                 # Weighted components
                                 best_score <- sorted_scores[1] * 0.8
                                 second_best <- sorted_scores[2] * 0.1
                                 worst <- sorted_scores[3] * 0.1
                                 
                                 return(best_score + second_best + worst)
                               },
                               
                               calculate_comfort_score = function(routes_metrics) {
                                 # Initialize comfort scores vector
                                 comfort_scores <- numeric(nrow(routes_metrics))
                                 
                                 for (i in 1:nrow(routes_metrics)) {
                                   # Extract metrics for current route
                                   current_route <- routes_metrics[i, ]
                                   
                                   # Penalize transfers (more transfers = less comfort)
                                   transfers_penalty <- current_route$transfers * 15  # Each transfer reduces score by 15 points
                                   
                                   # Penalize walking time (longer walks = less comfort)
                                   walk_penalty <- current_route$walkTime * 2  # Each minute of walking reduces score by 2 points
                                   
                                   # Base comfort score (starting from 100)
                                   comfort_score <- 100 - transfers_penalty - walk_penalty
                                   
                                   # Ensure score is within 0-100 range
                                   comfort_score <- max(0, min(100, comfort_score))
                                   
                                   comfort_scores[i] <- comfort_score
                                 }
                                 
                                 # Calculate overall comfort score (weighted average favoring best route)
                                 sorted_comfort <- sort(comfort_scores, decreasing = TRUE)
                                 overall_comfort <- sorted_comfort[1] * 0.7 + mean(sorted_comfort) * 0.3
                                 
                                 return(overall_comfort)
                               },
                               
                               calculate_robustness_score = function(route_sequences, routes) {
                                 # Component 1: Route Independence (0-40 points)
                                 transport_nodes <- lapply(route_sequences, function(x) {
                                   x[!x %in% c("Origin", "Destination")] 
                                 })
                                 
                                 # Calculate overlap between all route pairs
                                 route_pairs <- combn(length(transport_nodes), 2, simplify = FALSE)
                                 overlap_scores <- sapply(route_pairs, function(pair) {
                                   length(intersect(transport_nodes[[pair[1]]], transport_nodes[[pair[2]]]))
                                 })
                                 
                                 route_independence <- 40 * (1 - mean(overlap_scores/max(sapply(transport_nodes, length))))
                                 
                                 # Component 2: Mode Diversity (0-30 points)
                                 modes <- lapply(routes$legs, function(legs) {
                                   unique(legs$mode[legs$mode != "WALK"])
                                 })
                                 unique_modes <- unique(unlist(modes))
                                 
                                 mode_diversity <- if(length(unique_modes) == 0) {
                                   0
                                 } else {
                                   30 * (length(unique_modes)/2) # Max 2 points per unique mode type
                                 }
                                 
                                 # Component 3: Hybrid Route Bonus (0-20 points)
                                 has_hybrid <- any(sapply(modes, function(m) {
                                   "BUS" %in% m && any(c("SUBWAY", "RAIL") %in% m)
                                 }))
                                 hybrid_bonus <- ifelse(has_hybrid, 20, 0)
                                 
                                 # Component 4: Failure Resistance (0-10 points)
                                 common_nodes <- Reduce(intersect, transport_nodes)
                                 failure_resistance <- 10 * (1 - length(common_nodes)/max(sapply(transport_nodes, length)))
                                 
                                 # Sum components (automatically capped at 100)
                                 robustness_score <- min(100, 
                                                         route_independence + 
                                                           mode_diversity + 
                                                           hybrid_bonus + 
                                                           failure_resistance)
                                 
                                 return(round(robustness_score))
                               },
                               
                               calculate_route_options_service_quality = function(routes, departure_time) {
                                 if (is.null(self$analyzer)) {
                                   stop("Bus frequency analyzer not provided")
                                 }
                                 
                                 # Calculate scores for all available routes (up to 3)
                                 route_scores <- sapply(1:length(routes$legs), function(i) {
                                   # Extract bus numbers for this route
                                   bus_numbers <- na.omit(routes$legs[[i]]$route[nzchar(routes$legs[[i]]$route) & 
                                                                                   routes$legs[[i]]$mode == "BUS"])
                                   
                                   if (length(bus_numbers) == 0) return(0)  # No buses in this route
                                   
                                   # Get frequency data
                                   freq_data <- lapply(bus_numbers, function(bus_no) self$analyzer$get_service_frequency(bus_no))
                                   freq_data <- freq_data[!sapply(freq_data, is.null)]
                                   if (length(freq_data) == 0) return(0)
                                   
                                   # Determine time period (same for all routes)
                                   time_parts <- as.numeric(strsplit(departure_time, ":")[[1]])
                                   departure_min <- time_parts[1] * 60 + time_parts[2]
                                   
                                   time_period <- if (departure_min >= 6.5*60 && departure_min < 8.5*60) {
                                     "AM_Peak"
                                   } else if (departure_min >= 17*60 && departure_min < 19*60) {
                                     "PM_Peak"
                                   } else if (departure_min >= 6.5*60 && departure_min < 17*60) {
                                     "AM_Offpeak"
                                   } else {
                                     "PM_Offpeak"
                                   }
                                   
                                   # Calculate combined wait time for this route
                                   avg_waits <- sapply(freq_data, function(x) x[[time_period]]$avg)
                                   avg_waits <- avg_waits[!is.na(avg_waits)]
                                   if (length(avg_waits) == 0) return(0)
                                   
                                   1 / mean(1 / avg_waits)  # Returns combined wait time for this route
                                 })
                                 
                                 # Convert wait times to scores
                                 route_scores <- sapply(route_scores, function(wait) {
                                   case_when(
                                     wait <= 5  ~ 90,
                                     wait <= 8  ~ 75,
                                     wait <= 12 ~ 60,
                                     TRUE       ~ 40
                                   )
                                 })
                                 
                                 # Handle cases with <3 routes
                                 if (length(route_scores) == 1) {
                                   return(route_scores[1])
                                 } 
                                 
                                 # Sort scores in descending order
                                 sorted_scores <- sort(route_scores, decreasing = TRUE)
                                 
                                 # Weighted score calculation
                                 final_score <- 0.6 * sorted_scores[1] +  # Best route
                                   0.3 * sorted_scores[2] +  # Second-best
                                   0.1 * sorted_scores[min(3, length(sorted_scores))]  # Worst
                                 
                                 max(0, min(100, round(final_score)))
                               },
                               
                               calculate_rqs = function(start, end, date, time, maxWalkDistance = 1000,
                                                        weights = c(transport = 0.25, comfort = 0.25, 
                                                                    robustness = 0.25, service = 0.25)) {
                                 
                                 # Get route data with all required parameters
                                 routes <- self$get_route_data(
                                   start = start,
                                   end = end,
                                   date = date,
                                   time = time,
                                   maxWalkDistance = maxWalkDistance
                                 )
                                 
                                 # Process routes
                                 route_sequences <- self$generate_all_route_sequences(routes$legs)
                                 fixed_sequences <- self$fix_mrt_sequences(route_sequences)
                                 routes_metrics <- self$extract_route_metrics(routes) %>% 
                                   mutate(route_name = names(fixed_sequences))
                                 
                                 # Calculate component scores
                                 scores <- c(
                                   transport = round(self$combined_transport_efficiency(routes)),
                                   comfort = round(self$calculate_comfort_score(routes_metrics)),
                                   robustness = self$calculate_robustness_score(fixed_sequences, routes),
                                   service = if (!is.null(self$analyzer)) {
                                     self$calculate_route_options_service_quality(routes, time)
                                   } else {
                                     warning("Service quality score not calculated - no bus analyzer provided")
                                     0
                                   }
                                 )
                                 
                                 # Calculate weighted RQS
                                 weighted_scores <- scores * weights
                                 rqs <- round(sum(weighted_scores))
                                 
                                 # Return results
                                 list(
                                   rqs = rqs,
                                   components = scores,
                                   weights = weights,
                                   weighted_scores = weighted_scores
                                 )
                               },
                               
                               #convert to coords
                               convert_to_coords = function(location) {
                                 if (grepl("^\\d{6}$", location)) {
                                   return(get_coordinates_from_postal(location))
                                 }
                                 return(location)
                               },
                               
                               
                               calculate_multiple_rqs = function(starts, end, date, time, maxWalkDistance = 1000,
                                                                 weights = c(transport = 0.25, comfort = 0.25, 
                                                                             robustness = 0.25, service = 0.25)) {
                                 
                                 # Process all start points
                                 start_coords <- sapply(starts, self$convert_to_coords)
                                 end_coord <- self$convert_to_coords(end)
                                 
                                 # Calculate RQS for each start point
                                 results <- lapply(start_coords, function(start) {
                                   self$calculate_rqs(
                                     start = start,
                                     end = end_coord,
                                     date = date,
                                     time = time,
                                     maxWalkDistance = maxWalkDistance,
                                     weights = weights
                                   )
                                 })
                                 
                                 # Create summary table
                                 summary_table <- data.frame(
                                   Start = starts,
                                   RQS = sapply(results, function(x) x$rqs),
                                   Transport = sapply(results, function(x) x$components["transport"]),
                                   Comfort = sapply(results, function(x) x$components["comfort"]),
                                   Robustness = sapply(results, function(x) x$components["robustness"]),
                                   Service = sapply(results, function(x) x$components["service"]),
                                   stringsAsFactors = FALSE
                                 )
                                 
                                 
                                 # Return both detailed results and summary
                                 list(
                                   all_results = results,
                                   summary = summary_table
                                 )
                               }
                             ),
                             
                             private = list(
                               get_auth_token = function() {
                                 auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
                                 response <- POST(
                                   url = auth_url,
                                   body = list(email = self$email, password = self$password),
                                   encode = "json"
                                 )
                                 
                                 if (status_code(response) == 200) {
                                   content(response)$access_token
                                 } else {
                                   stop(paste("Authentication failed:", status_code(response)))
                                 }
                               }
                             )
)


# Initialize with your OneMap credentials and bus analyzer
route_analyzer <- RouteAnalyzer$new(
  email = "loowenwen1314@gmail.com",
  password = "sochex-6jobge-fomsYb",
  analyzer = bus_analyzer # Your initialized BusAnalyzer instance
)

#calculate RQS with the equal weights on all components
results <- route_analyzer$calculate_multiple_rqs(
  starts = c("520702", "551234", "528523"),  # Postal codes
  end = "118420",                            # Single destination
  date = "03-24-2025",
  time = "07:35:00"
)
print(results$summary)  # Summary table of all routes


#calculate RQS with user chosen weights
results_2 <- route_analyzer$calculate_multiple_rqs(
  starts = c("520702", "551234", "528523"),  # Postal codes
  end = "118420",                            # Single destination
  date = "03-24-2025",
  time = "07:35:00",
  weights = c(transport_efficiency = 0.40, comfort = 0.10, 
              robustness = 0.25, service = 0.25)
)

print(results_2$summary)




































# Function to get station crowd density forecast
get_station_crowd_forecast <- function(train_line, api_key) {
  # API endpoint
  url <- "https://datamall2.mytransport.sg/ltaodataservice/PCDForecast"
  
  # Make API request
  response <- GET(
    url = url,
    add_headers(AccountKey = api_key),
    query = list(TrainLine = train_line)
  )
  
  # Check response status
  if (status_code(response) == 200) {
    # Parse JSON response
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    # Convert to data frame
    df <- as.data.frame(data)
    
    # Clean up column names
    names(df) <- gsub("^value\\.", "", names(df))
    
    return(df)
  } else {
    stop(paste("API request failed with status code:", status_code(response)))
  }
}

# Your LTA DataMall API key (replace with your actual key)
api_key <- "o6OuJxI3Re+qYgFQzb+4+w=="  # Get this from LTA DataMall

# Example usage for North-South Line
train_line <- "NSL"  # Can change to "EWL", "DTL", etc.

# Get the forecast data
crowd_data <- get_station_crowd_forecast(train_line, api_key)

# View the first few rows
head(crowd_data)

# Create a summary table by station
library(dplyr)
crowd_summary <- crowd_data %>%
  group_by(Station, CrowdLevel) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Station, CrowdLevel)

# View the summary
print(crowd_summary)
#get NS1
crowd_data$Stations[[1]]$Interval[[1]]




