
combine_lat_lng <- function(lat_vector, lng_vector) {
  sprintf("%.7f,%.8f", lat_vector, lng_vector)
}
upcoming_bto <- upcoming_bto %>%
  mutate(coordinates = combine_lat_lng(upcoming_bto$lat, upcoming_bto$lng))

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



#convert postal to lat and long
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
               
                               
                               initialize = function(email, password) {
                                 self$email <- email
                                 self$password <- password
                                 self$token <- private$get_auth_token()
                         
                               },
                               
                               get_route_data = function(start, end, routeType = "pt", date, time_period, 
                                                         mode = "TRANSIT", maxWalkDistance, numItineraries = 3) {
                                 
                                 # Define time periods and corresponding representative times
                                 time_periods <- list(
                                   "Morning Peak (6:30-8:30am)" = "07:30:00",
                                   "Evening Peak (5-7pm)" = "18:00:00",
                                   "Daytime Off-Peak (8:30am-5pm)" = "12:00:00",
                                   "Nighttime Off-Peak (7pm-6:30am)" = "20:00:00"
                                 )
                                 
                                 
                                 # Get the representative time for the selected period
                                 time <- time_periods[[time_period]]
                                 
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
                                     
                                     #skips pure walking segments
                                     if (current_mode == "WALK" && i != 1 && i != length(mode)) next
                                     
                                     #finds the starting transit of the leg
                                     if (from_to_index <= length(from_to_stops)) {
                                       route_sequence <- c(route_sequence, from_to_stops[from_to_index])
                                       from_to_index <- from_to_index + 1
                                     }
                                     
                                     #cehck for valid intermediate stops
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
                               
                               #transport score
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
                               
                               #comfort score
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
                                 
                 
                                 sorted_comfort <- sort(comfort_scores, decreasing = TRUE)
                                 overall_comfort <- sorted_comfort[1] * 0.7 + mean(sorted_comfort) * 0.3
                                 
                                 return(overall_comfort)
                               },
                               
                               #robustness score
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
                               
                               calculate_route_options_service_quality = function(routes, time_period, freq_data = all_bus_services_frequencies) {
                                 
                                 # Map your time periods to the analyzer's time periods
                                 analyzer_period <- switch(time_period,
                                                           "Morning Peak (6:30-8:30am)" = "AM_Peak",
                                                           "Evening Peak (5-7pm)" = "PM_Peak",
                                                           "Daytime Off-Peak (8:30am-5pm)" = "AM_Offpeak",
                                                           "Nighttime Off-Peak (7pm-6:30am)" = "PM_Offpeak",
                                                           NA  # default if no match
                                 )
                                 
                                 time_period_avg_column <- paste0(analyzer_period, "_avg")
                                 all_freqs_raw <- freq_data[[time_period_avg_column]]
                                 all_freqs <- log(all_freqs_raw[!is.na(all_freqs_raw) & all_freqs_raw > 0])
                                 
                                 #intialise
                                 route_scores <- numeric(nrow(routes))
                                 
                                 for (route_id in 1:nrow(routes)) { 
                                   bus_scores <- NULL
                                   train_score <- NULL
                                   
                                   transport_modes <- routes$legs[[route_id]]$route  
                                   transport_modes <- transport_modes[transport_modes != ""]
                                   
                                   # Identify transport modes (improved regex)
                                   bus_service_nos <- transport_modes[grepl("^[0-9]+$", transport_modes)]  # Only numeric
                                   train_service_no <- transport_modes[grepl("^[A-Za-z]", transport_modes)]  # Starts with letter
                                   
                                   # Skip if no valid services
                                   if (length(bus_service_nos) == 0 && length(train_service_no) == 0) {
                                     route_scores[route_id] <- NA
                                     next
                                   }
                                   
                                   # Bus service scoring
                                   if (length(bus_service_nos) > 0) {
                                     bus_scores <- numeric(length(bus_service_nos))
                                     
                                     for (bus_idx in seq_along(bus_service_nos)) {  # Changed from i to bus_idx
                                       bus_service_no <- bus_service_nos[bus_idx]
                                       
                                       service_freq <- freq_data[freq_data$ServiceNo == bus_service_no, 
                                                                 time_period_avg_column][1]  # Ensure single value
                                       
                                       if (is.na(service_freq) || service_freq <= 0) {
                                         bus_scores[bus_idx] <- 0
                                       } else {
                                         log_freq <- log(service_freq)
                                         percentile_rank <- sum(all_freqs <= log_freq, na.rm = TRUE) / length(all_freqs)
                                         bus_scores[bus_idx] <- round((1 - percentile_rank) * 100)
                                       }
                                     }
                                   }
                                   
                                   # Train service scoring
                                   if (length(train_service_no) > 0) {
                                     train_service_no <- train_service_no[1]  # Take first if multiple
                                     service_freq <- ifelse(analyzer_period %in% c("AM_Peak", "PM_Peak"), 2.5, 6)
                                     
                                     log_freq <- log(service_freq)
                                     percentile_rank <- sum(all_freqs <= log_freq, na.rm = TRUE) / length(all_freqs)
                                     train_score <- round((1 - percentile_rank) * 100)
                                   }
                                   
                                   # Combine scores
                                   if (!is.null(bus_scores) && !is.null(train_score)) {
                                     route_scores[route_id] <- round(mean(c(bus_scores, train_score)))
                                   } else if (!is.null(bus_scores)) {
                                     route_scores[route_id] <- round(mean(bus_scores))
                                   } else {
                                     route_scores[route_id] <- train_score
                                   }
                                 }
                                 
                                 #balance best-case, average and reliability
                                 combined_score <- round(mean(c(
                                   max(route_scores),       
                                   mean(route_scores),       
                                   quantile(route_scores, 0.25)  
                                 ))) 
                                 
                                 return(combined_score)
                               },
            
                               calculate_rqs = function(start, end, date, time_period, maxWalkDistance = 1000,
                                                         weights = c(transport = 0.25, comfort = 0.25, 
                                                          robustness = 0.25, service = 0.25)) {
                               
                               # Get route data with all required parameters
                                 routes <- self$get_route_data(
                                   start = start,
                                   end = end,
                                   date = date,
                                   time_period = time_period,
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
                                     service = self$calculate_route_options_service_quality(routes, time_period)
                                     
                                    
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
                               
                               
                               
                               
                               calculate_multiple_rqs = function(end, date, time_period, maxWalkDistance = 1000,
                                                                 weights = c(transport = 0.25, comfort = 0.25, 
                                                                             robustness = 0.25, service = 0.25), starts = upcoming_bto$coordinates) {
                                 
                                 # Process all start points
                                 start_coords <- starts
                                 end_coord <- get_coordinates_from_postal(end)
                                 
                                 # Calculate RQS for each start point
                                 results <- lapply(start_coords, function(start) {
                                   self$calculate_rqs(
                                     start = start,
                                     end = end_coord,
                                     date = date,
                                     time_period = time_period,
                                     maxWalkDistance = maxWalkDistance,
                                     weights = weights
                                   )
                                 })
                                 
                                 # Create summary table
                                 summary_table <- data.frame(
                                   Start = upcoming_bto$town,
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
  password = "sochex-6jobge-fomsYb"
)


#calculate RQS with the equal weights on all components
results <- route_analyzer$calculate_multiple_rqs(
  end = "520702",
  date = "03-24-2025",
  time_period = "Morning Peak (6:30-8:30am)",
  weights = c(transport = 0.25, comfort = 0.25, 
              robustness = 0.25, service = 0.25)
)
print(results$summary)  


































