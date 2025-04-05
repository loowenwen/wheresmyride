##Crowd density - factors in the crowdness level of train involved
#get_crowd_density_score <- function(routes, lta_api_key) {
  # Initialize list to store scores for all routes
  route_crowd_scores <- list()
  
  # Process each route (typically 3 alternatives)
  for (i in seq_along(routes$legs)) {
    legs <- routes$legs[[i]]
    crowd_scores <- numeric(0)
    in_train_segment <- FALSE
    
    # Analyze each leg of the current route
    for (j in seq_along(legs$mode)) {
      current_mode <- legs$mode[j]
      
      # Detect start of new train segment
      if (!in_train_segment && current_mode == "SUBWAY" && !is.na(legs$route[j])) {
        in_train_segment <- TRUE
        train_line <- legs$route[j]
        station_code <- sub(".*:", "", legs$from$stopCode[j])  # Corrected from stopId to stopCode
        
        # API call for this station's crowd
        response <- GET(
          "https://datamall2.mytransport.sg/ltaodataservice/PCDForecast",
          add_headers(AccountKey = lta_api_key),
          query = list(TrainLine = train_line)
        )
        
        if (status_code(response) == 200) {
          forecast <- fromJSON(content(response, "text"))$value
          station_data <- forecast[forecast$Station == station_code, ]
          
          if (nrow(station_data) > 0) {
            latest <- tail(station_data, 1)
            crowd_scores <- c(crowd_scores, 
                              case_when(
                                latest$CrowdLevel == "l" ~ 90,  # Low
                                latest$CrowdLevel == "m" ~ 60,  # Moderate
                                latest$CrowdLevel == "h" ~ 30,  # High
                                TRUE ~ 50  # Default for NA/unexpected
                              ))
          }
        }
      }
      
      # Reset at non-train segments
      if (current_mode != "SUBWAY") {
        in_train_segment <- FALSE
      }
    }
    
    # Store score for this route (100 if no trains)
    route_crowd_scores[[i]] <- ifelse(length(crowd_scores) == 0, 100, mean(crowd_scores))
  }
  
  # Return scores for all routes
  unlist(route_crowd_scores)
}
#get_crowd_density_score(routes, "o6OuJxI3Re+qYgFQzb+4+w==")

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

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
                                     print(itineraries)  # Print the extracted itineraries
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
                                 # Initialize list to store all route sequences
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
                                       max_checks <- 3  # Maximum number of subsequent stops to check
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
                                         
                                         # If we get here, the current intermediate wasn't valid - try next one
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
                                     
                                     # Check if already in order
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
                                 # Initialize vector to store transport scores
                                 transport_scores <- numeric(length(routes))
                                 
                                 # Loop through all routes
                                 for (i in 1:length(routes$duration)) {
                                   # Safely extract duration and convert to hours
                                   duration_hours <- routes$duration[[i]] / 3600
                                   
                                   # Calculate total distance from all legs (in meters)
                                   total_distance <- sum(routes$legs[[i]]$distance)
                                   
                                   # Calculate speed in km/h (using total distance)
                                   speed <- (total_distance / 1000) / duration_hours
                                   
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
                                 # Initialize base score (100 = perfectly robust)
                                 base_score <- 100
                                 score <- base_score
                                 
                                 # Extract transport nodes (exclude Origin/Destination)
                                 transport_nodes <- lapply(route_sequences, function(x) {
                                   x[!x %in% c("Origin", "Destination")] 
                                 })
                                 
                                 # Single Point of Failure Penalty (SPFP)
                                 common_nodes <- Reduce(intersect, transport_nodes)
                                 N <- length(common_nodes)
                                 spof_penalty <- length(common_nodes) * 15  # Single Point of Failure penalty
                                 score <- score - spof_penalty
                                 
                                 # Mode Diversity Analysis
                                 modes <- lapply(routes$legs, function(legs) {
                                   unique(legs$mode[legs$mode != "WALK"])  # Exclude "WALK" mode
                                 })
                                 
                                 # Hybrid Bonus (+20 if any route uses both bus and rail)
                                 has_hybrid <- any(sapply(modes, function(m) "BUS" %in% m && any(c("SUBWAY", "RAIL") %in% m)))
                                 if (has_hybrid) {
                                   score <- score + 20  # Hybrid bonus for bus + rail
                                 }
                                 
                                 # Single-Mode Penalty (-25 if all routes use the same mode type)
                                 all_modes <- unique(unlist(modes))
                                 if (length(all_modes) == 1) {
                                   score <- score - 25  # Single-mode penalty
                                 }
                                 
                                 # Reward for Routes Being Completely Different
                                 route_combinations <- combn(length(transport_nodes), 2)
                                 completely_different_reward <- 0
                                 
                                 for (i in 1:ncol(route_combinations)) {
                                   route1 <- transport_nodes[[route_combinations[1, i]]]
                                   route2 <- transport_nodes[[route_combinations[2, i]]]
                                   
                                   if (length(intersect(route1, route2)) == 0) {
                                     completely_different_reward <- completely_different_reward + 10
                                   }
                                 }
                                 
                                 # Apply reward for complete differences
                                 min_score <- 0
                                 max_score <- 150
                                 
                                 # Normalize the score between 0 and 100
                                 normalized_score <- (score - min_score) / (max_score - min_score) * 100
                                 
                                 return(normalized_score)
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
                                                        weights = c(transport = 0.4, comfort = 0.3, 
                                                                    robustness = 0.2, service = 0.1)) {
                                 
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
                                   transport = self$combined_transport_efficiency(routes),
                                   comfort = self$calculate_comfort_score(routes_metrics),
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

result <- route_analyzer$calculate_rqs(
  start = "1.30374,103.83214",
  end = "1.294178,103.7698", 
  date = "03-24-2025",  # Note: format should be "YYYY-MM-DD"
  time = "07:35:00"       # Just hours:minutes, no seconds
)

##to get RQS 
print(result$rqs)  

##to get individual scores of RQS
print(result$components)






















##RQS individual score functions codes

#total distance/speed 
combined_transport_efficiency <- function(routes) {
  # Initialize vector to store transport scores
  transport_scores <- numeric(length(routes))
  
  # Loop through all routes
  for (i in 1:length(routes$duration)) {
    # Safely extract duration and convert to hours
    duration_hours <- routes$duration[[i]] / 3600
    
    # Calculate total distance from all legs (in meters)
    total_distance <- sum(routes$legs[[i]]$distance)
    
    # Calculate speed in km/h (using total distance)
    speed <- (total_distance / 1000) / duration_hours
    
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
}

combined_transport_efficiency(routes)


#comfort score - penalise transfers and walking time
calculate_comfort_score <- function(routes_metrics) {
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
}

calculate_comfort_score(routes_metrics)

#Robustness
calculate_robustness_score <- function(route_sequences, routes) {
  # Initialize base score (100 = perfectly robust)
  base_score <- 100
  score <- base_score
  
  # Extract transport nodes (exclude Origin/Destination)
  transport_nodes <- lapply(route_sequences, function(x) {
    x[!x %in% c("Origin", "Destination")] 
  })
  
  # Single Point of Failure Penalty (SPFP)
  common_nodes <- Reduce(intersect, transport_nodes)
  N <- length(common_nodes)
  spof_penalty <- length(common_nodes) * 15  # Single Point of Failure penalty
  score <- score - spof_penalty
  
  # Mode Diversity Analysis
  modes <- lapply(routes$legs, function(legs) {
    unique(legs$mode[legs$mode != "WALK"])  # Exclude "WALK" mode
  })
  
  #Hybrid Bonus (+20 if any route uses both bus and rail)
  has_hybrid <- any(sapply(modes, function(m) "BUS" %in% m && any(c("SUBWAY", "RAIL") %in% m)))
  if (has_hybrid) {
    score <- score + 20  # Hybrid bonus for bus + rail
  }
  
  #Single-Mode Penalty (-25 if all routes use the same mode type)
  all_modes <- unique(unlist(modes))
  if (length(all_modes) == 1) {
    score <- score - 25  # Single-mode penalty
  }
  
  #Reward for Routes Being Completely Different
  # Check if any two routes are completely different
  route_combinations <- combn(length(transport_nodes), 2)
  completely_different_reward <- 0
  
  for (i in 1:ncol(route_combinations)) {
    route1 <- transport_nodes[[route_combinations[1, i]]]
    route2 <- transport_nodes[[route_combinations[2, i]]]
    
    # If routes have no common nodes, they are considered "completely different"
    if (length(intersect(route1, route2)) == 0) {
      completely_different_reward <- completely_different_reward + 10  # Reward for complete difference
    }
  }
  
  # Apply reward for complete differences
  min_score <- 0
  max_score <- 150 # the system's highest possible score
  
  # Normalize the score between 0 and 100
  normalized_score <- (score - min_score) / (max_score - min_score) * 100
  
  # Return the final score
  return(normalized_score)
}



#SERVICE QUALITY - using bus frequency
calculate_route_options_service_quality <- function(routes, departure_time, analyzer) {
  # Calculate scores for all available routes (up to 3)
  route_scores <- sapply(1:length(routes$legs), function(i) {
    # Extract bus numbers for this route
    bus_numbers <- na.omit(routes$legs[[i]]$route[nzchar(routes$legs[[i]]$route) & 
                                                    routes$legs[[i]]$mode == "BUS"])
    
    if (length(bus_numbers) == 0) return(0)  # No buses in this route
    
    # Get frequency data
    freq_data <- lapply(bus_numbers, function(bus_no) analyzer$get_service_frequency(bus_no))
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
  
  # Your exact requested weighting
  final_score <- 0.6 * sorted_scores[1] +  # Best route
    0.3 * sorted_scores[2] +  # Second-best
    0.1 * sorted_scores[min(3, length(sorted_scores))]  # Worst
  
  max(0, min(100, round(final_score)))
}

