library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)

BusFrequencyAnalyzer <- setRefClass(
  "BusFrequencyAnalyzer",
  fields = list(
    api_key = "character",
    base_url = "character"
  ),
  methods = list(
    initialize = function(api_key) {
      .self$api_key <- api_key
      .self$base_url <- "https://datamall2.mytransport.sg/ltaodataservice/"
    },
    
    get_all_bus_services = function() {
      url <- paste0(.self$base_url, "BusServices")
      response <- GET(url, add_headers(AccountKey = .self$api_key))
      content <- content(response, "text", encoding = "UTF-8")
      fromJSON(content)$value
    },
    
    parse_frequency = function(freq_str) {
      if (is.null(freq_str) || freq_str == "" || !grepl("-", freq_str)) {
        return(list(min = NA, max = NA, avg = NA))
      }
      
      parts <- as.numeric(strsplit(freq_str, "-")[[1]])
      if (length(parts) != 2 || any(is.na(parts))) {
        return(list(min = NA, max = NA, avg = NA))
      }
      
      list(min = parts[1], max = parts[2], avg = mean(parts))
    },
    
    get_service_frequency = function(service_no) {
      all_services <- .self$get_all_bus_services()
      service <- all_services %>% 
        filter(ServiceNo == service_no) %>%
        slice(1)  # Take first direction if multiple
      
      if (nrow(service) == 0) return(NULL)
      
      list(
        AM_Peak = .self$parse_frequency(service$AM_Peak_Freq),
        AM_Offpeak = .self$parse_frequency(service$AM_Offpeak_Freq),
        PM_Peak = .self$parse_frequency(service$PM_Peak_Freq),
        PM_Offpeak = .self$parse_frequency(service$PM_Offpeak_Freq),
        ServiceNo = service_no
      )
    },
    
    get_bus_route_details = function(service_no, direction = 1) {
      url <- paste0(.self$base_url, "BusRoutes")
      all_records <- list()
      offset <- 0
      
      repeat {
        response <- GET(url,
                        add_headers(AccountKey = .self$api_key),
                        query = list(
                          "$filter" = paste0("ServiceNo eq '", service_no, "' and Direction eq ", direction),
                          "$skip" = offset
                        ))
        
        content <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(content)$value
        
        if (length(data) == 0) break
        
        all_records <- append(all_records, list(data))
        offset <- offset + 500
      }
      
      if (length(all_records) == 0) return(NULL)
      bind_rows(all_records)
    },
    
    verify_bus_at_stop = function(bus_no, stop_code) {
      for (dir in 1:2) {
        route <- .self$get_bus_route_details(bus_no, dir)
        if (!is.null(route) && stop_code %in% route$BusStopCode) {
          return(list(
            found = TRUE,
            direction = dir,
            sequence = which(route$BusStopCode == stop_code)[1],
            route_data = route
          ))
        }
      }
      return(list(found = FALSE))
    },
    
    combine_period = function(freq_data, period) {
      periods <- lapply(freq_data, function(x) x[[period]])
      valid <- periods[!sapply(periods, function(x) any(is.na(x$avg)))]
      
      if (length(valid) == 0) return(list(avg = NA, min = NA, max = NA))
      
      freqs <- lapply(valid, function(x) {
        list(
          avg = 1/x$avg,
          min = 1/x$max,
          max = 1/x$min
        )
      })
      
      combined_avg <- sum(sapply(freqs, function(x) x$avg))
      combined_min <- sum(sapply(freqs, function(x) x$min))
      combined_max <- sum(sapply(freqs, function(x) x$max))
      
      list(
        avg = 1/combined_avg,
        min = 1/combined_max,
        max = 1/combined_min,
        buses_per_hour = combined_avg * 60
      )
    },
    
    calculate_combined_frequency_at_stop = function(bus_numbers, stop_code) {
      valid_services <- list()
      validation_results <- list()
      
      for (bus_no in bus_numbers) {
        verification <- .self$verify_bus_at_stop(bus_no, stop_code)
        validation_results[[bus_no]] <- verification
        
        if (verification$found) {
          freq_data <- .self$get_service_frequency(bus_no)
          if (!is.null(freq_data)) {
            freq_data$direction <- verification$direction
            valid_services[[length(valid_services) + 1]] <- freq_data
          }
        }
      }
      
      if (length(valid_services) == 0) {
        cat("\nDiagnostics for stop", stop_code, ":\n")
        for (bus in names(validation_results)) {
          vr <- validation_results[[bus]]
          cat(bus, ":", ifelse(vr$found, 
                               paste("Found in direction", vr$direction),
                               "NOT found"), "\n")
        }
        return(NULL)
      }
      
      combined <- list(
        AM_Peak = .self$combine_period(valid_services, "AM_Peak"),
        AM_Offpeak = .self$combine_period(valid_services, "AM_Offpeak"),
        PM_Peak = .self$combine_period(valid_services, "PM_Peak"),
        PM_Offpeak = .self$combine_period(valid_services, "PM_Offpeak"),
        valid_buses = bus_numbers,
        stop_code = stop_code,
        directions = sapply(valid_services, function(x) x$direction)
      )
      
      return(combined)
    },
    
    print_stop_frequency = function(combined) {
      cat("\n=== Combined Frequency at Stop:", combined$stop_code, "===\n")
      cat("Buses:", paste(combined$valid_buses, collapse = ", "), "\n")
      
      periods <- list(
        AM_Peak = "AM Peak (6:30-8:30)",
        AM_Offpeak = "AM Offpeak (8:31-16:59)",
        PM_Peak = "PM Peak (17:00-19:00)",
        PM_Offpeak = "PM Offpeak (after 19:00)"
      )
      
      for (period in names(periods)) {
        pdata <- combined[[period]]
        if (!is.na(pdata$avg)) {
          cat(periods[[period]], ":\n")
          cat("  Average wait time:", round(pdata$avg, 1), "minutes\n")
          cat("  Wait time range:", round(pdata$min, 1), "-", round(pdata$max, 1), "minutes\n")
          cat("  Effective buses/hour:", round(pdata$buses_per_hour, 1), "\n")
          
          lambda <- 1/pdata$avg
          wait_times <- c(2, 5, 10)
          probs <- 1 - exp(-lambda * wait_times)
          cat("  Probability of bus arriving within:\n")
          for (i in seq_along(wait_times)) {
            cat(sprintf("    %d minutes: %.1f%%\n", wait_times[i], probs[i]*100))
          }
          cat("\n")
        }
      }
    },
    
    get_all_bus_stops = function() {
      url <- paste0(.self$base_url, "BusStops")
      response <- GET(url, add_headers(AccountKey = .self$api_key))
      content <- content(response, "text", encoding = "UTF-8")
      fromJSON(content)$value
    }
  )
)

#usage
bus_analyzer <- BusFrequencyAnalyzer$new(api_key = "o6OuJxI3Re+qYgFQzb+4+w==")

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
                               analyzer = bus_analyzer,
                               
                               initialize = function(email, password, analyzer = NULL) {
                                 self$email <- email
                                 self$password <- password
                                 self$token <- private$get_auth_token()
                                 self$analyzer <- analyzer
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
                               
                              
                               calculate_route_options_service_quality = function(routes, time_period) {
                                 if (is.null(self$analyzer)) {
                                   stop("Bus frequency analyzer not provided")
                                 }
                                 
                                 # Map your time periods to the analyzer's time periods
                                 analyzer_period <- switch(time_period,
                                                           "Morning Peak (6:30-8:30am)" = "AM_Peak",
                                                           "Evening Peak (5-7pm)" = "PM_Peak",
                                                           "Daytime Off-Peak (8:30am-5pm)" = "AM_Offpeak",
                                                           "Nighttime Off-Peak (7pm-6:30am)" = "PM_Offpeak",
                                                          NA  # default if no match
                                 )
                                   
                                 # Calculate scores for all available routes (up to 3)
                                 route_scores <- sapply(1:length(routes$legs), function(i) {
                                   # Extract bus numbers and modes for this route
                                   bus_numbers <- na.omit(routes$legs[[i]]$route[nzchar(routes$legs[[i]]$route) & 
                                                                                   routes$legs[[i]]$mode == "BUS"])
                                   has_subway <- "SUBWAY" %in% routes$legs[[i]]$mode
                                   
                                   # Calculate MRT score if route includes subway
                                   mrt_score <- if (has_subway) {
                                     if (analyzer_period %in% c("AM_Peak", "PM_Peak")) {
                                       20  # 5-7 minutes during peak
                                     } else {
                                       80  # 2-3 minutes during off-peak
                                     }
                                   } else {
                                     0  # No MRT component
                                   }
                                   
                                   # Calculate bus score if route includes buses
                                   bus_score <- if (length(bus_numbers) > 0) {
                                     # Get frequency data
                                     freq_data <- lapply(bus_numbers, function(bus_no) self$analyzer$get_service_frequency(bus_no))
                                     freq_data <- freq_data[!sapply(freq_data, is.null)]
                                     if (length(freq_data) == 0) return(0)
                                     
                                     # Calculate combined wait time for buses
                                     avg_waits <- sapply(freq_data, function(x) x[[analyzer_period]]$avg)
                                     avg_waits <- avg_waits[!is.na(avg_waits)]
                                     if (length(avg_waits) == 0) return(0)
                                     
                                     combined_wait <- 1 / mean(1 / avg_waits)
                                     
                                     # Convert wait time to score
                                     case_when(
                                       combined_wait <= 5  ~ 90,
                                       combined_wait <= 8  ~ 75,
                                       combined_wait <= 12 ~ 60,
                                       TRUE               ~ 40
                                     )
                                   } else {
                                     0  # No bus component
                                   }
                                   
                                   # Combine MRT and bus scores (weighted by presence)
                                   if (has_subway && length(bus_numbers) > 0) {
                                     mean(c(mrt_score, bus_score))
                                   } else if (has_subway) {
                                     mrt_score
                                   } else if (length(bus_numbers) > 0) {
                                     bus_score
                                   } else {
                                     0  # No public transport in this route
                                   }
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
                                     service = if (!is.null(self$analyzer)) {
                                       self$calculate_route_options_service_quality(routes, time_period)
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
                              
                              
                               
                               
                               
                               calculate_multiple_rqs = function(starts, end, date, time_period, maxWalkDistance = 1000,
                                                                 weights = c(transport = 0.25, comfort = 0.25, 
                                                                             robustness = 0.25, service = 0.25)) {
                                 
                                 # Process all start points
                                 start_coords <- starts
                                 end_coord <- self$convert_to_coords(end)
                                 
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
  starts = c("1.2917806,103.81910559", "1.2930653,103.81608701", "1.3825971,103.7740129", 
             "1.3169289,103.76270196", "1.4558945,103.81857284", 
             "1.3411162,103.95980571","1.3392719,103.84212148", "1.4442599,103.78469873"),
  end = "118420",
  date = "03-24-2025",
  time_period = "Morning Peak (6:30-8:30am)"  # Using time period instead of specific time
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



library(readr)
BTO_projects <- read_csv("../data/BTO_projects.csv")





















get_routes_to_closest_mall <- function(postal_code, date, time_period, 
                                       routeType = "pt", mode = "TRANSIT", 
                                       maxWalkDistance = 1000, numItineraries = 3) {
  
  # 1. Find the closest mall
  closest_mall <- find_closest_mall(postal_code)
  
  if (is.null(closest_mall)) {
    stop("Failed to find closest mall")
  }
  
  # 2. Prepare coordinates for routing
  start_coord <- paste(closest_mall$search_coordinate, collapse = ",")
  end_coord <- paste(closest_mall$mall_coordinate, collapse = ",")
  
  # 3. Get route data
  routes <- route_analyzer$get_route_data(
    start = start_coord,
    end = end_coord,
    routeType = routeType,
    date = date,
    time_period = time_period,
    mode = mode,
    maxWalkDistance = maxWalkDistance,
    numItineraries = numItineraries
  )
  
  if (is.null(routes)) {
    stop("Failed to get route data")
  }
  
  
  return(routes)
}


result <- get_routes_to_closest_mall(
  postal_code = "520702",
  date = "2023-11-15",
  time_period = "Morning Peak (6:30-8:30am)",
  maxWalkDistance = 500)

start_coord <- paste(closest_mall$search_coordinate, collapse = ",")
end_coord <- paste(closest_mall$mall_coordinate, collapse = ",")

route_analyzer$get_route_data(
  start = start_coord,
  end = end_coord,
  routeType = "pt",
  date = date,
  time_period = "Morning Peak (6:30-8:30am)",
  mode = mode,
  maxWalkDistance = 1000,
  numItineraries = numItineraries
)











