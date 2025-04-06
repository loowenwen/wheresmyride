library(igraph)
library(dplyr)
library(httr)
library(jsonlite)
library(igraph)
library(sf)

# 1. AUTHENTICATION FUNCTION ----
get_auth_token <- function(email, password) {
  auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
  response <- POST(
    url = auth_url,
    body = list(email = email, password = password),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    content(response)$access_token
  } else {
    stop(paste("Authentication failed:", status_code(response)))
  }
}

token <- get_auth_token("loowenwen1314@gmail.com", "sochex-6jobge-fomsYb")


#2. PREPARE TRANSPORT NODES
prepare_transport_nodes <- function(mrt_data, bus_data) {
  bind_rows(
    mrt_data %>% transmute(
      id = stn_code,
      name = mrt_station,
      longitude = centroid_lon,
      latitude = centroid_lat,
      node_type = "train",
      line = mrt_line
    ),
    bus_data %>% transmute(
      id = as.character(BusStopCode),
      name = Description,
      longitude = Longitude,
      latitude = Latitude,
      node_type = "bus",
      line = NA_character_
    )
  ) %>%
    distinct(id, .keep_all = TRUE) %>%
    st_drop_geometry() %>%
    select(-geometry)
}

transport_nodes <- prepare_transport_nodes(mrt_lrt, bus_stops)



# 3. GENERATING THE ROUTE SEQUENCES
get_route_data <- function(start, end, routeType = "pt", date, time, mode = "TRANSIT", maxWalkDistance, numItineraries = 3, authToken) {
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
    add_headers(Authorization = authToken)
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
}

##generate all route sequences function - since our api can generate up to 3 itineraries 
generate_all_route_sequences <- function(api_response) {
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
      
      # Check for valid intermediate stops (handles NULL and empty lists by checking next ones)
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
}

##need to fix the route sequences/check if its correct
fix_mrt_sequences <- function(route_sequences) {
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
}


#usage: input any start or end coordinates
route <- get_route_data(
  start = "1.30374,103.83214", #orchard
  end = "1.294178,103.7698", #kent ridge terminal 
  date = "03-24-2025",
  time = "07:35:00",
  maxWalkDistance = 1000,
  authToken = token
)
route_sequences <- fix_mrt_sequences(generate_all_route_sequences(route$legs))



##GRAPH CONSTRUCTION 

#Node:##also handles missing nodes in transport nodes by using placeholder values to not exclude them from dataset
get_route_nodes <- function(route_sequence, transport_nodes) {
  # Ensure IDs are character type and remove leading/trailing spaces
  transport_nodes <- transport_nodes %>%
    mutate(id = trimws(as.character(id)))
  
  route_sequence <- trimws(as.character(route_sequence))
  
  # Identify missing nodes
  missing_ids <- setdiff(route_sequence, transport_nodes$id)
  
  if (length(missing_ids) > 0) {
    # Get known nodes from the same route
    known_nodes <- transport_nodes %>% filter(id %in% route_sequence)
    
    # Estimate missing nodes' locations using the mean of known nodes
    estimated_lat <- mean(known_nodes$latitude, na.rm = TRUE)
    estimated_lon <- mean(known_nodes$longitude, na.rm = TRUE)
    
    # Create placeholder nodes
    placeholder_nodes <- data.frame(
      id = missing_ids,
      latitude = estimated_lat,
      longitude = estimated_lon
    )
    
    # Append to transport_nodes
    transport_nodes <- bind_rows(transport_nodes, placeholder_nodes)
  }
  
  # Assign route_position (ensuring order is preserved)
  route_nodes <- transport_nodes %>%
    filter(id %in% route_sequence) %>%
    mutate(route_position = match(id, route_sequence)) %>%
    arrange(route_position)  # Ensure ordering
  
  return(route_nodes)
}


#edges: for now i added dummy time weights and transfer penalties because I haven't figured out where time weights and transfer penalities come from the api_response
create_route_edges <- function(route_nodes) {
  # Ensure route_nodes is ordered correctly
  route_nodes <- route_nodes %>% arrange(route_position)
  
  # Create edges with all necessary attributes
  route_edges <- data.frame(
    from = route_nodes$id[-nrow(route_nodes)],
    to = route_nodes$id[-1],
    from_type = route_nodes$node_type[-nrow(route_nodes)],
    to_type = route_nodes$node_type[-1],
    from_position = route_nodes$route_position[-nrow(route_nodes)],
    to_position = route_nodes$route_position[-1],
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Segment classification
      segment_type = case_when(
        from_type == "bus" & to_type == "bus" ~ "bus_route",
        from_type == "train" & to_type == "train" ~ "mrt_line",
        TRUE ~ "mode_transfer"
      ),
      
      # Transfer penalties
      transfer_penalty = case_when(
        segment_type == "mode_transfer" & from_type == "bus" ~ 3.0 * runif(n(), 0.8, 1.2),
        segment_type == "mode_transfer" & from_type == "train" ~ 2.5 * runif(n(), 0.8, 1.2),
        TRUE ~ 0
      ),
      
      
      # Sequence number
      sequence = row_number()
    )
  
  # Calculate distances if coordinates exist
  if (all(c("longitude", "latitude") %in% names(route_nodes))) {
    route_edges$distance_m <- geosphere::distGeo(
      route_nodes[match(route_edges$from, route_nodes$id), c("longitude", "latitude")],
      route_nodes[match(route_edges$to, route_nodes$id), c("longitude", "latitude")]
    )
  }
  
  return(route_edges)
}



# Function to process all routes and create combined graph
create_combined_transport_graph <- function(route_sequences, transport_nodes) {
  # Initialize containers for all nodes and edges
  all_nodes <- data.frame()
  all_edges <- data.frame()
  route_counter <- 1
  
  # Process each route
  for (route_name in names(route_sequences)) {
    route_sequence <- route_sequences[[route_name]]
    
    # Get nodes for this route
    route_nodes <- get_route_nodes(route_sequence, transport_nodes) %>%
      mutate(route_id = route_counter,
             route_name = route_name)
    
    # Get edges for this route
    route_edges <- create_route_edges(route_nodes) %>%
      mutate(route_id = route_counter,
             route_name = route_name)
    
    # Combine with existing data
    all_nodes <- bind_rows(all_nodes, route_nodes)
    all_edges <- bind_rows(all_edges, route_edges)
    
    route_counter <- route_counter + 1
  }
  
  # Remove duplicate nodes (keeping first occurrence)
  unique_nodes <- all_nodes %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  
  # Create combined graph
  combined_graph <- graph_from_data_frame(
    d = all_edges,
    vertices = unique_nodes,
    directed = TRUE
  )
  
  # Add route information as graph attributes
  combined_graph <- set_graph_attr(combined_graph, "name", "combined_transport_network")
  combined_graph <- set_graph_attr(combined_graph, "num_routes", length(route_sequences))
  
  return(list(graph = combined_graph, 
              nodes = unique_nodes, 
              edges = all_edges))
}


extract_route_metrics <- function(itinerary) {
  tibble(
    duration = itinerary$duration / 60, # Convert to minutes
    walkTime = itinerary$walkTime / 60,
    transitTime = itinerary$transitTime / 60,
    waitingTime = itinerary$waitingTime / 60,
    transfers = itinerary$transfers
  )
}


##USAGE:
route_metrics <- extract_route_metrics(route) %>% mutate(route_name = names(route_sequences))

#gives a tibble of the information of each of the 3 routes (total duration, walkTime, transitTime, waitingTime, transfer)
transport_graph <- create_combined_transport_graph(route_sequences, transport_nodes) #puts it all into a graph



##Route Vulnerability Analysis



##Optimal Route Function based on user preferences
optimize_routes <- function(route_sequences, route_metrics, user_prefs) {
  # Create a route info dataframe
  route_info <- tibble(
    route_name = names(route_sequences),
    route_id = seq_along(route_sequences),
    num_stops = sapply(route_sequences, length),
    route_sequence = route_sequences
  ) %>%
    left_join(route_metrics, by = "route_name")
  
  # Normalize metrics (0-1 where 1 is best)
  metrics_to_normalize <- c("duration", "walkTime", "transfers", "waitingTime")
  
  normalized <- route_info %>%
    mutate(across(all_of(metrics_to_normalize), 
                  ~ 1 - (.x - min(.x)) / (max(.x) - min(.x)),
                  .names = "norm_{.col}"))
  
  # Calculate weighted score based on user preferences
  recommendations <- normalized %>%
    mutate(
      score = (user_prefs$time_weight * norm_duration) +
        (user_prefs$walk_weight * norm_walkTime) +
        (user_prefs$transfer_weight * norm_transfers) +
        (user_prefs$wait_weight * norm_waitingTime),
      
      # Convert to percentage and rank
      score_pct = round(score * 100),
      rank = dense_rank(desc(score))
    ) %>%
    arrange(rank) %>%
    select(route_name, rank, score_pct, duration, walkTime, transfers, waitingTime, route_sequence)
  
  # Calculate importance of each factor for explanation
  factor_importance <- tibble(
    factor = c("Trip Time", "Walking", "Transfers", "Waiting"),
    weight = c(user_prefs$time_weight, 
               user_prefs$walk_weight,
               user_prefs$transfer_weight,
               user_prefs$wait_weight),
    importance = round(weight / sum(weight) * 100)
  )
  
  return(list(
    recommendations = recommendations,
    factor_importance = factor_importance,
    optimal_route = recommendations$route_sequence[1]
  ))
}

# Example for Jane (real estate agent)
jane_prefs <- list(
  time_weight = 0.4,    # Total trip time importance
  walk_weight = 0.3,    # Walking time importance
  transfer_weight = 0.1, # Transfer aversion
  wait_weight = 0.2     # Waiting time aversion
)

results <- optimize_routes(
  route_sequences = route_sequences,
  route_metrics = route_metrics,
  user_prefs = jane_prefs
)

# View results
print(results$recommendations)

# See why factors were weighted
print(results$factor_importance)

# Get the optimal route sequence
print(results$optimal_route)



analyze_route_vulnerability <- function(transport_graph, route_sequences) {
  # Input validation
  if (!is.igraph(transport_graph$graph)) stop("Input must be an igraph object")
  if (!is.list(route_sequences)) stop("route_sequences must be a list")
  
  vulnerability_results <- purrr::map_dfr(names(route_sequences), function(route_id) {
    # Get all edges for this route
    route_edges <- E(transport_graph$graph)[E(transport_graph$graph)$route_name == route_id]
    
    # Skip if no edges found (invalid route)
    if (length(route_edges) == 0) return(NULL)
    
    # 1. Extract route nodes and subgraph
    route_nodes <- unique(as.vector(igraph::ends(transport_graph$graph, route_edges)))
    subgraph <- igraph::induced_subgraph(transport_graph$graph, route_nodes)
    
    # 2. Calculate centrality measures
    edge_betweenness <- igraph::edge_betweenness(subgraph, directed = TRUE)
    node_betweenness <- igraph::betweenness(subgraph)
    
    # 3. Find alternative paths
    route_start <- route_sequences[[route_id]][1]
    route_end <- route_sequences[[route_id]][length(route_sequences[[route_id]])]
    
    alt_paths <- tryCatch({
      igraph::all_simple_paths(
        transport_graph$graph,
        from = route_start,
        to = route_end,
        mode = "out"
      )
    }, error = function(e) list()) # Return empty list if path finding fails
    
    # 4. Calculate metrics
    tibble::tibble(
      route = route_id,
      critical_nodes = list(names(sort(node_betweenness, decreasing = TRUE)[1:3])),
      worst_edge = ifelse(length(edge_betweenness) > 0,
                          names(which.max(edge_betweenness)),
                          NA_character_),
      path_redundancy = max(0, length(alt_paths) - 1), # Ensure non-negative
      avg_centrality = ifelse(length(edge_betweenness) > 0,
                              mean(edge_betweenness),
                              NA_real_),
      vulnerability_score = ifelse(length(edge_betweenness) > 0,
                                   (max(edge_betweenness) * 0.6) + (0.4 * (1 / max(1, length(alt_paths)))),
                                   NA_real_)
    )
  })
  
  # Remove NULL results from invalid routes
  vulnerability_results <- dplyr::filter(vulnerability_results, !is.null(route))
  
  return(vulnerability_results)
}

vulnerability_results <- analyze_route_vulnerability(
  transport_graph = transport_graph,
  route_sequences = route_sequences
)

print(vulnerability_results)














#TRANSPORT MODE DIVERSITY INDEX:
calculate_mode_diversity <- function(graph_data, origin_node) {
# Get all edges from origin
edges <- incident_edges(graph_data$graph, origin_node, mode = "out")[[1]]

# Count unique transport modes
modes <- unique(E(graph_data$graph)$segment_type[edges])

# Calculate diversity score (normalized 0-1)
max_possible_modes <- length(unique(E(graph_data$graph)$segment_type))
diversity_score <- length(modes) / max_possible_modes

return(diversity_score)
}













### Function to calculate PTA score for a route 
calculate_pta_score <- function(route_graph, route_edges) {
  # Calculate travel time components
  total_travel_time <- 
  
  # Calculate transfer penalty
  transfer_penalty <- sum(route_edges$transfer_penalty)
  
  # Calculate frequency score (you'll need to get frequency data from API)
  frequency_score <- calculate_frequency_score(route_edges)
  
  # Weights (adjust based on your preferences)
  w1 <- 0.5  # Travel time weight
  w2 <- 0.3  # Transfer penalty weight
  w3 <- 0.2  # Frequency score weight
  
  # Calculate raw PTA score
  raw_score <- 100 - (w1 * total_travel_time) - (w2 * transfer_penalty) + (w3 * frequency_score)
  
  # Normalize to 0-100 range
  pta_score <- pmax(pmin(raw_score, 100), 0)
  
  return(pta_score)
}


  



























































