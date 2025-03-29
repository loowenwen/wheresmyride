library(igraph)
library(dplyr)

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


#API that will help me to dynamically draw the edges:
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
start = "1.355948,103.9372" # in WGS84 latitude, longitude format  (my house bus stop)
end = "1.294178,103.7698" # in WGS84 latitude, longitude format (kent ridge terminal)
routeType = "pt" # route types available: walk, drive, pt, and cycle
date = "03-29-2025" # date of the selected start point in MM-DD-YYYY
time = "21:50:00" # time of the selected start point in [HH][MM][SS], using the 24-hour clock system
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




#Dynamically drawing routes and edges

api_response <- itineraries_list$itinerary_14  #list of 2 = 2 itineraries


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


route_sequences <- generate_all_route_sequences(api_response)


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

# Fixed routes
route_sequences <- fix_mrt_sequences(route_sequences)




#setting up the dataframe which will help to create the nodes
mrt_nodes <- mrt_station_codes_locations %>%
  select(
    id = stn_code,
    name = station_name,
    longitude,
    latitude,
    mrt_line = mrt_line_english
  ) %>%
  mutate(
    node_type = "train")


bus_nodes <- bus_stops_final %>%
  select(
    id = bus_stop_number,
    name = bus_stop_name,
    longitude,
    latitude
  ) %>%
  mutate(
    node_type = "bus",
    id = as.character(id))

transport_nodes <- bind_rows(
  mrt_nodes,
  bus_nodes
) 
  
transport_nodes <- transport_nodes %>%
  distinct(id, .keep_all = TRUE)


# Combine with existing dataset
transport_nodes <- rbind(transport_nodes, placeholder_nodes)


##CREATE NODES

##also handles missing nodes in transport nodes by using placeholder values to not exclude them from dataset
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
    estimated_lat <- mean(known_nodes$lat, na.rm = TRUE)
    estimated_lon <- mean(known_nodes$lon, na.rm = TRUE)
    
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


##CREATE EDGES: for now i added dummy time weights and transfer penalties
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
      
      # Dummy time weights (in minutes)
      time_weight = case_when(
        segment_type == "bus_route" ~ 2.5 * runif(n(), 0.7, 1.3),
        segment_type == "mrt_line" ~ 1.8 * runif(n(), 0.7, 1.3),
        segment_type == "mode_transfer" ~ 4.0 * runif(n(), 0.7, 1.3)
      ),
      
      # Transfer penalties
      transfer_penalty = case_when(
        segment_type == "mode_transfer" & from_type == "bus" ~ 3.0 * runif(n(), 0.8, 1.2),
        segment_type == "mode_transfer" & from_type == "train" ~ 2.5 * runif(n(), 0.8, 1.2),
        TRUE ~ 0
      ),
      
      # Composite weight
      composite_weight = time_weight + transfer_penalty,
      
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

route_edges <- create_route_edges(route_nodes)


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


# Usage:
transport_graph <- create_combined_transport_graph(route_sequences, transport_nodes)

# Access the components:
combined_graph <- transport_graph$graph
all_nodes <- transport_graph$nodes
all_edges <- transport_graph$edges



### Function to calculate PTA score for a route
calculate_pta_score <- function(route_graph, route_edges) {
  # Calculate travel time components
  total_travel_time <- sum(route_edges$time_weight)
  
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

# Helper function for frequency score
calculate_frequency_score <- function(route_edges) {
  # This should use actual frequency data from your API
  route_edges %>%
    mutate(
      wait_time = case_when(
        segment_type == "bus_route" ~ 5,  # Example: average bus wait time
        segment_type == "mrt_line" ~ 3,   # Example: average MRT wait time
        TRUE ~ 0
      ),
      freq_score = pmax(10 - wait_time, 0)
    ) %>%
    pull(freq_score) %>%
    mean()
}













































##SOME PLOTTING TO SEE (not very refined yet but just to check)

library("igraph")
# Plot the combined graph
plot(transport_graph$graph, 
     main = "Combined Transport Network", 
     vertex.size = 5,   # Control the size of the nodes
     vertex.label.cex = 0.7,  # Control the size of the labels
     vertex.color = "skyblue", # Node color
     edge.arrow.size = 0.5, # Control arrow size for directed edges
     edge.width = 1,  # Control edge width
     layout = layout_with_fr)  # Force-directed layout for better aesthetics



# Adjust node size based on degree (number of connections)
node_degree <- degree(transport_graph$graph)
plot(transport_graph$graph,
     main = "Combined Transport Network",
     vertex.size = node_degree * 2,  # Larger nodes for higher degree
     vertex.label.cex = 0.7,
     vertex.color = "lightgreen",
     edge.arrow.size = 0.5,
     edge.width = 1,
     layout = layout_with_fr)



















##CREATE ROUTE SEQUENCE VECTOR (VECTOR WITH BUS STOP CODES AND MRT STOP CODES)
mode <- as.list(api_response[[1]]$mode) #list of the modes of transport involved 
route <- as.list(api_response[[1]]$route[api_response[[1]]$route != ""]) #list of all the buses and train lines that are involved
from_to_stops <- api_response[[1]]$from$stopCode[!is.na(api_response[[1]]$from$stopCode)] #every two element is the first and last transit stop of one leg of the route

generate_route_sequence <- function(api_response, from_to_stops, mode) {
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
    
    # Add intermediates for even-indexed stops (2nd, 4th, 6th)
    if (current_mode != "WALK" && intermediate_index %% 2 == 0) {
      if (length(api_response[[1]]$intermediateStops) >= intermediate_index) {
        intermediates <- api_response[[1]]$intermediateStops[[intermediate_index]]$stopCode
        if (!is.null(intermediates) && length(intermediates) > 0) {
          route_sequence <- c(route_sequence, intermediates)
        }
      }
    }
    
    # Add ending point of this leg if it's not the last transportation segment
    if (current_mode != "WALK" && i < length(mode) - 1 && from_to_index <= length(from_to_stops)) {
      route_sequence <- c(route_sequence, from_to_stops[from_to_index])
      from_to_index <- from_to_index + 1
    }
    
    intermediate_index <- intermediate_index + 1
  }
  
  # Add final destination
  route_sequence <- c(route_sequence, "Destination")
  
  return(route_sequence)
}




##my manual interpretation of generating the route sequence vector
route_seq1 <- c(
  "Origin",
  from_to_stops[1], # First bus stop
  api_response[[1]]$intermediateStops[[2]]$stopCode,#intermediate bus stop codes of first bus 
  from_to_stops[2], # Last bus stop of first bus 
  from_to_stops[3], # First MRT stop
  api_response[[1]]$intermediateStops[[4]]$stopCode, # MRT intermediates of first mrt line 
  from_to_stops[4], #last mrt stop
  from_to_stops[5], #first bus stop
  api_response[[1]]$intermediateStops[[6]]$stopCode,# intermediates of bus stop codes of second bus
  from_to_stops[6],
  "Destination"
)
