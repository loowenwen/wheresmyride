create_travel_time_dataset <- function(route_analyzer, start_point, destination_points, 
                                       date, time, maxWalkDistance = 1000) {
  
  # Initialize results dataframe
  results <- data.frame(
    Destination = character(),
    AverageTravelTime = numeric(),
    Status = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each destination
  for (dest in destination_points) {
    tryCatch({
      cat("Processing destination:", dest, "\n") # Debug output
      
      # Get route data
      routes <- route_analyzer$get_route_data(
        start = start_point,
        end = dest,
        date = date,
        time = time,
        maxWalkDistance = maxWalkDistance
      )
      
      # Check if routes were returned
      if (is.null(routes) || !"duration" %in% names(routes)) {
        stop("No valid route data returned")
      }
      
      # Calculate average travel time
      avg_time <- mean(routes$duration, na.rm = TRUE)
      
      # Add to results
      results <- rbind(results, data.frame(
        Destination = dest,
        AverageTravelTime = avg_time,
        Status = "Success"
      ))
      
    }, error = function(e) {
      warning(paste("Error processing destination", dest, ":", e$message))
      results <- rbind(results, data.frame(
        Destination = dest,
        AverageTravelTime = NA,
        Status = paste("Error:", e$message)
      ))
    })
  }
  
  # Sort by average travel time
  results <- results[order(results$AverageTravelTime, na.last = TRUE), ]
  row.names(results) <- NULL
  
  return(results)
}

#destinations
destinations <- c("118420")


# Create dataset
travel_times <- create_travel_time_dataset(
  route_analyzer = route_analyzer,
  start_point = "520702",  # Your starting location
  destination_points = sapply(destinations, get_coordinates_from_postal),
  date = "06-15-2023",
  time = "08:30:00"  # Morning peak hour
)

