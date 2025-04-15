build_travel_time_df <- function(start_postalcode, 
                                 time_period,
                                 route_analyzer_instance,
                                 date,
                                 maxWalkDistance = 1000,
                                 names = c("Raffles Place", "One North", "Orchard Gateway", "Jurong East", "Changi Airport Terminal", "Singapore General Hospital"),
                                 location_coords = c("1.28433,103.85138", "1.29939,103.78728", "1.30108,103.83899", "1.33310,103.74253", "1.36132,103.99016", "1.28036,103.83474")) {
  

  start_coord <- get_coordinates_from_postal(start_postalcode)
  travel_times <- numeric(length(location_coords))
  
  #loop through location coordinate
  for (j in seq_along(location_coords)) {
    end_coord <- location_coords[j] 
    
    # Get route data
    routes <- route_analyzer_instance$get_route_data(
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

                     





