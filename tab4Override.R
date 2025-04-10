source("predict_accessibility.R", local = TRUE)

# Access the upcoming_bto data frame that contains lat/lng
get_coords_from_bto <- function(bto_label) {
  match_row <- upcoming_bto[upcoming_bto$label == bto_label, ]
  if (nrow(match_row) == 1) {
    lat <- match_row$lat
    lng <- match_row$lng
    if (!is.na(lat) && !is.na(lng)) return(c(lat, lng))
  }
  return(NULL)
}

observeEvent(input$t4_get_score, {
  tryCatch({
    print("DEBUG: t4_get_score triggered")
    
    # Dynamically infer which input type was used
    if (!is.null(input$t4_postal_code) && input$t4_postal_code != "") {
      loc_type <- "postal"
      location_input <- input$t4_postal_code
    } else if (!is.null(input$t4_bto_project) && input$t4_bto_project != "") {
      loc_type <- "bto"
      location_input <- tryCatch({
        get_coords_from_bto(input$t4_bto_project)
      }, error = function(e) NULL)
    } else {
      loc_type <- "unknown"
      location_input <- NULL
    }
    
    print(paste("DEBUG: parsed BTO coords:", paste(location_input, collapse = ", ")))
    print(paste("DEBUG: loc_type resolved as", loc_type))
    print(paste("DEBUG: location_input is", location_input))
    
    if (is.null(location_input) || (is.character(location_input) && location_input == "")) {
      showNotification("Please provide a location by entering a postal code or selecting a BTO project.", type = "error")
      return(NULL)
    }
    
    print("Calling predict_accessibility()...")
    
    result <- predict_accessibility(
      location_input = location_input,  # ✅ FIXED!
      weight_mrt = input$t4_mrt,
      weight_bus = input$t4_bus,
      weight_walk = input$t4_walk,
      weight_congestion = input$t4_congestion,
      selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak"),
      distance = 500
    )
    
    print("predict_accessibility() succeeded")
    
    breakdown <- result$score_breakdown
    
    accessibility_scores$overall_score <- round(breakdown$total_score, 1)
    accessibility_scores$mrt_score <- round(breakdown$score_mrt, 1)
    accessibility_scores$bus_score <- round(breakdown$score_bus, 1)
    accessibility_scores$walk_score <- round(breakdown$walkability_score, 1)
    accessibility_scores$congestion_score <- round(breakdown$congestion_score, 1)
    
    accessibility_scores$travel_times <- data.frame(
      Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
      `Estimated Travel Time (min)` = sample(10:60, 6)
    )
    
    colnames(accessibility_scores$travel_times)[2] <- "Estimated Travel Time (min)"
    
    mrt_df <- result$features$mrt_stop_distances %>%
      rename(Description = mrt_station, `Distance (m)` = dist) %>%
      mutate(Type = "MRT") %>%
      select(Type, Description, `Distance (m)`) %>%
      filter(`Distance (m)` <= 500)
    
    bus_df <- result$features$bus_stop_distances %>%
      rename(`Distance (m)` = dist) %>%
      mutate(Type = "Bus") %>%
      select(Type, Description, `Distance (m)`) %>%
      filter(`Distance (m)` <= 500)
    
    accessibility_scores$nearby_stops <- bind_rows(mrt_df, bus_df)
    
    print("Accessibility scoring completed successfully.")
    
  }, error = function(e) {
    showNotification("Failed to compute accessibility score.", type = "error")
    print("ERROR OCCURRED:")
    print(e)
  })
})

observeEvent(input$t4_recalculate, {
  tryCatch({
    print("DEBUG: t4_recalculate triggered")
    
    # Get updated weights and radius from input
    print(paste("Using updated weights and radius:", input$t4_nearby_radius))
    
    # Handle empty time slot fallback
    time_slots <- input$t4_travel_time_preference
    if (length(time_slots) == 0) {
      time_slots <- c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak")
      print("No time slots selected, using default all-day slots.")
    }
    print(paste("Selected time slots:", paste(time_slots, collapse = ", ")))
    
    # Reuse previously selected location
    if (!is.null(input$t4_postal_code) && input$t4_postal_code != "") {
      location_input <- input$t4_postal_code
    } else if (!is.null(input$t4_bto_project) && input$t4_bto_project != "") {
      location_input <- get_coords_from_bto(input$t4_bto_project)
    } else {
      showNotification("Please provide a location by entering a postal code or selecting a BTO project.", type = "error")
      return(NULL)
    }
    
    # Call accessibility function with updated weights and radius
    result <- predict_accessibility(
      location_input = location_input,
      weight_mrt = input$t4_mrt,
      weight_bus = input$t4_bus,
      weight_walk = input$t4_walk,
      weight_congestion = input$t4_congestion,
      selected_time_slots = time_slots,
      distance = as.numeric(input$t4_nearby_radius)
    )
    
    breakdown <- result$score_breakdown
    
    accessibility_scores$overall_score <- round(breakdown$total_score, 1)
    accessibility_scores$mrt_score <- round(breakdown$score_mrt, 1)
    accessibility_scores$bus_score <- round(breakdown$score_bus, 1)
    accessibility_scores$walk_score <- round(breakdown$walkability_score, 1)
    accessibility_scores$congestion_score <- round(breakdown$congestion_score, 1)
    
    accessibility_scores$travel_times <- data.frame(
      Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
      `Estimated Travel Time (min)` = sample(10:60, 6)
    )
    
    mrt_df <- result$features$mrt_stop_distances %>%
      rename(Description = mrt_station, `Distance (m)` = dist) %>%
      mutate(Type = "MRT") %>%
      select(Type, Description, `Distance (m)`) %>%
      filter(`Distance (m)` <= 500)
    
    bus_df <- result$features$bus_stop_distances %>%
      rename(`Distance (m)` = dist) %>%
      mutate(Type = "Bus") %>%
      select(Type, Description, `Distance (m)`) %>%
      filter(`Distance (m)` <= 500)
    
    accessibility_scores$nearby_stops <- bind_rows(mrt_df, bus_df)
    
    print(" Recalculation completed.")
    
  }, error = function(e) {
    showNotification("Failed to recalculate accessibility score.", type = "error")
    print(" ERROR OCCURRED DURING RECALCULATION:")
    print(e)
  })
})

