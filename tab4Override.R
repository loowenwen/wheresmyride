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
      showNotification("Invalid or missing location input.", type = "error")
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
    
    mrt_df <- result$features$mrt_stop_distances %>%
      rename(Description = mrt_station, `Distance (m)` = dist) %>%
      mutate(Type = "MRT") %>%
      select(Type, Description, `Distance (m)`)
    
    bus_df <- result$features$bus_stop_distances %>%
      rename(`Distance (m)` = dist) %>%
      mutate(Type = "Bus") %>%
      select(Type, Description, `Distance (m)`)
    
    accessibility_scores$nearby_stops <- bind_rows(mrt_df, bus_df)
    
    print("Accessibility scoring completed successfully.")
    
  }, error = function(e) {
    showNotification("Failed to compute accessibility score.", type = "error")
    print("ERROR OCCURRED:")
    print(e)
  })
})
