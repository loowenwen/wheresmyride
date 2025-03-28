library(shiny)
library(shinyjs)
library(leaflet)

shinyServer(function(input, output) {
  useShinyjs()
  
  # Dummy data for demonstration
  regions <- data.frame(
    Region = c("Central", "East", "North", "North-East", "West"),
    Bus_Stops = c(200, 150, 100, 120, 180),
    MRT_Stations = c(20, 15, 10, 12, 18),
    Population = c(100000, 80000, 60000, 70000, 90000),
    Accessibility_Score = c(85, 75, 65, 70, 80)  # Dummy accessibility scores
  )
  
  # Reactive expression for Location Search tab
  observeEvent(input$search_location, {
    output$location_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 15) %>%
        addMarkers(lng = c(103.8198, 103.822), lat = c(1.3521, 1.3500), 
                   popup = c("Bus Stop A", "MRT Station B"))  # Dummy markers
    })
    
    output$location_summary <- renderText({
      paste("Number of Bus Stops: 5\nNumber of MRT Stations: 2\nAverage Distance: 300m")
    })
  })
  
  # Reactive expression to calculate accessibility score
  accessibility_score <- reactive({
    region_data <- regions[regions$Region == input$region, ]
    bus_score <- if ("Bus" %in% input$transport_mode) region_data$Bus_Stops / 100 else 0
    mrt_score <- if ("MRT" %in% input$transport_mode) region_data$MRT_Stations / 10 else 0
    total_score <- bus_score + mrt_score
    return(total_score)
  })
  
  # Reactive expression to calculate travel time (dummy implementation)
  travel_time <- reactive({
    region_data <- regions[regions$Region == input$region, ]
    base_time <- if (input$time %in% 7:9 | input$time %in% 17:19) 30 else 20  # Peak vs off-peak
    adjusted_time <- base_time + (100 - region_data$Accessibility_Score) / 10
    return(adjusted_time)
  })
  
  # Output: Leaflet map for optimal route (dummy implementation)
  output$route_map <- renderLeaflet({
    if (input$calculate_route > 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 13) %>%
        addMarkers(lng = c(103.8198, 103.8500), lat = c(1.3521, 1.3000), 
                   popup = c("Home", "Workplace")) %>%
        addPolylines(lng = c(103.8198, 103.8300, 103.8500), 
                     lat = c(1.3521, 1.3200, 1.3000), color = "#18BC9C")
    }
  })
  
  # Output: Table of route steps (dummy implementation)
  output$route_steps_table <- renderTable({
    if (input$calculate_route > 0) {
      data.frame(
        Step = c("Start at Home", "Take Bus 123", "Transfer to MRT Line A", "Arrive at Destination"),
        Time = c("0 min", "10 min", "20 min", "30 min")
      )
    }
  })
  
  # Placeholder for map generation
  observeEvent(input$generate_map, {
    output$travel_time_map <- renderLeaflet({
      # Placeholder map, replace with actual Graph-Based Network Analysis
      leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 12)
    })
  })
  
  # Placeholder for commute time estimator result
  observeEvent(input$estimate_commute, {
    output$commute_time_result <- renderText({
      paste("Commute from", input$home_location, "to", input$workplace, "will take approximately 45 minutes with 1 transfer.")
    })
  })
  
  # Placeholder for accessibility score result
  observeEvent(input$get_score, {
    output$accessibility_score_result <- renderText({
      paste("The accessibility score for postal code", input$postal_code, "is 85 out of 100.")
    })
    
    output$last_mile_info <- renderText({
      paste("Nearest MRT Station: ABC MRT\nLast-mile connectivity: Walking distance of 5 minutes.")
    })
  })
  
  # Placeholder for recalculated MLR score
  observeEvent(input$recalculate_score, {
    output$accessibility_score_result <- renderText({
      paste("Recalculated Accessibility Score: 90 out of 100.")
    })
  })


  
    
    
    # Tab 2: Accessibility Analysis ----
    
    # Reactive values to control calculations
    postal_code <- reactiveVal(NULL)
    initial_calculation <- reactiveVal(FALSE)
    recalculated <- reactiveVal(FALSE)
    
    # Store current slider values for comparison
    current_settings <- reactiveValues(
      travel_time = 15,
      walking_distance = 400,
      waiting_time = 5,
      transport_type = "MRT & Bus"
    )
    
    # Observe postal code input
    observeEvent(input$t2_postal_code, {
      postal_code(input$t2_postal_code)
      initial_calculation(FALSE)
      recalculated(FALSE)
    })
    
    # Observe initial calculation button
    observeEvent(input$t2_accessibility_score, {
      req(postal_code())
      initial_calculation(TRUE)
      recalculated(FALSE)
      
      # Store current default settings
      current_settings$travel_time <- 15
      current_settings$walking_distance <- 400
      current_settings$waiting_time <- 5
      current_settings$transport_type <- "MRT & Bus"
    })
    
    # Observe recalculate button
    observeEvent(input$t2_recalculate, {
      req(postal_code())
      req(initial_calculation())  # Only works after initial calculation
      
      # Only recalculate if settings have changed
      if (current_settings$travel_time != input$t2_travel_time ||
          current_settings$walking_distance != input$t2_walking_distance ||
          current_settings$waiting_time != input$t2_waiting_time ||
          current_settings$transport_type != input$t2_transport_type) {
        
        recalculated(TRUE)
        
        # Update stored settings
        current_settings$travel_time <- input$t2_travel_time
        current_settings$walking_distance <- input$t2_walking_distance
        current_settings$waiting_time <- input$t2_waiting_time
        current_settings$transport_type <- input$t2_transport_type
      }
    })
    
    # Determine which settings to use for calculation
    active_settings <- reactive({
      if (recalculated()) {
        # Use current slider values if recalculated
        list(
          travel_time = input$t2_travel_time,
          walking_distance = input$t2_walking_distance,
          waiting_time = input$t2_waiting_time,
          transport_type = input$t2_transport_type
        )
      } else if (initial_calculation()) {
        # Use default settings for initial calculation
        list(
          travel_time = 15,
          walking_distance = 400,
          waiting_time = 5,
          transport_type = "MRT & Bus"
        )
      } else {
        NULL
      }
    })
    
    # Dummy data for nearest bus stops and MRT stations
    nearest_transport <- reactive({
      req(initial_calculation())  # Only calculate when initial button is pressed
      
      # Generate random dummy data based on postal code
      set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
      
      # Random number of bus stops (3-5)
      n_bus <- sample(3:5, 1)
      bus_stops <- data.frame(
        Name = paste("Bus Stop", LETTERS[1:n_bus]),
        Distance = round(runif(n_bus, 100, 800), 1),
        Routes = replicate(n_bus, paste(sample(10:199, sample(2:4, 1)), collapse = ", "))
      )
      
      # Random number of MRT stations (1-2)
      n_mrt <- sample(1:2, 1)
      mrt_stations <- data.frame(
        Name = paste("MRT", c("North-South Line", "East-West Line", "Circle Line")[1:n_mrt]),
        Distance = round(runif(n_mrt, 200, 1500), 1),
        Travel_Time = sample(5:15, n_mrt, replace = TRUE)
      )
      
      list(bus_stops = bus_stops, mrt_stations = mrt_stations)
    })
    
    # Dummy data for key location travel times
    key_locations <- reactive({
      req(initial_calculation())  # Only calculate when initial button is pressed
      
      set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
      
      locations <- c("CBD", "Nearest Mall", "Nearest School", "Nearest Hospital", "Nearest Park")
      data.frame(
        Location = locations,
        Distance_km = round(runif(length(locations), 0.5, 10), 1),
        Travel_Time_mins = sample(5:60, length(locations), replace = TRUE),
        Transport_Mode = sample(c("MRT", "Bus", "MRT + Bus"), length(locations), replace = TRUE)
      )
    })
    
    # Calculate accessibility scores
    accessibility_scores <- reactive({
      settings <- active_settings()
      req(settings)  # Only calculate when buttons are pressed
      
      # Generate seed based on postal code for consistent dummy results
      set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
      
      # Base scores affected by user inputs
      mrt_score <- round(100 - (settings$travel_time * 1.5) + rnorm(1, 10, 3), 1)
      bus_score <- round(100 - (settings$walking_distance / 20) + rnorm(1, 5, 2), 1)
      walk_score <- round(100 - (settings$walking_distance / 10) + rnorm(1, 15, 5), 1)
      congestion_score <- round(100 - (settings$waiting_time * 3) + rnorm(1, 20, 5), 1)
      
      # Adjust based on transport preference
      if (settings$transport_type == "MRT") {
        mrt_score <- mrt_score * 1.2
        bus_score <- bus_score * 0.8
      } else if (settings$transport_type == "Bus") {
        mrt_score <- mrt_score * 0.8
        bus_score <- bus_score * 1.2
      }
      
      # Ensure scores are within 0-100 range
      mrt_score <- min(max(mrt_score, 0), 100)
      bus_score <- min(max(bus_score, 0), 100)
      walk_score <- min(max(walk_score, 0), 100)
      congestion_score <- min(max(congestion_score, 0), 100)
      
      # Calculate overall score (weighted average)
      overall_score <- round(
        (mrt_score * 0.3 + bus_score * 0.3 + walk_score * 0.2 + congestion_score * 0.2),
        1
      )
      
      list(
        overall = overall_score,
        mrt = mrt_score,
        bus = bus_score,
        walk = walk_score,
        congestion = congestion_score
      )
    })
    
    # Dynamic score display with color
    output$t2_dynamic_score_display <- renderUI({
      if (!initial_calculation()) {
        return(div(
          style = "font-size: 48px; font-weight: bold; color: #666;",
          "00.0"
        ))
      }
      
      scores <- accessibility_scores()
      score <- scores$overall
      
      # Determine color based on score
      color <- case_when(
        score >= 80 ~ "#2ecc71",  # green
        score >= 60 ~ "#f39c12",  # orange
        TRUE ~ "#e74c3c"          # red
      )
      
      div(
        style = paste0("font-size: 48px; font-weight: bold; color: ", color, ";"),
        score
      )
    })
    
    # Score interpretation text
    output$t2_score_interpretation <- renderUI({
      if (!initial_calculation()) {
        return(div(
          style = "font-size: 16px; margin-top: 10px; color: #666;",
          "Enter a postal code and click 'Get Accessibility Score' to see results"
        ))
      }
      
      scores <- accessibility_scores()
      score <- scores$overall
      
      text <- case_when(
        score >= 80 ~ "Excellent accessibility! This location has great transport options.",
        score >= 60 ~ "Good accessibility. Most amenities are easily reachable.",
        score >= 40 ~ "Moderate accessibility. Some transport options available.",
        TRUE ~ "Poor accessibility. Limited transport options available."
      )
      
      div(
        style = "font-size: 16px; margin-top: 10px;",
        text
      )
    })
    
    # Render metric boxes
    output$t2_mrt_score <- renderText({
      if (!initial_calculation()) "00.0" else accessibility_scores()$mrt
    })
    
    output$t2_bus_score <- renderText({
      if (!initial_calculation()) "00.0" else accessibility_scores()$bus
    })
    
    output$t2_walk_score <- renderText({
      if (!initial_calculation()) "00.0" else accessibility_scores()$walk
    })
    
    output$t2_congestion_score <- renderText({
      if (!initial_calculation()) "00.0" else accessibility_scores()$congestion
    })
    
    # Render key location times table
    output$t2_key_location_times <- renderTable({
      if (!initial_calculation()) {
        return(data.frame(Note = "Click 'Get Accessibility Score' to see travel times"))
      }
      key_locations()
    })
    
    # Render nearest bus/MRT table
    output$t2_nearest_bus_mrt <- renderTable({
      if (!initial_calculation()) {
        return(data.frame(Note = "Click 'Get Accessibility Score' to see nearby transport options"))
      }
      
      transport <- nearest_transport()
      
      # Combine bus and MRT data for display
      bus_df <- transport$bus_stops %>% 
        mutate(Type = "Bus Stop") %>%
        select(Type, Name, Distance, Info = Routes)
      
      mrt_df <- transport$mrt_stations %>% 
        mutate(Type = "MRT Station", Info = paste("Travel time:", Travel_Time, "mins")) %>%
        select(Type, Name, Distance, Info)
      
      rbind(bus_df, mrt_df) %>% arrange(Distance)
    })
    
    # Isochrone map
    output$t2_isochrone_map <- renderLeaflet({
      # Base map always shown
      map <- leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 12)
      
      if (initial_calculation()) {
        # Use appropriate walking distance based on whether we've recalculated
        walk_dist <- if (recalculated()) {
          input$t2_walking_distance
        } else {
          400  # Default value
        }
        
        map <- map %>%
          addMarkers(lng = 103.8198, lat = 1.3521, popup = "Approximate location") %>%
          addCircles(
            lng = 103.8198, lat = 1.3521,
            radius = walk_dist,
            color = "#ff7800", fillOpacity = 0.2,
            popup = paste("Walking distance:", walk_dist, "m")
          )
      }
    })
})