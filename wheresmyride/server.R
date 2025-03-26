library(shiny)
library(shinyjs)
library(leaflet)

# Define server logic
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
  
  
  
  # tab 2 outputs
  
  ## dynamic score display
  output$t2_dynamic_score_display <- renderUI({
    req(accessibility_data$score)
    score <- accessibility_data$score
    color <- case_when(score >= 80 ~ "#2E8B57", score >= 50 ~ "#FFA500", TRUE ~ "#E63946")
    div(style = sprintf("font-size: 50px; font-weight: bold; color: %s;", color), score)
  })
  
  output$t2_score_interpretation <- renderUI({
    req(accessibility_data$score)
    score <- accessibility_data$score
    text <- case_when(score >= 80 ~ "Excellent", score >= 50 ~ "Moderate", TRUE ~ "Poor")
    div(style = "font-size: 18px;", text)
  })
  
  ## key locations table
  output$t2_key_location_times <- renderTable({
    req(accessibility_data$key_locations)
    accessibility_data$key_locations
  })
  
  # 1. Initialize reactive values
  accessibility_data <- reactiveValues(
    score = NULL, mrt_score = NULL, bus_score = NULL,
    walk_score = NULL, congestion_score = NULL, key_locations = NULL
  )
  
  # 2. Calculate initial scores
  observeEvent(input$t2_accessibility_score, {
    req(input$t2_postal_code)
    shinybusy::show_modal_spinner(text = "Calculating scores...")
    
    # Mock calculations (replace with real logic)
    accessibility_data$score <- runif(1, 50, 100) %>% round(1)
    accessibility_data$mrt_score <- runif(1, 0, 100) %>% round(1)
    accessibility_data$bus_score <- runif(1, 0, 100) %>% round(1)
    accessibility_data$walk_score <- runif(1, 0, 100) %>% round(1)
    accessibility_data$congestion_score <- runif(1, 0, 100) %>% round(1)
    accessibility_data$key_locations <- tibble(
      Destination = c("CBD", "Airport", "Mall", "Park"),
      Time = paste0(sample(5:60, 4), " mins"),
      Transport = c("MRT", "Bus/MRT", "Walk", "Bus")
    )
    
    shinyjs::show("t2_recalculate")
    shinybusy::remove_modal_spinner()
  })
  
  # 4. Metric boxes
  output$mrt_score <- renderText(accessibility_data$mrt_score)
  output$bus_score <- renderText(accessibility_data$bus_score)
  output$walk_score <- renderText(accessibility_data$walk_score)
  output$congestion_score <- renderText(accessibility_data$congestion_score)
  

  
  # 7. Recalculation
  observeEvent(input$t2_recalculate, {
    req(input$t2_postal_code)
    shinybusy::show_modal_spinner(text = "Recalculating...")
    new_score <- accessibility_data$score * (
      (input$t2_travel_time / 15) * 0.3 +
        (input$walking_dist / 400) * 0.2 +
        (input$t2_freq / 5) * 0.1
    ) %>% round(1)
    accessibility_data$score <- new_score
    shinybusy::remove_modal_spinner()
  })
  
  ## isochrone map
  output$t2_isochrone_map <- renderLeaflet({
    req(input$t2_postal_code)
    lat <- 1.3521 + runif(1, -0.05, 0.05)
    lng <- 103.8198 + runif(1, -0.05, 0.05)
    leaflet() %>%
      addTiles() %>%
      setView(lng, lat, zoom = 14) %>%
      addMarkers(lng, lat, popup = "Selected Location") %>%
      addCircles(lng, lat, radius = input$walking_dist, color = "#219EBC", fillOpacity = 0.2)
  })
  
})