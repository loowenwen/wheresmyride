library(shiny)
library(leaflet)
library(plotly)
library(igraph)  # For graph-based network analysis

# Define server logic
shinyServer(function(input, output) {
  
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
  output$t2_accessibility_score <- renderPrint({ "Accessibility Score Calculation Here" })
  output$t2_key_location_times <- renderTable({ data.frame(Location = c("CBD", "Changi", "One-North"), Time = c("30 min", "45 min", "25 min")) })
  
  output$t2_isochrone_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = 103.851959, lat = 1.290270, popup = "Example Location")
  })
  
  output$t2_commute_time <- renderPrint({ "Estimated Commute Time Calculation Here" })
  output$t2_route_comparison <- renderTable({ data.frame(Route = c("MRT Only", "Mixed"), Time = c("35 min", "30 min"), Transfers = c(1, 2)) })
  
})