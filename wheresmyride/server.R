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
  
  # Output: Text summary of accessibility
  output$accessibility_summary <- renderText({
    paste("Accessibility Score for", input$region, ":", round(accessibility_score(), 2))
  })
  
  # Output: Text summary of travel time
  output$travel_time_summary <- renderText({
    paste("Estimated Travel Time:", travel_time(), "minutes")
  })
  
  # Output: Bar plot of accessibility scores
  output$accessibility_plot <- renderPlotly({
    plot_ly(regions, x = ~Region, y = ~Accessibility_Score, type = "bar", 
            marker = list(color = "#18BC9C")) %>%
      layout(yaxis = list(title = "Accessibility Score", gridcolor = "#ECF0F1"), 
             xaxis = list(title = "Region", gridcolor = "#ECF0F1"),
             plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF")
  })
  
  # Output: Table of optimal route details (dummy implementation)
  output$optimal_route_table <- renderTable({
    if (input$calculate_route > 0) {
      data.frame(
        Step = c("Start at Home", "Take Bus 123", "Transfer to MRT Line A", "Arrive at Destination"),
        Time = c("0 min", "10 min", "20 min", "30 min")
      )
    }
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
})