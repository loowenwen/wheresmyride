library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(sf)
library(scales)
library(shinyjs)
library(shiny)
library(plotly)
library(tidyverse)

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # ==== Home Tab ====
  # --- Dummy Data for BTO Locations ---
  bto_dummy <- data.frame(
    project_name = c("Tampines North Grove", "Woodlands Spring", "Bukit Batok Vista"),
    lat = c(1.3700, 1.4370, 1.3480),
    lng = c(103.9400, 103.7860, 103.7490),
    launch_date = c("Jun 2025", "Sep 2025", "Dec 2025")
  )
  
  # --- Render BTO Map -- 
  output$bto_map <- renderLeaflet({
    leaflet(bto_dummy) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        ~lng, ~lat,
        popup = ~paste0("<b>", project_name, "</b><br>Launch Date: ", launch_date)
      ) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  # --- Navigation Logic ---
  observeEvent(input$go_heatmap, {
    updateTabsetPanel(session, "mainTabs", selected = "heatmap")
  })
  observeEvent(input$go_commute, {
    updateTabsetPanel(session, "mainTabs", selected = "commute")
  })
  observeEvent(input$go_compare, {
    updateTabsetPanel(session, "mainTabs", selected = "compare")
  })
  observeEvent(input$go_insights, {
    updateTabsetPanel(session, "mainTabs", selected = "insights")
  })
  observeEvent(input$go_to_map, {
    updateTabsetPanel(session, "mainTabs", selected = "commute")
  })
  
  
  # ==== TAB 1: Location Overview & Density Maps ====
  observeEvent(input$search_location, {
    output$location_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 15) %>%
        addMarkers(
          lng = c(103.8198, 103.822), lat = c(1.3521, 1.3500),
          popup = c("Bus Stop A", "MRT Station B")
        )
    })
    
    output$location_summary <- renderText({
      paste("Number of Bus Stops: 5\nNumber of MRT Stations: 2\nAverage Distance: 300m")
    })
  })
  
  output$bus_stop_density_map <- renderLeaflet({
    palette <- col_numeric(palette = c("white", "darkblue"), domain = bus_stop_density$n)
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(bus_stop_density$n[match(planning_areas$pln_area_n, bus_stop_density$pln_area_n)]),
        color = "grey", weight = 1, opacity = 1, fillOpacity = 0.7,
        popup = ~paste("Region:", pln_area_n,
                       "<br>Nearby Bus Stops:", bus_stop_density$n[match(pln_area_n, bus_stop_density$pln_area_n)])
      )
  })
  
  output$mrt_station_density_map <- renderLeaflet({
    palette <- col_numeric(palette = c("white", "darkorchid4"), domain = mrt_station_density$n)
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]),
        color = "grey", weight = 1, opacity = 1, fillOpacity = 0.7,
        popup = ~paste("Region:", pln_area_n,
                       "<br>Nearby MRT Stops:", mrt_station_density$n[match(pln_area_n, mrt_station_density$pln_area_n)])
      )
  })
  
  # ==== TAB 2: Comparing BTO Estates ====
  
  observeEvent(input$compare_commute, {
    output$commute_table_compare <- renderTable({
      data.frame(
        Metric = c("Estimated Commute (mins)", "Distance (km)"),
        `BTO A` = c(sample(30:60, 1), round(runif(1, 5, 15), 1)),
        `BTO B` = c(sample(25:55, 1), round(runif(1, 4, 12), 1))
      )
    })
    
    output$radar_a <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        r = c(80, 75, 85, 90, 70, 80),
        theta = c("Accessibility", "Commute Time", "Bus Connectivity", "MRT Proximity", "Walkability", "Accessibility"),
        fill = 'toself', name = "BTO A"
      ) %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
    })
    
    output$radar_b <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        r = c(65, 70, 60, 55, 80, 65),
        theta = c("Accessibility", "Commute Time", "Bus Connectivity", "MRT Proximity", "Walkability", "Accessibility"),
        fill = 'toself', name = "BTO B"
      ) %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
    })
    
    output$bto_a_review <- renderText({
      paste("BTO A (", input$bto_a_postal, ") shows strong MRT and walkability scores.")
    })
    
    output$bto_b_review <- renderText({
      paste("BTO B (", input$bto_b_postal, ") has shorter commute but weaker MRT access.")
    })
  })
  
  
  # ==== TAB 4: Accessibility Dashboard ====
  # --- Reactive Values to Store Results ---
  accessibility_scores <- reactiveValues(
    overall_score = NULL,
    mrt_score = NULL,
    bus_score = NULL,
    walk_score = NULL,
    congestion_score = NULL,
    travel_times = NULL,
    nearby_stops = NULL
  )
  
  # --- Check if Calculation Has Been Performed ---
  initial_calculation <- reactive({
    !is.null(accessibility_scores$overall_score)
  })
  
  # --- Bundle scores into a reactive list for UI access ---
  get_scores <- reactive({
    req(initial_calculation())
    list(
      overall = accessibility_scores$overall_score,
      mrt = accessibility_scores$mrt_score,
      bus = accessibility_scores$bus_score,
      walk = accessibility_scores$walk_score,
      congestion = accessibility_scores$congestion_score
    )
  })
  
  # --- Simulate Key Locations (reactive) ---
  key_locations <- reactive({
    req(initial_calculation())
    accessibility_scores$travel_times
  })
  
  # --- Simulate Nearest Stops (reactive) ---
  nearest_bus_mrt <- reactive({
    req(initial_calculation())
    accessibility_scores$nearby_stops
  })
  
  # --- Initial Accessibility Score Calculation ---
  observeEvent(input$t4_get_score, {
    accessibility_scores$overall_score <- round(runif(1, 50, 100), 1)
    accessibility_scores$mrt_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$bus_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$walk_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$congestion_score <- round(runif(1, 0, 100), 1)
    
    accessibility_scores$travel_times <- data.frame(
      Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
      TravelTime_Min = sample(10:60, 6)
    )
    colnames(accessibility_scores$travel_times)[2] <- "Estimated Travel Time (min)"
    
    accessibility_scores$nearby_stops <- data.frame(
      Type = c("MRT", "MRT", "Bus", "Bus"),
      Description = c("Tampines", "Simei", "Bus 293", "Bus 10"),
      Distance_m = sample(100:500, 4)
    )
    colnames(accessibility_scores$nearby_stops)[3] <- "Distance (m)"
    
  })
  
  # --- Recalculate Accessibility with Weights ---
  observeEvent(input$t4_recalculate, {
    total_weight <- input$t4_mrt + input$t4_bus + input$t4_walk + input$t4_congestion
    if (total_weight == 0) total_weight <- 1  # Prevent division by zero
    
    w_mrt <- input$t4_mrt / total_weight
    w_bus <- input$t4_bus / total_weight
    w_walk <- input$t4_walk / total_weight
    w_congestion <- input$t4_congestion / total_weight
    
    # Update component scores
    accessibility_scores$mrt_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$bus_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$walk_score <- round(runif(1, 0, 100), 1)
    accessibility_scores$congestion_score <- round(runif(1, 0, 100), 1)
    
    # Weighted overall score
    accessibility_scores$overall_score <- round(
      accessibility_scores$mrt_score * w_mrt +
        accessibility_scores$bus_score * w_bus +
        accessibility_scores$walk_score * w_walk +
        accessibility_scores$congestion_score * w_congestion, 1
    )
  })
  
  # --- UI Outputs ---
  
  output$t4_score_display <- renderUI({
    if (!initial_calculation())
      return(h1("00.0", style = "font-weight: bold"))
    
    score <- get_scores()$overall
    color <- case_when(
      score >= 80 ~ "text-success",
      score >= 60 ~ "text-warning",
      TRUE ~ "text-danger"
    )
    h1(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_score_interpretation <- renderUI({
    if (!initial_calculation()) {
      return(p("Enter a postal code and click 'Get Accessibility Score' to see results."))
    }
    
    score <- get_scores()$overall
    interpretation <- case_when(
      score >= 80 ~ "Excellent accessibility - this location offers top-tier transport connectivity and convenience.",
      score >= 60 ~ "Good accessibility - most destinations are easily reachable with decent transport options.",
      score >= 40 ~ "Moderate accessibility - there are some transport links, but coverage or convenience may be limited.",
      TRUE ~ "Poor accessibility - the area has limited transport options and may be harder to reach."
    )
    
    p(interpretation)
  })
  
  output$t4_mrt_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$mrt
    color <- case_when(score >= 80 ~ "text-success", score >= 60 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_bus_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$bus
    color <- case_when(score >= 80 ~ "text-success", score >= 60 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_walk_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$walk
    color <- case_when(score >= 80 ~ "text-success", score >= 60 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_congestion_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$congestion
    color <- case_when(score >= 80 ~ "text-success", score >= 60 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_key_location_times <- renderTable({
    key_locations()
  })
  
  output$t4_key_location_note <- renderUI({
    if (!initial_calculation()) {
      return(p("Enter a postal code and click 'Get Accessibility Score' to see results."))
    }
    return(NULL)
  })
  
  output$t4_nearest_bus_mrt <- renderTable({
    nearest_bus_mrt()
  })
  
  output$t4_nearest_bus_mrt_note <- renderUI({
    if (!initial_calculation()) {
      return(p("Enter a postal code and click 'Get Accessibility Score' to see results."))
    }
    return(NULL)
  })
  
})
