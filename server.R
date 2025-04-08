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
    updateTabsetPanel(session, "mainTabs", selected = "insights")
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
  
  # ==== TAB 2: Isochrone Map ====
  output$t2_isochrone_map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://www.onemap.gov.sg/maps/tiles/Default/{z}/{x}/{y}.png",
        attribution = '<img src="https://www.onemap.gov.sg/web-assets/images/logo/om_logo.png" style="height:20px;width:20px;"/>&nbsp;<a href="https://www.onemap.gov.sg/" target="_blank" rel="noopener noreferrer">OneMap</a>&nbsp;&copy;&nbsp;contributors&nbsp;&#124;&nbsp;<a href="https://www.sla.gov.sg/" target="_blank" rel="noopener noreferrer">Singapore Land Authority</a>'
      ) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
  })
  
  # If user types in postal code, clear BTO selection
  observeEvent(input$t2_postal_code, {
    if (nzchar(input$t2_postal_code)) {
      updateSelectInput(session, "t2_bto_project", selected = "")
    }
  })
  
  # If user selects a BTO project, clear postal code
  observeEvent(input$t2_bto_project, {
    if (!is.null(input$t2_bto_project) && input$t2_bto_project != "") {
      updateTextInput(session, "t2_postal_code", value = "")
    }
  })
  
  # Determine source of coordinates (postal code or selected BTO)
  get_selected_coords <- reactive({
    if (!is.null(input$t2_postal_code) && input$t2_postal_code != "") {
      coords <- tryCatch({
        get_coords_from_postal(input$t2_postal_code)
      }, error = function(e) {
        showNotification("Invalid postal code.", type = "error")
        return(NULL)
      })
      return(coords)
    } 
    else if (!is.null(input$t2_bto_project)) {
      bto_row <- upcoming_bto %>% filter(label == input$t2_bto_project)
      if (nrow(bto_row) > 0) {
        return(c(bto_row$lng, bto_row$lat))
      }
    }
    return(NULL)
  })
  
  observeEvent(input$t2_show_commute_map, {
    coords <- get_selected_coords()
    req(coords)
    
    lng <- coords[1]
    lat <- coords[2]
    
    travel_time <- as.numeric(input$t2_time)
    
    isochrone_pts <- generate_fast_isochrone(lng, lat, duration_mins = travel_time)
    
    output$t2_isochrone_map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "https://www.onemap.gov.sg/maps/tiles/Default/{z}/{x}/{y}.png",
          attribution = '<img src="https://www.onemap.gov.sg/web-assets/images/logo/om_logo.png" style="height:20px;width:20px;"/>&nbsp;<a href="https://www.onemap.gov.sg/" target="_blank" rel="noopener noreferrer">OneMap</a>&nbsp;&copy;&nbsp;contributors&nbsp;&#124;&nbsp;<a href="https://www.sla.gov.sg/" target="_blank" rel="noopener noreferrer">Singapore Land Authority</a>'
        ) %>%
        setView(lng = lng, lat = lat, zoom = 13) %>%
        addMarkers(lng = lng, lat = lat, popup = paste("Postal Code:", input$t2_postal_code)) %>%
        addPolygons(
          lng = isochrone_pts$lon,
          lat = isochrone_pts$lat,
          fillColor = "2C7FB8",
          fillOpacity = 0.25,
          color = "045A8D",
          weight = 1.5,
          popup = paste("Area reachable within ", travel_time," minutes")
        ) %>%
        fitBounds(
          lng1 = min(isochrone_pts$lon), lat1 = min(isochrone_pts$lat),
          lng2 = max(isochrone_pts$lon), lat2 = max(isochrone_pts$lat)
        )
    })
  })

    
  # ==== TAB 3: Comparing BTO Estates ====
  render_radar_chart_by_coords <- function(coord_string) {
    bto_scores <- rqs_summary %>%
      filter(Start == coord_string)
    
    if (nrow(bto_scores) == 0) return(NULL)
    
    plot_ly(
      type = 'scatterpolar',
      r = c(bto_scores$Transport, bto_scores$Comfort, bto_scores$Robustness, bto_scores$Service, bto_scores$Transport),
      theta = c("Trip Speed", "Ride Comfort", "Route Reliability", "Transport Frequency", "Trip Speed"),
      fill = 'toself',
      name = coord_string
    ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        showlegend = FALSE
      )
  }
  
  
  output$radar_a <- renderPlotly({
    req(input$bto_a_postal)
    render_radar_chart_by_coords(input$bto_a_postal)
  })
  
  output$radar_b <- renderPlotly({
    req(input$bto_b_postal)
    render_radar_chart_by_coords(input$bto_b_postal)
  })
  
  output$radar_c <- renderPlotly({
    req(input$bto_c_postal)
    render_radar_chart_by_coords(input$bto_c_postal)
  })
  
  output$radar_d <- renderPlotly({
    req(input$bto_d_postal)
    render_radar_chart_by_coords(input$bto_d_postal)
  })
  

  # ==== TAB 4: Accessibility Dashboard ====
  # If user types in postal code, clear BTO selection
  observeEvent(input$t4_postal_code, {
    if (nzchar(input$t4_postal_code)) {
      updateSelectInput(session, "t4_bto_project", selected = "")
    }
  })
  
  # If user selects a BTO project, clear postal code
  observeEvent(input$t4_bto_project, {
    if (!is.null(input$t4_bto_project) && input$t4_bto_project != "") {
      updateTextInput(session, "t4_postal_code", value = "")
    }
  })
  
  # Determine source of coordinates (postal code or selected BTO)
  get_selected_coords <- reactive({
    if (!is.null(input$t4_postal_code) && input$t4_postal_code != "") {
      coords <- tryCatch({
        get_coords_from_postal(input$t4_postal_code)
      }, error = function(e) {
        showNotification("Invalid postal code.", type = "error")
        return(NULL)
      })
      return(coords)
    } 
    else if (!is.null(input$t4_bto_project)) {
      bto_row <- upcoming_bto %>% filter(label == input$t4_bto_project)
      if (nrow(bto_row) > 0) {
        return(c(bto_row$lng, bto_row$lat))
      }
    }
    return(NULL)
  })
  
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
