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
  # --- Render BTO Map --- 
  output$bto_map <- renderLeaflet({
    
    # --- Custom Icons ---
    house_icon <- awesomeIcons(
      icon = "house", iconColor = "white", markerColor = "orange", library = "fa"
    )
    
    mrt_icon <- awesomeIcons(
      icon = "train", iconColor = "white", markerColor = "red", library = "fa"
    )
    
    bus_icon <- awesomeIcons(
      icon = "bus", iconColor = "white", markerColor = "blue", library = "fa"
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # --- BTO Markers with Clustering ---
      addAwesomeMarkers(
        data = upcoming_bto,
        ~lng, ~lat,
        icon = house_icon,
        popup = ~paste0(
          "<h5>", town, "</h5>",
          "<p><strong>Available Flat Types:</strong><br>", flatType, "</p>",
          "<p><strong>BTO Exercise:</strong> ",
          format(as.Date(paste0(ballotQtr, "-01")), "%B %Y"), "</p>"
        ),
        group = "BTO Projects"
      ) %>%
      
      # --- MRT Stations ---
      addAwesomeMarkers(
        data = mrt_with_planning,
        ~centroid_lon, ~centroid_lat,
        icon = mrt_icon,
        popup = ~paste0("<h5>", str_to_title(mrt_station), " ", station_type, "</h5>",
                        "<p><strong>MRT Line:</strong><br>", str_to_title(mrt_line_combined), "</p>",
                        "<p><strong>Station Code:</strong><br>", stn_code_combined, "</p>"),
        group = "MRT Stations"
      ) %>%
      
      # --- Bus Stops with Clustering ---
      addAwesomeMarkers(
        data = bus_with_planning,
        ~Longitude, ~Latitude,
        icon = bus_icon,
        popup = ~paste0("<h5>", BusStopCode, "</h5>",
                        "<p><strong>Description:</strong><br>", Description, "</p>",
                        "<p><strong>Road Name:</strong><br>", RoadName, "</p>"),
        clusterOptions = markerClusterOptions(),
        group = "Bus Stops"
      ) %>%
      
      # --- Layers Control ---
      hideGroup(c("MRT Stations", "Bus Stops")) %>%
      addLayersControl(
        overlayGroups = c("BTO Projects", "MRT Stations", "Bus Stops"),
        options = layersControlOptions(collapsed = FALSE)
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
    updateTabsetPanel(session, "mainTabs", selected = "compare")
  })
  
  
  # ==== TAB 1: Location Overview & Density Maps ====
  
  # --- MRT Station Density Map ---
  output$mrt_density_high <- renderUI({
    if (input$mapType != "MRT Stations") return(NULL)
    
    # calculate highest and lowest density
    highest_density <- max(mrt_station_density$n)
    highest_area <- mrt_station_density$pln_area_n[which.max(mrt_station_density$n)]
    
    tagList(
      h4(stringr::str_to_title(highest_area), style = "font-weight: bold"),
      p(paste(highest_density, " MRT stations"))
    )
  })
  
  output$mrt_density_low <- renderUI({
    if (input$mapType != "MRT Stations") return(NULL)
    
    lowest_density <- min(mrt_station_density$n)

    zero_density_areas <- mrt_station_density %>%
      filter(n == 0) %>%
      pull(pln_area_n)
    area_list <- paste(stringr::str_to_title(zero_density_areas), collapse = ", ")
    
    tagList(
      h4(area_list, style = "font-weight: bold"),
      p(paste(lowest_density, " MRT stations"))
    )
  })
  
  output$mrt_station_density_map <- renderLeaflet({
    if (input$mapType != "MRT Stations") return(NULL)
    
    palette <- colorNumeric(palette = c("white", "#1E90FF"), domain = mrt_station_density$n)
    
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]),
        color = "black", weight = 1, fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", pln_area_n, "</strong><br>",
          "Number of MRT Stations: ",
          mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)],"<br>",
          "National Ranking: ",
          mrt_station_density$rank[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]
        ),
        label = ~pln_area_n,
        layerId = ~pln_area_n
      )
    
    if (length(input$mrt_lines) > 0) {
      line_colors <- c(
        "BUKIT PANJANG LRT" = "#748477",
        "CHANGI AIRPORT BRANCH LINE" = "#008040",
        "CIRCLE LINE" = "#FFA500",
        "CIRCLE LINE EXTENSION" = "#FFA500",
        "NORTH-SOUTH LINE" = "#D62010",
        "EAST-WEST LINE" = "#008040",
        "NORTH EAST LINE" = "#8B00FF",
        "DOWNTOWN LINE" = "#004494",
        "THOMSON-EAST COAST LINE" = "#966F33",
        "PUNGGOL LRT" = "#748477",
        "SENGKANG LRT" = "#748477"
      )
      
      filtered_data <- mrt_with_planning %>%
        filter(mrt_line %in% input$mrt_lines)
      
      filtered_data <- filtered_data %>%
        mutate(marker_color = unname(line_colors[mrt_line]))
      
      map <- map %>%
        addCircleMarkers(
          data = filtered_data,
          lng = ~centroid_lon,
          lat = ~centroid_lat,
          radius = 6,
          color = "black",
          fillColor = ~marker_color,
          fillOpacity = 0.9,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<strong>", mrt_station, "</strong><br>",
            "Station Code: ", stn_code, "<br>",
            "MRT Line: ", str_to_title(mrt_line), "<br>",
            "Planning Area: ", str_to_title(pln_area_n)
          )
        )
    }
    
    return(map)
  })
  
  
  # --- Bus Stop Density Map ---
  output$bus_density_high <- renderUI({
    if (input$mapType != "Bus Stops") return(NULL)
    
    highest_density <- max(bus_stop_density$n)
    highest_area <- bus_stop_density$pln_area_n[which.max(bus_stop_density$n)]
    
    tagList(
      h4(stringr::str_to_title(highest_area), style = "font-weight: bold"),
      p(paste(highest_density, " bus stops")),
    )
  })
  
  output$bus_density_low <- renderUI({
    if (input$mapType != "Bus Stops") return(NULL)
    
    lowest_density <- min(bus_stop_density$n)
    
    zero_density_areas <- bus_stop_density %>%
      filter(n == 0) %>%
      pull(pln_area_n)
    area_list <- paste(stringr::str_to_title(zero_density_areas), collapse = ", ")
    
    highest_area <- bus_stop_density$pln_area_n[which.max(bus_stop_density$n)]
    lowest_area <- bus_stop_density$pln_area_n[which.min(bus_stop_density$n)]
    
    tagList(
      h4(area_list, style = "font-weight: bold"),
      p(paste(lowest_density, " bus stops"))
    )
  })
  
  output$bus_stop_density_map <- renderLeaflet({
    if (input$mapType != "Bus Stops") return(NULL)
    
    palette <- colorNumeric(palette = c("white", "#1E90FF"), domain = bus_stop_density$n)
    
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(bus_stop_density$n[match(planning_areas$pln_area_n, bus_stop_density$pln_area_n)]),
        color = "black", weight = 1, fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", pln_area_n, "</strong><br>",
          "Number of Bus Stops: ", 
          bus_stop_density$n[match(pln_area_n, bus_stop_density$pln_area_n)], "<br>",
          "National Ranking: ",
          bus_stop_density$rank[match(pln_area_n, bus_stop_density$pln_area_n)], "<br>"
        ),
        label = ~pln_area_n,
        layerId = ~pln_area_n
      )
    
    # filter and display bus stops based on selected planning areas
    if (length(input$bus_planning_area) > 0) {
      bus_data_filtered <- bus_with_planning %>%
        filter(pln_area_n %in% input$bus_planning_area)
      
      map <- map %>%
        addMarkers(
          data = bus_data_filtered,
          lat = ~Latitude,
          lng = ~Longitude,
          popup = ~paste0(
            "<strong>", Description, "</strong><br>",
            "Bus Stop Code: ", BusStopCode, "<br>",
            "Road Name: ", RoadName
          )
        )
    }
    map
  })
  
  
  # ==== TAB 2: Isochrone Map ====
  output$t2_isochrone_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
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
  t2_get_selected_coords <- reactive({
    if (!is.null(input$t2_postal_code) && input$t2_postal_code != "") {
      coords <- tryCatch({
        get_coords_from_postal(input$t2_postal_code)
      }, error = function(e) {
        showNotification("Invalid postal code.", type = "error")
        return(NULL)
      })
      return(coords)
    } else if (!is.null(input$t2_bto_project)) {
      bto_row <- upcoming_bto %>% filter(label == input$t2_bto_project)
      if (nrow(bto_row) > 0) {
        return(c(bto_row$lng, bto_row$lat))
      }
    }
    return(NULL)
  })
  
  observeEvent(input$t2_show_commute_map, {
    print("Button clicked")
    print(paste("Postal:", input$t2_postal_code))
    print(paste("BTO:", input$t2_bto_project))
    
    coords <- t2_get_selected_coords()
    print("Result from get_selected_coords:")
    print(coords)
    req(coords)
    
    lng <- coords[1]
    lat <- coords[2]
    
    travel_time <- as.numeric(input$t2_time)
    
    isochrone_pts <- generate_fast_isochrone(lng, lat, duration_mins = travel_time)
    
    output$t2_isochrone_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
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
  
  # Initialize route analyzer
  route_analyzer <- RouteAnalyzer$new()
  
  # Store results
  rqs_results <- reactiveValues(
    a = NULL,
    b = NULL,
    c = NULL,
    d = NULL
  )
  
  rqs_radar_data <- reactiveValues(
    x = NULL,
    y = NULL,
    z = NULL,
    w = NULL
  )
  
  # Map UI time periods to RQS format
  rqs_time_period <- reactive({
    req(input$t3_travel_time_preference)
    switch(input$t3_travel_time_preference,
           "AM_peak" = "Morning Peak (6:30-8:30am)",
           "PM_peak" = "Evening Peak (5-7pm)",
           "AM_offpeak" = "Daytime Off-Peak (8:30am-5pm)",
           "PM_offpeak" = "Nighttime Off-Peak (7pm-6:30am)")
  })
  
  # Get coordinates for selected BTO
  get_bto_coords <- function(bto_label) {
    req(bto_label, upcoming_bto)
    upcoming_bto %>% 
      filter(label == bto_label) %>% 
      pull(Start)
  }
  
  
  # Handle comparison button click
  observeEvent(input$t3_get_comparison, {
    req(input$t3_destination_postal)
    
    # Validate at least one BTO selected
    if (is.null(input$t3_bto_a) && is.null(input$t3_bto_b) && 
        is.null(input$t3_bto_c) && is.null(input$t3_bto_d)) {
      showNotification("Please select at least one BTO project", type = "warning")
      return()
    }
    
    # Validate postal code
    if (!grepl("^\\d{6}$", input$t3_destination_postal)) {
      showNotification("Please enter a valid 6-digit postal code", type = "warning")
      return()
    }
    
    # Calculate for each selected BTO
    if (!is.null(input$t3_bto_a)) {
      bto_coords <- get_bto_coords(input$t3_bto_a)
      results <- route_analyzer$calculate_rqs(
        start = bto_coords,
        end = input$t3_destination_postal,
        date = "03-24-2025",
        time_period = rqs_time_period()
      )
      rqs_radar_data$x <- results$components
      rqs_results$a <- results$rqs
    }
    
    if (!is.null(input$t3_bto_b)) {
      bto_coords <- get_bto_coords(input$t3_bto_b)
      results <- route_analyzer$calculate_rqs(
        start = bto_coords,
        end = input$t3_destination_postal,
        date = "03-24-2025",
        time_period = rqs_time_period()
      )
      rqs_radar_data$y <- results$components
      rqs_results$b <- results$rqs
    }
    
    if (!is.null(input$t3_bto_c)) {
      bto_coords <- get_bto_coords(input$t3_bto_c)
      results <- route_analyzer$calculate_rqs(
        start = bto_coords,
        end = input$t3_destination_postal,
        date = "03-24-2025",
        time_period = rqs_time_period()
      )
      rqs_radar_data$z <- results$components
      rqs_results$c <- results$rqs
    }
    
    if (!is.null(input$t3_bto_d)) {
      bto_coords <- get_bto_coords(input$t3_bto_d)
      results <- route_analyzer$calculate_rqs(
        start = bto_coords,
        end = input$t3_destination_postal,
        date = "03-24-2025",
        time_period = rqs_time_period()
      )
      rqs_radar_data$w <- results$components
      rqs_results$d <- results$rqs
    }
    
  })
  
  # Render radar charts
  output$t3_radar_a <- renderPlotly({
    req(rqs_radar_data$x)
    create_radar_chart(rqs_radar_data$x)
  })
  
  output$t3_radar_b <- renderPlotly({
    req(rqs_radar_data$y)
    create_radar_chart(rqs_radar_data$y)
  })
  
  output$t3_radar_c <- renderPlotly({
    req(rqs_radar_data$z)
    create_radar_chart(rqs_radar_data$z)
  })
  
  output$t3_radar_d <- renderPlotly({
    req(rqs_radar_data$w)
    create_radar_chart(rqs_radar_data$w)
  })
  
  # Radar chart creation function
  create_radar_chart <- function(rqs_data) {
    plot_ly(
      type = 'scatterpolar',
      r = c(as.numeric(rqs_data["transport"]), 
            as.numeric(rqs_data["comfort"]), 
            as.numeric(rqs_data["robustness"]), 
            as.numeric(rqs_data["service"]), 
            as.numeric(rqs_data["transport"])),
      theta = c("Trip Speed", "Ride Comfort", "Route Reliability", 
                "Transport Frequency", "Trip Speed"),
      fill = 'toself',
      name = "BTO"
    ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  }
  
  # --- Render Radar Chart Individual Score
  output$t3_radar_a_score <- renderUI({
    # dummy score value, replace this with your actual logic later
    dummy_score <- round(runif(1, 60, 95), 1)  # Random score between 60 and 95
    h2(dummy_score, style = "font-weight: bold")
  })
  
  output$t3_radar_b_score <- renderUI({
    # dummy score value, replace this with your actual logic later
    dummy_score <- round(runif(1, 60, 95), 1)  # Random score between 60 and 95
    h2(dummy_score, style = "font-weight: bold")
  })
  
  output$t3_radar_c_score <- renderUI({
    # dummy score value, replace this with your actual logic later
    dummy_score <- round(runif(1, 60, 95), 1)  # Random score between 60 and 95
    h2(dummy_score, style = "font-weight: bold")
  })
  
  output$t3_radar_d_score <- renderUI({
    # dummy score value, replace this with your actual logic later
    dummy_score <- round(runif(1, 60, 95), 1)  # Random score between 60 and 95
    h2(dummy_score, style = "font-weight: bold")
  })
  
  # render_radar_chart_by_coords <- function(coord_string) {
  #   bto_scores <- rqs_summary %>%
  #     filter(Start == coord_string)
  #   
  #   if (nrow(bto_scores) == 0) return(NULL)
  #   
  #   plot_ly(
  #     type = 'scatterpolar',
  #     r = c(bto_scores$Transport, bto_scores$Comfort, bto_scores$Robustness, bto_scores$Service, bto_scores$Transport),
  #     theta = c("Trip Speed", "Ride Comfort", "Route Reliability", "Transport Frequency", "Trip Speed"),
  #     fill = 'toself',
  #     name = coord_string
  #   ) %>%
  #     layout(
  #       polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
  #       showlegend = FALSE
  #     )
  # }
  # 
  # 
  # output$radar_a <- renderPlotly({
  #   req(input$bto_a_postal)
  #   render_radar_chart_by_coords(input$bto_a_postal)
  # })
  # 
  # output$radar_b <- renderPlotly({
  #   req(input$bto_b_postal)
  #   render_radar_chart_by_coords(input$bto_b_postal)
  # })
  # 
  # output$radar_c <- renderPlotly({
  #   req(input$bto_c_postal)
  #   render_radar_chart_by_coords(input$bto_c_postal)
  # })
  # 
  # output$radar_d <- renderPlotly({
  #   req(input$bto_d_postal)
  #   render_radar_chart_by_coords(input$bto_d_postal)
  # })
  

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
    
    #travel_time_df <- build_travel_time_df(t4_postal_code)
    #accessibility_scores$travel_times <- data.frame(
      #Location = travel_time_df$Name,
      #TravelTime_Min = travel_time_df$EstimatedTimeMin
    #)

    accessibility_scores$travel_times <- data.frame(
    Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
    TravelTime_Min = sample(10:60, 6)
    )
    colnames(accessibility_scores$travel_times)[2] <- "Estimated Travel Time (min)"
    
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

   # --- Optionally Source Custom Logic from testServer.R ---
   
   override_file <- "tab4Override.R"
   predict_file <- "predict_accessibility.R"
   
   if (file.exists(predict_file)) {
     message(" Loading base scoring logic from predict_accessibility.R...")
     source(predict_file, new.env())
   } else {
     stop(" Missing required file: predict_accessibility.R")
   }
   
   if (file.exists(override_file)) {
     message(" Sourcing custom override logic from tab4Override.R...")
     source(override_file, new.env())
   } else {
     stop(" Missing override logic file: tab4Override.R")
   }
  

})
