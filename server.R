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
  # --- Render BTO Map -- 
  output$bto_map <- renderLeaflet({
    leaflet(upcoming_btos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        ~lng, ~lat,
        popup = ~paste0("<b>", town, "</b><br>Launch Date: ", launchStartDate)
      ) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)  # Set the initial view for the map
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
  # MRT Station Density Map
  output$mrt_density_info <- renderUI({
    if (input$mapType != "MRT Stations") return(NULL)
    
    # Calculate highest and lowest density
    highest_density <- max(mrt_station_density$n)
    lowest_density <- min(mrt_station_density$n)
    
    highest_area <- mrt_station_density$pln_area_n[which.max(mrt_station_density$n)]
    lowest_area <- mrt_station_density$pln_area_n[which.min(mrt_station_density$n)]
    
    tagList(
      div(
        style = "background-color: #f9f9f9; padding: 10px; border: 1px solid #ccc;",
        h4("Density Information"),
        p(paste("Area of highest density: ", highest_area, " with ", highest_density, " MRT stations.")),
        p(paste("Area of lowest density: ", lowest_area, " with ", lowest_density, " MRT stations."))
      )
    )
  })
  
  output$mrt_station_density_map <- renderLeaflet({
    if (input$mapType != "MRT Stations") return(NULL)
    
    # Create the color palette for MRT stations polygons
    palette <- colorNumeric(palette = c("white", "black"), domain = mrt_station_density$n)
    
    # Initialize the map with polygons only (no markers)
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]),
        color = "grey", weight = 2, opacity = 1, fillOpacity = 0.7,
        popup = ~paste("<strong>", pln_area_n, "</strong>",
                       "<br>Number of MRT Stations:", 
                       mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]),
        layerId = ~pln_area_n
      )
    
    # Only add markers if a checkbox is selected
    if (length(input$mrt_lines) > 0) {
      filtered_data <- list()
      
      # Add data for each selected MRT line
      if ("ew_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(ew_line))
      }
      if ("ns_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(ns_line))
      }
      if ("cc_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(cc_line))
      }
      if ("ne_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(ne_line))
      }
      if ("dt_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(dt_line))
      }
      if ("te_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(te_line))
      }
      if ("Punggol_LRT_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(Punggol_LRT_line))
      }
      if ("Sengkang_LRT_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(Sengkang_LRT_line))
      }
      if ("BukitPanjang_LRT_line" %in% input$mrt_lines) {
        filtered_data <- append(filtered_data, list(BukitPanjang_LRT_line))
      }
      
      # Combine filtered data for selected MRT lines
      filtered_data <- bind_rows(filtered_data)
      
      # Check if the filtered data contains the necessary columns before adding markers
      if ("Latitude" %in% colnames(filtered_data) && "Longitude" %in% colnames(filtered_data)) {
        # Add MRT station markers to the map using STN_NAME for the popup and STN_NO for station number
        map <- map %>%
          addCircleMarkers(
            data = filtered_data,
            lat = ~Latitude,           # Use the 'Latitude' column for latitude
            lng = ~Longitude,          # Use the 'Longitude' column for longitude
            radius = 6,   # Set the radius of the circle marker (adjust as needed)
            weight = 2,  
            opacity = 1,    
            stroke = TRUE,
            color = "black",
            fillColor = ~colour,            # Use the 'color' column for the color of the circle
                       # Set the opacity of the circle
            fillOpacity = 1,         # Set the fill opacity
            popup = ~paste("<strong>", STN_NAME, "</strong>",  # Use STN_NAME for station name
                           "<br>Station No:", STN_NO)  # Use STN_NO for station number
          )
      }
    }
    
    return(map)
  })
  
  
  # Bus Stop Density Map
  output$bus_density_info <- renderUI({
    if (input$mapType != "Bus Stops") return(NULL)
    
    # Calculate highest and lowest density
    highest_density <- max(bus_stop_density$n)
    lowest_density <- min(bus_stop_density$n)
    
    highest_area <- bus_stop_density$pln_area_n[which.max(bus_stop_density$n)]
    lowest_area <- bus_stop_density$pln_area_n[which.min(bus_stop_density$n)]
    
    tagList(
      div(
        style = "background-color: #f9f9f9; padding: 10px; border: 1px solid #ccc;",
        h4("Density Information"),
        p(paste("Area of highest density: ", highest_area, " with ", highest_density, " bus stops.")),
        p(paste("Area of lowest density: ", lowest_area, " with ", lowest_density, " bus stops."))
      )
    )
  })
  
  output$bus_stop_density_map <- renderLeaflet({
    # Only render map if Show Map is True and map type is 'Bus Stops'
    if (input$mapType != "Bus Stops") return(NULL)
    
    # Create the color palette for bus stops (example)
    palette <- colorNumeric(palette = c("white", "black"), domain = bus_stop_density$n)
    
    # Initialize map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
    
    # Add planning area polygons to the map
    map <- map %>%
      addPolygons(
        data = planning_areas,  # Replace with actual planning areas data
        fillColor = ~palette(bus_stop_density$n[match(planning_areas$pln_area_n, bus_stop_density$pln_area_n)]),
        color = "grey",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.7,
        popup = ~paste("<strong>", pln_area_n, "</strong>",
                       "<br>Number of Bus Stops:", bus_stop_density$n[match(pln_area_n, bus_stop_density$pln_area_n)]),
        layerId = ~pln_area_n
      )
    
    # Only add markers if a checkbox is selected
    if (length(input$bus_planning_area) > 0) {
      # Initialize the list to store filtered data for each selected planning area
      filtered_data <- list()
      
      # Add data for each selected planning area (like df_[pln_area_n])
      if ("ROCHOR" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_ROCHOR))
      }
      if ("DOWNTOWN CORE" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_DOWNTOWN_CORE))
      }
      if ("KALLANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_KALLANG))
      }
      if ("OUTRAM" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_OUTRAM))
      }
      if ("MARINA SOUTH" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_MARINA_SOUTH))
      }
      if ("STRAITS VIEW" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_STRAITS_VIEW))
      }
      if ("MUSEUM" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_MUSEUM))
      }
      if ("SINGAPORE RIVER" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SINGAPORE_RIVER))
      }
      if ("BUKIT MERAH" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BUKIT_MERAH))
      }
      if ("ORCHARD" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_ORCHARD))
      }
      if ("RIVER VALLEY" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_RIVER_VALLEY))
      }
      if ("TANGLIN" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_TANGLIN))
      }
      if ("QUEENSTOWN" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_QUEENSTOWN))
      }
      if ("BUKIT TIMAH" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BUKIT_TIMAH))
      }
      if ("CLEMENTI" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_CLEMENTI))
      }
      if ("SOUTHERN ISLANDS" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SOUTHERN_ISLANDS))
      }
      if ("JURONG EAST" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_JURONG_EAST))
      }
      if ("JURONG WEST" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_JURONG_WEST))
      }
      if ("BOON LAY" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BOON_LAY))
      }
      if ("PIONEER" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_PIONEER))
      }
      if ("WESTERN WATER CATCHMENT" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_WESTERN_WATER_CATCHMENT))
      }
      if ("TUAS" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_TUAS))
      }
      if ("CHOA CHU KANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_CHOACHUKANG))
      }
      if ("TENGAH" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_TENGAH))
      }
      if ("LIM CHU KANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_LIMCHUKANG))
      }
      if ("NEWTON" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_NEWTON))
      }
      if ("NOVENA" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_NOVENA))
      }
      if ("CENTRAL WATER CATCHMENT" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_CENTRAL_WATER_CATCHMENT))
      }
      if ("BUKIT BATOK" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BUKIT_BATOK))
      }
      if ("BUKIT PANJANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BUKIT_PANJANG))
      }
      if ("SUNGEI KADUT" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SUNGEI_KADUT))
      }
      if ("WOODLANDS" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_WOODLANDS))
      }
      if ("SEMBAWANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SEMBWANG))
      }
      if ("MANDAI" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_MANDAI))
      }
      if ("YISHUN" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_YISHUN))
      }
      if ("TOA PAYOH" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_TOA_PAYOH))
      }
      if ("BISHAN" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BISHAN))
      }
      if ("ANG MO KIO" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_ANG_MO_KIO))
      }
      if ("SERANGOON" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SERANGOON))
      }
      if ("SENGKANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SENGKANG))
      }
      if ("GEYLANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_GEYLANG))
      }
      if ("HOUGANG" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_HOUGANG))
      }
      if ("PAYA LEBAR" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_PAYA_LEBAR))
      }
      if ("PUNGGOL" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_PUNGGOL))
      }
      if ("SELETAR" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_SELETAR))
      }
      if ("BEDOK" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_BEDOK))
      }
      if ("TAMPINES" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_TAMPINES))
      }
      if ("PASIR RIS" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_PASIR_RIS))
      }
      if ("MARINE PARADE" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_MARINE_PARADE))
      }
      if ("CHANGI" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_CHANGI))
      }
      if ("CHANGI BAY" %in% input$bus_planning_area) {
        filtered_data <- append(filtered_data, list(df_CHANGI_BAY))
      }
      
      bus_data_filtered <- bind_rows(filtered_data)
      
      # Check if the filtered data contains the necessary columns before adding markers
      if ("Latitude" %in% colnames(bus_data_filtered) && "Longitude" %in% colnames(bus_data_filtered)) {
        # Add bus stop markers to the map
        map <- map %>%
          addMarkers(
            data = bus_data_filtered,
            lat = ~Latitude,
            lng = ~Longitude,
            popup = ~paste("<strong>", Description, "</strong>", "<br>Bus Stop ID:", BusStopCode)  # Adjust column names accordingly
          )
      }
    }
    
    # Return the map object
    map
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
  
})
