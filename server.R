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
  
  # --- Reactive Value ---
  show_all_flag <- reactiveVal(FALSE)
  
  observeEvent(input$show_all_mrt, {
    show_all_flag(TRUE)
    updateCheckboxGroupInput(
      session,
      inputId = "mrt_lines",
      selected = character(0)  # sets it to empty
    )
  })
  
  observeEvent(input$mrt_lines, {
    show_all_flag(FALSE)
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
          mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)], "<br>",
          "National Ranking: ",
          mrt_station_density$rank[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]
        ),
        label = ~pln_area_n,
        layerId = ~pln_area_n,
        group = "Planning Area Density"
      )
    
    line_colors <- c(
      "BUKIT PANJANG LRT" = "lightgray",
      "CHANGI AIRPORT BRANCH LINE" = "darkgreen",
      "CIRCLE LINE" = "orange",
      "CIRCLE LINE EXTENSION" = "orange",
      "NORTH-SOUTH LINE" = "red",
      "EAST-WEST LINE" = "darkgreen",
      "NORTH EAST LINE" = "purple",
      "DOWNTOWN LINE" = "darkblue",
      "THOMSON-EAST COAST LINE" = "darkred",
      "PUNGGOL LRT" = "lightgray",
      "SENGKANG LRT" = "lightgray"
    )
    
    filtered_data <- if (show_all_flag()) {
      mrt_with_planning
    } else if (length(input$mrt_lines) > 0) {
      mrt_with_planning %>%
        filter(mrt_line %in% input$mrt_lines)
    } else {
      NULL  # if nothing selected
    }
    
    if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
      filtered_data <- filtered_data %>%
        mutate(marker_color = unname(line_colors[mrt_line]))
      
      mrt_polygons_sf <- filtered_data %>%
        select(stn_code, mrt_station, mrt_line, station_type, stn_code_combined, 
               mrt_line_combined, pln_area_n, marker_color, geometry) %>%
        st_as_sf(crs = 3414) %>%
        st_transform(4326)
      
      mrt_lines_sf <- filtered_data %>%
        st_as_sf(sf_column_name = "centroid", crs = 3414) %>%
        st_transform(4326) %>%
        mutate(
          line_code = str_extract(stn_code, "^[A-Za-z]+"),
          stn_num = as.numeric(str_extract(stn_code, "\\d+"))
        ) %>%
        arrange(line_code, stn_num) %>%
        group_by(mrt_line, line_code, marker_color) %>%
        summarise(
          geometry = st_combine(centroid) %>% st_cast("LINESTRING"),
          .groups = "drop"
        )
      
      map <- map %>%
        addPolygons(
          data = mrt_polygons_sf,
          fillColor = ~marker_color,
          fillOpacity = 0.5,
          color = "black",
          weight = 1,
          group = "MRT Line Network"
        ) %>%
        addPolylines(
          data = mrt_lines_sf,
          color = ~marker_color,
          weight = 4,
          opacity = 0.7,
          group = "MRT Line Network"
        ) %>%
        addAwesomeMarkers(
          data = filtered_data,
          lng = ~centroid_lon,
          lat = ~centroid_lat,
          icon = ~awesomeIcons(
            icon = 'train',
            library = 'fa',
            iconColor = 'white',
            markerColor = marker_color
          ),
          popup = ~paste0(
            "<strong>", mrt_station, "</strong><br>",
            "Station Code: ", stn_code, "<br>",
            "MRT Line: ", str_to_title(mrt_line), "<br>",
            "Planning Area: ", str_to_title(pln_area_n)
          ),
          group = "MRT Station Markers"
        )
    }
    
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Planning Area Density", "MRT Line Network", "MRT Station Markers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palette,
        values = mrt_station_density$n,
        title = "MRT Station Count",
        opacity = 0.7
      )
    
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
  
  # --- Reactive ---
  show_all_flag <- reactiveVal(FALSE)
  
  observeEvent(input$show_all_bus, {
    show_all_flag(TRUE)
    updateCheckboxGroupInput(
      session,
      inputId = "bus_planning_area",
      selected = character(0)  # sets it to empty
    )
  })
  
  observeEvent(input$bus_planning_area, {
    show_all_flag(FALSE)
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
        layerId = ~pln_area_n,
        group = "Planning Area Density"
      )
    
    bus_icon <- awesomeIcons(
      icon = "bus", iconColor = "white", markerColor = "blue", library = "fa"
    )
    
    filtered_data <- if (show_all_flag()) {
      bus_with_planning
    } else if (length(input$bus_planning_area) > 0) {
      subset(bus_with_planning, pln_area_n %in% input$bus_planning_area)
    } else {
      NULL
    }
    
    if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
      map <- map %>%
        addAwesomeMarkers(
          data = filtered_data,
          lat = ~Latitude,
          lng = ~Longitude,
          icon = bus_icon,
          popup = ~paste0(
            "<strong>", Description, "</strong><br>",
            "Bus Stop Code: ", BusStopCode, "<br>",
            "Road Name: ", RoadName, "<br>",
            "Service Number: ", services
          ),
          clusterOptions = markerClusterOptions(),
          group = "Bus Stop Markers"
        )
    }
    
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Planning Area Density", "Bus Stop Markers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palette,
        values = bus_stop_density$n,
        title = "Bus Stop Count",
        opacity = 0.7
      )
    return(map)
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
  
  # Create reactive values
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
  
  rqs_weights <- reactive({
    total <- input$t3_comfort + input$t3_speed + input$t3_frequency + input$t3_reliability
    c(
      comfort = input$t3_comfort / total,
      speed = input$t3_speed / total,
      frequency = input$t3_frequency / total,
      reliability = input$t3_reliability / total
    )
  })
  
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
  observeEvent(c(input$t3_get_comparison, input$t3_recalculate, input$t3_calculate_with_new_time), {
    
    # --- Validate Postal Code ---
    if (is.null(input$t3_destination_postal) || !grepl("^\\d{6}$", input$t3_destination_postal)) {
      showNotification("Please enter a valid 6-digit postal code.", type = "error")
      return(NULL)
    }
    
    # --- Validate At Least One BTO is Selected ---
    if (all(c(input$t3_bto_a, input$t3_bto_b, input$t3_bto_c, input$t3_bto_d) == "")) {
      showNotification("Please select at least one BTO project.", type = "error")
      return(NULL)
    }
    
    # --- Process Each Selected BTO Safely ---
    if (!is.null(input$t3_bto_a) && input$t3_bto_a != "") {
      tryCatch({
        bto_coords <- get_bto_coords(input$t3_bto_a)
        results <- route_analyzer$calculate_rqs(
          start = bto_coords,
          end = input$t3_destination_postal,
          date = format(Sys.Date(), "%m-%d-%Y"),
          time_period = rqs_time_period(),
          maxWalkDistance = 1000,
          weights = rqs_weights()
        )
        rqs_radar_data$x <- results$components
        rqs_results$a <- results$rqs
      }, error = function(e) {
        showNotification("Failed to calculate Route Quality for BTO A", type = "error")
        print(e)
      })
    }
    
    if (!is.null(input$t3_bto_b) && input$t3_bto_b != "") {
      tryCatch({
        bto_coords <- get_bto_coords(input$t3_bto_b)
        results <- route_analyzer$calculate_rqs(
          start = bto_coords,
          end = input$t3_destination_postal,
          date = format(Sys.Date(), "%m-%d-%Y"),
          time_period = rqs_time_period(),
          maxWalkDistance = 1000,
          weights = rqs_weights()
        )
        rqs_radar_data$y <- results$components
        rqs_results$b <- results$rqs
      }, error = function(e) {
        showNotification("Failed to calculate Route Quality for BTO B", type = "error")
        print(e)
      })
    }
    
    if (!is.null(input$t3_bto_c) && input$t3_bto_c != "") {
      tryCatch({
        bto_coords <- get_bto_coords(input$t3_bto_c)
        results <- route_analyzer$calculate_rqs(
          start = bto_coords,
          end = input$t3_destination_postal,
          date = format(Sys.Date(), "%m-%d-%Y"),
          time_period = rqs_time_period(),
          maxWalkDistance = 1000,
          weights = rqs_weights()
        )
        rqs_radar_data$z <- results$components
        rqs_results$c <- results$rqs
      }, error = function(e) {
        showNotification("Failed to calculate Route Quality for BTO C", type = "error")
        print(e)
      })
    }
    
    if (!is.null(input$t3_bto_d) && input$t3_bto_d != "") {
      tryCatch({
        bto_coords <- get_bto_coords(input$t3_bto_d)
        results <- route_analyzer$calculate_rqs(
          start = bto_coords,
          end = input$t3_destination_postal,
          date = format(Sys.Date(), "%m-%d-%Y"),
          time_period = rqs_time_period(),
          maxWalkDistance = 1000,
          weights = rqs_weights()
        )
        rqs_radar_data$w <- results$components
        rqs_results$d <- results$rqs
      }, error = function(e) {
        showNotification("Failed to calculate Route Quality for BTO D", type = "error")
        print(e)
      })
    }
    
  })
  
  # --- Render Radar Charts for Each BTO ---
  output$t3_radar_a <- renderPlotly({
    if (is.null(rqs_radar_data$x)) {
      return(create_base_radar_chart())
    }
    create_radar_chart(rqs_radar_data$x)
  })
  
  output$t3_radar_b <- renderPlotly({
    if (is.null(rqs_radar_data$y)) {
      return(create_base_radar_chart())
    }
    create_radar_chart(rqs_radar_data$y)
  })
  
  output$t3_radar_c <- renderPlotly({
    if (is.null(rqs_radar_data$z)) {
      return(create_base_radar_chart())
    }
    create_radar_chart(rqs_radar_data$z)
  })
  
  output$t3_radar_d <- renderPlotly({
    if (is.null(rqs_radar_data$w)) {
      return(create_base_radar_chart())
    }
    create_radar_chart(rqs_radar_data$w)
  })
  
  # --- Base Radar Chart ---
  create_base_radar_chart <- function() {
    plot_ly(
      type = 'scatterpolar',
      r = c(0, 0, 0, 0, 0),
      theta = c("Trip Speed", "Ride Comfort", "Route Reliability", 
                "Transport Frequency", "Trip Speed"),
      fill = 'toself',
      name = "No Data",
      opacity = 0.3
    ) %>%
      layout(
        title = list(text = "Awaiting Results", x = 0.5),
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  }
  
  # --- Actual Radar Chart Create Function ---
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
    score <- rqs_results$a
    if (!is.null(score) && !is.na(score)) {
      color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
      return(h2(round(score, 1), class = color, style = "font-weight: bold"))
    } else {
      return(h2("00.0", style = "font-weight: bold"))
    }
  })
  
  
  output$t3_radar_b_score <- renderUI({
    score <- rqs_results$b
    if (!is.null(score) && !is.na(score)) {
      color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
      return(h2(round(score, 1), class = color, style = "font-weight: bold"))
    } else {
      return(h2("00.0", style = "font-weight: bold"))
    }
  })
  
  output$t3_radar_c_score <- renderUI({
    score <- rqs_results$c
    if (!is.null(score) && !is.na(score)) {
      color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
      return(h2(round(score, 1), class = color, style = "font-weight: bold"))
    } else {
      return(h2("00.0", style = "font-weight: bold"))
    }
  })
  
  output$t3_radar_d_score <- renderUI({
    score <- rqs_results$d
    if (!is.null(score) && !is.na(score)) {
      color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
      return(h2(round(score, 1), class = color, style = "font-weight: bold"))
    } else {
      return(h2("00.0", style = "font-weight: bold"))
    }
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
  
  # --- Determine Source of Coordinates (postal code or selected bto) ---
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
    accessibility_scores$overall_score <- NA
    accessibility_scores$mrt_score <- NA
    accessibility_scores$bus_score <- NA
    accessibility_scores$walk_score <- NA
    accessibility_scores$congestion_score <- NA
    
    accessibility_scores$travel_times <- data.frame(
    Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
    TravelTime_Min = rep(NA, 6)
    )
    
    colnames(accessibility_scores$travel_times)[2] <- "Estimated Travel Time (min)"
    
    accessibility_scores$nearby_stops <- data.frame(
       Type = c("MRT", "Bus"),
       Description = c(NA, NA),
       TravelTime_Min = rep(NA, 2)
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
    accessibility_scores$mrt_score <- NA
    accessibility_scores$bus_score <- NA
    accessibility_scores$walk_score <- NA
    accessibility_scores$congestion_score <- NA
    
    # Weighted overall score
    accessibility_scores$overall_score <- round(
      accessibility_scores$mrt_score * w_mrt +
        accessibility_scores$bus_score * w_bus +
        accessibility_scores$walk_score * w_walk +
        accessibility_scores$congestion_score * w_congestion, 1
    )
    
    accessibility_scores$travel_times <- data.frame(
      Location = c("Raffles Place", "One-North", "Orchard Road", "Jurong East", "Changi Airport", "Singapore General Hospital"),
      TravelTime_Min = rep(NA, 6)
    )
    
    colnames(accessibility_scores$travel_times)[2] <- "Estimated Travel Time (min)"
    
    accessibility_scores$nearby_stops <- data.frame(
      Type = c("MRT", "Bus"),
      Description = c(NA, NA),
      TravelTime_Min = rep(NA, 2)
    )
    
    colnames(accessibility_scores$nearby_stops)[3] <- "Distance (m)"
  })
  
  # --- UI Outputs ---
  output$t4_score_display <- renderUI({
    if (!initial_calculation())
      return(h1("00.0", style = "font-weight: bold"))
    
    score <- get_scores()$overall
    color <- case_when(
      score >= 60 ~ "text-success",
      score >= 40 ~ "text-warning",
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
      score >= 60 ~ "Excellent accessibility - this location offers top-tier transport connectivity and convenience.",
      score >= 40 ~ "Good accessibility - most destinations are easily reachable with decent transport options.",
      score >= 20 ~ "Moderate accessibility - there are some transport links, but coverage or convenience may be limited.",
      TRUE ~ "Poor accessibility - the area has limited transport options and may be harder to reach."
    )
    
    p(interpretation)
  })
  
  output$t4_mrt_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$mrt
    color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_bus_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$bus
    color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_walk_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$walk
    color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
    h2(score, class = color, style = "font-weight: bold")
  })
  
  output$t4_congestion_score <- renderUI({
    if (!initial_calculation()) return(h2("00.0", style = "font-weight: bold"))
    score <- get_scores()$congestion
    color <- case_when(score >= 60 ~ "text-success", score >= 40 ~ "text-warning", TRUE ~ "text-danger")
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
    return(tags$p(class = "text-muted",
                  "Within 500 metres of your location."))
  })

   # --- Optionally Source Custom Logic from testServer.R ---
   override_file <- "src/tab4Override.R"
   predict_file <- "src/predict_accessibility.R"
   
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
