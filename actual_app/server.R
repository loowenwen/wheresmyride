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
library(htmlwidgets)

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # ==== TAB 1: Location Overview & Density Maps ====
  
  mrt_station_data <- read_csv("../data/MRTStations.csv")
  # Bus Stop Density Map
  output$bus_stop_density_map <- renderLeaflet({
    palette <- colorNumeric(palette = c("white", "darkblue"), domain = bus_stop_density$n)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(bus_stop_density$n[match(planning_areas$pln_area_n, bus_stop_density$pln_area_n)]),
        color = "grey", weight = 2, opacity = 1, fillOpacity = 0.7,
        popup = ~paste("<strong>", pln_area_n, "</strong>",
                       "<br>Number of Bus Stops:", bus_stop_density$n[match(pln_area_n, bus_stop_density$pln_area_n)]),
        layerId = ~pln_area_n,  # Unique layerId for each region
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      )
  })
  
  # MRT Station Density Map
  output$mrt_station_density_map <- renderLeaflet({
    palette <- colorNumeric(palette = c("white", "darkorchid4"), domain = mrt_station_density$n)
    # Convert R objects to JSON for JavaScript to access
    mrt_station_data_json <- toJSON(mrt_station_data, pretty = TRUE)
    planning_areas_json <- toJSON(planning_areas, pretty = TRUE)
    # Create leaflet map
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      
      # Add polygons (planning areas)
      addPolygons(
        data = planning_areas,
        fillColor = ~palette(mrt_station_density$n[match(planning_areas$pln_area_n, mrt_station_density$pln_area_n)]),
        color = "grey", weight = 2, opacity = 1, fillOpacity = 0.7,
        popup = ~paste("<strong>", pln_area_n, "</strong>",
                       "<br>Number of MRT Stations:", mrt_station_density$n[match(pln_area_n, mrt_station_density$pln_area_n)]),
        layerId = ~pln_area_n,  
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      
      # Add MRT station markers (just for now without filtering based on the polygon)
      addMarkers(
        data = mrt_station_data,
        lat = ~Latitude,
        lng = ~Longitude,
        popup = ~paste("<strong>", STN_NAME, "</strong>", "<br>Station Number:", STN_NO)
      ) %>%
      
      onRender("
      var map = this;
      var prevCenter = [1.3521, 103.8198];  // Default center (Singapore)
      var prevZoom = 12;  // Default zoom level
      var selectedRegion = null; // Store the clicked polygon layer

      // Convert the passed R data into JavaScript objects
      var mrt_station_data = " + mrt_station_data_json + ";
      var planning_areas = " + planning_areas_json + ";
      
      // Handle polygon clicks
      map.on('click', function(e) {
        var clickedLayer = e.target;
        
        // Ensure the clicked object is a polygon
        if (clickedLayer instanceof L.Polygon) {
          var bounds = clickedLayer.getBounds();
          var regionName = clickedLayer.feature.properties.pln_area_n;
          
          // Log clicked polygon details
          console.log('Clicked Polygon:', clickedLayer);
          
          // Save the current center and zoom before zooming
          prevCenter = map.getCenter();
          prevZoom = map.getZoom();
          
          // Zoom to the clicked polygon's bounds
          map.fitBounds(bounds);
          
          // Change color of all polygons to black, except the clicked one
          map.eachLayer(function(layer) {
            if (layer instanceof L.Polygon) {
              if (layer.feature.properties.pln_area_n !== regionName) {
                layer.setStyle({ fillColor: 'black', color: 'black' });  // Change others to black
              } else {
                layer.setStyle({ fillColor: layer.options.fillColor, color: 'grey' });  // Keep the clicked polygon color
              }
            }
          });

          // Filter MRT stations within the clicked polygon's bounds
          var filteredStations = mrt_station_data.filter(function(station) {
            var latlng = L.latLng(station.Latitude, station.Longitude);
            return bounds.contains(latlng);  // Check if the station is inside the polygon's bounds
          });

          // Log filtered stations
          console.log('Filtered Stations:', filteredStations);
          
          // Remove existing MRT station markers (if any)
          map.eachLayer(function(layer) {
            if (layer instanceof L.Marker) {
              map.removeLayer(layer);
            }
          });

          // Add markers for filtered MRT stations
          filteredStations.forEach(function(station) {
            L.marker([station.Latitude, station.Longitude])
              .addTo(map)
              .bindPopup('<strong>' + station.STN_NAME + '</strong><br>Station Number: ' + station.STN_NO);
          });
        }
      });

      // Back button to restore the original zoom and center
      var backButton = L.control({ position: 'topright' });
      backButton.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
        
        var button = L.DomUtil.create('button', 'back-button');
        button.style.backgroundColor = '#fff';
        button.style.border = 'none';
        button.style.padding = '10px';
        button.style.cursor = 'pointer';
        button.innerHTML = 'Back';
        
        L.DomEvent.on(button, 'click', function() {
          // Restore the previous center and zoom level
          map.setView(prevCenter, prevZoom);
        });
        
        div.appendChild(button);
        return div;
      };
      backButton.addTo(map);
    ")
  })
  
  # ==== TAB 2: Accessibility Analysis ====
  
  # --- State Management ---
  postal_code <- reactiveVal(NULL)
  initial_calculation <- reactiveVal(FALSE)
  recalculated <- reactiveVal(FALSE)
  
  current_settings <- reactiveValues(
    travel_time = 15,
    walking_distance = 400,
    waiting_time = 5,
    transport_type = "MRT & Bus"
  )
  
  # --- Reactive State Observers ---
  observeEvent(input$t2_postal_code, {
    postal_code(input$t2_postal_code)
    initial_calculation(FALSE)
    recalculated(FALSE)
  })
  
  observeEvent(input$t2_accessibility_score, {
    req(postal_code())
    initial_calculation(TRUE)
    recalculated(FALSE)
    
    current_settings$travel_time <- 15
    current_settings$walking_distance <- 400
    current_settings$waiting_time <- 5
    current_settings$transport_type <- "MRT & Bus"
  })
  
  observeEvent(input$t2_recalculate, {
    req(postal_code(), initial_calculation())
    
    if (
      current_settings$travel_time != input$t2_travel_time ||
      current_settings$walking_distance != input$t2_walking_distance ||
      current_settings$waiting_time != input$t2_waiting_time ||
      current_settings$transport_type != input$t2_transport_type
    ) {
      recalculated(TRUE)
      
      current_settings$travel_time <- input$t2_travel_time
      current_settings$walking_distance <- input$t2_walking_distance
      current_settings$waiting_time <- input$t2_waiting_time
      current_settings$transport_type <- input$t2_transport_type
    }
  })
  
  active_settings <- reactive({
    if (recalculated()) {
      list(
        travel_time = input$t2_travel_time,
        walking_distance = input$t2_walking_distance,
        waiting_time = input$t2_waiting_time,
        transport_type = input$t2_transport_type
      )
    } else if (initial_calculation()) {
      list(
        travel_time = 15,
        walking_distance = 400,
        waiting_time = 5,
        transport_type = "MRT & Bus"
      )
    } else NULL
  })
  
  # --- Dummy Calculation Logic ---
  nearest_transport <- reactive({
    req(initial_calculation())
    set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
    
    n_bus <- sample(3:5, 1)
    bus_stops <- data.frame(
      Name = paste("Bus Stop", LETTERS[1:n_bus]),
      Distance = round(runif(n_bus, 100, 800), 1),
      Routes = replicate(n_bus, paste(sample(10:199, sample(2:4, 1)), collapse = ", "))
    )
    
    n_mrt <- sample(1:2, 1)
    mrt_stations <- data.frame(
      Name = paste("MRT", c("North-South Line", "East-West Line", "Circle Line")[1:n_mrt]),
      Distance = round(runif(n_mrt, 200, 1500), 1),
      Travel_Time = sample(5:15, n_mrt, replace = TRUE)
    )
    
    list(bus_stops = bus_stops, mrt_stations = mrt_stations)
  })
  
  key_locations <- reactive({
    req(initial_calculation())
    set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
    
    data.frame(
      Location = c("CBD", "Nearest Mall", "Nearest School", "Nearest Hospital", "Nearest Park"),
      Distance_km = round(runif(5, 0.5, 10), 1),
      Travel_Time_mins = sample(5:60, 5, replace = TRUE),
      Transport_Mode = sample(c("MRT", "Bus", "MRT + Bus"), 5, replace = TRUE)
    )
  })
  
  accessibility_scores <- reactive({
    settings <- active_settings()
    req(settings)
    
    set.seed(as.numeric(charToRaw(substr(postal_code(), 1, 6))))
    
    mrt <- round(100 - (settings$travel_time * 1.5) + rnorm(1, 10, 3), 1)
    bus <- round(100 - (settings$walking_distance / 20) + rnorm(1, 5, 2), 1)
    walk <- round(100 - (settings$walking_distance / 10) + rnorm(1, 15, 5), 1)
    congestion <- round(100 - (settings$waiting_time * 3) + rnorm(1, 20, 5), 1)
    
    if (settings$transport_type == "MRT") {
      mrt <- mrt * 1.2; bus <- bus * 0.8
    } else if (settings$transport_type == "Bus") {
      mrt <- mrt * 0.8; bus <- bus * 1.2
    }
    
    list(
      overall = round((mrt * 0.3 + bus * 0.3 + walk * 0.2 + congestion * 0.2), 1),
      mrt = min(max(mrt, 0), 100),
      bus = min(max(bus, 0), 100),
      walk = min(max(walk, 0), 100),
      congestion = min(max(congestion, 0), 100)
    )
  })
  
  # --- Render Outputs for Accessibility Score UI ---
  output$t2_dynamic_score_display <- renderUI({
    if (!initial_calculation()) return(div(style = "font-size: 48px; font-weight: bold; color: #666;", "00.0"))
    score <- accessibility_scores()$overall
    color <- case_when(score >= 80 ~ "#2ecc71", score >= 60 ~ "#f39c12", TRUE ~ "#e74c3c")
    div(style = paste0("font-size: 48px; font-weight: bold; color: ", color, ";"), score)
  })
  
  output$t2_score_interpretation <- renderUI({
    if (!initial_calculation()) {
      return(div(style = "font-size: 16px; margin-top: 10px; color: #666;",
                 "Enter a postal code and click 'Get Accessibility Score' to see results"))
    }
    
    score <- accessibility_scores()$overall
    text <- case_when(
      score >= 80 ~ "Excellent accessibility! This location has great transport options.",
      score >= 60 ~ "Good accessibility. Most amenities are easily reachable.",
      score >= 40 ~ "Moderate accessibility. Some transport options available.",
      TRUE ~ "Poor accessibility. Limited transport options available."
    )
    div(style = "font-size: 16px; margin-top: 10px;", text)
  })
  
  output$t2_mrt_score        <- renderText({ if (!initial_calculation()) "00.0" else accessibility_scores()$mrt })
  output$t2_bus_score        <- renderText({ if (!initial_calculation()) "00.0" else accessibility_scores()$bus })
  output$t2_walk_score       <- renderText({ if (!initial_calculation()) "00.0" else accessibility_scores()$walk })
  output$t2_congestion_score <- renderText({ if (!initial_calculation()) "00.0" else accessibility_scores()$congestion })
  
  output$t2_key_location_times <- renderTable({
    if (!initial_calculation()) return(data.frame(Note = "Click 'Get Accessibility Score' to see travel times"))
    key_locations()
  })
  
  output$t2_nearest_bus_mrt <- renderTable({
    if (!initial_calculation()) return(data.frame(Note = "Click 'Get Accessibility Score' to see nearby transport options"))
    
    transport <- nearest_transport()
    
    bus_df <- transport$bus_stops %>% 
      mutate(Type = "Bus Stop") %>%
      select(Type, Name, Distance, Info = Routes)
    
    mrt_df <- transport$mrt_stations %>% 
      mutate(Type = "MRT Station", Info = paste("Travel time:", Travel_Time, "mins")) %>%
      select(Type, Name, Distance, Info)
    
    bind_rows(bus_df, mrt_df) %>% arrange(Distance)
  })
  
  output$t2_isochrone_map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
    
    if (initial_calculation()) {
      walk_dist <- if (recalculated()) input$t2_walking_distance else 400
      map <- map %>%
        addMarkers(lng = 103.8198, lat = 1.3521, popup = "Approximate location") %>%
        addCircles(lng = 103.8198, lat = 1.3521, radius = walk_dist,
                   color = "#ff7800", fillOpacity = 0.2,
                   popup = paste("Walking distance:", walk_dist, "m"))
    }
    map
  })
  
  # ==== TAB 3: Comparing BTO Estates ====
  
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
  
  # --- Home Tab ---
  # dummy data for BTO locations
  bto_dummy <- data.frame(
    project_name = c("Tampines North Grove", "Woodlands Spring", "Bukit Batok Vista"),
    lat = c(1.3700, 1.4370, 1.3480),
    lng = c(103.9400, 103.7860, 103.7490),
    launch_date = c("Jun 2025", "Sep 2025", "Dec 2025")
  )
  
  # render BTO map
  output$bto_map <- renderLeaflet({
    leaflet(bto_dummy) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        ~lng, ~lat,
        popup = ~paste0("<b>", project_name, "</b><br>Launch Date: ", launch_date)
      ) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  # navigation logic
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
  
})
