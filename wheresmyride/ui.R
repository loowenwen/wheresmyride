library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(sf)
library(scales)
source("helpers.R") 

shinyUI(
  fluidPage(useShinyjs(),
    
    # application title
    titlePanel(
      div(
        # main title
        h1("Public Transport Accessibility Dashboard",
           style = "color: #023047; font-weight: bold; text-align: center;
           font-size: 40px; margin-bottom: 5px; font-family: 'Times New Roman', serif;"),
        
        # subtitle
        h4("Helping Future Homeowners Make Data-Driven Location Choices", 
           style = "color: #023047; text-align: center; font-weight: normal; 
                font-size: 18px; margin-bottom: 20px;
           font-family: 'Times New Roman', serif;"),
        
        # styling wrapper
        style = "background-color: #8ecae6; padding: 20px; border-radius: 15px; 
             box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"
      )
    ),
    
    # tabbed layout for multiple features
    tabsetPanel(
      
      # tab 1: overview
      tabPanel(
        "Overview",
        tabsetPanel(
          # First subtab: Overview (current code)
          tabPanel(
            "Overview",
            sidebarLayout(
              sidebarPanel(
                style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px;",
                
                # Input: Text input for postal code
                textInput("postal_code", "Enter Postal Code", 
                          value = "", placeholder = "e.g., 123456"),
                
                # Input: Slider to select search radius (in meters)
                sliderInput("search_radius", "Search Radius (meters)", 
                            min = 100, max = 2000, value = 500, step = 100),
                
                # Input: Button to search
                actionButton("search_location", "Search", 
                             class = "btn-primary", style = "width: 100%;"),
                
                # Output: Summary statistics
                h4("Summary Statistics", style = "color: #2C3E50; margin-top: 20px;"),
                verbatimTextOutput("location_summary")
              ),
              mainPanel(
                leafletOutput("location_map", height = 600)  # Show the map here
              )
            )
          ),
          
          # Second subtab: Bus Stop Density Ranking Map
          tabPanel(
            "Bus Stop Density",
            leafletOutput("bus_stop_ranking_map", height = 600)  # Bus stop ranking map output
          ),
          
          # Third subtab: MRT Station Density Ranking Map
          tabPanel(
            "MRT Station Density",
            leafletOutput("mrt_stop_proximity_ranking_map", height = 600)  # MRT station ranking map output
          )
        )
      ),
      
      
      # tab 2: accessibility analysis
      tabPanel("Accessibility Analysis",
               # add margin on top of sidebar panel and main panel
               style = "margin-top: 15px;", 
               
               sidebarLayout(
                 sidebarPanel( 
                   style = "background-color: #F8F9FA; 
                   font-family: 'Times New Roman', serif; color: #023047;",
                   
                   h4("User Inputs",
                      style = "font-size: 18px; font-weight: bold;"),
                   # input postal code
                   textInput("t2_postal_code", "Enter Postal Code:", placeholder = "e.g., 123456"),
                   # action button to calculate accessibiltiy score
                   actionButton("t2_accessibility_score", "Get Accessibility Score"),
                   
                   h4("Customize Accessibility Model",
                      style = "font-size: 18px; font-weight: bold;
                      margin-top: 20px;"),
                   # input maximum travel time to nearest mrt
                   sliderInput("t2_travel_time", 
                               "Max Travel Time to MRT (mins):", 
                               min = 0, max = 60, value = 15, step = 1),
                   # input maximum walking distance to nearest bus/mrt
                   sliderInput("t2_walking_distance", 
                               "Max Walking Distance (m):", 
                               min = 0, max = 1000, value = 400, step = 10),
                   # input maximum waiting time for transport
                   sliderInput("t2_waiting_time", 
                               "Max Waiting Time (mins):", 
                               min = 1, max = 60, value = 5, step = 1),
                   # input preferred mode of transport
                   selectInput("t2_transport_type", 
                               "Preferred Transport Mode:", 
                               choices = c("MRT", "Bus", "MRT & Bus")),
                   # action button to recalculate accessibility score
                   actionButton("t2_recalculate", "Recalculate with New Settings")
                 ),
                 
                 mainPanel(
                   div(id = "nestedTabs",
                       tabsetPanel(
                         tabPanel("Accessibility Score", 
                                  div(class = "score-section", style = "font-family: 'Times New Roman', serif; 
                                      color: #023047;",
                                      h3("Overall Accessibility Score",
                                         style = "font-size: 24px; font-weight: bold;"),
                                      
                                      # dynamic score display (changes color based on value)
                                      uiOutput("t2_dynamic_score_display"),
                                      
                                      # score interpretation (e.g., "Excellent", "Poor")
                                      uiOutput("t2_score_interpretation"),
                                  ),
                                  
                                  br(),
                                  
                                  # metric boxes
                                  fluidRow(
                                    column(3, metric_box("🚆 MRT Score", "t2_mrt_score", color = "#ffb703")),
                                    column(3, metric_box("🚌 Bus Score", "t2_bus_score", color = "#ffb703")),
                                    column(3, metric_box("🚶 Walkability", "t2_walk_score", color = "#ffb703")),
                                    column(3, metric_box("⏳ Congestion", "t2_congestion_score", color = "#ffb703"))
                                  ),
                                  
                                  # travel time insights
                                  fluidRow(
                                    column(7, h3("Travel Time to Key Locations", style = "font-size: 24px; font-weight: bold;
                                                 font-family: 'Times New Roman', serif;"), 
                                           tableOutput("t2_key_location_times")),
                                    column(5, h3("Nearest Bus Stops and MRT Stations", style = "font-size: 24px; font-weight: bold;
                                                 font-family: 'Times New Roman', serif;"),
                                           tableOutput("t2_nearest_bus_mrt"))
                                  ),
                         ),
                         
                         tabPanel("Travel Time Map", 
                                  h3("Isochrone Visualization",
                                     style = "font-size: 24px; font-weight: bold;
                                     font-family: 'Times New Roman', serif; 
                                     color: #023047;"),
                                  leafletOutput("t2_isochrone_map", height = 500))
                       )
                   )
                 )
               )
      ),
      
      
      
      # tab 3: comparing transport accessibility
      tabPanel(
        "Comparing Transport Accessibility",
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px;",
            
            # Input: Text input for home location
            textInput("home_location", "Enter Home Location (e.g., Postal Code)", 
                      value = "", placeholder = "e.g., 123456"),
            
            # Input: Text input for destination
            textInput("destination", "Enter Destination (e.g., Workplace)", 
                      value = "", placeholder = "e.g., Raffles Place"),
            
            # Input: Button to calculate optimal route
            actionButton("calculate_route", "Calculate Optimal Route", 
                         class = "btn-primary", style = "width: 100%;")
          ),
          
          # Main panel for displaying outputs
          mainPanel(
            style = "background-color: #FFFFFF; padding: 20px; border-radius: 10px;",
            
            # Output: Leaflet map with optimal route
            h4("Optimal Route Map", style = "color: #2C3E50;"),
            leafletOutput("route_map", height = "500px"),
            
            # Output: Table of route steps
            h4("Route Steps", style = "color: #2C3E50; margin-top: 20px;"),
            tableOutput("route_steps_table")
            )
          )
        )
      ),
    
    
    
    tags$head(
      tags$style(HTML("
    /* apply custom styling to all tab labels */
    .nav-tabs > li > a {
      color: #023047;  
      font-family: 'Times New Roman', serif;
      font-size: 20px;
      font-weight: bold;
      text-align: center;
    }

    /* change tab appearance when hovered */
    .nav-tabs > li > a:hover {
      background-color: #8ecae6;
      color: white;
    }

    /* force active tab to retain color */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #023047 ; 
      color: white;
    }

    /* custom styling for nested tab labels */
    #nestedTabs .nav-tabs > li > a {
      color: #fb8500;
      font-size: 16px;
    }

    /* custom hover for nested tabs */
    #nestedTabs .nav-tabs > li > a:hover {
      background-color: #ffb703;
      color: white;
    }

    /* custom active tab for nested tabs */
    #nestedTabs .nav-tabs > li.active > a {
      background-color: #fb8500;
      color: white;
    }
  ")))
  )
)
