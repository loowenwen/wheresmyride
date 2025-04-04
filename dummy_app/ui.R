library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(plotly)
library(sf)
library(scales)
library(shinyjs)
library(shiny)
source("helpers.R") 

shinyUI(
  navbarPage(
    title = div(
      img(src = "logo.png", style = "width: 150px")
    ),
    id = "mainNavbar",
    
    header = tagList(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          body { font-family: 'Times New Roman', serif; background-color: #f5f7fa; }
          h1, h2, h3, h4 { color: #1e293b; }
          .panel { border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); padding: 20px; background: white; margin-bottom: 20px; }
          .btn-primary { background-color: #0f172a; border: none; }
          .btn-primary:hover { background-color: #1e40af; }
          .nav-tabs > li > a { font-size: 32px; font-weight: 500; color: #1e293b; }
          .nav-tabs > li.active > a { background-color: #0f172a; color: white; }
          .tab-content { padding-top: 20px; }
        ")),
        tags$style(HTML("
          .navbar { min-height: 75px; background-color: #219ebc !important; border: none;}
          .navbar-brand { padding-top: 5px; padding-bottom: 5px;}
          .navbar-nav > li > a { padding-top: 30px; padding-bottom: 30px; font-size: 18px; font-weight: bold; color: black !important}
          .navbar-nav > li > a:hover {background-color: #176087 !important; color: black !important;}
          .navbar-nav > li.active > a,
          .navbar-nav > li.active > a:focus,
          .navbar-nav > li.active > a:hover {background-color: #023047 !important; color: white !important;}
        "))
      )
    ),
    
    ## Tab 1
    tabPanel(
      "Overview",
      div(
        style = "height: 100vh; display: flex; flex-direction: column;",  # Full height panel
        fluidRow(
          style = "flex-grow: 1;",  # Make sure the row expands
          
          # Left column: Overview content
          column(
            width = 6,
            div(
              style = "display: flex; flex-direction: column; height: 100%;",
              sidebarLayout(
                sidebarPanel(
                  style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; height: 250px; flex-shrink: 0;", 
                  
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
                  style = "flex-grow: 1; display: flex;",  # Ensure map takes remaining space
                  div(
                    style = "flex-grow: 1; position: relative;",  # Allow Leaflet to expand
                    leafletOutput("location_map", height = "700px")  # Fixed pixel height
                  )
                )
              )
            )
          ),
          
          # Right column: Dropdown + density map
          column(
            width = 6,
            div(
              style = "display: flex; flex-direction: column; height: 100%;",
              
              # Dropdown menu for selecting density map
              div(
                style = "margin-bottom: 10px; height: 80px; flex-shrink: 0;",
                selectInput("density_map_type", "Density Map of:", 
                            choices = c("MRT Stations" = "mrt", "Bus Stops" = "bus"))
              ),
              
              # Map container
              div(
                style = "flex-grow: 1; position: relative;",  # Allow map to expand properly
                conditionalPanel(
                  condition = "input.density_map_type == 'mrt'",
                  leafletOutput("mrt_station_density_map", height = "610px")  # Fixed height
                ),
                conditionalPanel(
                  condition = "input.density_map_type == 'bus'",
                  leafletOutput("bus_stop_density_map", height = "610px")  # Fixed height
                )
              )
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
          
          # Input: Dropdown input for home location
          selectInput("bto_a_postal", "Select BTO A:", 
                      choices = c("Northshore Edge (820123)", "Fernvale Vines (792456)", 
                                  "Tengah Parkview (688901)", "Geylang Meadow (389001)", 
                                  "Queenstown Beacon (149752)")),
          selectInput("bto_b_postal", "Select BTO B:", 
                      choices = c("Northshore Edge (820123)", "Fernvale Vines (792456)", 
                                  "Tengah Parkview (688901)", "Geylang Meadow (389001)", 
                                  "Queenstown Beacon (149752)")),
          
          #Input : Text input for destination.
          textInput("workplace_compare", "Enter Workplace Postal Code:", value = "", placeholder = "e.g., 018989"),
          
          #Input : Select input for transport mode
          selectInput("transport_mode_compare", "Select Transport Mode:", choices = c("Bus", "MRT", "Mixed")),
          
          #Input : Button input to compare
          actionButton("compare_commute", "Estimate Commute Time", class = "btn-primary", style = "width: 100%;"),
          hr(),
          h4("Estimated Commute Time"),
          tableOutput("commute_table_compare")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
          fluidRow(
            column(width = 6,
                   h4("Radar Chart: BTO A"),
                   plotlyOutput("radar_a"),
                   h5("Human Review for BTO A"),
                   verbatimTextOutput("bto_a_review")
            ),
            column(width = 6,
                   h4("Radar Chart: BTO B"),
                   plotlyOutput("radar_b"),
                   h5("Human Review for BTO B"),
                   verbatimTextOutput("bto_b_review")
            )
          )
        )
      )
    ),
    
    
    # tab 3: accessibility analysis
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
    )
    )
)
