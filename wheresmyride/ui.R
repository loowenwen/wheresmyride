library(shiny)

shinyUI(
  fluidPage(
    
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
          
          # Main panel for displaying outputs
          mainPanel(
            style = "background-color: #FFFFFF; padding: 20px; border-radius: 10px;",
            
            # Output: Leaflet map
            h4("Nearby Bus Stops and MRT Stations", style = "color: #2C3E50;"),
            leafletOutput("location_map", height = "500px")
          )
        )
      ),
      
      # tab 2: accessibility analysis
      tabPanel(
        "Accessibility Analysis",
        sidebarLayout(
          sidebarPanel(
            # Section for personalized travel time map
            h4("Personalized Travel Time Map"),
            textInput("location", "Enter Home Location (Postal Code / MRT Station):"),
            numericInput("time_limit", "Select Time Limit (in minutes):", value = 30, min = 1, max = 60, step = 5),
            selectInput("transport_mode", "Choose Transport Mode:", choices = c("MRT", "Bus", "Mixed-mode")),
            actionButton("generate_map", "Generate Map"),
            
            hr(),
            
            # Section for commute time estimator
            h4("Commute Time Estimator"),
            textInput("home_location", "Home Location (BTO Estate):"),
            textInput("workplace", "Workplace / Destination:"),
            actionButton("estimate_commute", "Estimate Commute"),
            
            hr(),
            
            # Section for specific postal code accessibility scores
            h4("Postal Code Accessibility Score"),
            textInput("postal_code", "Enter Postal Code:"),
            actionButton("get_score", "Get Accessibility Score"),
            
            hr(),
            
            # Section for dynamic MLR inputs
            h4("Adjust MLR Inputs"),
            sliderInput("mrt_weight", "MRT Importance (0-100):", min = 0, max = 100, value = 50),
            sliderInput("bus_weight", "Bus Importance (0-100):", min = 0, max = 100, value = 50),
            sliderInput("congestion_weight", "Peak-Hour Congestion Impact (0-100):", min = 0, max = 100, value = 50),
            sliderInput("walking_time_weight", "Walking Time from Nearest Stop (0-100):", min = 0, max = 100, value = 50),
            actionButton("recalculate_score", "Recalculate Accessibility Score")
          ),
          
          mainPanel(
            # Display map for travel time visualization
            leafletOutput("travel_time_map"),
            
            hr(),
            
            # Display estimated commute time results
            h4("Estimated Commute Time"),
            verbatimTextOutput("commute_time_result"),
            
            hr(),
            
            # Display accessibility score and transport options
            h4("Accessibility Score & Transport Options"),
            verbatimTextOutput("accessibility_score_result"),
            verbatimTextOutput("last_mile_info")
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
    
    # add custom CSS to change tab labels
    tags$head(
      tags$style(HTML("
    .nav-tabs > li > a {
      color: #023047 !important;  
      font-family: 'Times New Roman', serif !important;
      font-size: 20px !important;
      font-weight: bold !important;
      text-align: center;
      background-color: white !important; /* Light blue background */
      border-radius: 5px !important; /* Rounded tabs */
    }

    /* change tab appearance when hovered */
    .nav-tabs > li > a:hover {
      background-color: #8ecae6 !important;
      color: white !important;
    }

    /* force active tab to retain color instead of turning grey */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #023047 !important; 
      color: white !important;
      border: none !important;
    }
  ")))
  )
)
