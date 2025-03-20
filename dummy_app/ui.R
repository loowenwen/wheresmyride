library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)  # For professional themes

# Define UI for the application
shinyUI(
  fluidPage(
    # Apply a professional theme
    theme = shinytheme("flatly"),  # Other options: "cerulean", "cosmo", "paper"
    
    # Application title
    titlePanel(
      h1("Singapore Public Transport Connectivity Analysis", 
         style = "color: #2C3E50; font-weight: bold; text-align: center;")
    ),
    
    # Tabbed layout for multiple features
    tabsetPanel(
      # Tab 1: Location Search
      tabPanel(
        "Location Search",
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
      
      # Tab 2: Accessibility Analysis
      tabPanel(
        "Accessibility Analysis",
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px;",
            
            # Input: Dropdown to select region
            selectInput("region", "Select Planning Region", 
                        choices = c("Central", "East", "North", "North-East", "West"),
                        selected = "Central"),
            
            # Input: Slider to select time of day
            sliderInput("time", "Select Time of Day", 
                        min = 0, max = 23, value = 12, step = 1),
            
            # Input: Checkbox to toggle between bus and MRT accessibility
            checkboxGroupInput("transport_mode", "Transport Mode", 
                               choices = c("Bus", "MRT"), selected = c("Bus", "MRT")),
            
            # Output: Text summary of accessibility
            h4("Accessibility Summary", style = "color: #2C3E50; margin-top: 20px;"),
            verbatimTextOutput("accessibility_summary"),
            
            # Output: Text summary of travel time
            h4("Travel Time Summary", style = "color: #2C3E50; margin-top: 20px;"),
            verbatimTextOutput("travel_time_summary")
          ),
          
          # Main panel for displaying outputs
          mainPanel(
            style = "background-color: #FFFFFF; padding: 20px; border-radius: 10px;",
            
            # Output: Bar plot of accessibility scores
            h4("Accessibility Scores by Region", style = "color: #2C3E50;"),
            plotlyOutput("accessibility_plot", height = "300px"),
            
            # Output: Table of optimal route details
            h4("Optimal Route Details", style = "color: #2C3E50; margin-top: 20px;"),
            tableOutput("optimal_route_table")
          )
        )
      ),
      
      # Tab 3: Optimal Route Planner
      tabPanel(
        "Optimal Route Planner",
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
    )
  )
)