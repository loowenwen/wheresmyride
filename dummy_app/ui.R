library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(scales)
source("helpers.R")

shinyUI(
  navbarPage(
    title = div(
      img(src = "logo.png", style = "width: 150px"),
      "Public Transport Accessibility Dashboard"
    ),
    id = "mainNavbar",
    windowTitle = "Accessibility Dashboard",
    
    header = tagList(
      useShinyjs(),
      tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;500;700&display=swap")),
      tags$head(tags$style(HTML("body { font-family: 'Roboto', sans-serif; background-color: #f5f7fa; }
        h1, h2, h3, h4 { color: #1e293b; }
        .panel { border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); padding: 20px; background: white; margin-bottom: 20px; }
        .btn-primary { background-color: #0f172a; border: none; }
        .btn-primary:hover { background-color: #1e40af; }
        .nav-tabs > li > a { font-size: 18px; font-weight: 500; color: #1e293b; }
        .nav-tabs > li.active > a { background-color: #0f172a; color: white; }
        .tab-content { padding-top: 20px; }
      "))),
      tags$head(tags$style(HTML("
      .navbar { min-height: 50px; }
      .navbar-brand { padding-top: 10px; padding-bottom: 10px; }
      .navbar-nav > li > a { padding-top: 20px; padding-bottom: 20px; }
      ")))
    ),
    
    tabPanel("Overview",
             fluidRow(
               column(6,
                      div(class = "panel",
                          textInput("postal_code", "Postal Code", placeholder = "e.g., 123456"),
                          sliderInput("search_radius", "Search Radius (meters)", min = 100, max = 2000, value = 500, step = 100),
                          actionButton("search_location", "Search", class = "btn btn-primary btn-block"),
                          h4("Summary", style = "margin-top: 20px;"),
                          verbatimTextOutput("location_summary")
                      ),
                      div(class = "panel",
                          leafletOutput("location_map", height = 600)
                      )
               ),
               column(6,
                      div(class = "panel",
                          selectInput("density_map_type", "Density Type", choices = c("MRT Stations" = "mrt", "Bus Stops" = "bus")),
                          conditionalPanel(condition = "input.density_map_type == 'mrt'",
                                           leafletOutput("mrt_station_density_map", height = 540)
                          ),
                          conditionalPanel(condition = "input.density_map_type == 'bus'",
                                           leafletOutput("bus_stop_density_map", height = 540)
                          )
                      )
               )
             )
    ),
    
    tabPanel("Accessibility Analysis",
             sidebarLayout(
               sidebarPanel(
                 class = "panel",
                 textInput("t2_postal_code", "Postal Code", placeholder = "e.g., 123456"),
                 actionButton("t2_accessibility_score", "Get Score", class = "btn btn-primary btn-block"),
                 sliderInput("t2_travel_time", "Max Travel Time (min)", min = 0, max = 60, value = 15, step = 1),
                 sliderInput("t2_walking_distance", "Max Walking Distance (m)", min = 0, max = 1000, value = 400, step = 10),
                 sliderInput("t2_waiting_time", "Max Waiting Time (min)", min = 1, max = 60, value = 5, step = 1),
                 selectInput("t2_transport_type", "Transport Mode", choices = c("MRT", "Bus", "MRT & Bus")),
                 actionButton("t2_recalculate", "Recalculate", class = "btn btn-primary btn-block")
               ),
               mainPanel(
                 tabsetPanel(id = "nestedTabs",
                             tabPanel("Accessibility Score",
                                      div(class = "panel",
                                          h3("Overall Score"),
                                          uiOutput("t2_dynamic_score_display"),
                                          uiOutput("t2_score_interpretation"),
                                          fluidRow(
                                            column(3, metric_box("MRT", "t2_mrt_score", color = "#1e40af")),
                                            column(3, metric_box("Bus", "t2_bus_score", color = "#1e40af")),
                                            column(3, metric_box("Walkability", "t2_walk_score", color = "#1e40af")),
                                            column(3, metric_box("Congestion", "t2_congestion_score", color = "#1e40af"))
                                          ),
                                          fluidRow(
                                            column(6, h4("Travel Times"), tableOutput("t2_key_location_times")),
                                            column(6, h4("Nearest Stops"), tableOutput("t2_nearest_bus_mrt"))
                                          )
                                      )
                             ),
                             tabPanel("Travel Time Map",
                                      div(class = "panel",
                                          h3("Isochrone Map"),
                                          leafletOutput("t2_isochrone_map", height = 500)
                                      )
                             )
                 )
               )
             )
    ),
    
    tabPanel("Comparing Transport Accessibility",
             sidebarLayout(
               sidebarPanel(class = "panel",
                            textInput("home_location", "Home Location", placeholder = "e.g., 123456"),
                            textInput("destination", "Destination", placeholder = "e.g., Raffles Place"),
                            actionButton("calculate_route", "Calculate Route", class = "btn btn-primary btn-block")
               ),
               mainPanel(class = "panel",
                         h4("Route Map"),
                         leafletOutput("route_map", height = 500),
                         h4("Steps"),
                         tableOutput("route_steps_table")
               )
             )
    )
  )
)
