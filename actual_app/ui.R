library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(plotly)
library(sf)
library(scales)
library(shinyjs)
library(shiny)
source("global.R") 

shinyUI(
  navbarPage(
    theme = shinytheme("yeti"),
    title = div(
      img(src = "logo.png", style = "height: 30px"),
    ),
    id = "mainTabs",
    windowTitle = "Where's My Ride",
    
    # --- Home ---
    tabPanel("Home",
             fluidPage(
               fluidRow(
                 column(12,
                        h2("Discover the Best BTO Locations with Seamless Public Transport Access"),
                        p("Compare neighborhoods using live and historical data to find the perfect home that fits your lifestyle and daily commute.")
                 )
               ),
               br(),
              
               fluidRow(
                 column(3, actionLink("go_heatmap", label = tagList(
                   icon("map", lib = "font-awesome", style = "font-size: 28px"),
                   h5("Transport Heatmap"),
                   p("See which regions have the highest density of bus stops and MRT stations.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_commute", label = tagList(
                   icon("location-dot", lib = "font-awesome", style = "font-size: 28px"),
                   h5("Travel Reach & Nearby Transport"),
                   p("Discover how far you can travel within 15–45 minutes by public transport, and find the nearest bus stops and MRT stations around your location.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_compare", label = tagList(
                   icon("balance-scale", lib = "font-awesome", style = "font-size: 28px"),
                   h5("BTO Transport Comparison"),
                   p("Compare public transport quality across BTO projects based on commute comfort, efficiency, and accessibility to your destination.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_insights", label = tagList(
                   icon("chart-line", lib = "font-awesome", style = "font-size: 28px"),
                   h5("Transport Access Dashboard"),
                   p("Get personalized insights into public transport accessibility by postal code. View model-based scores, congestion trends, and identify service gaps near your location.")
                 ), style = "text-decoration: none"))
               ),
               br(),
               
               fluidRow(
                 column(12,
                        h3("Explore Upcoming BTO Projects and Transport Connectivity"),
                        p("Discover new BTO launches and examine their accessibility to nearby MRT stations and bus stops. Use the interactive map below to zoom into each estate and visualize surrounding transport nodes."),
                        leafletOutput("bto_map", height = "400px"),
                 )
               ),
               br(),
               
               fluidRow(
                 column(12, tags$div(style = "background-color: #e9f0f5; padding: 10px; text-align: center;",
                                     h3("Ready to explore your ideal BTO location?"),
                                     actionButton("go_to_map", "Explore Now", class = "btn btn-primary btn-lg")))
               )
             )
    ),
    
    # --- Explore ---
    navbarMenu("Explore",
               
               # --- Transport Heatmap ---
              tags$head(
                 tags$style(HTML("
                 html, body {
                 height: 100%;
                 margin: 0;
                 }
                 .tab-container { 
                 height: 100%;
                 display: flex;
                 flex-direction: column;
                 }
                 .dropdown-container {
                 display: flex;
                 justify-content: center;
                 margin: 0;
                 flex-direction: column; 
                 align-items: center;
                 }
                 .dropdown-container label {
                 font-size: 20px;
                 }
                 .map-container {
                 height: 100%;
                 margin-top: 20px; 
                 width: 100%;
                 display: flex;
                 }"))
               ),
               
               tabPanel(
                 "Transport Heatmap", value = "heatmap",
                 
                 # Dropdown for map type selection
                 div(class = "dropdown-container",
                     selectInput("mapType", "Density map of:", 
                                 choices = c("MRT Stations", "Bus Stops"),
                                 selected = "MRT Stations")
                 ),
                 
                 # Conditional rendering of maps below the dropdown
                 conditionalPanel(
                   condition = "input.mapType == 'MRT Stations'", 
                   div(class = "map-container", 
                       leafletOutput("mrt_station_density_map", height = 700))
                 ),
                 
                 conditionalPanel(
                   condition = "input.mapType == 'Bus Stops'", 
                   div(class = "map-container", 
                       leafletOutput("bus_stop_density_map", height = 700))
                 )
               ),
               
               # --- Travel Reach & Nearby Transport ---
               tabPanel(
                 "Travel Reach & Nearby Transport", value = "commute",
                 h3("Isochrone Visualization", style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: #023047;"),
                 leafletOutput("t2_isochrone_map", height = 500)
               )
    ),
    
      
    
    tabPanel("Comparing Transport Accessibility", value = "compare",
             sidebarLayout(
               sidebarPanel(style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px;",
                            selectInput("bto_a_postal", "Select BTO A:", choices = c("Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)", "Geylang Meadow (389001)", "Queenstown Beacon (149752)")),
                            selectInput("bto_b_postal", "Select BTO B:", choices = c("Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)", "Geylang Meadow (389001)", "Queenstown Beacon (149752)")),
                            textInput("workplace_compare", "Enter Workplace Postal Code:", value = "", placeholder = "e.g., 018989"),
                            selectInput("transport_mode_compare", "Select Transport Mode:", choices = c("Bus", "MRT", "Mixed")),
                            actionButton("compare_commute", "Estimate Commute Time", class = "btn-primary", style = "width: 100%;"),
                            hr(),
                            h4("Estimated Commute Time"),
                            tableOutput("commute_table_compare")
               ),
               mainPanel(
                 fluidRow(
                   column(6, h4("Radar Chart: BTO A"), plotlyOutput("radar_a"), h5("Human Review for BTO A"), verbatimTextOutput("bto_a_review")),
                   column(6, h4("Radar Chart: BTO B"), plotlyOutput("radar_b"), h5("Human Review for BTO B"), verbatimTextOutput("bto_b_review"))
                 )
               )
             )
    ),
    
    tabPanel("Accessibility Analysis", style = "margin-top: 15px;",
             sidebarLayout(
               sidebarPanel(style = "background-color: #F8F9FA; font-family: 'Times New Roman', serif; color: #023047;",
                            h4("User Inputs", style = "font-size: 18px; font-weight: bold;"),
                            textInput("t2_postal_code", "Enter Postal Code:", placeholder = "e.g., 123456"),
                            actionButton("t2_accessibility_score", "Get Accessibility Score"),
                            h4("Customize Accessibility Model", style = "font-size: 18px; font-weight: bold; margin-top: 20px;"),
                            sliderInput("t2_travel_time", "Max Travel Time to MRT (mins):", min = 0, max = 60, value = 15, step = 1),
                            sliderInput("t2_walking_distance", "Max Walking Distance (m):", min = 0, max = 1000, value = 400, step = 10),
                            sliderInput("t2_waiting_time", "Max Waiting Time (mins):", min = 1, max = 60, value = 5, step = 1),
                            selectInput("t2_transport_type", "Preferred Transport Mode:", choices = c("MRT", "Bus", "MRT & Bus")),
                            actionButton("t2_recalculate", "Recalculate with New Settings")
               ),
               mainPanel(
                 div(id = "nestedTabs",
                     tabsetPanel(
                       tabPanel("Accessibility Score", value = "insights",
                                div(class = "score-section", style = "font-family: 'Times New Roman', serif; color: #023047;",
                                    h3("Overall Accessibility Score", style = "font-size: 24px; font-weight: bold;"),
                                    uiOutput("t2_dynamic_score_display"),
                                    uiOutput("t2_score_interpretation")
                                ),
                                br(),
                                fluidRow(
                                  column(3, metric_box("\ud83d\ude86 MRT Score", "t2_mrt_score", color = "#ffb703")),
                                  column(3, metric_box("\ud83d\ude8c Bus Score", "t2_bus_score", color = "#ffb703")),
                                  column(3, metric_box("\ud83d\udeb6 Walkability", "t2_walk_score", color = "#ffb703")),
                                  column(3, metric_box("\u23f3 Congestion", "t2_congestion_score", color = "#ffb703"))
                                ),
                                fluidRow(
                                  column(7, h3("Travel Time to Key Locations", style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif;"), 
                                         tableOutput("t2_key_location_times")),
                                  column(5, h3("Nearest Bus Stops and MRT Stations", style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif;"),
                                         tableOutput("t2_nearest_bus_mrt"))
                                )
                       ),
                     )
                 )
               )
             )
    ),
    
    header = tagList(
      useShinyjs(),
      tags$head(tags$style(HTML("body { font-family: 'Times New Roman', serif; } h1, h2, h3, h4, h5, h6, p, div, span { font-family: 'Times New Roman', Times, serif; }")))
    )
  )
)
