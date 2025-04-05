library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(plotly)
library(sf)
library(scales)
library(shiny)
library(shinyjs)
library(shinythemes)
source("global.R") 

shinyUI(
  navbarPage(
    theme = shinytheme("yeti"),
    title = div(
      img(src = "white_logo.png", style = "height: 30px"),
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
                   h5("Transport Heatmap", style = "font-weight: bold"),
                   p("See which regions have the highest density of bus stops and MRT stations.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_commute", label = tagList(
                   icon("location-dot", lib = "font-awesome", style = "font-size: 28px"),
                   h5("Travel Reach & Nearby Transport", style = "font-weight: bold"),
                   p("Discover how far you can travel within 15–45 minutes by public transport, and find the nearest bus stops and MRT stations around your location.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_compare", label = tagList(
                   icon("balance-scale", lib = "font-awesome", style = "font-size: 28px"),
                   h5("BTO Transport Comparison", style = "font-weight: bold"),
                   p("Compare public transport quality across BTO projects based on commute comfort, efficiency, and accessibility to your destination.")
                 ), style = "text-decoration: none")),
                 
                 column(3, actionLink("go_insights", label = tagList(
                   icon("chart-line", lib = "font-awesome", style = "font-size: 28px"),
                   h5("Transport Access Dashboard", style = "font-weight: bold"),
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
               tabPanel(
                 "Transport Heatmap", value = "heatmap",
                 div(
                   style = "height: 100vh; display: flex; flex-direction: column;",
                   fluidRow(
                     style = "flex-grow: 1;",
                     column(6,
                            div(
                              style = "display: flex; flex-direction: column; height: 100%;",
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; height: 250px; flex-shrink: 0;",
                                  textInput("postal_code", "Enter Postal Code", value = "", placeholder = "e.g., 123456"),
                                  sliderInput("search_radius", "Search Radius (meters)", min = 100, max = 2000, value = 500, step = 100),
                                  actionButton("search_location", "Search", class = "btn-primary", style = "width: 100%;"),
                                  h4("Summary Statistics", style = "color: #2C3E50; margin-top: 20px;"),
                                  verbatimTextOutput("location_summary")
                                ),
                                mainPanel(
                                  style = "flex-grow: 1; display: flex;",
                                  div(
                                    style = "flex-grow: 1; position: relative;",
                                    leafletOutput("location_map", height = "700px")
                                  )
                                )
                              )
                            )
                     ),

                     column(6,
                            div(
                              style = "display: flex; flex-direction: column; height: 100%;",
                              div(
                                style = "margin-bottom: 10px; height: 80px; flex-shrink: 0;",
                                selectInput("density_map_type", "Density Map of:", choices = c("MRT Stations" = "mrt", "Bus Stops" = "bus"))
                              ),
                              div(
                                style = "flex-grow: 1; position: relative;",
                                conditionalPanel(
                                  condition = "input.density_map_type == 'mrt'",
                                  leafletOutput("mrt_station_density_map", height = "610px")
                                ),
                                conditionalPanel(
                                  condition = "input.density_map_type == 'bus'",
                                  leafletOutput("bus_stop_density_map", height = "610px")
                                )
                              )
                            )
                     )
                   )
                 )
               ),
               
               # --- Travel Reach & Nearby Transport ---
               tabPanel(
                 "Travel Reach & Nearby Transport", value = "commute",
                 h3("Isochrone Visualization", style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: #023047;"),
                 leafletOutput("t2_isochrone_map", height = 500)
               )
    ),
    
    # --- BTO Transport Comparison ---
    tabPanel("BTO Transport Comparison", value = "compare",
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
    
    # --- BTO Transport Comparison ---
    tabPanel("Transport Access Dashboard", value = "insights",
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
                       tabPanel("Accessibility Score",
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
    
    # --- Project Information ---
    tabPanel("Project Information",
             fluidPage(
               
               # --- Team Members ---
               h3(tagList(icon("users", lib = "font-awesome"), " Team Members")),
               
               fluidRow(
                 column(2, tags$div(
                   img(src = "wenwen.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Loo Wen Wen"),
                   p("Frontend Developer")
                 )),
                 column(2, tags$div(
                   img(src = "khalisah.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Khalisah Binte Shari"),
                   p("Frontend Developer")
                 )),
                 column(2, tags$div(
                   img(src = "amali.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Mohamed Amali Bin M Akbar"),
                   p("Frontend Developer")
                 )),
                 column(2, tags$div(
                   img(src = "mili.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Mili Vinod"),
                   p("Backend Developer")
                 )),
                 column(2, tags$div(
                   img(src = "ryan.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Ryan Chow Yi Feng"),
                   p("Backend Developer")
                 )),
                 column(2, tags$div(
                   img(src = "jinpeng.jpg", width = "100%", style = "border-radius: 50%;"),
                   h5("Oon Jin Peng"),
                   p("Backend Developer")
                 ))
               ),
               br(),
               
               # --- Data Sets & APIs ---
               h3(tagList(icon("database", lib = "font-awesome"), " Data Sets & APIs")),
               fluidRow(
                 column(4, tags$div(
                   img(src = "logo_lta.png", height = "60px", style = "margin-bottom: 10px;"),
                   p(
                     tags$b("LTA Data Mall: "),
                     tags$a(href = "https://datamall.lta.gov.sg/content/datamall/en.html", 
                            "Official Portal", target = "_blank"),
                     br(),
                     "Provides access to MRT station locations, bus stop coordinates, and public transport routes through both real-time APIs and static datasets."
                   ))
                 ),
                 
                 column(4, tags$div(
                   img(src = "logo_hdb.png", height = "60px", style = "margin-bottom: 10px;"),
                   p(
                     tags$b("Housing Development Board: "),
                     tags$a(href = "https://homes.hdb.gov.sg/home/landing", 
                            "HDB Flat Portal", target = "_blank"),
                     br(),
                     "Used to extract information on upcoming BTO projects featured in the application."
                   ))
                 ),
                 
                 column(4, tags$div(
                   img(src = "logo_onemap.png", height = "60px", style = "margin-bottom: 10px;"),
                   p(
                     tags$b("OneMap API: "),
                     tags$a(href = "https://www.onemap.gov.sg/apidocs/", 
                            "OneMap API Documentation", target = "_blank"),
                     br(),
                     "Extensively used for its API functionalities including route planning, reverse geocoding, and retrieving planning area boundaries."
                   ))
                 )),
               br(),
               
               # --- Repository ---
               h3(tagList(icon("laptop", lib = "font-awesome"), " Repository & Contact")),
               p("This project is open source and the full source code is available on GitHub. You can also explore the live app online."),
               
               tags$ul(
                 tags$li(
                   tags$b(tagList(icon("github", lib = "font-awesome"), " GitHub Repository:")),
                   tags$a(href = "https://github.com/loowenwen/wheresmyride", 
                          "View on GitHub", target = "_blank")
                 ),
                 tags$li(
                   tags$b(tagList(icon("external-link-alt", lib = "font-awesome"), " Live App (shinyapps.io):")),
                   tags$a(href = "https://loowenwen.shinyapps.io/wheresmyride/", 
                          "Launch Where's My Ride", target = "_blank")
                 )
               ),
               br(),
               
               # --- Academic Acknowledgement ---
               fluidRow(
                 column(12, 
                        tags$div(style = "background-color: #e9f0f5; padding: 10px; text-align: center;",
                                 h4(tagList(icon("university", lib = "font-awesome"), " Academic Acknowledgement")),
                                 p("This project was developed as part of the course"),
                                 p("DSE3101 - Practical Data Science for Economics", style = "font-weight: bold"), 
                                 p("National University of Singapore (NUS), Faculty of Science"),
                                 p("Academic Year 2024/25, Semester 2")
                                 ))
               )
             )
    ),
    
    # --- Global UI Settings ---
    header = tagList(
      useShinyjs(),
      tags$head(
        tags$style(HTML("body { font-family: 'Times New Roman', serif; } 
                        h1, h2, h3, h4, h5, h6, p, div, span { font-family: 'Times New Roman', Times, serif; }")),
        )
    )
  )
)
