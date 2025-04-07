library(leaflet)
library(plotly)
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
               
               tabPanel(
                 "Travel Reach & Nearby Transport", value = "commute",
                 sidebarLayout(
                   sidebarPanel(
                     h4(tagList(icon("location-dot", lib = "font-awesome"), " Location Input"), style = "font-weight: bold"),
                     textInput("t2_postal_code", "Enter Postal Code:",
                               placeholder = "e.g., 123456"),
                     actionButton("t2_show_commute_map", "Show Map", icon = icon("map-location-dot")),
                     br(),
                   ),
                   
                   mainPanel(
                     h3(
                       tagList(icon("traffic-light", lib = "font-awesome"), " Overall Accessibility Score"),
                       class = "fw-bold"
                     ),
                     uiOutput("t4_score_display"),
                     uiOutput("t4_score_interpretation"),
                     tags$p(
                       class = "text-muted",
                       "This score gives an overall measure of accessibility based on four key factors: MRT access, bus connectivity, walkability, and congestion. 
                   Each factor contributes equally by default (25%), but you can adjust their importance based on your preferences on the left"
                     ),
                     br(),
                     
                     # --- Travel Details Section ---
                     h3(
                       tagList(icon("clock", lib = "font-awesome"), "Travel Details"),
                       class = "fw-bold"
                     ),
                     
                     h3("Isochrone Visualization", 
                        style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: #023047;"),
                     leafletOutput("t2_isochrone_map", height = 500),
                     br(),
                     fluidRow(
                       column(
                         6,
                         h4("Nearby MRT Stations", style = "color: #219EBC; font-weight: bold;"),
                         tableOutput("t2_nearby_mrt_table")
                       ),
                       column(
                         6,
                         h4("Nearby Bus Stops", style = "color: #219EBC; font-weight: bold;"),
                         tableOutput("t2_nearby_bus_table")
                       )
                     )
                   )
                 )
               )
               
    ),
               
    # --- BTO Transport Comparison ---
    tabPanel("BTO Transport Comparison", value = "compare",
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px;",
                 
                 selectInput("bto_a_postal", "Select BTO A:", choices = c(
                   "Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)",
                   "Geylang Meadow (389001)", "Queenstown Beacon (149752)"
                 )),
                 selectInput("bto_b_postal", "Select BTO B:", choices = c(
                   "Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)",
                   "Geylang Meadow (389001)", "Queenstown Beacon (149752)"
                 )),
                 selectInput("bto_c_postal", "Select BTO C:", choices = c(
                   "Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)",
                   "Geylang Meadow (389001)", "Queenstown Beacon (149752)"
                 )),
                 selectInput("bto_d_postal", "Select BTO D:", choices = c(
                   "Northshore Edge (820123)", "Fernvale Vines (792456)", "Tengah Parkview (688901)",
                   "Geylang Meadow (389001)", "Queenstown Beacon (149752)"
                 )),
                 
                 tags$hr(),
                 tags$h4(
                   tagList(
                     "Preferred Travel Time",
                     HTML('&nbsp;'),
                     tags$i(
                       class = "fas fa-circle-question text-muted",
                       style = "cursor: pointer;",
                       title = "Choose when you usually commute. This affects how frequent and crowded the transport options are.",
                       `data-bs-toggle` = "tooltip",
                       `data-bs-placement` = "right"
                     )
                   )
                 ),
                 radioButtons("bto_travel_time_pref", label = NULL,
                                    choices = c(
                                      "AM Peak (630 - 830am)" = "AM_peak",
                                      "AM Off-Peak (8.31am - 4.59pm)" = "AM_offpeak",
                                      "PM Peak (5–7pm)" = "PM_peak",
                                      "PM Off-Peak (7pm onwards)" = "PM_offpeak"
                                    ),
                                    selected = "AM_peak"
                 )
               ),
               
               
               mainPanel(
                 fluidRow(
                   column(6, h4("Radar Chart: BTO A"), plotlyOutput("radar_a")),
                   column(6, h4("Radar Chart: BTO B"), plotlyOutput("radar_b"))
                 ),
                 fluidRow(
                   column(6, h4("Radar Chart: BTO C"), plotlyOutput("radar_c")),
                   column(6, h4("Radar Chart: BTO D"), plotlyOutput("radar_d"))
                 )
               )
             )
    ),
    
    # --- Transport Access Dashboard ---
    tabPanel("Transport Access Dashboard", value = "insights",
             sidebarLayout(
               sidebarPanel(
                 h4(tagList(icon("location-dot", lib = "font-awesome"), " Location Input"), style = "font-weight: bold"),
                 textInput("t4_postal_code", "Enter Postal Code:", placeholder = "e.g., 123456"),
                 actionButton("t4_get_score", "Get Accessibility Score", icon = icon("search")),
                 
                 hr(),
                 h4(tagList(icon("gears", lib = "font-awesome"), " Customize Accessibility Model"), style = "font-weight: bold"),
                 
                 tags$div(
                   class = "form-group",
                   tags$label(
                     "Nearby Radius (meters)",
                     HTML('&nbsp;'),
                     tags$i(
                       class = "fas fa-circle-question text-muted",
                       style = "cursor: pointer;",
                       title = "Set the maximum distance you're willing to walk to reach MRT stations or bus stops. This determines which nearby options are included when calculating the component scores.",
                       `data-bs-toggle` = "tooltip",
                       `data-bs-placement` = "right"
                     )
                   ),
                   numericInput("t4_nearby_radius", label = NULL, value = 500, min = 100, max = 2000, step = 50)
                 ),
                 
                 tags$div(
                   class = "form-group",
                   tags$label(
                     "Preferred Travel Time",
                     HTML('&nbsp;'),
                     tags$i(
                       class = "fas fa-circle-question text-muted",
                       style = "cursor: pointer;",
                       title = "Choose when you usually commute. This helps assess how crowded buses and trains are at that time.",
                       `data-bs-toggle` = "tooltip",
                       `data-bs-placement` = "right"
                     )
                   ),
                   checkboxGroupInput("t4_travel_time_preference", label = NULL,
                                      choices = c(
                                        "AM Peak (6–9am)" = "AM_peak",
                                        "AM Off-Peak (5am & 9am–5pm)" = "AM_offpeak",
                                        "PM Peak (5–7pm)" = "PM_peak",
                                        "PM Off-Peak (7pm–2am)" = "PM_offpeak"
                                      ),
                                      selected = "AM_peak")
                 ),
                 
                 tags$h5(
                   class = "fw-bold",
                   tagList(
                     "Accessibility Score Weights",
                     HTML('&nbsp;'),  # spacing
                     tags$i(
                       class = "fas fa-circle-question text-muted",
                       style = "cursor: pointer;",
                       title = "By default, each component (MRT, Bus, Walkability, Congestion) contributes 25% to the overall score. You can adjust these weights based on what matters most to you.",
                       `data-bs-toggle` = "tooltip",
                       `data-bs-placement` = "right"
                     )
                   )
                 ),
                 helpText("You can adjust how much each factor matters to you. The app will automatically normalize the weights."),
                 sliderInput("t4_mrt", "MRT Importance", min = 0, max = 100, value = 25),
                 sliderInput("t4_bus", "Bus Importance", min = 0, max = 100, value = 25),
                 sliderInput("t4_walk", "Walkability Importance", min = 0, max = 100, value = 25),
                 sliderInput("t4_congestion", "Congestion Importance", min = 0, max = 100, value = 25),
                 
                 actionButton("t4_recalculate", "Recalculate with New Settings", icon = icon("sync")),
               ),
               
               mainPanel(
                 # --- Overall Score Section ---
                 h3(
                   tagList(icon("traffic-light", lib = "font-awesome"), " Overall Accessibility Score"),
                   class = "fw-bold"
                 ),
                 uiOutput("t4_score_display"),
                 uiOutput("t4_score_interpretation"),
                 tags$p(
                   class = "text-muted",
                   "This score gives an overall measure of accessibility based on four key factors: MRT access, bus connectivity, walkability, and congestion. 
                   Each factor contributes equally by default (25%), but you can adjust their importance based on your preferences on the left"
                 ),
                 br(),
                 
                 # --- Component Scores Section ---
                 h4(
                   tagList(icon("magnifying-glass", lib = "font-awesome"), " Component Scores"),
                   class = "fw-bold"
                 ),
                 fluidRow(
                   column(3,
                          h5(tagList(icon("train-subway", lib = "font-awesome"), " MRT Score")),
                          uiOutput("t4_mrt_score"),
                          tags$p(class = "text-muted",
                                 "This score reflects your access to MRT stations. It considers how many stations are within your preferred walking distance, how close they are on average, and how many different MRT lines are available nearby.")
                   ),
                   column(3,
                          h5(tagList(icon("bus-simple", lib = "font-awesome"), " Bus Score")),
                          uiOutput("t4_bus_score"),
                          tags$p(class = "text-muted",
                                 "This score captures your access to bus services. It’s based on the number of bus stops near you, how close they are on average, and how many different bus services are available within your preferred radius.")
                   ),
                   column(3,
                          h5(tagList(icon("person-walking", lib = "font-awesome"), " Walkability Score")),
                          uiOutput("t4_walk_score"),
                          tags$p(class = "text-muted",
                                 "This score shows how easily you can walk to nearby MRT stations and bus stops. It’s calculated based on the average walking distance to each within your selected radius.")
                   ),
                   column(3,
                          h5(tagList(icon("hourglass", lib = "font-awesome"), " Congestion Score")),
                          uiOutput("t4_congestion_score"),
                          tags$p(class = "text-muted",
                                 "This score measures public transport crowd levels during your preferred travel time. It factors in MRT crowd density and bus passenger volumes throughout the day.")
                   )
                 ),
                 br(),
                 
                 
                 # --- Travel Details Section ---
                 h3(
                   tagList(icon("clock", lib = "font-awesome"), "Travel Details"),
                   class = "fw-bold"
                 ),
                 fluidRow(
                   column(
                     7,
                     h4("Travel Time to Key Locations", class = "fw-bold"),
                     uiOutput("t4_key_location_note"),
                     tableOutput("t4_key_location_times")
                   ),
                   column(
                     5,
                     h4("Nearest Bus Stops and MRT Stations", class = "fw-bold"),
                     uiOutput("t4_nearest_bus_mrt_note"),
                     tableOutput("t4_nearest_bus_mrt")
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
        ),
        tags$script(HTML("$(function () {$('[data-bs-toggle=\"tooltip\"]').tooltip();})"))
    )
  )
)
