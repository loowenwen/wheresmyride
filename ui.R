suppressPackageStartupMessages({
  library(leaflet)
  library(plotly)
  library(shiny)
  library(shinyjs)
  library(shinythemes)
})

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
                 
                 # Sidebar Layout
                 sidebarLayout(
                   sidebarPanel(
                     # Dropdown for map type selection
                     selectInput("mapType", "Density map of:", 
                                 choices = c("MRT Stations", "Bus Stops"),
                                 selected = "MRT Stations"),  # Default selection is MRT Stations
                     
                     # Conditional rendering for MRT Line Selection (only shown if MRT Stations is selected)
                     conditionalPanel(
                       condition = "input.mapType == 'MRT Stations'",
                       checkboxGroupInput("mrt_lines", "Select MRT Lines to Display", 
                                          choices = list(
                                            "East-West Line" = "ew_line", 
                                            "North-South Line" = "ns_line",
                                            "Circle Line" = "cc_line",
                                            "North East Line" = "ne_line",
                                            "Downtown Line" = "dt_line",
                                            "Thomson-East Coast Line" = "te_line",
                                            "Punggol LRT Line" = "Punggol_LRT_line",
                                            "Sengkang LRT Line" = "Sengkang_LRT_line",
                                            "Bukit Panjang LRT Line" = "BukitPanjang_LRT_line"
                                          ),
                                          selected = NULL  # No default selection
                       )
                     ),
                     
                     conditionalPanel(
                       condition = "input.mapType == 'Bus Stops'",
                       actionButton("select_all_bus_stops", "Show bus stops in all areas"),
                       checkboxGroupInput("bus_planning_area", "Show bus stops in", 
                                          choices = list(
                                            "Ang Mo Kio" = "ANG MO KIO", 
                                            "Bedok" = "BEDOK", 
                                            "Bishan" = "BISHAN", 
                                            "Boon Lay" = "BOON LAY", 
                                            "Bukit Batok" = "BUKIT BATOK", 
                                            "Bukit Merah" = "BUKIT MERAH", 
                                            "Bukit Panjang" = "BUKIT PANJANG", 
                                            "Bukit Timah" = "BUKIT TIMAH", 
                                            "Central Water Catchment" = "CENTRAL WATER CATCHMENT", 
                                            "Changi" = "CHANGI", 
                                            "Changi Bay" = "CHANGI BAY", 
                                            "Choa Chu Kang" = "CHOA CHU KANG", 
                                            "Clementi" = "CLEMENTI", 
                                            "Downtown Core" = "DOWNTOWN CORE", 
                                            "Geylang" = "GEYLANG", 
                                            "Hougang" = "HOUGANG", 
                                            "Jurong East" = "JURONG EAST", 
                                            "Jurong West" = "JURONG WEST", 
                                            "Kallang" = "KALLANG", 
                                            "Lim Chu Kang" = "LIM CHU KANG", 
                                            "Mandai" = "MANDAI", 
                                            "Marine Parade" = "MARINE PARADE", 
                                            "Marina South" = "MARINA SOUTH", 
                                            "Museum" = "MUSEUM", 
                                            "Newton" = "NEWTON", 
                                            "Novena" = "NOVENA", 
                                            "Orchard" = "ORCHARD", 
                                            "Outram" = "OUTRAM", 
                                            "Pasir Ris" = "PASIR RIS", 
                                            "Paya Lebar" = "PAYA LEBAR", 
                                            "Pioneer" = "PIONEER", 
                                            "Punggol" = "PUNGGOL", 
                                            "Queenstown" = "QUEENSTOWN", 
                                            "River Valley" = "RIVER VALLEY", 
                                            "Rochor" = "ROCHOR", 
                                            "Serangoon" = "SERANGOON", 
                                            "Seletar" = "SELETAR", 
                                            "Sembawang" = "SEMBAWANG", 
                                            "Sengkang" = "SENGKANG", 
                                            "Singapore River" = "SINGAPORE RIVER", 
                                            "Straits View" = "STRAITS VIEW", 
                                            "Sungei Kadut" = "SUNGEI KADUT", 
                                            "Tanglin" = "TANGLIN", 
                                            "Tampines" = "TAMPINES", 
                                            "Toa Payoh" = "TOA PAYOH", 
                                            "Tuas" = "TUAS", 
                                            "Western Water Catchment" = "WESTERN WATER CATCHMENT", 
                                            "Woodlands" = "WOODLANDS", 
                                            "Yishun" = "YISHUN"),
                                          selected = NULL  # Default selection is none
                       )
                     )
                   ),
                   # Main Panel for map rendering
                   mainPanel(
                     
                     # Conditional rendering for MRT Station Density Map
                     conditionalPanel(
                       condition = "input.mapType == 'MRT Stations'", 
                       # Area of highest/lowest density box
                       uiOutput("mrt_density_info"),
                       tags$div(style = "height: 20px;"),
                       leafletOutput("mrt_station_density_map", height = 700)
                     ),
                     
                     # Conditional rendering for Bus Stop Density Map
                     conditionalPanel(
                       condition = "input.mapType == 'Bus Stops'", 
                       # Area of highest/lowest density box
                       uiOutput("bus_density_info"),
                       tags$div(style = "height: 20px;"),
                       leafletOutput("bus_stop_density_map", height = 700)
                     )
                   )
                 )
               ),
               
    # --- BTO Transport Comparison ---
    tabPanel("BTO Transport Comparison", value = "compare",
             sidebarLayout(
               sidebarPanel(
                 h4(tagList(icon("location-dot", lib = "font-awesome"), " Location Input"), style = "font-weight: bold"),
                 
                 selectInput("t3_bto_a", "Select BTO A:", choices = c("Select a BTO Project" = "", upcoming_bto$label)),
                 selectInput("t3_bto_b", "Select BTO B:", choices = c("Select a BTO Project" = "", upcoming_bto$label)),
                 selectInput("t3_bto_c", "Select BTO C:", choices = c("Select a BTO Project" = "", upcoming_bto$label)),
                 selectInput("t3_bto_d", "Select BTO D:", choices = c("Select a BTO Project" = "", upcoming_bto$label)),
                 
                 textInput("t3_destionation_postal", "Enter Destination Postal Code:", placeholder = "e.g., 123456"),
                 
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
                   )
                 ),
                 radioButtons("t3_travel_time_preference", label = NULL,
                              choices = c(
                                "AM Peak (6:30–8:30am)" = "AM_peak",
                                "AM Off-Peak (8:31am–4:59pm)" = "AM_offpeak",
                                "PM Peak (5–7pm)" = "PM_peak",
                                "PM Off-Peak (7pm–)" = "PM_offpeak"
                              ),
                              selected = "AM_peak"
                 ),
                 
                 actionButton("t3_get_comparison", "Get BTO Transport Route Comparison", icon = icon("search"))
               ),
               
               mainPanel(
                 h3(
                   tagList(icon("chart-simple", lib = "font-awesome"), " Route Quality Score"),
                   class = "fw-bold"
                 ),
                 
                 h5("Understanding the Route Quality Factors"),
                 tags$ul(
                   tags$li(
                     icon("tachometer-alt", lib = "font-awesome"), 
                     tags$b(" Trip Speed: "), "How fast will I get there? ",
                     tags$span("This measures overall travel time and route efficiency, favouring options that get you to your destination quicker.")
                   ),
                   tags$li(
                     icon("bus", lib = "font-awesome"),
                     tags$b(" Ride Comfort: "), "How pleasant is the journey? ",
                     tags$span("Considers walking time and number of transfers. Fewer changes and less walking mean higher comfort.")
                   ),
                   tags$li(
                     icon("exclamation-triangle", lib = "font-awesome"),
                     tags$b(" Route Reliability: "), "Will my commute be disrupted? ",
                     tags$span("Evaluates backup options and mode diversity. More alternative routes and transport types improve reliability.")
                   ),
                   tags$li(
                     icon("clock", lib = "font-awesome"),
                     tags$b(" Transport Frequency: "), "How often do the buses or trains come? ",
                     tags$span("Measures expected waiting times based on your selected travel period.")
                   )
                 ),
                 
                 fluidRow(
                   column(6, h4("Radar Chart: BTO A", style = "font-weight: bold"), plotlyOutput("t3_radar_a")),
                   column(6, h4("Radar Chart: BTO B", style = "font-weight: bold"), plotlyOutput("t3_radar_b"))
                 ),
                 fluidRow(
                   column(6, h4("Radar Chart: BTO C", style = "font-weight: bold"), plotlyOutput("t3_radar_c")),
                   column(6, h4("Radar Chart: BTO D", style = "font-weight: bold"), plotlyOutput("t3_radar_d"))
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
                 selectInput("t4_bto_project", "Or Select a BTO Project:",
                             choices = c("Select a BTO Project" = "", upcoming_bto$label)),
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
               fluidRow(
                 column(8,
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
                        )
                 ),
                 column(4,
                        br(),
                        tags$div(
                          img(src = "black_logo.png", width = "80%", style = "margin-bottom: 10px;"))
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
