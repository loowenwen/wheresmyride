library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Public Transport Connectivity Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pln_area", "Select PLN Area", choices = NULL),
      actionButton("draw_btn", "Draw Polygon"),
      selectInput("transport_mode", "Transport Mode", choices = c("MRT", "Bus", "Mixed")),
      sliderInput("max_travel_time", "Max Travel Time (mins)", min = 10, max = 120, value = 60)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Connectivity Map", leafletOutput("connectivity_map")),
        tabPanel("Accessibility Scores", plotOutput("score_plot")),
        tabPanel("Congestion Analysis", plotOutput("congestion_plot"))
      )
    )
  )
)