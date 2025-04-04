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
    theme = shinytheme("flatly"),
    title = div(
      img(src = "logo.png", style = "height: 30px"),
    ),
    id = "mainTabs",
    
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
                   icon("map", lib = "font-awesome"), h5("Heatmap Overview"),
                   p("Visualize public transport accessibility across regions.")
                 ), style = "text-decoration: none;")),
                 column(3, actionLink("go_commute", label = tagList(
                   icon("clock", lib = "font-awesome", style = "font-size: 30px;"), h5("Commute Time Estimator"),
                   p("Estimate personalized travel times from your potential home.")
                 ), style = "text-decoration: none;")),
                 column(3, actionLink("go_compare", label = tagList(
                   icon("balance-scale", lib = "font-awesome"), h5("BTO Comparison"),
                   p("Compare two neighborhoods side-by-side by accessibility.")
                 ), style = "text-decoration: none;")),
                 column(3, actionLink("go_insights", label = tagList(
                   icon("chart-line", lib = "font-awesome"), h5("Transport Insights"),
                   p("Uncover congestion zones, hotspots, and service gaps.")
                 ), style = "text-decoration: none;"))
               ),
               br(),
               
               fluidRow(
                 column(12,
                        h4("Explore Upcoming BTO Projects on the Map"),
                        p("Zoom in to see new housing projects and nearby transport nodes."),
                        leafletOutput("bto_map", height = "400px")
                 )
               ),
               br(),
               
               fluidRow(
                 column(4, wellPanel(h4("Top Connected Town"), p("Tampines"), icon("train", lib = "font-awesome"))),
                 column(4, wellPanel(h4("Avg. Travel Time (New Towns)"), p("42 mins"), icon("clock", lib = "font-awesome"))),
                 column(4, wellPanel(h4("MRT Lines Analyzed"), p("6"), icon("subway", lib = "font-awesome")))
               ),
               br(),
               
               fluidRow(
                 column(12, tags$div(style = "background-color: #f5f5f5; padding: 20px; border-radius: 10px; text-align: center;",
                                     h3("Ready to explore your ideal BTO location?"),
                                     actionButton("go_to_map", "Explore Now", class = "btn btn-primary btn-lg")))
               )
             )
    ),
    
    tabPanel("Overview", value = "heatmap",
             div(style = "height: 100vh; display: flex; flex-direction: column;",
                 fluidRow(style = "flex-grow: 1;",
                          column(6,
                                 div(style = "display: flex; flex-direction: column; height: 100%;",
                                     sidebarLayout(
                                       sidebarPanel(style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; height: 250px; flex-shrink: 0;",
                                                    textInput("postal_code", "Enter Postal Code", value = "", placeholder = "e.g., 123456"),
                                                    sliderInput("search_radius", "Search Radius (meters)", min = 100, max = 2000, value = 500, step = 100),
                                                    actionButton("search_location", "Search", class = "btn-primary", style = "width: 100%;"),
                                                    h4("Summary Statistics", style = "color: #2C3E50; margin-top: 20px;"),
                                                    verbatimTextOutput("location_summary")
                                       ),
                                       mainPanel(style = "flex-grow: 1; display: flex;",
                                                 div(style = "flex-grow: 1; position: relative;", leafletOutput("location_map", height = "700px"))
                                       )
                                     )
                                 )
                          ),
                          column(6,
                                 div(style = "display: flex; flex-direction: column; height: 100%;",
                                     div(style = "margin-bottom: 10px; height: 80px; flex-shrink: 0;",
                                         selectInput("density_map_type", "Density Map of:", choices = c("MRT Stations" = "mrt", "Bus Stops" = "bus"))
                                     ),
                                     div(style = "flex-grow: 1; position: relative;",
                                         conditionalPanel(condition = "input.density_map_type == 'mrt'",
                                                          leafletOutput("mrt_station_density_map", height = "610px")),
                                         conditionalPanel(condition = "input.density_map_type == 'bus'",
                                                          leafletOutput("bus_stop_density_map", height = "610px"))
                                     )
                                 )
                          )
                 )
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
                       tabPanel("Travel Time Map", value = "commute",
                                h3("Isochrone Visualization", style = "font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: #023047;"),
                                leafletOutput("t2_isochrone_map", height = 500)
                       )
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
