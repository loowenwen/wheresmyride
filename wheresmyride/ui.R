library(shiny)
library(leaflet)
library(geojsonio)
library(sf)

fluidPage(
  titlePanel("Polygon Plotter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pln_area", 
                  label = "Select PLN Area", 
                  choices = NULL),
      actionButton("draw_btn", "Draw Polygon")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)