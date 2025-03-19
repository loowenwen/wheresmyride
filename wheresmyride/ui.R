library(shiny)
library(leaflet)
library(geojsonio)
library(sf)

fluidPage(
  titlePanel("Singapore Master Plan 2019 Subzone Boundary (No Sea)"),
  
  sidebarLayout(
    sidebarPanel(
      ),
    
    mainPanel(
      leafletOutput("map")
      )
    )
  )