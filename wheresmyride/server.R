library(shiny)
library(leaflet)
library(geojsonio)
library(sf)

function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
    })
  }