library(shiny)
library(leaflet)
library(dplyr)
library(purrr)

function(input, output, session) {
  polygon_data <- read.csv("../masterplan.csv")

  observe({
    updateSelectInput(session, "pln_area", choices = unique(polygon_data$pln_area_n))
  })
  
  selected_polygon <- reactive({
    req(input$pln_area) 
    polygon_data %>%
      filter(pln_area_n == input$pln_area)
  })
  
  output$map <- renderLeaflet({
    req(input$draw_btn)  
    polygon_info <- selected_polygon()
    
    if (nrow(polygon_info) > 0) {
      lng <- polygon_info$longitude
      lat <- polygon_info$latitude
      
      popup_info <- paste(names(polygon_info)[-c(1, 2)], ":", 
                          unlist(polygon_info[,-c(1, 2)]), collapse = "<br>")

      leaflet() %>%
        addTiles() %>%
        addPolygons(
          lng = lng,
          lat = lat,
          popup = popup_info,
          color = "blue",
          weight = 2,
          fillColor = "yellow",
          fillOpacity = 0.5
        )
    }
  })
}
