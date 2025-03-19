library(shiny)
library(leaflet)
library(dplyr)
library(purrr)
library(ggplot2)

server <- function(input, output, session) {
  # Load polygon data
  polygon_data <- read.csv("../data/masterplan.csv")
  
  # Update select input choices
  observe({
    updateSelectInput(session, "pln_area", choices = unique(polygon_data$pln_area_n))
  })
  
  # Reactive selection of polygons
  selected_polygon <- reactive({
    req(input$pln_area) 
    polygon_data %>% filter(pln_area_n == input$pln_area)
  })
  
  # Render connectivity map
  output$connectivity_map <- renderLeaflet({
    req(input$draw_btn)  
    polygon_info <- selected_polygon()
    
    if (nrow(polygon_info) > 0) {
      lng <- polygon_info$longitude
      lat <- polygon_info$latitude
      
      popup_info <- paste(names(polygon_info)[-c(1, 2)], " : ", 
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
  
  # Placeholder for accessibility scores plot
  output$score_plot <- renderPlot({
    ggplot(data.frame(x = 1:10, y = runif(10, 0, 100)), aes(x, y)) +
      geom_line() +
      ggtitle("Accessibility Scores")
  })
  
  # Placeholder for congestion analysis plot
  output$congestion_plot <- renderPlot({
    ggplot(data.frame(x = 1:10, y = runif(10, 0, 50)), aes(x, y)) +
      geom_bar(stat = "identity") +
      ggtitle("Congestion Analysis")
  })
}