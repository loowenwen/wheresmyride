library(shiny)

function(input, output, session) {
  output$greeting <- renderText({
    paste("hi", input$name)
    })
  }