library(shiny)

fluidPage(
  titlePanel("Where's My Ride?"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "inputname")
      ),
    
    mainPanel(
      textOutput("greeting")
      )
    )
  )