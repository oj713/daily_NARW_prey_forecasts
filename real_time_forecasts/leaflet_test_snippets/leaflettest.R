library(shiny)
library(leaflet)

buildLeaflet <- function() {
  leaflet() |>
    addTiles() |>
    fitBounds(-70, 30, -20, 70)
}

ui <- fluidPage(
  leafletOutput("uniqueId"), 
  radioButtons("markerSide",
              label = "Where should the marker be?",
              choices = c("Left" , "Right"), 
              selected = "Left")
)

server <- function(input, output, session) {
  output$uniqueId <- renderLeaflet({
    buildLeaflet()
  })
  
  coordinates <- reactive({
    req(input$markerSide)
    
    xCoordinate <- ifelse(input$markerSide == "Left", -60, -30)
    yCoordinate <- sample(30:70, 1)
    
    list(xCoordinate, yCoordinate)
  })
  
  # i think in this case you also could skip coordinates() altogether and just build
  # an observeEvent(input$markerSide)
  observeEvent(coordinates(), {
    leafletProxy("uniqueId") |>
           clearMarkers() |>
           addCircleMarkers(coordinates()[[1]], coordinates()[[2]], color = "blue")
  })
}

shinyApp(ui, server)
