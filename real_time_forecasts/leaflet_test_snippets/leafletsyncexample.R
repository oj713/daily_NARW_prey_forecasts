library(shiny)
library(leaflet)
library(leaflet.extras2)

tags$script(HTML("
  var currentView = {};

  function storeView(mapId) {
    var map = $('#' + mapId).data('leaflet-map');
    if (map) {
      currentView[mapId] = {
        center: map.getCenter(),
        zoom: map.getZoom()
      };
    }
  }

  function restoreView(mapId) {
    var map = $('#' + mapId).data('leaflet-map');
    if (map && currentView[mapId]) {
      map.setView(currentView[mapId].center, currentView[mapId].zoom);
    }
  }

  Shiny.addCustomMessageHandler('storeLeafletView', function(mapId) {
    storeView(mapId);
  });

  Shiny.addCustomMessageHandler('restoreLeafletView', function(mapId) {
    restoreView(mapId);
  });
"))
observeEvent(toListen(), {
  session$sendCustomMessage("storeLeafletView", "dailyPlot")
})
observe({
  invalidateLater(500)  # slight delay to allow map render
  session$sendCustomMessage("restoreLeafletView", "dailyPlot")
})


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .map-container {
        display: flex;
        height: 400px;
        gap: 10px;
      }
      .map-box {
        flex: 1;
        border: 1px solid #ccc;
        border-radius: 4px;
      }
    "))
  ),
  div(class = "map-container",
      div(class = "map-box",
          leafletOutput("map1")
      ),
      div(class = "map-box",
          leafletOutput("map2")
      )
  ),
  actionButton("add_markers", "Add markers")
)

server <- function(input, output, session){
  
  output$map1 <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 0, lat = 52, zoom = 6)
  })
  
  output$map2 <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 0, lat = 52, zoom = 6)
  })
  
  leafletProxy("map1") |>
    addLeafletsync(c("map1","map2"))
  
  observeEvent(input$add_markers, {
    for (m in c("map1","map2")){
      lat <- runif(1, 51, 52)
      lng <- runif(1, -1, 1)
      leafletProxy(m) |>
        addMarkers(lng = lng, lat = lat)
    }
    
  })
  
}

shinyApp(ui = ui, server = server)