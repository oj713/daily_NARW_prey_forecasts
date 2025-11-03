library(shiny)
library(leaflet)
library(leafem)
library(leaflet.extras2)

code_root <- "/mnt/ecocast/projects/students/ojohnson/daily-forecasts"
source(file.path(code_root, "setup.R"))
source(file.path(code_root, "io_stars.R"))

template <- read_quantile_stars(
  get_path_main("coelenterates", "shiny_data", "coelenterates_shiny_forecast_data.nc"))
availableDates <- st_get_dimension_values(template, "date")
bounds <- st_bbox(template) |> as.vector()

reflayer <- template[,,,which(availableDates == Sys.Date()), drop = TRUE]

###################################### LEAFLET EDITING HELPER FUNCTIONS

#' Render a basic leaflet map for later data overlay.
#' @return leaflet object with appropriate styling and bounds
create_base_leaflet <- function() {
  leaflet(options = leafletOptions(minZoom = 4)) |>
    addProviderTiles("OpenStreetMap") |>
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) |>
    setMaxBounds(bounds[1] - 20, bounds[2] - 15, bounds[3] + 20, bounds[4] + 15)
}

#' Add stars data as a raster image to a leaflet object
#' @param proxy, leafletProxy, proxy object to add data to
#' @param stars_obj, stars, stars object to add
#' @return nothing, adds raster image of stars object to leaflet proxy
add_starsdata_to_leaflet <- function(proxy_name, stars_obj) {
  
  paletteSettings <- colorOptions(palette =  function(n) viridis(n, option = "viridis"), 
                                  domain = c(0, 1), na.color = "transparent")

  leafletProxy(proxy_name) |>
    clearControls() |>
    leafem::addGeoRaster(stars_obj,
                         colorOptions = paletteSettings,
                         resolution = 192, autozoom = FALSE,
                         layerId = paste0(proxy_name, "_active_layer"),
                         imagequery = FALSE) 
}

###################################### BUILDING THE APPLICATION 

ui <- fluidPage(
    fluidRow(class = "bottom-row", style = "height:90vh;",
             column(width = 6, style = "height:100%;",
                    leafletOutput("5percentlayer", height = "100%")),
             column(width = 6, style = "height:100%;",
                    leafletOutput("95percentlayer", height = "100%"))
    )
)

server <- function(input, output, session) {
  
  # Initialize base leaflet maps & title strings, without data
  output$`5percentlayer` <- renderLeaflet({ create_base_leaflet() })
  output$`95percentlayer` <- renderLeaflet({ create_base_leaflet() })
  
  # Replace data when the selected parameters update
  observeEvent(reflayer, {
    add_starsdata_to_leaflet("5percentlayer", 
                    reflayer["5%",,,drop=TRUE])
    
    add_starsdata_to_leaflet("95percentlayer", 
                    reflayer["95%",,,drop=TRUE])
  })
}

shinyApp(ui, server)

