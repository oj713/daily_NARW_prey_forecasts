library(shiny)
library(leaflet)
library(leafem)
library(leaflet.extras2)

######################################  Ugly data preparation code, to swap out

#setwd("..")
species <- "coelenterates"
v <- "coel.1.00"
source("setup.R")
source("io_stars.R")


regions_sf <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)

if (FALSE) {
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  ym_stars <- ym_stars[regions_sf]
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,], 
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$Uncertainty <- m_agg$`95%` - m_agg$`5%`
  write_quantile_stars(m_agg, "real_time_forecasts/monthly_averages_plaything.nc")
  rm(regions_sf, ym_stars, m_agg)
}

preds <- read_quantile_stars("real_time_forecasts/09_25_2025_to_09_30_2025_plaything.nc")
preds$Uncertainty <- preds$`95%` - preds$`5%`
availableDates <- st_get_dimension_values(preds, "date")
bounds <- st_bbox(preds) |> as.vector()

monthly_averages <- read_quantile_stars("real_time_forecasts/monthly_averages_plaything.nc")

setwd("real_time_forecasts")

###################################### LEAFLET EDITING HELPER FUNCTIONS

#' Render a basic leaflet map for later data overlay.
#' @return leaflet object with appropriate styling and bounds
create_base_leaflet <- function() {
  leaflet() |>
    addProviderTiles("OpenStreetMap") |>
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) |>
    addMouseCoordinates(
      css = list(
        "position" = "absolute", 
        "right" = ".5em", "top" = ".5em",
        "text-align" = "center",
        "font-size" = "1em",
        "background-color" = "rgb(255, 255, 255, .5)",
        "color" = "black",
        "padding" = ".5em", 
        "border-radius" = ".5em"
      ))
}

#' Add stars data as a raster image to a leaflet object
#' @param proxy, leafletProxy, proxy object to add data to
#' @param stars_obj, stars, stars object to add
#' @param paletteColumn, str, plot column that dictates color palette of raster
#' @return nothing, adds raster image of stars object to leaflet proxy
add_starsdata_to_leaflet <- function(proxy, stars_obj, paletteColumn) {
  colorOptionsObj <- 
    (if (paletteColumn == "Difference") {
      colorOptions(
        palette = colorRampPalette(c("darkorchid4", "darkslateblue", "royalblue3", "deepskyblue2",
                    "white", "goldenrod2", "darkorange1", "orangered3", "red4")),
        domain = c(-0.5, 0.5),
        na.color = "transparent")
    } else {
      colorOptions(
        palette = function(n) viridis(n, option = ifelse(paletteColumn == "Uncertainty", 
                                                         "inferno", "viridis")),
        na.color = "transparent"
    )
  })

  proxy |>
    leafem::addGeoRaster(stars_obj,
                         colorOptions = colorOptionsObj,
                         resolution = 192, autozoom = FALSE,
                         layerId = "active_raster", 
                         imagequery = FALSE)
}

#' UI element block to render a Leaflet Stars in this project
#' @param id str, unique ID for leaflet object
#' @param width int, bootstrap column width 1-12
#' @return bootstrap column with leaflet object
starsLeafletOutput <- function(id, width) {
  column(width = width, class = "leaflet-plot-square",
         textOutput(paste0(id, "Title")),
         leafletOutput(id, height = "100%"))
}

#' Apply a function to all leaflet objects
#' @param fnToApply fn, function accepting leafletProxy that applies an edit
#' @return nothing, applies function to all leaflets in application
applyToLeaflets <- function(fnToApply) {
  c("dailyPlot", "monthlyAveragePlot", "comparisonPlot") |>
    walk(~leafletProxy(.x) |> fnToApply())
}

#' Extract stars values at given coordinate
#' @param stars_obj stars, object from which to extract
#' @param lon dbl, longitude
#' @param lat dbl, latitude
#' @return named value paris of stars attributes & value at specified coordinate
extract_reflayer_values <- function(stars_obj, lon, lat) {
  tryCatch({
    sf_point <- st_sfc(st_point(c(lon, lat)), crs = st_crs(stars_obj))
    values <- st_extract(stars_obj, sf_point) |> as_tibble() |> as.list()
    return(values)
  }, error = function(e) {return(NA)})
}

###################################### BUILDING THE APPLICATION 

ui <- fluidPage(
  includeCSS("www/styling.css"),
  div(class = "header", 
      h2("EcoMon daily species patch forecasts")),
  div(class = "main",
    fluidRow(class = "top-row",
      starsLeafletOutput("dailyPlot", 9),
      column(width = 3, class = "settings-sidebar",
             h3("Options"),
             selectInput("plotSpecies", 
                         label = "Species:",
                         choices = c("Coelenterates", "Other [not implemented]"), 
                         selected = "Coelenterates"),
             sliderInput("plotDate",
                         label = "Date:",
                         min = availableDates[[1]],
                         max = availableDates[[length(availableDates)]],
                         value = Sys.Date(),
                         ticks = FALSE,
                         timeFormat = "%m/%d/%y"),
             selectInput("plotColumn", 
                         label = "Prediction Column:",
                         choices = names(preds), 
                         selected = "50%"),
             uiOutput("selectedCoordInfo"))
    ),
    fluidRow(class = "bottom-row",
      starsLeafletOutput("monthlyAveragePlot", 6),
      starsLeafletOutput("comparisonPlot", 6)
    )),
  div(class = "footer", 
      div("Contact: Omi Johnson"),
      img(src='images/bigelow_logo.svg', alt = "Bigelow Laboratory Logo"))
)

server <- function(input, output, session) {
  # Dynamic plotted values - stars data on display, markers
  plot_reflayer <- reactive({
    req(input$plotDate, input$plotColumn)
    
    preds_subset <- preds[input$plotColumn,,,which(availableDates == input$plotDate), drop = TRUE]
    preds_subset$historical_month <- monthly_averages[input$plotColumn, 
                                                      month(input$plotDate),, 
                                                      drop = TRUE]
    preds_subset$difference <- preds_subset[[input$plotColumn]] - preds_subset$historical_month
    st_crs(preds_subset) <- st_crs(preds)
    preds_subset
  })
  markers <- reactiveValues(data = data.frame())
  active_marker <- reactiveVal(NULL)
  
  # Initialize base leaflet maps & title strings, without data
  output$dailyPlot <- renderLeaflet({ ## Adding leafletSync to this one so zooming is synchronous
    create_base_leaflet() |>
      addLeafletsync(c("dailyPlot", "monthlyAveragePlot", "comparisonPlot"),
                     options = leafletsyncOptions(syncCursor = FALSE))
  })
  output$dailyPlotTitle <- renderText({
    paste(str_to_title(species), input$plotColumn, "predictions: ", input$plotDate)
  })
  output$monthlyAveragePlot <- renderLeaflet({ create_base_leaflet() })
  output$monthlyAveragePlotTitle <- renderText({
    paste("Average", input$plotColumn, "prediction:", month.name[month(input$plotDate)])
  })
  output$comparisonPlot <- renderLeaflet({ create_base_leaflet() })
  output$comparisonPlotTitle <- renderText({
    "Difference from average prediction"
  })
  
  # Replace data when the selected parameters update
  observeEvent(plot_reflayer(), {
    add_starsdata_to_leaflet(leafletProxy("dailyPlot"), 
                    plot_reflayer()[input$plotColumn,,], 
                    input$plotColumn)
    
    add_starsdata_to_leaflet(leafletProxy("monthlyAveragePlot"), 
                    plot_reflayer()["historical_month",,], 
                    input$plotColumn)
    
    add_starsdata_to_leaflet(leafletProxy("comparisonPlot"), 
                    plot_reflayer()["difference",,], 
                    "Difference")
    
    update_all_markers()
  })
  
  #' Updates all markers on the map
  #' @return nothing, iterates through each map & replaces all markers
  update_all_markers <- function() {
    applyToLeaflets(clearMarkers)
    
    if (nrow(markers$data) == 0) {return()}

    for(i in 1:nrow(markers$data)) {
      marker_id <- paste0("marker_", i)
      lon <- markers$data$lon[i]
      lat <- markers$data$lat[i]

      marker_color <- 
        ifelse((!is.null(active_marker()) && active_marker() == i), "red", "blue")
      
      applyToLeaflets(function(proxy) {
        addCircleMarkers(proxy, lon, lat, layerId = marker_id, color = marker_color, 
                         opacity = .7, radius = 6)
      })
    }
  }
  
  ############ HANDLE MAP CLICKS
  leafletClicks <- reactive({
    list(input$dailyPlot_click, input$monthlyAveragePlot_click, input$comparisonPlot_click)
  })
  leafletMarkerClicks <- reactive({
    list(input$dailyPlot_marker_click, input$monthlyAveragePlot_marker_click, input$comparisonPlot_marker_click)
  })
  
  # When a click occurs, identify the latitude, longitude, and marker id (if applicable) 
  # and pass that information to handle_map_click()
  observeEvent(leafletClicks(), {
    # do nothing if no click occurred (instantiation)
    if (all(sapply(leafletClicks(), is.null))) {return()}
    
    marker_click <- Filter(Negate(is.null), leafletMarkerClicks())
    click <- (if (length(marker_click) > 0) {
        marker_click[[1]]
      } else {
        Filter(Negate(is.null), leafletClicks())[[1]]
      })
    
    handle_map_click(click$lng, click$lat, click$id)
  })
  
  #' Updates markers on leaflets based on click information
  #' @param lon dbl, longitude of click
  #' @param lat dbl, latitude of click
  #' @param id str, unique layerId of marker or NULL if no marker clicked
  #' @return nothing, updates markers on all maps
  handle_map_click <- function(lon, lat, id) {
    req(plot_reflayer())
    
    # Don't do anything if lon/lat not valid
    if (st_point(c(lon, lat)) |> st_sfc(crs = 4326) |>
        st_within(regions_sf) |> as.numeric() |> is.na()) {
      return()
    }
    
    # Existing marker
    if (!is.null(id) && startsWith(id, "marker_")) {
      active_marker(sub("marker_", "", id) |> as.numeric())
    } else { # New marker
      new_marker <- data.frame(lon = lon, lat = lat)
      
      if(nrow(markers$data) == 0) {
        markers$data <- new_marker
        active_marker(1)
      } else {
        markers$data <- rbind(markers$data, new_marker)
        active_marker(nrow(markers$data))
      }
    }
    
    update_all_markers()
  }
  
  # Display information about selected marker to the UI
  output$selectedCoordInfo <- renderUI({
    req(plot_reflayer())
    if (is.null(active_marker())) {
      return("Select or create a marker to see information!")
    } else {
      coords <- markers$data[active_marker(),]
      values <- extract_reflayer_values(plot_reflayer(), coords$lon, coords$lat)
      
      return_strings <- c("Longitude" = coords$lon, 
                          "Latitude" = coords$lat, 
                          values[1:(length(values) - 1)]) |>
        imap(~paste0(.y, ": ", round(.x, 3)))
      
      return(div(HTML(paste(return_strings, collapse = "<br>")), 
                 actionButton("deleteSelectedMarker", "Delete Marker")))
    }
  })
  
  # Deletes a marker
  observeEvent(input$deleteSelectedMarker, {
    markers$data <- markers$data[-active_marker(),]
    active_marker(NULL)
    update_all_markers()
  })
}

shinyApp(ui, server)

