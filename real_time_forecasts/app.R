library(shiny)
library(leaflet)
library(leafem)
library(leaflet.extras2)

setwd("..")
species <- "coelenterates"
v <- "coel.1.00"
source("setup.R")
source("io_stars.R")

if (FALSE) {
  regions_sf <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  ym_stars <- ym_stars[regions_sf]
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,], 
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$Uncertainty <- m_agg$`95%` - m_agg$`5%`
  write_quantile_stars(m_agg, "real_time_forecasts/monthly_averages_plaything.nc")
  rm(regions_sf, ym_stars, m_agg)
}

preds <- read_quantile_stars("real_time_forecasts/09_20_2025_to_09_25_2025_plaything.nc")
preds$Uncertainty <- preds$`95%` - preds$`5%`
availableDates <- st_get_dimension_values(preds, "date")
bounds <- st_bbox(preds) |> as.vector()

monthly_averages <- read_quantile_stars("real_time_forecasts/monthly_averages_plaything.nc")

setwd("real_time_forecasts")

create_base_leaflet <- function() {
  leaflet() |>
    addProviderTiles("OpenStreetMap") |>
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

add_starsdata_to_leaflet <- function(proxy, stars_obj, paletteColumn) {
  colorOptionsObj <- (
    if (paletteColumn == "Difference") {
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
    clearGroup(paletteColumn) |>
    leafem::addGeoRaster(stars_obj,
                         colorOptions = colorOptionsObj,
                         resolution = 192, autozoom = FALSE,
                         group = paletteColumn)
}

starsLeafletOutput <- function(id, width) {
  column(width = width, class = "leaflet-plot-square",
         textOutput(paste0(id, "Title")),
         leafletOutput(id, height = "100%"))
}

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
             div(class = "display-info",
                 "Additional information or plots here based on selected point/other criteria!"))
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
  plot_reflayer <- reactive({
    req(input$plotDate, input$plotColumn)
    
    preds_subset <- preds[input$plotColumn,,,which(availableDates == input$plotDate), drop = TRUE]
    preds_subset$historical_month <- monthly_averages[input$plotColumn, 
                                                      month(input$plotDate),, 
                                                      drop = TRUE]
    preds_subset$difference <- preds_subset[[input$plotColumn]] - preds_subset$historical_month
    
    preds_subset
  })
  
  output$dailyPlot <- renderLeaflet({
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
  
  # Replace data layers as appropriate
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
  })
}

shinyApp(ui, server)

