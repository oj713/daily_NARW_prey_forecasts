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

leaflet_plot_general <- function(stars_obj, paletteColumn) {
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
  
  leaflet() |>
    addProviderTiles("OpenStreetMap") |>
    leafem::addGeoRaster(stars_obj,
                         colorOptions = colorOptionsObj,
                         resolution = 192, autozoom = FALSE) |>
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

starsLeafletOutput <- function(id, width, height) {
  column(width = width, 
         div(class = "leaflet-plot-square",
             textOutput(paste0(id, "Title")),
             leafletOutput(id, height = height)))
}

ui <- fluidPage(
  includeCSS("www/styling.css"),
  div(class = "header", 
      h2("EcoMon daily species patch forecasts")),
  div(class = "main",
    fluidRow(
      starsLeafletOutput("dailyPlot", 9, "90vh"),
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
    fluidRow(
      starsLeafletOutput("monthlyAveragePlot", 6, "45vh"),
      starsLeafletOutput("comparisonPlot", 6, "45vh")
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
    leaflet_plot_general(plot_reflayer()[input$plotColumn,,], input$plotColumn)
  })
  output$dailyPlotTitle <- renderText({
    paste(str_to_title(species), input$plotColumn, "predictions: ", input$plotDate)
  })
  
  output$monthlyAveragePlot <- renderLeaflet({
    leaflet_plot_general(plot_reflayer()["historical_month",,], input$plotColumn)
  })
  output$monthlyAveragePlotTitle <- renderText({
    paste("Average", input$plotColumn, "prediction:", month.name[month(input$plotDate)])
  })
  
  output$comparisonPlot <- renderLeaflet({
    leaflet_plot_general(plot_reflayer()["difference",,], "Difference")
  })
  output$comparisonPlotTitle <- renderText({
    "Difference from average prediction"
  })
  
  toListen <- reactive({
    list(input$plotDate,input$plotColumn)
  })
  
  observeEvent(toListen(), {
    leafletProxy("dailyPlot") |>
      addLeafletsync(c("dailyPlot","monthlyAveragePlot", "comparisonPlot"), 
                     options = leafletsyncOptions(syncCursor = FALSE))
  })
}

shinyApp(ui, server)

