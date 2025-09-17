library(shiny)
library(leaflet)
library(leafem)
species <- "coelenterates"
v <- "coel.1.00"
source("setup.R")
source("io_stars.R")

if (FALSE) {
  ym_stars <- read_quantile_stars(v_pred_path(v, "monthly"))
  m_agg <- aggregate(ym_stars[c("5%", "50%", "95%"),,,], 
                     by = function(d) (lubridate::month(d)), FUN = mean)
  m_agg$Uncertainty <- m_agg$`95%` - m_agg$`5%`
  write_quantile_stars(m_agg, "real_time_forecasts/monthly_averages_plaything.nc")
}

preds <- read_quantile_stars("real_time_forecasts/09_14_2025_to_09_19_2025_plaything.nc")
preds$Uncertainty <- preds$`95%` - preds$`5%`
availableDates <- st_get_dimension_values(preds, "date")
bounds <- st_bbox(preds) |> as.vector()

monthly_averages <- read_quantile_stars("real_time_forecasts/monthly_averages_plaything.nc")

leaflet_plot <- function(stars_obj, paletteColumn) {
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

ui <- fluidPage(
  titlePanel(paste(str_to_title(species), "real time daily forecast")),
  fluidRow(
    column(width = 9, 
           textOutput("dailyPlotTitle"),
           leafletOutput("dailyPlot", height = "90vh")),
    column(width = 3, 
           sliderInput("plotDate",
                       label = "Choose a date",
                       min = availableDates[[1]],
                       max = availableDates[[length(availableDates)]],
                       value = Sys.Date(),
                       ticks = FALSE,
                       timeFormat = "%m/%d/%y"),
           selectInput("plotColumn", 
                       label = "Choose a column",
                       choices = names(preds), 
                       selected = "50%"))
  ),
  fluidRow(
    column(width = 6,
           textOutput("Monthly Average"),
           leafletOutput("monthlyAveragePlot", height = "45vh")),
    column(width = 6, 
           textOutput("Difference from Monthly Average"),
           leafletOutput("comparisonPlot", height = "45vh"))
  )
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
    leaflet_plot(plot_reflayer()[input$plotColumn,,], input$plotColumn)
  })
  
  output$monthlyAveragePlot <- renderLeaflet({
    leaflet_plot(plot_reflayer()["historical_month",,], input$plotColumn)
  })
  
  output$comparisonPlot <- renderLeaflet({
    leaflet_plot(plot_reflayer()["difference",,], "Difference")
  })
}

shinyApp(ui, server)
