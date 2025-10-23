#### THIS IS A HYPOTHETICAL CODE SNIPPET
#### PURPOSE: POTENTIAL REDUCE RUNTIME FOR RENDERING STARS RASTERS BY INSTEAD DRAWING THE MAP AS A SHAPEFILE WITH CUSTOM FILL

###### THIS CODE IS UNTESTED

library(shiny)
library(bslib)
library(leaflet)
library(stars)
library(sf)
library(leafem)
library(dplyr)

# Generate sample stars raster data (replace with your actual data loading)
create_sample_data <- function() {
  # Create a grid
  grid <- st_as_stars(st_bbox(c(xmin = -75, ymin = 40, xmax = -45, ymax = 70)), 
                      dx = 0.1, dy = 0.1)
  
  # Add some sample variables with mostly NAs (80% NAs)
  n_cells <- length(grid[[1]])
  n_data_cells <- round(n_cells * 0.2)  # 20% have data
  
  # Create sample data for different dates and variables
  dates <- c("2023-01-01", "2023-06-01", "2023-12-01")
  variables <- c("temperature", "precipitation", "humidity")
  
  # Initialize with NAs
  temp_data <- precipitation_data <- humidity_data <- array(NA, dim = dim(grid[[1]]))
  
  # Add random data to ~20% of cells
  data_indices <- sample(n_cells, n_data_cells)
  temp_data[data_indices] <- runif(n_data_cells, -10, 30)
  precipitation_data[data_indices] <- runif(n_data_cells, 0, 100)
  humidity_data[data_indices] <- runif(n_data_cells, 20, 90)
  
  # Create stars object with multiple attributes and time dimension
  raster_data <- c(
    st_as_stars(list(temperature = temp_data), dimensions = st_dimensions(grid)),
    st_as_stars(list(precipitation = precipitation_data), dimensions = st_dimensions(grid)),
    st_as_stars(list(humidity = humidity_data), dimensions = st_dimensions(grid))
  )
  
  # Add time dimension
  raster_data <- st_set_dimensions(raster_data, "band", values = dates, names = "time")
  
  return(raster_data)
}

# Function to convert stars raster to polygons (only non-NA cells)
stars_to_polygons <- function(stars_obj, date_val, variable) {
  # Filter by date and variable
  filtered <- stars_obj %>%
    filter(time == date_val) %>%
    select(all_of(variable))
  
  # Convert to polygons, excluding NAs
  polygons <- st_as_sf(filtered, as_points = FALSE, merge = FALSE, na.rm = TRUE)
  
  # Ensure valid geometries
  polygons <- st_make_valid(polygons)
  
  return(polygons)
}

ui <- page_sidebar(
  title = "Raster vs Polygons Performance Comparison",
  
  sidebar = sidebar(
    width = 300,
    
    card(
      card_header("Data Parameters"),
      selectInput("date", "Date:",
                  choices = c("2023-01-01", "2023-06-01", "2023-12-01"),
                  selected = "2023-01-01"),
      
      selectInput("variable", "Variable:",
                  choices = c("temperature", "precipitation", "humidity"),
                  selected = "temperature"),
      
      selectInput("method", "Display Method:",
                  choices = c("Raster" = "raster", "Polygons" = "polygons"),
                  selected = "raster")
    ),
    
    card(
      card_header("Performance Info"),
      verbatimTextOutput("performance_info")
    )
  ),
  
  card(
    card_header("Map Display"),
    leafletOutput("map", height = "600px")
  )
)

server <- function(input, output, session) {
  
  # Load/create sample data
  raster_data <- create_sample_data()
  
  # Reactive for current data selection
  current_data <- reactive({
    list(
      date = input$date,
      variable = input$variable,
      method = input$method
    )
  })
  
  # Convert to polygons when needed
  polygon_data <- reactive({
    req(input$method == "polygons")
    start_time <- Sys.time()
    
    polygons <- stars_to_polygons(raster_data, input$date, input$variable)
    
    end_time <- Sys.time()
    conversion_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    list(
      data = polygons,
      conversion_time = conversion_time,
      n_features = nrow(polygons)
    )
  })
  
  # Initialize base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -60, lat = 55, zoom = 5)
  })
  
  # Update map when parameters change
  observe({
    req(current_data())
    
    start_time <- Sys.time()
    
    if (input$method == "raster") {
      # Filter raster data
      filtered_raster <- raster_data %>%
        filter(time == input$date) %>%
        select(all_of(input$variable))
      
      leafletProxy("map") %>%
        clearGroup("data_layer") %>%
        addGeoRaster(filtered_raster, 
                     colors = viridisLite::viridis(256),
                     group = "data_layer",
                     opacity = 0.8)
      
    } else {
      # Use polygon data
      poly_data <- polygon_data()$data
      
      # Create color palette
      pal <- colorNumeric(viridisLite::viridis(256), 
                          domain = poly_data[[input$variable]], 
                          na.color = "transparent")
      
      leafletProxy("map") %>%
        clearGroup("data_layer") %>%
        addPolygons(data = poly_data,
                    fillColor = ~pal(get(input$variable)),
                    fillOpacity = 0.8,
                    color = "transparent",
                    weight = 0,
                    group = "data_layer",
                    popup = ~paste0(input$variable, ": ", round(get(input$variable), 2)))
    }
    
    end_time <- Sys.time()
    render_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Store performance info
    if (input$method == "polygons") {
      perf_info <- paste0(
        "Method: Polygons\n",
        "Features: ", polygon_data()$n_features, "\n",
        "Conversion time: ", round(polygon_data()$conversion_time, 3), "s\n",
        "Render time: ", round(render_time, 3), "s\n",
        "Total time: ", round(polygon_data()$conversion_time + render_time, 3), "s"
      )
    } else {
      perf_info <- paste0(
        "Method: Raster\n",
        "Render time: ", round(render_time, 3), "s"
      )
    }
    
    output$performance_info <- renderText({
      perf_info
    })
  })
}

shinyApp(ui = ui, server = server)