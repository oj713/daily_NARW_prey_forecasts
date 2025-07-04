source("setup.R") # loads some more packages, e.g. dplyr, ggplot2, viridis
library(raster)
library(stars)
library(lubridate)

# For my dataset this worked for 180 dates without overloading the memory, 
# but it was a close thing... Partitioning the data processing is a next step

# Specify dates for animation
dates <- seq(as.Date("2015/03/15"), by = "day", length.out = 180)
# Tidymodels workflow object, fitted
fitted_wkf <- readRDS("model_exploration/test_model_v0.csv.gz")

#' Generates a data cube of prediction values over space and time
#' @param dates Date, vector of dates to include
#' @param workflow Workflow, tidymodels fitted workflow object
#' @param format_stars boolean, should the return format be a stars object?
#' @return either tibble or stars obj with information .pred_TRUE, .pred_FALSE, x, y, date
generate_data_cube <- function(dates, workflow, format_stars = FALSE) {
  
  print("Collecting data...")
  
  ## Copernicus data
  coper_path <- copernicus_path("nwa/GLOBAL_MULTIYEAR_PHY_001_030")
  coper_db <- coper_path |> read_database() |>
    filter(date %in% dates)
  coper_data <- read_andreas(coper_db, coper_path) 
  
  ## adding in static bathymetry layer, retrieved from ETOPO
  ## Todo: replace with copernicus bathymetry layer
  bathymetry_tiff <- raster(file.path("/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data", 
                                      "bathymetry/etopo_bathymetry.tiff"))
  # Read in template copernicus data and warp bathymetry object to match dims
  coper_template <- read_andreas(coper_db |> filter(date == date[[1]]), coper_path)
  bathy_stars <- st_as_stars(bathymetry_tiff, att = 1, ignore_file = FALSE) |>
    st_warp(coper_template, method = "bilinear", use_gdal = TRUE, no_data_value = 99999) 
  names(bathy_stars) <- "etopo_bathy"
  bathy_stars <- bathy_stars |> # convert from bathymetry to sea depth
    transmute(bathy_depth = ifelse(etopo_bathy <= 0, etopo_bathy * -1, NA))
  
  print("Converting to predictable format...")
  
  ## Conversion to predictable tibble (input for workflow)
  coper_data$bathy_depth <- bathy_stars
  coper_data <- as_tibble(coper_data) |>
    na.omit() |>
    mutate(vel = sqrt(uo^2 + vo^2), 
           date = as.Date(time),
           month = month(time) |> factor(levels = 1:12))
  
  ## Predicting
  print("Predicting...")
  coper_data <- augment(workflow, coper_data, type = "prob") |>
    dplyr::select(x, y, date, .pred_FALSE, .pred_TRUE)
  
  
  print("Returning...")
  if (format_stars) {
    coper_data <- coper_data |>
      st_as_stars(crs = 4326, dims = c("x", "y", "date"))
  }
  
  coper_data
}

coper_data_cube <- generate_data_cube(dates, fitted_wkf)

#' Generates an animated gif of prediction values over time and saves to file.
#' Works by leveraging ImageMagick CLI, inbuilt software, to coalesce a list of 
#' png files saved to a temp directory into an animated gif. 
#' @param data_cube tibble of all data, required cols .pred_TRUE, x, y, date
#' @param output_file str, filename for output 
#' @return TRUE assuming nothing broke ~ using a try catch is todo
data_cube_gif <- function(data_cube, output_file) {
  # Specifying the temp directory where we'll store png files yet to be added 
  # to a gif. Each png file will be named frame001.png, frame002.png, etc.
  frame_dir <- tempdir()
  png_path <- file.path(frame_dir, "frame%03d.png")
  
  # For each date in the data cube, select all data for that date and plot it
  dates <- unique(data_cube$date)
  for (i in seq_along(dates)) {
    d <- dates[i]
    plotd <- data_cube |> filter(date == d)
    
    p <- ggplot(plotd, aes(x = x, y = y, fill = .pred_TRUE)) + 
      geom_raster() + 
      coord_quickmap() +
      scale_fill_viridis(option = "turbo", limits = c(0, 1)) +
      labs(title = d, fill = "Probability", y = "Latitude", x = "Longitude") +
      theme_bw() + 
      theme(legend.position = "bottom")
    
    # Save the created plot as a png to the tmp directory
    ggsave(sprintf(png_path, i), plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Retrieve a list of every frame file in the tmp directory
  frame_files <- Sys.glob(file.path(frame_dir, "frame*.png"))
  # Collapse the list into a single string separated by spaces
  frame_list <- paste(shQuote(frame_files), collapse = " ")

  # Using convert provided by ImageMagick CLI tools to combine pngs into gif
  convert_command <- sprintf("convert -delay 25 -loop 0 %s %s", frame_list, shQuote(output_file))
  system(convert_command)
  
  # Remove the tmp files
  file.remove(frame_files)
  
  TRUE
}

gif_output <- "model_exploration/2015_3_15_halfyear.gif"
data_cube_gif(coper_data_cube, gif_output)
