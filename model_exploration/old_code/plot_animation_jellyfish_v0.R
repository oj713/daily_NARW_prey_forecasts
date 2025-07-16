source("setup.R") # loads some more packages, e.g. dplyr, ggplot2, viridis
library(raster)
library(stars)
library(lubridate)

# For my dataset this worked for 180 dates without overloading the memory, 
# but it was a close thing... Partitioning the data processing is a next step

# Specify dates for animation
dates <- seq(as.Date("2015/09/28"), by = "day", length.out = 95)
# every other day!
dates <- dates[seq(1,length(dates),2)]
# Tidymodels workflow object, fitted
fitted_wkf <- readRDS("model_exploration/jelly_test_model_v0.csv.gz")

#' Generates a data cube of prediction values over space and time
#' @param dates Date, vector of dates to include
#' @param workflow Workflow, tidymodels fitted workflow object
#' @param format_stars boolean, should the return format be a stars object?
#' @return either tibble or stars obj with information .pred_TRUE, .pred_FALSE, x, y, date
generate_data_cube <- function(dates, workflow, format_stars = FALSE) {
  
  print("Collecting data...")
  
  ## Copernicus data
  coper_path <- copernicus_path("chfc/GLOBAL_MULTIYEAR_PHY_001_030")
  coper_db <- coper_path |> read_database() |>
    filter(date %in% dates)
  coper_phys <- read_andreas(coper_db, coper_path) 
  coper_path2 <- copernicus_path("world/GLOBAL_MULTIYEAR_BGC_001_029")
  coper_db2 <- coper_path2 |> read_database() |>
    filter(date %in% dates)
  coper_bgc <- read_andreas(coper_db2, coper_path2)
  coper_bgc_resampled <- st_warp(coper_bgc, dest = coper_phys, method = "near")
  
  coper_data <- c(coper_phys, coper_bgc_resampled)
  rm(coper_phys, coper_bgc, coper_bgc_resampled)
  
  ## adding in static bathymetry layer, retrieved from ETOPO
  ## Todo: replace with copernicus bathymetry layer
  bathymetry_tiff <- raster(file.path("/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data", 
                                      "bathymetry/etopo_bathymetry_chfc.tiff"))
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
    dplyr::select(x, y, date, .pred_TRUE)
  
  
  print("Returning...")
  if (format_stars) {
    coper_data <- coper_data |>
      st_as_stars(crs = 4326, dims = c("x", "y", "date"))
  }
  
  coper_data
}

coper_data_cube <- generate_data_cube(dates, fitted_wkf)

frame_dir <- tempdir()
png_path <- file.path(frame_dir, "frame%03d.png")

#' Generates an animated gif of prediction values over time and saves to file.
#' Works by leveraging ImageMagick CLI, inbuilt software, to coalesce a list of 
#' png files saved to a temp directory into an animated gif. 
#' @param data_cube tibble of all data, required cols .pred_TRUE, x, y, date
#' @param output_file str, filename for output 
#' @return TRUE assuming nothing broke ~ using a try catch is todo
data_cube_gif <- function(data_cube, starting_i = 0) {
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
    ggsave(sprintf(png_path, i + starting_i), plot = p, width = 6, height = 5, dpi = 150)
  }
  TRUE
}
data_cube_gif(coper_data_cube, starting_i = 135)
rm(coper_data_cube)
gc()

# Retrieve a list of every frame file in the tmp directory
frame_files <- Sys.glob(file.path(frame_dir, "frame*.png"))
# Collapse the list into a single string separated by spaces
frame_list <- paste(shQuote(frame_files), collapse = " ")

# Using convert provided by ImageMagick CLI tools to combine pngs into gif
gif_output <- "model_exploration/jelly_test2.gif"
convert_command <- sprintf("convert -delay 25 -loop 0 %s %s", frame_list, shQuote(gif_output))
system(convert_command)

# Remove the tmp files
file.remove(frame_files)
