suppressPackageStartupMessages(
  {
    library(stars) # spatial data
    library(calanusthreshold) #calanus data
    library(brickman) # brickman data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(purrr)
    library(gridExtra)
    library(grDevices) # for printing tau character to pdf
  })

### DATA HELPERS

# defining local function that will filter the data based on date 
# data must have a "date" column
filter_dates <- function(data, date_start, date_end) {
  if (!is.null(date_start)) {
    data <- filter(data, date >= as.Date(date_start))
  }
  if (!is.null(date_end)) {
    data <- filter(data, date <= as.Date(date_end))
  }
  data
}

### PREDICTION AND PLOT HELPERS
plot_ae <- function(data, plot_col = "patch", title = "Plot", size = .3) {
  ggplot(data, aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    geom_point(aes(col = get(plot_col)), alpha = .7, size = size) +
    coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
    labs(col = plot_col) +
    theme_bw() + 
    ggtitle(title)
}

# Returns a list of variable abbreviations
var_abb <- function() {
  list(Bathy_depth = "Bathymetry", 
       MLD = "Mixed layer depth", 
       SST = "Surface temperature", 
       Tbtm = "Bottom temperature", 
       Sbtm = "Bottom salinity", 
       SSS = "Surface salinity", 
       Vel = "Velocity", 
       month = "Month")
}

# returns a list of monthly variables in Brickman dataset
mon_vars <- function() {
  c("Xbtm", "MLD", "Sbtm", "SSS", "SST", "Tbtm", "U", "V")
}