##### GENERATES DAILY FORECAST DATA FOR ALL SPECIES OF INTEREST
###### SHOULD RUN DAILY
species <- ""
source("setup.R")
source("generate_prediction_cubes.R")

# Regions file, as we crop the output
regions_sf <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)
# Dates to forecast
forecast_dates <- seq(Sys.Date() - 3, Sys.Date() + 3, by = "days")

#' Generates a forecast stars object for recent dates
#'  Length of dates must be less than 90 to prevent memory overload
#' @param v str, version
#' @param dates Date, vector of dates to predict
#' @return stars object with quantile predictions
generate_forecast <- function(v, dates) {
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
  
  # Covariate cube
  covariate_cube <- generate_covariate_cube(config, dates, scope = "present", 
                                            crop_sf = regions_sf)
  if (is.null(covariate_cube)) {return(NULL)}
  
  coper_preds <- apply_quantile_preds(v_wkfs, 
                                      covariate_cube, 
                                      desired_quants = c(0.05, .5, .95), 
                                      verbose = FALSE, 
                                      na.ignore = TRUE)
  coper_preds$uncertainty <- coper_preds$`95%` - coper_preds$`50%`
  
  coper_preds |>
    st_as_stars(dims = c("lon", "lat", "date")) |>
    st_set_crs(4326)
}

##### MAIN LOOP

shiny_species <- list(
  # Jellyfish
  list("spec" = "coelenterates", "v" = "coel.1.00"), 
  list("spec" = "salpa", "v" = "salp.1.00"),
  list("spec" = "siphonophora", "v" = "siph.1.00"),
  # Calanus
  list("spec" = "pseudocalanus", "v" = "pseu.0.01"),
  list("spec" = "centropages", "v" = "cent.0.01"),
  list("spec" = "cfin", "v" = "cfin.0.01")
)

for (shiny_spec in shiny_species) {
  gc()
  
  species <- shiny_spec$spec
  v <- shiny_spec$v
  root <- get_root(species)
  
  ### Initialize folder setup
  shiny_root <- file.path(root, "shiny_data")
  if (!dir.exists(shiny_root)) {
    dir.create(file.path(shiny_root, "archive"), recursive = TRUE)
  }
  
  # First pull existing shiny forecast object and see what dates we need to predict
  dates_to_calculate <- forecast_dates
  forecast_path <- file.path(shiny_root, paste0(species, "_shiny_forecast_data.nc"))
  old_forecast_data <- NULL
  if (file.exists(forecast_path)) {
    old_forecast_data <- read_quantile_stars(forecast_path)
    dates_to_calculate <- 
      setdiff(dates_to_calculate, 
              st_get_dimension_values(old_forecast_data, "date"))
  }
  if (length(dates_to_calculate) == 0) {
    cat("Skipping species...", species)
    next
  } # skip species if we don't need to forecast
  
  # Run prediction for new forecast data
  new_forecast_data <- generate_forecast(v, dates_to_calculate)
  
  # Replacing the data read by Shiny Server
  if (!is.null(new_forecast_data)) {
    forecast_data_tosave <- (if (is.null(old_forecast_data)) {
      new_forecast_data
    } else {
      old_dates <- st_get_dimension_values(old_forecast_data, "date")
      dates_to_keep <- which(old_dates %in% forecast_dates)
      
      c(old_forecast_data[,,,dates_to_keep], new_forecast_data, along = "date") |>
        st_set_dimensions("date", values = forecast_dates)
    })
    write_quantile_stars(forecast_data_tosave, forecast_path, as_float = TRUE)
  }
  
  # Adding new data to archive of all past forecasts for daily tool
  if (FALSE) {#!is.null(new_forecast_data)) {
    calculated_dates <- st_get_dimension_values(new_forecast_data, which = "date")
    # Separate files by year
    for (yr in unique(year(calculated_dates))) {
      new_archive_yr <- new_forecast_data[,,,which(year(calculated_dates) == yr)]
      archive_yr_path <- file.path(shiny_root, "archive", 
                                   paste0(species, "_forecast_archive_", yr, ".nc"))
      
      # Either save as new file or append to existing file
      if (!file.exists(archive_yr_path)) {
        write_quantile_stars(new_archive_yr, archive_yr_path, as_float = TRUE)
      } else {
        existing_archive_yr <- read_quantile_stars(archive_yr_path)
        existing_dates <- st_get_dimension_values(existing_archive_yr, which = "date")
        missing_dates <- which(calculated_dates > max(existing_dates))
        
        c(existing_archive_yr, new_archive_yr[,,,missing_dates], along = "date") |>
          st_set_dimensions("date", c(existing_dates, calculated_dates[missing_dates])) |>
          write_quantile_stars(archive_yr_path, as_float = TRUE)
      }
    }
  }
}





