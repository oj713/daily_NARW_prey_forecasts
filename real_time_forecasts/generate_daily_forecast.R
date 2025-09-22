species <- "coelenterates"
v <- "coel.1.00"
source("setup.R")
source("generate_prediction_cubes.R")

dates <- seq(Sys.Date() - 2, Sys.Date() + 3, by = "days")

generate_forecast <- function(v, dates) {
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
  
  regions_sf <- read_sf(dsn = "post_prediction/daily_forecasts_regions/daily_forecasts_regions.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  # Covariate cube
  covariate_cube <- generate_covariate_cube(config, dates, scope = "present", 
                                            crop_sf = regions_sf)
  
  coper_preds <- apply_quantile_preds(v_wkfs, 
                                      covariate_cube, 
                                      desired_quants = c(0.05, .5, .95), 
                                      verbose = FALSE, 
                                      na.ignore = TRUE)
  coper_preds |>
    st_as_stars(dims = c("lon", "lat", "date")) |>
    st_set_crs(4326)
}

forecast_obj <- generate_forecast(v, dates)

write_quantile_stars(forecast_obj, 
                     paste0("real_time_forecasts/", date_range_to_string(dates), "_plaything.nc"),
                     as_float = TRUE)
