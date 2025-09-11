species <- "coelenterates"
v <- "coel.1.00"
source("setup.R")
source("generate_prediction_cubes.R")

dates <- seq(Sys.Date() - 7, Sys.Date() + 7, by = "days")

generate_forecast <- function(v, dates) {
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
  
  # Covariate cube
  covariate_cube <- generate_covariate_cube(config, dates, scope = "present")
  
  
}