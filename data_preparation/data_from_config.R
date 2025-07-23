source("data_preparation/calculated_variables_functions.R")

####### CALCULATED VARIABLE WRAPPERS

get_daylength_data <- function(data) {
  get_daylength(data$day_of_year, data$lat)
}

get_ddx_daylength_data <- function(data) {
  get_ddx_daylength(data$day_of_year, data$lat)
}

get_velocity_data <- function(data) {
  get_velocity(data$uo, data$vo)
}

####### MAIN FUNCTION

#' Retrieves data from a yaml configuration 
#' @param config list, yaml configuration for a version
#' @param extra_vars str, optional override list of extra vars to retrieve
#' @return df, input data for a model
data_from_config <- function(config, extra_vars = NULL) {
  coper_conf <- config$training_data$coper_data
  
  base_data <- get_input_data("abund_phys_bgc_bathy_chfc.csv.gz")
  
  # Parsing out which variables are already in the data and which aren't
  all_variables <- c(coper_conf$vars_static, coper_conf$vars_phys, 
                     coper_conf$vars_bgc, coper_conf$vars_time)
  if (!is.null(extra_vars)) {all_variables <- c(all_variables, extra_vars)}
  calculated_vars <- all_variables[!(all_variables %in% colnames(base_data))]
  
  # Calculating variables that are derived from other variables
  var_function_map <- list(
    "day_length" = get_daylength_data,
    "ddx_day_length" = get_ddx_daylength_data, 
    "vel" = get_velocity_data)
  # If a variable isn't yet accounted for, throw an error
  has_calc_function <- calculated_vars %in% names(var_function_map)
  if (all(!has_calc_function)) {
    stop("1+ variables have no calculation defined: ", 
               paste(calculated_vars[!has_calc_function], collapse = ", "))
  }
  # Calculate each variable and append to base_data
  base_data <- base_data |>
    bind_cols(calculated_vars |> 
              map(~var_function_map[[.x]](base_data)) |> 
              setNames(calculated_vars) |> 
              bind_cols())
  
  # Now trim the data to only include the data we want and return
  base_data |> 
    select(lon, lat, date, ind_m2, all_of(all_variables))
}
