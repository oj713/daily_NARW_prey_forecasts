#' Approximates solar declination angle, helper
#' https://www.pveducation.org/pvcdrom/properties-of-sunlight/declination-angle
get_soldecang <- function(day_of_year) {
  -23.45 * cos((360 / 365) * (day_of_year + 10) * pi/180)
}

#' Approximates length of day
#' www.researchgate.net/post/Geometrically_Derived_Formula_for_Day_Length_Based_on_Latitude_and_Time_of_Year
get_day_length <- function(day_of_year, latitude) {
  24/pi * acos(-tan(latitude * pi/180) * tan(get_soldecang(day_of_year) * pi/180))
}

#' Approximates derivative of length of day
get_ddx_day_length <- function(day_of_year, latitude) {
  p1 <- get_day_length(day_of_year, latitude)
  p2 <- get_day_length(day_of_year + 1, latitude)
  return(p2-p1)
}

#' Calculates velocity based on horizontal and vertical currents
get_velocity <- function(uo, vo) {
  sqrt(uo^2 + vo^2)
}

#### WRAPPERS -- take entire dataset as input rather than individual columns

get_day_length_data <- function(data) {
  get_day_length(data$day_of_year, data$lat)
}

get_ddx_day_length_data <- function(data) {
  get_ddx_day_length(data$day_of_year, data$lat)
}

get_velocity_data <- function(data) {
  get_velocity(data$uo, data$vo)
}

#' Derives calculated variables based on version config and appends to dataset
#' @param data df, data to append calculated variables
#' @param config list, version configuration
#' @param vars_override list, if not NULL ignore config & instead calculate these
#' @return df, data with calculated variables appended
derive_calculated_variables <- function(data, config, vars_override = NULL) {
  # Vars to calculate either come from config or are provided manually
  calculated_vars <- (if (is.null(vars_override)) {
    all_vars <- config$training_data$coper_data |> unlist() |> as.vector()
    all_vars[!(all_vars %in% colnames(data))]
  } else {
    vars_override
  })
  
  var_function_map <- list(
    "day_length" = get_day_length_data,
    "ddx_day_length" = get_ddx_day_length_data, 
    "vel" = get_velocity_data)
  
  # If a variable isn't yet accounted for, throw an error
  has_calc_function <- calculated_vars %in% names(var_function_map)
  if (any(!has_calc_function)) {
    stop("1+ variables have no calculation defined: ", 
         paste(calculated_vars[!has_calc_function], collapse = ", "))
  }
  
  # Calculate each variable and append to base_data
  data |>
    bind_cols(calculated_vars |> 
                map(~var_function_map[[.x]](data)) |> 
                setNames(calculated_vars))
}
