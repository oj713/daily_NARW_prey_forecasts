source("data_preparation/derive_calculated_variables.R")

#' Retrieves data from a yaml configuration 
#' @param config list, yaml configuration for a version
#' @param extra_vars str, optional override list of extra vars to retrieve
#' @return df, input data for a model
data_from_config <- function(config, extra_vars = NULL) {
  
  base_data <- get_input_data("abund_phys_bgc_bathy_chfc.csv.gz")
  
  # Parse calculated variables, including extra vars
  all_variables <- config$training_data$coper_data |> unlist() |> as.vector()
  if (!is.null(extra_vars)) {all_variables <- c(all_variables, extra_vars)}
  vars_to_calc <- all_variables[!(all_variables %in% colnames(base_data))]
  
  # Derive calculated variables, patch, and return
  base_data |>
    derive_calculated_variables(config, vars_override = vars_to_calc) |> 
    mutate(patch = (ind_m2 > config$training_data$species_data$threshold$pre) |>
             as.numeric() |> factor(levels = c("1", "0"))) |>
    select(lon, lat, date, patch, ind_m2, all_of(all_variables))
}