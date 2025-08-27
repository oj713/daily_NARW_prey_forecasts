source("data_preparation/derive_calculated_variables.R")

#' Retrieves data from a yaml configuration 
#' @param config list, yaml configuration for a version
#' @param extra_vars str, optional override list of extra vars to retrieve
#' @return df, input data for a model
data_from_config <- function(config, extra_vars = NULL) {
  # Read in base dataset from file based on config 
  base_data <- NULL
  ecomon_column <- config$training_data$species_data$ecomon_column
  if (!is.null(ecomon_column)) { # Pulling a species from the broader ecomon dataset
    base_data <- readr::read_csv(get_path_main("_general_data", "ecomon_copernicus_matched.csv.gz"),
                                 col_types = readr::cols())
    if (!(ecomon_column %in% colnames(base_data))) {
      stop("Ecomon column", ecomon_column, "doesn't exist.")
    }
    base_data <- base_data |> rename(ind_m2 = !!sym(ecomon_column))
    
  } else { # Read from a custom save file
    alt_source_file <- paste0(config$training_data$species_data$alt_source, "_copernicus_matched.csv.gz")
    base_data <- get_input_data(alt_source_file)
  }
  
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
