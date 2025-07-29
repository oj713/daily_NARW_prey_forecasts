suppressPackageStartupMessages(
  {
    library(stars) # spatial data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(purrr)
    library(sf)
    library(lubridate)
    library(viridis)
    library(copernicus) # Copernicus retrieval
    library(andreas)
    library(bundle) # for saving MLP model types
  })

#' Sets the species for the remainder of the session. 
#' Creates a new set of directories if needed.
#' @param species str, species name
#' @return root path for rest of session
set_root <- function(species) {
  root <- file.path("/mnt/ecocast/projectdata/students/ojohnson/copernicus", species)
  if (!dir.exists(root)) {
    user_input <- readline(paste0("Create new directory for ", species, "? (y/n) "))
    if(user_input != 'y') {
      stop("Species doesn't exist.")
    } else {
      print(paste0("Creating new directory for ", species, "..."))
      dir.create(root)
      dir.create(file.path(root, "input_data/eda_plots"), recursive = TRUE)
      dir.create(file.path(root, "versions"))
    }
  }
  root
}

if (!exists("species")) {
  species <- readline("Need to define 'species' before running setup. Enter species: ")
}
root <- set_root(species)

################## DATA RETRIEVAL AND SAVE HELPERS

#' Saves an eda plot object to file
#' @param plot_obj, obj to save to pdf
#' @param filename str, name of file excluding .pdf
save_eda_ecocast <- function(plot_obj, filename) {
  pdf(file.path(root, "input_data", "eda_plots", paste0(filename, ".pdf")))
  print(plot_obj)
  dev.off()
} 

#' Reads a csv file from the input_data folder
#' @param filename str, filename with extension
#' @return data frame
get_input_data <- function(filename) {
  path <- file.path(root, "input_data", filename)
  if (!file.exists(path)) {
    stop(paste("File doesn't exist: ", path))
  } else {
    readr::read_csv(path, col_types = readr::cols())
  }
}

#' Retrieves a Copernicus meta database & defining information
#' @param region str, region
#' @param type str, type of data. currently nonfunctional.
#' @return named list with copernicus path, date range, bounding box, 
#'  resolution sorted by x/y/starting coord/step, meta database
get_coper_info <- function(region = c("chfc", "nwa", "world")[[1]], 
                           type = c("phys", "bgc", "static")[[1]]) {
  if (type == "static") {
    print("Type not supported.")
    return(FALSE)
  }
  
  name_table = list(
    phys = "GLOBAL_MULTIYEAR_PHY_001_030", 
    bgc = "GLOBAL_MULTIYEAR_BGC_001_029"
  )
  
  coper_path <- file.path(region, name_table[[type]]) |>
    copernicus_path()
  coper_DB <- coper_path |> read_database()
  date_range <- coper_DB$date |> range()
  
  example_layer <- coper_DB[1,] |> read_andreas(coper_path)
  bbox <- st_bbox(example_layer)
  
  list(coper_path = coper_path, 
       date_range = date_range,
       bbox = bbox,
       meta_db = coper_DB)
}

#' Corrects stars objects with incorrect missing values. Intended for use with Copernicus.
#' Assumes that the stars object uses NA to both indicate masked values, i.e. land, and missing values.
#' Assumes that at least half of the attributes are not missing any values. 
#' @param stars_obj stars, stars object to correct. 
#' @param replacement_values list, named list of attribute names and a value to sub in for any missing entries.
#' @return stars object with any incorrect missing values replaced w/ replacement value.
correct_andreas <- function(stars_obj, 
                            replacement_values = list("mlotst" = 700)) {
  na_counts <- sapply(stars_obj, function(x) sum(is.na(x)))
  # this method does assume that at least half of the attributes aren't missing any data
  to_correct <- na_counts != median(na_counts)
  flagged_columns <- names(na_counts[to_correct])
  
  # Return if everything is in order
  if(length(flagged_columns) == 0) {
    return(stars_obj)
  }
  
  # Check that all columns which are missing values have a specified replacement value
  accounted <- flagged_columns %in% names(replacement_values)
  if (!all(accounted)) {
    stop(paste("A column with missing values does not have a replacement value specified. Affected columns:",
               paste(flagged_columns[!accounted], collapse = ", ")))
  }
  
  # Which values are supposed to be NAs? i.e. are land mask
  correct_NAs <- stars_obj |>
    pull(which(!to_correct)[[1]]) |> # attribute with no missing values
    is.na()
  
  # Replacing missing values
  for (flag_col in flagged_columns) {
    # Columns for target value with NA values which aren't supposed to be NA
    is_missing <- is.na(stars_obj[[flag_col]]) & !correct_NAs
    
    stars_obj[[flag_col]][is_missing] <- replacement_values[[flag_col]]
  }
  
  return(stars_obj)
}

####### PREDICTION AND PLOT HELPERS

#' Plots a generic spatial scatterplot of a value
#' @param data df, data to plot
#' @param plot_col str, column name for color attribute
#' @param title str, title
#' @param size dbl, point size
#' @param log_col bool, log the color variable? 
#' @param xy_names str, list of 2 names for lon and lat columns in data
#' @return ggplot2 object
plot_gen <- function(data, plot_col, title = "Plot", 
                     size = .3, log_col = FALSE, 
                     xy_names = c("lon", "lat")) {
  xlim <- data[[xy_names[[1]]]] |> range()
  ylim <- data[[xy_names[[2]]]] |> range()
  
  p <- ggplot(data, aes(x = get(xy_names[[1]]), y = get(xy_names[[2]])))
  
  if (log_col) {
    p <- p + 
      geom_point(aes(col = log(get(plot_col) + 1)), alpha = .7, size = size) + 
      labs(col = paste(plot_col, "(log[x + 1])"))
  } else {
    p <- p + 
      geom_point(aes(col = get(plot_col)), alpha = .7, size = size) + 
      labs(col = plot_col)
  }
  
  p + 
    geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group),
               fill = "lightgray", col = "gray") +
    labs(x = "Longitude", y = "Latitude") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = TRUE) +
    theme_bw() + 
    scale_color_viridis() +
    ggtitle(title)
}

# Returns a named list of variable abbreviations for Copernicus
var_abb <- function() {
  list(bathy_depth = "Bathymetry", 
       mlotst = "Mixed layer depth", 
       thetao = "Surface temperature", 
       bottomT = "Bottom temperature", 
       sob = "Bottom salinity", 
       so = "Surface salinity", 
       vo = "Northward velocity",
       uo = "Westward velocity",
       zos = "Sea surface height",
       chl = "Chlorophyll",
       o2 = "Dissolved Oxygen",
       po4 = "Phosphate", 
       si = "Silicon", 
       no3 = "Nitrate", 
       nppv = "Net Primary Productivity")
}

######## VERSIONING HELPERS

#' Constructs a file path to given version folder
#' 
#' @param v model version
#' @param ... additional path specifiers
#' @return file path to version folder
v_path <- function(v = "sp.0.00", ...) {
  major <- (strsplit(v, '.', fixed = TRUE) |> unlist())[1:2] |>
    paste(collapse = ".")
  file.path(root, "versions", major, v, ...)
}

#' Retrieves a model version from file
#' @param v str, model version
#' @return workflow set
get_v_wkfs <- function(v = "sp.0.00") {
  model_obj <- readRDS(v_path(v, "model", "model_fits.csv.gz"))
  
  if (any(class(model_obj) == "bundle")) {
    model_obj <- unbundle(model_obj)
  }
  
  model_obj
}

#' Retrieves testing data for a model version
#' @param v str, model version
#' @return df, augmented testing results over all folds
get_v_testing <- function(v = "sp.0.00") {
  data <- v_path(v, "model", "testing_results.csv.gz") |>
    readr::read_csv(col_types = readr::cols()) 
  
  mutate(data, across(c("patch", ".pred_class"),
                      ~factor(.x, levels = c("1", "0"))))
}

#' Reads the yaml configuration for the given version
#' 
#' @param v the desired version
#' @return list of configuration values
read_config <- function(v = "sp.0.00") {
  yaml::read_yaml(v_path(v = v, paste0(v, ".yaml")))
}


#' Writes the given configuration to file
#' 
#' @param config the configuration list
#' @param overwrite whether to allow overwrite of existing files
#' @return list of config values
write_config <- function(config, 
                         overwrite = FALSE) {
  v <- config$version
  path = v_path(v)
  if (!dir.exists(path)) {
    print(paste0("Creating directory ", v, "..."))
    dir.create(file.path(path), recursive = TRUE)
    dir.create(file.path(path, "model"))
    dir.create(file.path(path, "preds"))
  }
  
  yaml_file <- file.path(path, paste0(v, ".yaml"))
  if(overwrite == FALSE && file.exists(yaml_file)) {
    stop('Configuration already exists:', version)
  }
  
  yaml::write_yaml(config, yaml_file)
  return(config)
}

#' Retrieves a list of all current versions available for species with notes
review_versions <- function() {
  v_majors <- list.files(file.path(root, "versions"))
  
  get_v_minors <- function(v_major) {
    v_minors <- list.files(file.path(root, "versions", v_major))
    
    v_minors |>
      map(~read_config(.x)$note) |>
      setNames(v_minors)
  }
  
  v_majors |>
    map(get_v_minors) |>
    setNames(v_majors)
}


