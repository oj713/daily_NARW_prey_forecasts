suppressPackageStartupMessages(
  {
    library(stars) # spatial data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(purrr)
    library(sf)
    library(lubridate)
    library(viridis)
    library(copernicus)
    library(andreas)
  })

################## Data Retrieval / Save helpers

#' Saves a plot object to file
#' @param plot_obj, obj to save to pdf
#' @param filename str, name of file excluding .pdf
#' @param root root folder location
save_pdf_ecocast <- function(plot_obj, filename, 
                             root = "/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data") {
  pdf(file.path(root, paste0(filename, ".pdf")))
  print(plot_obj)
  dev.off()
} 

#' Reads a csv file from the input_data folder
#' @param filename str, filename with extension
#' @return data frame
get_input_data <- function(filename) {
  root <- "/mnt/ecocast/projectdata/students/ojohnson/copernicus/input_data"
  readr::read_csv(file.path(root, filename), col_types = readr::cols())
}

#' Retrieves a Copernicus meta database & defining information
#' @param region str, region
#' @param type str, type of data. currently nonfunctional.
#' @return named list with copernicus path, date range, bounding box, 
#'  resolution sorted by x/y/starting coord/step, meta database
get_coper_info <- function(region = c("chfc", "nwa", "world")[[1]], 
                           type = c("phys", "bgc", "static")[[1]]) {
  if (type == "static") {
    print("Type not yet supported.")
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
  
  coper_x <- st_get_dimension_values(example_layer, 'x', center = TRUE)[1:2]
  coper_y <- st_get_dimension_values(example_layer, 'y', center = TRUE)[1:2]
  res_inv_x <- round(1/(coper_x[[2]] - coper_x[[1]])) # 12
  res_inv_y <- round(1/(coper_y[[2]] - coper_y[[1]])) # -12
  
  # For a given axis value, returns the integer corresponding to the index
  # of the geographically nearest axis value in the copernicus dataset
  get_xy_index <- function(val, axis = c("x", "y")[[1]]) {
    diff_from_start <- val - ifelse(axis == "x", coper_x[[1]], coper_y[[1]])
    # index will be how many 1/frac it is from start (plus one)
    round(diff_from_start * ifelse(axis == "x", res_inv_x, res_inv_y)) + 1
  }
  
  resolution <- list(x = list(start = coper_x[[1]], inv_by = res_inv_x), 
                     y = list(start = coper_y[[1]], inv_by = res_inv_y))
  
  list(coper_path = coper_path, 
       date_range = date_range,
       bbox = bbox,
       get_xy_index = get_xy_index,
       meta_db = coper_DB)
}

### PREDICTION AND PLOT HELPERS

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
                     xy_names = c("longitude", "latitude")) {
  p <- ggplot(data, aes(x = get(xy_names[[1]]), y = get(xy_names[[2]]))) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    labs(x = "Longitude", y = "Latitude") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 50), expand = TRUE) +
    theme_bw() + 
    scale_color_viridis() +
    ggtitle(title)
  
  if (log_col) {
    p + 
      geom_point(aes(col = log(get(plot_col) + 1)), alpha = .7, size = size) + 
      labs(col = paste(plot_col, "(log[x + 1])"))
  } else {
    p + 
      geom_point(aes(col = get(plot_col)), alpha = .7, size = size) + 
      labs(col = plot_col)
  }
}

# Returns a named list of variable abbreviations for Copernicus
var_abb <- function() {
  list(bathymetry = "Bathymetry", 
       mlotst = "Mixed layer depth", 
       thetao = "Surface temperature", 
       bottomT = "Bottom temperature", 
       sob = "Bottom salinity", 
       so = "Surface salinity", 
       vo = "Northward velocity",
       uo = "Westward velocity",
       zos = "Sea surface height")
}

#' Uses breadth-first search to replace incorrectly NA-matched indices with 
#' a neighboring non-NA value for column of interest
#' Notes: Assumes that any and all .match_cols values in main_df are also in matching_df
#' `This algorithm should NOT be used if there are many land-matched coordinates, 
#'  far-inland coordinates, or mismatched coordinates right at the edge of the available dataset.
#' 
#' @param na_matched_indices, numeric, indices of rows to replace
#' @param main_df df, dataframe for which to replace missing values
#' @param matching_df df, dataframe with full set of available values for .col
#' @param .col, str, column of interest. Must exist in both main_df and matching_df
#' @param .match_cols, list, named list of longitude/latitude columns using when 
#'  conducting spatial search. Columns must exist in main_df and matching_df
#' @return main_df with .col and .match_cols values updated to avoid NA's in 
#'  .col, as well as new column "search radius" specifying distance in 1/12° units
#'   away from original location
bfs_mismatched_indices <- function(na_matched_indices, 
                                   main_df, matching_df,
                                   .col = "bathy_depth",
                                   .match_cols = list("lon" = "x.12", 
                                                      "lat" = "y.12")) {
  main_df$search_radius <- NA
  
  bfs_mismatched_index <- function(na_matched_index) {
    row <- main_df[na_matched_index,]
    
    if (!is.na(pull(row, .col))) {
      print("This bathymetry value isn't NA, dummy")
      return(row)
    }
    
    ocean_found <- FALSE
    search_radius <- 1
    while(!ocean_found) {
      lon_indices <- (pull(row, .match_cols$lon) - search_radius):
        (pull(row, .match_cols$lon) + search_radius)
      lat_indices <- (pull(row, .match_cols$lat) - search_radius):
        (pull(row, .match_cols$lat) + search_radius)
      
      match_search_tibble <- 
        expand.grid(lon_indices, lat_indices) |>
        left_join(matching_df, by = c("Var1" = .match_cols$lon, 
                                      "Var2" = .match_cols$lat))
      
      first_ocean_row <- which(complete.cases(match_search_tibble))[1]
      
      if (is.na(first_ocean_row)) { # ocean points not yet found
        # widen the search radius
        search_radius <- search_radius + 1
      } else { # ocean points found!
        ocean_found <- TRUE
        row[c(.col, .match_cols$lon, .match_cols$lat)] <- 
          match_search_tibble[first_ocean_row,][c(.col, "Var1", "Var2")]
        row$search_radius <- search_radius
      }
    }
    
    return(row)
  }
  
  updated_rows <- map_dfr(na_matched_indices, bfs_mismatched_index)
  main_df[na_matched_indices,] <- updated_rows
  
  main_df
}

