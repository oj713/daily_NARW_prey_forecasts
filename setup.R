suppressPackageStartupMessages(
  {
    library(stars) # spatial data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(purrr)
    library(sf)
    library(lubridate)
    library(viridis)
  })

### Data Retrieval / Save helpers

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


### PREDICTION AND PLOT HELPERS
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

# Returns a list of variable abbreviations
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

