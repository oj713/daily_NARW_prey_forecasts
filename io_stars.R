
####### Helper function for as_float = TRUE objects

#' Recovers interval dimensions for a stars object with numeric dimensions
#' that didn't cleanly convert from fractions to floats
#' @param stars_obj stars, stars object
#' @param true_offset dbl, absolute value of true offset. Always 1/12° for copernicus
#' @return stars, stars object with appropriate dimensions fixed according to true_offset
recover_interval_dims <- function(stars_obj, true_offset = 1/12) {
  # If offset is present for all variables then no fix is needed
  needs_fixing <- data.frame(st_dimensions(stars_obj))$offset == "NA"
  if (!any(needs_fixing)) {return(stars_obj)}
  
  # Extract values of dimensions that 1) need to be fixed and 2) are numeric
  dims_values <- expand_dimensions(stars_obj, center = FALSE)[needs_fixing]
  dims_values <- dims_values[sapply(dims_values, class) == "numeric"]
  crs_orig <- st_crs(stars_obj)
  
  for (dim_name in names(dims_values)) {
    # Round values to the nearest (true_offset/2)th, dividing by 2 due to center behavior
    fixed_values <- round(dims_values[[dim_name]]/(true_offset/2)) * true_offset/2
    # Update dimension of stars_obj accordingly
    stars_obj <- st_set_dimensions(stars_obj, dim_name, values = fixed_values)
  }
  
  st_set_crs(stars_obj, crs_orig)
}


######## Read and write quantile stars objects as NetCDF

#' Saves a quantile stars object to file if desired as netCDF
#' @param quantile_stars stars object with quantile attributes: 5%, 50%, etc
#' @param save_path str, file path to save to OR NULL for no save
#' @param as_float bool, save raster dimensions as floats or doubles? 
#' @return TRUE if saved successfully, input if no save
write_quantile_stars <- function(quantile_stars, 
                                 save_path = NULL, 
                                 as_float = TRUE) {
  
  if (is.null(save_path)) {return(quantile_stars)}
  if (substr(save_path, nchar(save_path)-2, nchar(save_path)) != ".nc") {
    stop("Incorrect file extension, requires .nc")
  }
  if (file.exists(save_path)) { ## Avoid errors due to existing template
    file.remove(save_path)
  }
  
  suppressWarnings({
    stars::write_mdim(quantile_stars, save_path, as_float = as_float)
  })
  
  TRUE
}

#' Reads quantile stars objects from file
#' @param file_path str, file path to saved quantile stars object
#' @return stars object
read_quantile_stars <- function(file_path) {
  if (file.exists(file_path)) {
    if (substr(file_path, nchar(file_path)-2, nchar(file_path)) != ".nc") {
      stop("Incorrect file extension, requires .nc")
    }
    read_mdim(file_path) |>
      recover_interval_dims()
  } else {
    stop("File doesn't exist.")
  }
}