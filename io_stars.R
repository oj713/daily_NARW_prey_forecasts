########## OLD FUNCTIONS, MULTIFILE .TIF EXTENSION

#' Saves a quantile stars object to file if desired
#' Saves each layer individually, plus a dimensions object
#' @param quantile_stars stars object with quantile attributes: 5%, 50%, etc
#' @param save_path str, path to folder OR NULL for no save
#' @param filename_prefix str, prefix for file. Ignored if no save
#' @return TRUE if saved successfully, input if no save
write_quantile_stars_old <- function(quantile_stars, 
                                     save_path = NULL, filename_prefix = NULL) {
  
  if(is.null(save_path)) {return(quantile_stars)}
  
  # Helper, saves a single layer
  save_layer <- function(quantile_name, index) {
    filename <- paste0(filename_prefix, "_", gsub("%", "", quantile_name), ".tif")
    write_stars(quantile_stars, file.path(save_path, filename), layer = index)
  }
  
  names(quantile_stars) |>
    iwalk(save_layer)
  
  st_dimensions(quantile_stars) |>
    saveRDS(file = file.path(save_path, paste0(filename_prefix, "_dimensions.rds")))
  
  TRUE
}

#' Reads quantile stars objects from file
#' @param folder_path str, file path to folder with saved quantile stars
#' @return quantile stars read in from file, either named list or single item
read_quantile_stars_old <- function(folder_path) {
  # Reads in files and splits up by individual stars object
  if (!dir.exists(folder_path)) {stop("Folder does not exist.")}
  tif_files <- list.files(folder_path, pattern = "*.tif")
  tif_files_groups <- split(tif_files, sub("_\\d+.tif", "", tif_files))
  dims_files <- list.files(folder_path, pattern = "*_dimensions.rds")
  names(dims_files) <- sub("_dimensions.rds", "", dims_files)
  
  #' Helper: reads in all stars objects and collapses into one
  read_quantile_star <- function(file_prefix) {
    quantile_star <- file.path(folder_path, tif_files_groups[[file_prefix]]) |>
      read_stars()
    # Rearrange layers in increasing order and reformat names to XX% format
    quantile_layers_numeric <- sub(".tif", "", names(quantile_star)) |> as.numeric()
    sort_order <- order(quantile_layers_numeric)
    quantile_star <- quantile_star[sort_order] |>
      setNames(paste0(quantile_layers_numeric[sort_order], "%"))
    
    # Add back in date information
    if (file_prefix %in% names(dims_files)) {
      dims_specs <- readRDS(file.path(folder_path, dims_files[[file_prefix]]))
      
      # Have to use different functions depending on if we're adding back the date band or overriding the corrupted one
      is_single_date <- length(st_dimensions(quantile_star)) == 2
      if (is_single_date) {
        quantile_star <- st_redimension(quantile_star, new_dims = dims_specs)
      } else {
        st_dimensions(quantile_star) <- dims_specs
      }
      
    } else {
      warning("Stars object ", file_prefix, " did not save with dimension specifications. Date dimension likely missing or incomplete.")
    }
    
    quantile_star
  }
  
  quantile_stars <- unique(names(tif_files_groups)) |>
    map(read_quantile_star)
  
  if(length(quantile_stars) == 1) {
    quantile_stars <- quantile_stars[[1]]
  }
  
  quantile_stars
}

####### NEW FUNCTIONS, NETCDF EXTENSION

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
  
  for (dim_name in names(dims_values)) {
    # Round values to the nearest (true_offset/2)th, dividing by 2 due to center behavior
    fixed_values <- round(dims_values[[dim_name]]/(true_offset/2)) * true_offset/2
    # Update dimension of stars_obj accordingly
    stars_obj <- st_set_dimensions(stars_obj, dim_name, values = fixed_values)
  }
  
  stars_obj
}

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
  
  suppressWarnings({
    stars::write_mdim(quantile_stars, save_path, as_float = as_float)
  })
  
  TRUE
}

#' Reads quantile stars objects from file
#' Makes call to read_quantile_stars_old if appropriate, to retire eventually
#' @param file_path str, file path to saved quantile stars object/folder
#' @return stars object
read_quantile_stars <- function(file_path) {
  is_folder <- dir.exists(file_path)
  is_file <- file.exists(file_path)
  
  # Is the path valid?
  if (!is_folder && !is_file) {
    stop("File doesn't exist.")
  # Is the path a .nc object to read in? 
  } else if (is_file) {
    if (substr(file_path, nchar(file_path)-2, nchar(file_path)) != ".nc") {
      stop("Incorrect file extension, requires .nc")
    }
    read_mdim(file_path) |>
      recover_interval_dims()
  # Is the path a folder containing multifile TIF stars object (deprecated)?
  } else {
    read_quantile_stars_old(file_path)
  }
}