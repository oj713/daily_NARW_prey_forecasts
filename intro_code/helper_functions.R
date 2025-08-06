#' Saves a quantile stars object to file if desired
#' Saves each layer individually, plus a dimensions object
#' The dimensions attribute must be saved because otherwise the dates band is corrupted
#' @param quantile_stars stars object with quantile attributes: 5%, 50%, etc
#' @param folder_path str, path to folder OR NULL for no save
#' @param filename_prefix str, prefix for file. Ignored if no save
#' @return TRUE if saved successfully, input if no save
write_quantile_stars <- function(quantile_stars, 
                                 folder_path = NULL, filename_prefix = NULL) {
  
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
#' Assumes there may be multiple stars objects in a single directory
#' @param folder_path str, file path to folder with saved quantile stars
#' @return quantile stars read in from file, either named list or single item
read_quantile_stars <- function(folder_path) {
  # Parses all file names and splits up by stars object id
  if (!dir.exists(folder_path)) {stop("Folder does not exist.")}
  tif_files <- list.files(folder_path, pattern = "*.tif")
  tif_files_groups <- split(tif_files, sub("_\\d+.tif", "", tif_files))
  dims_files <- list.files(folder_path, pattern = "*_dimensions.rds")
  names(dims_files) <- sub("_dimensions.rds", "", dims_files)
  
  #' Helper: reads in all stars attribute layers, collapses into object, adds dims
  read_quantile_star <- function(file_prefix) {
    quantile_star <- file.path(folder_path, tif_files_groups[[file_prefix]]) |>
      read_stars()
    # Rearrange layers in increasing order and reformat names to XX% format
    quantile_layers_numeric <- sub(".tif", "", names(quantile_star)) |> as.numeric()
    sort_order <- order(quantile_layers_numeric)
    quantile_star <- quantile_star[sort_order] |>
      setNames(paste0(quantile_layers_numeric[sort_order], "%"))
    
    if (file_prefix %in% names(dims_files)) {
      dims_specs <- readRDS(file.path(folder_path, dims_files[[file_prefix]]))
      st_dimensions(quantile_star) <- dims_specs
    } else {
      warning("Stars object ", file_prefix, " did not save with dimension specifications. Date dimension is likely corrupted.")
    }
    
    quantile_star
  }
  
  # Read in all stars objects from folder
  quantile_stars <- unique(names(tif_files_groups)) |>
    map(read_quantile_star)
  
  if(length(quantile_stars) == 1) {
    quantile_stars <- quantile_stars[[1]]
  }
  
  quantile_stars
}