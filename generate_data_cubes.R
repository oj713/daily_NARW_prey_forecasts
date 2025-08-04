source("data_preparation/derive_calculated_variables.R")

#' Calculates quantile predictions for a set of workflows and a dataset
#' Calculates mean, median, max, min as appropriate if num folds ≤ 3
#' Leaves any NA rows in the dataset as is
#' @param wkfs workflow set
#' @param data df, data to predict on
#' @param desired_quants numeric, list of percentiles to return
#'  This param is ignored if num folds ≤ 3
#' @param verbose bool, print progress? 
#' @param na.ignore bool, ignore rows with any NA values? 
#' @return df, quantile results with id columns lon, lat, date
apply_quantile_preds <- function(wkfs, data, 
                                 desired_quants = c(0, .05, .5, .95, 1), 
                                 verbose = FALSE, na.ignore = TRUE) {
  n_folds <- length(wkfs)
  predictable_indices <- (if (na.ignore) { complete.cases(data) }
                          else {TRUE})
  
  if(verbose) {cat("\n Predicting... 0 /", n_folds)}
  
  # Calculates predictions from a single workflow
  get_wkf_column <- function(wkf, idx) {
    res <- predict(wkf, data[predictable_indices,], type = "prob") |>
      select(.pred_1)
    
    if (verbose) {
      cat("\r Predicting...", idx, "/", n_folds)
    }
    
    res
  }
  pred_quantiles <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages()
  
  if(verbose) {cat("\r Calculating quantiles...")}
  # Different calculation methods for high fold versus fold count ≤ 3
  if (n_folds >= 3) {
    # Calculate quantiles (n_folds ≥ 4) OR sort to min/middle/max (n_folds = 3)
    if (n_folds == 3) {desired_quants <- c(0, .5, 1)}
    function_to_apply <- ifelse(n_folds == 3, sort, function(x) quantile(x, probs = desired_quants))
    
    # pbapply::pbapply() is an improvement to the base apply() function
    # it shows a progress bar and is about 10% faster for quantile()
    pbapply::pboptions(type = ifelse(verbose, "timer", "none"))
    pred_quantiles <- pbapply(pred_quantiles, 1, function_to_apply) |>
      t() |>
      as_tibble(.name_repair = "unique_quiet")
    
  } else if (n_folds <= 2) {
    # Mean (n_folds = 2) OR identity (n_folds = 1)
    desired_quants <- c(.5)
    if (n_folds == 2) {
      pred_quantiles <- rowMeans(pred_quantiles) |>
      as_tibble_col()
    }
  }
  
  if(verbose) {cat("\r Formatting to return...")}
  colnames(pred_quantiles) <- paste0(desired_quants * 100, "%")
  
  # Adding back empty entries for NA values
  returnable_data <- data |>
    select(lon, lat, date)
  returnable_data[, colnames(pred_quantiles)] <- NA
  returnable_data[predictable_indices, colnames(pred_quantiles)] <- pred_quantiles
  
  returnable_data
}

#' Saves a quantile stars object to file if desired
#' Saves each layer individually, plus a dimensions object
#' @param quantile_stars stars object with quantile attributes: 5%, 50%, etc
#' @param save_path str, path to folder OR NULL for no save
#' @param filename_prefix str, prefix for file. Ignored if no save
#' @return TRUE if saved successfully, input if no save
write_quantile_stars <- function(quantile_stars, 
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
read_quantile_stars <- function(folder_path) {
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
    
    if (file_prefix %in% names(dims_files)) {
      dims_specs <- readRDS(file.path(folder_path, dims_files[[file_prefix]]))
      st_dimensions(quantile_star) <- dims_specs
    } else {
      message("Caution: stars object ", file_prefix, " did not save with dimension specifications. Date dimension is likely incomplete.")
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

# One layer of the biogeochemical coper stars is 47 MB
# One layer of the physical coper stars is 6 MB
# The amount of memory needed to process one date is approximately 130 MB

#' Generates prediction data cubes and saves to file
#' @param v str, version
#' @param dates list, Date objects to predict OR named, nested list of date objects
#' @param save_folder str, name of folder to create and save to. No save if NULL.
#' @param verbose bool, print progression?
#' @param max_chunk_size int, maximum number of dates to process at a time
#' @param desired_quants numeric, quantile percentages to calculate
#' @param fold_number int, if not NULL subset workflows to reduce calculation time
#' @param add bool, adding to existing material saved to file? 
#'  If FALSE, only unused save_folder names are allowed
#' @return either prediction stars object or list of subfolders with success booleans
generate_prediction_cubes <- function(v, dates, 
                                      save_folder = NULL, 
                                      verbose = TRUE, 
                                      max_chunk_size = 92, 
                                      desired_quants = c(0, .05, .25, .5, .75, .95, 1),
                                      fold_number = NULL,
                                      add = FALSE) {
  # Force save if dates size is too large
  if (length(unlist(dates)) > max_chunk_size & is.null(save_folder)) {
    stop("Must save to file for dates selection larger than max chunk size.")
  }
  if (!add && !is.null(save_folder) && dir.exists(v_path(v, "preds", save_folder))) {
    stop("Unless 'add' is TRUE, must specify new save folder.")
  }
  
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
  # Subset workflows if desired
  if (!is.null(fold_number) && fold_number < length(v_wkfs)) {
    v_wkfs <- v_wkfs[1:fold_number]
  }
  ci_phys <- get_coper_info("chfc", "phys")
  ci_bgc <- get_coper_info("world", "bgc")
  
  #' Helper, retrieves stars data for dates and coper info object
  get_coper_stars <- function(coper_info, dates) {
    coper_info$meta_db |> filter(date %in% dates) |> 
      read_andreas(coper_info$coper_path)
  }
  
  # Static bathymetry data
  coper_bathy <- read_static(name = "deptho", path = ci_phys$coper_path)
  
  #' Helper: Processes a single date vector and saves to subfolder
  #' Divides data into smaller chunks as necessary
  #' @param dates_vec dates to save 
  #' @param save_subfolder name of subfolder, NULL if no subfolder
  #' @return TRUE if successful OR stars object if no save
  generate_prediction_cube <- function(dates_vec, save_subfolder = NULL) {
    if (verbose) {cat("\r Processing partition:", 
                      ifelse(is.null(save_subfolder), "all", save_subfolder),
                      "( n =", length(dates_vec), ")")}
    
    save_path <- NULL
    recovered_chunks <- NULL
    dates_to_calculate <- dates_vec
    # Are we saving to file??
    if (!is.null(save_folder)) {
      save_path <- v_path(v, "preds", save_folder, save_subfolder)
      # Does the partition folder already exist? 
      if (dir.exists(save_path)) {
        tmp_path <- file.path(save_path, "tmp_chunks")
        # Either initiating recovery mode OR skipping a previously-completed folder
        if (dir.exists(tmp_path)) {
          recovered_chunks <- read_quantile_stars(tmp_path)
          recovered_dates <- recovered_chunks |>
            map(st_get_dimension_values, which = "date") |> unlist() |> as.Date()
          dates_to_calculate <- dates_vec[!(dates_vec %in% recovered_dates)]
          cat("\n Recovering partition...", length(recovered_chunks), "chunks retrieved.")
        } else {
          cat("\n Partition already exists. Skipping...")
          return(TRUE)
        }
      } else {
        dir.create(save_path, recursive = TRUE)
      }
    }
    
    #' Helper: Processes a date chunk and returns stars object
    #' Saves chunks to temporary directory if desired
    #' @param dates_chunk dates to process
    #' @param chunk_dir str, path to tmp chunks directory or NULL for no save
    #' @return stars object of predictions
    generate_prediction_chunk <- function(dates_chunk, chunk_dir = NULL) {
      if (verbose) {cat("\n Processing chunk: size", length(dates_chunk))}
      
      # Physical copernicus data - must replace incorrect NAs in mlotst
      coper_phys <- ci_phys |>
        get_coper_stars(dates_chunk) |>
        correct_andreas(diagnose = verbose)
      
      # BGC copernicus data - must warp to match physical data
      coper_bgc <- ci_bgc |>
        get_coper_stars(dates_chunk) |>
        st_warp(dest = coper_phys, method = "near")
      
      # Combining into single dataset
      coper_data <- c(coper_phys, coper_bgc)
      coper_data$bathy_depth <- coper_bathy
      rm(coper_phys, coper_bgc)
      gc()
      
      # Converting to tibble, and adding calculated variables
      coper_data <- as_tibble(coper_data)
      coper_data <- (if(length(dates_chunk) > 1) {
        mutate(coper_data, date = as.Date(time)) |>
          select(-time)
      } else {
        mutate(coper_data, date = dates_chunk, .after = y)
      })
      coper_data <- coper_data |>
        rename(lon = x, lat = y) |>
        mutate(day_of_year = lubridate::yday(date), 
               ind_m2 = -1) |> # Can't be NA bc of apply_quantile_preds
        derive_calculated_variables(config)
      
      # Retrieving predictions
      coper_chunk <- apply_quantile_preds(v_wkfs, 
                                          coper_data, 
                                          desired_quants = desired_quants, 
                                          verbose = verbose)
      rm(coper_data)
      coper_chunk <- coper_chunk |>
        st_as_stars(dims = c("lon", "lat", "date"))
      
      # Saving to file if chunk_dir specified and returning 
      write_quantile_stars(coper_chunk, save_path = chunk_dir, 
          filename_prefix = date_range_to_string(range(dates_chunk), "CHUNK"))
      
      coper_chunk
    }
    
    # Creating data chunks & processing
    coper_preds <- NULL
    chunk_dir <- NULL
    if (length(dates_vec) <= max_chunk_size) {
      # Case 1: we can just process everything in one go
      coper_preds <- generate_prediction_chunk(dates_to_calculate, chunk_dir)
    } else { 
      # Case 2: we have to chunk the data and save backups in case something goes wrong
      dates_chunks <- split(dates_to_calculate, 
                            ceiling(seq_along(dates_to_calculate)/max_chunk_size))
      if (!is.null(save_folder)) {
        chunk_dir <- file.path(save_path, "tmp_chunks")
        if (!dir.exists(chunk_dir)) {dir.create(chunk_dir)}
      }
      
      # Process one chunk at a time. If something breaks, allows chunk recovery
      recovery <- tryCatch({
        # Map through and retrieve all chunks, saving intermediary chunks to file
        coper_preds <- dates_chunks |>
          map(~generate_prediction_chunk(.x, chunk_dir))
        coper_preds <- c(recovered_chunks, coper_preds) # adding back recovered chunks
        
        coper_preds <- Reduce(function(a, b) c(a, b, along = 3), coper_preds) |>
          st_set_dimensions("date", values = dates_vec)
        
        unlink(chunk_dir, recursive = TRUE) # Delete chunk directory, we don't need it anymore!
      }, 
      error = function(e) {
        # Provide information on error and assemble chunk meta stats
        chunk_files <- list.files(path = chunk_dir)
        saved_dates <- chunk_files |> map(string_to_date_range) |>
          unlist() |> as.Date() |> unique()
        
        message("Something went wrong while processing partition. \n
                Error: ", e, "\n", 
                "Partition name: ", ifelse(is.null(save_subfolder), "all", save_subfolder), "\n",
                "Saved chunks: ", length(chunk_files)/length(desired_quants), "\n",
                "Saved date range: ", paste(range(saved_dates), collapse = " to "), "\n",
                "Returning recovery information...")
        
        # Returning information necessary for recovery
        recovery_info <- list(
          error = e,
          v = v, 
          save_folder = save_folder, 
          save_subfolder = save_subfolder,
          saved_dates = saved_dates,
          failed_chunk = dates_chunk,
          partition_dates = dates_vec
        )
        
        return (recovery_info)
      })
      
      if (recovery != 0) {
        cat("\n Partition error. Returning recovery information. \n")
        return(recovery)
      }
    }
  
    # Save and return stars object
    cat("\n Partition Done! \n")
    write_quantile_stars(
      coper_preds,
      save_path = save_path, 
      filename_prefix = paste(c(species, save_folder, save_subfolder), collapse = "_")
    )
  }

  if (class(dates) == "Date") {
    generate_prediction_cube(dates, save_subfolder = NULL)
  } else {
    dates |>
      imap(generate_prediction_cube)
  }
}

#' Generates data cubes for a version at yearly resolution
#' Wrapper for generate_prediction_cubes
#' 
#' @param v str, version
#' @param date_start Date, start date
#' @param date_end Date, end date
#' @param save_folder str, filename for main folder. Autogenerated if NULL
#' @param verbose bool, print progress?
#' @param fold_number int, if not NULL subset workflows to reduce calculation time
#' @param date_downsample int, downsample dates vector integer value or NULL
#' @param add bool, adding to existing material saved to file? 
#' @return path to save folder
generate_yearly_cubes <- function(v, 
                                  date_start, 
                                  date_end,
                                  save_folder = NULL,
                                  verbose = TRUE, 
                                  fold_number = NULL,
                                  date_downsample = NULL,
                                  add = FALSE) {
  
  # Create partitioned date vector
  all_dates <- seq(date_start, date_end, by = "days")
  if (!is.null(date_downsample)) {
    all_dates <- all_dates[seq(1,length(all_dates), date_downsample)]
  }
  dates_years <- split(all_dates, lubridate::year(all_dates))
  
  # Create main folder filename if no override provided
  if (is.null(main_folder)) {
    save_folder <- paste0(
      ifelse(is.null(date_downsample), "daily_resolution", 
            paste0(date_downsample, "day_resolution")),
      ifelse(is.null(fold_number), "", 
             paste0("_", fold_number, "fold"))
    )
  }
  
  res <- generate_prediction_cubes(v, dates_years, 
                                   save_folder = save_folder, 
                                   verbose = verbose, 
                                   max_chunk_size = 92, 
                                   fold_number = fold_number,
                                   add = add)
  
  # Are all entries a TRUE?? 
  if (!all(unlist(res) |> vapply(isTRUE, logical(1)))) {
    return(res) # return results if not
  } else {
    if(verbose) {cat("Success!")}
    return(v_path(v, "preds", main_folder))
  }
}
