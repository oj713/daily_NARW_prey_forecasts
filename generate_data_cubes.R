source("data_preparation/derive_calculated_variables.R")

#' Calculates quantile predictions for a set of workflows and a dataset
#' Leaves any NA rows in the dataset as is
#' @param wkfs workflow set
#' @param data df, data to predict on
#' @param desired_quants numeric, list of percentiles to return
#' @param verbose bool, print progress? 
#' @return df, quantile results with id columns lon, lat, date
apply_quantile_preds <- function(wkfs, data, desired_quants, verbose = FALSE) {
  n_folds <- length(wkfs)
  predictable_indices <- complete.cases(data)
  
  if(verbose) {cat("\n Predicting... 0 / 5")}
  
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
  # pbapply::pbapply() is an improvement to the base apply() function
  # it shows a progress bar and is about 10% faster for quantile()
  pbapply::pboptions(type = ifelse(verbose, "timer", "none"))
  pred_quantiles <- pbapply(pred_quantiles, 1, 
                          function(x) quantile(x, probs = desired_quants))
  
  if(verbose) {cat("\r Formatting to return...")}
  pred_quantiles <- (if (length(desired_quants) > 1) {
    pred_quantiles |>
      t() |>
      as_tibble(.name_repair = "unique")
  } else {
    pred_quantiles |>
      as_tibble_col(paste0(desired_quants * 100, "%"))
  })
  
  # Adding back empty entries for NA values
  returnable_data <- data |>
    select(lon, lat, date)
  returnable_data[, colnames(pred_quantiles)] <- NA
  returnable_data[predictable_indices, colnames(pred_quantiles)] <- pred_quantiles
  
  returnable_data
}

#' Saves a quantile stars object to file if desired
#' @param quantile_stars stars object with quantile attributes: 5%, 50%, etc
#' @param save_path str, path to folder OR NULL for no save
#' @param filename_prefix str, prefix for file
#' @return TRUE if saved successfully, input if no save
save_quantile_stars <- function(quantile_stars, save_path, filename_prefix) {
  if(is.null(save_path)) {return(quantile_stars)}
  
  # Helper, saves a single layer
  save_layer <- function(quantile_name, index) {
    filename <- paste0(filename_prefix, "_", gsub("%", "", quantile_name), ".tif")
    write_stars(quantile_stars, file.path(save_path, filename), layer = index)
  }
  
  names(quantile_stars) |>
    iwalk(save_layer)
  
  TRUE
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
#' @return either prediction stars object or list of subfolders with success booleans
generate_prediction_cubes <- function(v, dates, 
                                      save_folder = NULL, 
                                      verbose = TRUE, 
                                      max_chunk_size = 92, 
                                      desired_quants = c(0, .05, .25, .5, .75, .95, 1)) {
  # Force save if dates size is too large
  if (length(unlist(dates)) > max_chunk_size & is.null(save_folder)) {
    stop("Must save to file for dates selection larger than max chunk size")
  }
  
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
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
    
    # Creating save path
    save_path <- NULL
    if (!is.null(save_folder)) {
      save_path <- v_path(v, "preds", save_folder, save_subfolder)
      if (!dir.exists(save_path)) {dir.create(save_path, recursive = TRUE)}
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
      coper_data <- (if(length(dates_vec) > 1) {
        mutate(coper_data, date = as.Date(time)) |>
          select(-time)
      } else {
        mutate(coper_data, date = dates_vec, .after = y)
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
      save_quantile_stars(coper_chunk, save_path = chunk_dir, 
          filename_prefix = date_range_to_string(range(dates_chunk), "CHUNK"))
      
      coper_chunk
    }
    
    # Creating data chunks & processing
    coper_preds <- NULL
    chunk_dir <- NULL
    if (length(dates_vec) <= max_chunk_size) {
      # Case 1: we can just process everything in one go
      coper_preds <- generate_prediction_chunk(dates_vec, chunk_dir)
    } else { 
      # Case 2: we have to chunk the data and save backups in case something goes wrong
      dates_chunks <- split(dates_vec, ceiling(seq_along(dates_vec)/max_chunk_size))
      if (!is.null(save_folder)) {
        chunk_dir <- file.path(save_path, "tmp_chunks")
        if (!dir.exists(chunk_dir)) {dir.create(chunk_dir)}
      }
      
      # Process one chunk at a time. If something breaks, allows chunk recovery
      recovery <- tryCatch({
        # Map through and retrieve all chunks, saving intermediary chunks to file
        coper_preds <- dates_chunks |>
          map(~generate_prediction_chunk(.x, chunk_dir))
        
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
                "Saved chunks: ", length(chunk_files)/length(desired_quants), "\n",
                "Saved date range: ", paste(range(saved_dates), collapse = " to "), "\n",
                "Returning recovery information...")
        
        # Returning information necessary for recovery
        recovery_info <- list(
          error = e,
          chunk_dir = chunk_dir, 
          saved_dates = saved_dates,
          unsaved_dates = dates_vec[!(dates_vec %in% saved_dates)]
        )
        
        return (recovery_info)
      })
      
      if (!identical(recovery, "success")) {
        cat("\n Partition error. Returning recovery information. \n")
        return(recovery)
      }
    }
  
    # Save and return stars object
    cat("\n Partition Done! \n")
    save_quantile_stars(
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
#' @param verbose bool, print progress?
#' @return path to save folder
generate_yearly_cubes <- function(v, 
                                  date_start, 
                                  date_end,
                                  verbose = TRUE) {
  
  # Create partitioned date vector
  all_dates <- seq(date_start, date_end, by = "days")
  dates_years <- split(all_dates, lubridate::year(all_dates))
  
  # Create main folder filename
  main_folder <- date_range_to_string(list(date_start, date_end))
  
  res <- generate_prediction_cubes(v, dates_years, save_folder = main_folder, 
                                   verbose = TRUE, max_chunk_size = 92)
  
  if (!all(res)) {
    return(res)
  } else {
    if(verbose) {cat("Success!")}
    return(v_path(v, "preds", main_folder))
  }
}
