source("data_preparation/derive_calculated_variables.R")
library(pbapply)

#' Calculates quantile predictions for a set of workflows and a dataset
#' Leaves any NA rows in the dataset as is
#' @param wkfs workflow set
#' @param data df, data to predict on
#' @param desired_quants numeric, list of percentiles to return
#' @param verbose bool, print progress? 
#'  - Note that quantile progress bar at present cannot be hidden
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

# One layer of the biogeochemical coper stars is 47 MB
# One layer of the physical coper stars is 6 MB
# The amount of memory needed to process one date is approximately 130 MB

#' Generates prediction data cubes and saves to file
#' @param v str, version
#' @param dates list, Date objects to predict OR named, nested list of date objects
#' @param save_folder str, name of folder to create and save to. No save if NULL.
#' @param verbose bool, print progression?
#' @param max_chunk_size int, maximum number of dates to process at a time
#' @return either prediction stars object or list of subfolders with success booleans
generate_prediction_cubes <- function(v, dates, 
                                      save_folder = NULL, 
                                      verbose = TRUE, 
                                      max_chunk_size = 92) {
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
    
    #' Helper: Processes a date vector and returns stars object
    #' @param dates_chunk dates to process
    #' @return stars object of predictions
    generate_prediction_chunk <- function(dates_chunk) {
      if (verbose) {cat("\n Processing chunk: size", length(dates_chunk))}
      
      # Physical copernicus data - must replace incorrect NAs in mlotst
      coper_phys <- ci_phys |>
        get_coper_stars(dates_chunk) |>
        correct_andreas(replacement_values = list("mlotst" = 700, 
                                                  "vo" = 0))
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
      coper_preds <- apply_quantile_preds(v_wkfs, 
                                          coper_data, 
                                          desired_quants = c(0, .05, .25, .5, .75, .95, 1), 
                                          verbose = verbose)
      rm(coper_data)
      
      # Converting to stars object
      coper_preds |>
        st_as_stars(dims = c("lon", "lat", "date"))
    }
    
    # Creating data chunks & processing
    coper_preds <- NULL
    if (length(dates_vec) <= max_chunk_size) {
      coper_preds <- generate_prediction_chunk(dates_vec)
    } else {
      dates_chunks <- split(dates_vec, ceiling(seq_along(dates_vec)/max_chunk_size))
      
      # Process by chunk, combine at the end
      coper_preds <- dates_chunks |>
        map(generate_prediction_chunk)
      
      coper_preds <- Reduce(function(a, b) c(a, b, along = 3), coper_preds) |>
        st_set_dimensions("date", values = dates_vec)
    }
    
    # Saving
    if (is.null(save_folder)) {
      return(coper_preds)
    } else {
      save_path <- ifelse(is.null(save_subfolder), 
                          v_path(v, "preds", save_folder),
                          v_path(v, "preds", save_folder, save_subfolder))
      if (!dir.exists(save_path)) {dir.create(save_path, recursive = TRUE)}
      
      save_layer <- function(attribute_name, index) {
        filename <- 
          paste0(paste(c(species, save_folder, save_subfolder, 
                         gsub("%", "", attribute_name)), collapse = "_"), ".tif")
        write_stars(coper_preds, file.path(save_path, filename), layer = index)
      }
      
      names(coper_preds) |>
        iwalk(save_layer)
      
      TRUE
    }
  }
  
  cat("\n Done!")
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
  dates_chunks <- split(all_dates, lubridate::year(all_dates))
  
  # Create main folder filename
  main_folder <- sprintf("%s_to_%s", 
                         format(date_start, "%m_%d_%Y"), 
                         format(date_end, "%m_%d_%Y"))
  
  res <- generate_prediction_cubes(v, dates_chunks, save_folder = main_folder, 
                                   verbose = TRUE, max_chunk_size = 92)
  
  if (!all(res)) {
    stop("Something went wrong.")
  } else {
    return(v_path(v, "preds", main_folder))
  }
}
