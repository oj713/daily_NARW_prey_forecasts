source("data_preparation/derive_calculated_variables.R")
source("io_stars.R")
library(future) # Parallel processing
library(furrr) # Cleanly leverage parallel processing with purrr-like functions

############ DATA PROCESSING HELPER

#' Calculates quantile predictions for a set of workflows and a dataset
#' Calculates mean, median, max, min as appropriate if num folds ≤ 3
#' Leaves any NA rows in the dataset as is
#' @param wkfs workflow set
#' @param data df, data to predict on
#' @param desired_quants numeric, list of percentiles to return
#'  This param is ignored if num folds ≤ 3
#' @param verbose bool, print progress? 
#' @param na.ignore bool, ignore rows with any NA values? 
#' @param parallel bool, use parallel processing?
#' @param parallel_seed int, seed number for modeling
#' @return df, quantile results with id columns lon, lat, date
apply_quantile_preds <- function(wkfs, data, 
                                 desired_quants = c(0, .05, .5, .95, 1), 
                                 verbose = FALSE, na.ignore = TRUE,
                                 parallel = FALSE,
                                 parallel_seed = 1) {
  n_folds <- length(wkfs)
  predictable_indices <- (if (na.ignore) { complete.cases(data) }
                          else {TRUE})
  
  # The recipe is the same across all workflows: therefore, we can 
  # bake the data just once to save processing time
  wkf_rec <- extract_preprocessor(wkfs[[1]]) |> prep()
  id_vars <- filter(wkf_rec$term_info, role == "ID")$variable 
  # Predefine the matrix for xgboost predictions
  predictable_matrix <- wkf_rec |>
    bake(new_data = data[predictable_indices,]) |>
    select(-all_of(id_vars)) |>
    as.matrix()
  # Extract just the xgboost models from the workflows as well
  models <- map(wkfs, ~extract_fit_parsnip(.x)$fit |>
                  xgboost::xgb.Booster.complete())
  
  # Progress bar can handle parallel processing input
  if(verbose) {cat("\n|", paste(rep(" ", n_folds), collapse = ""), "| Predicting...")}
  
  #' Helper: Calculates predictions from a single model
  get_mod_column <- function(mod, idx, mat) {
    cat("\r|", paste(rep("+", idx), collapse = ""))
    
    predict(mod, xgboost::xgb.DMatrix(mat)) ## Returns vector
  }
  pred_matrix <- models |>
    furrr::future_imap(~get_mod_column(.x, .y, predictable_matrix), 
                       .options = furrr_options(seed = parallel_seed, globals = FALSE))
  pred_matrix <- Reduce(rbind, pred_matrix)
  
  if(verbose) {cat("\r Calculating quantiles...                  ")}
  pred_quantiles <- NULL
  # Different calculation methods for high fold versus fold count ≤ 3
  if (n_folds >= 3) {
    # Calculate quantiles
    if (n_folds == 3) {desired_quants <- c(0, .5, 1)}
    # matrixStats is 10x faster than apply(x, 1, quantile)
    pred_quantiles <- pred_matrix |>
      matrixStats::colQuantiles(probs = desired_quants)
    
  } else if (n_folds <= 2) {
    # Mean (n_folds = 2) OR identity (n_folds = 1)
    desired_quants <- c(.5)
    if (n_folds == 2) {
      pred_quantiles <- rowMeans(t(pred_matrix)) |>
      as_tibble_col()
    }
    colnames(pred_quantiles) <- paste0(desired_quants * 100, "%")
  }
  
  if(verbose) {cat("\r Formatting to return...")}
  
  # Adding back empty entries for NA values
  returnable_data <- data |>
    select(lon, lat, date)
  returnable_data[, colnames(pred_quantiles)] <- NA
  returnable_data[predictable_indices, colnames(pred_quantiles)] <- pred_quantiles
  
  returnable_data
}

########### COPERNICUS RETRIEVAL AND PROCESSING HELPERS

#' Retrieves appropriate dynamic variables from Copernicus
#' Assumes that ≥ 1 physical variable is specified in config.
#' @param config version yaml config
#' @param dates Date, vector of dates
#' @param ci_phys coper_info object, physical copernicus information
#' @param ci_bgc coper_info object, biogeochemical copernicus information
#' @param diagnose bool, allow diagnosis for correct_andreas? 
#' @return 4d stars object
retrieve_dynamic_coper_data <- function(config, dates, ci_phys, ci_bgc, diagnose = FALSE) {
  vars <- config$training_data$coper_data
  if ("vel" %in% vars$vars_phys) {vars$vars_phys <- c(vars$vars_phys, "uo", "vo")}
  if (is.null(vars$vars_phys)) {stop("At least one physical variable must be specified.")}
  
  #' Helper, retrieves stars data for dates and coper info object
  get_coper_stars <- function(coper_info, variables) {
    # Throw error if some requested dates aren't available
    date_available <- dates %in% coper_info$meta_db$date
    if (!all(date_available)) {
      stop("Date(s) unavailable in Copernicus: ", paste(dates[!date_available], collapse = ", "))
    }
    
    coper_info$meta_db |>
      filter(date %in% dates, variable %in% variables) |>
      read_andreas(coper_info$coper_path)
  }
  
  # Extract physical variables and correct if necessary
  coper_phys <- get_coper_stars(ci_phys, vars$vars_phys) |>
    correct_andreas(diagnose = diagnose)
  # Extract biogeochemical variables & warp to match physical
  coper_bgc <- NULL
  if (!is.null(vars$vars_bgc)) {
    coper_bgc <- get_coper_stars(ci_bgc, config$training_data$coper_data$vars_bgc) |>
      st_warp(dest = coper_phys, method = "near")
  }
  
  # Combining into single dataset
  coper_data <- c(coper_phys, coper_bgc)
  rm(coper_phys, coper_bgc)
  gc()
  
  coper_data
}

#' Retrieves static stars variables from/derived from Copernicus
#' Currently only supports bathy_depth and bathy_slope
#' @param config version yaml config
#' @return 3d stars object
retrieve_static_coper_data <- function(config) {
  vars_static <- config$training_data$coper_data$vars_static
  # Adding static variables as appropriate
  coper_static <- NULL
  if ("bathy_depth" %in% vars_static) {
    coper_static$bathy_depth <- 
      read_static(name = "deptho", path = copernicus_path("chfc/GLOBAL_MULTIYEAR_PHY_001_030")) |>
      setNames("bathy_depth")
  }
  if ("bathy_slope" %in% vars_static) {
    coper_static$bathy_slope <- 
      read_stars(get_path_main("_general_data", "bathymetric_slope_terra.tif")) |>
      setNames("bathy_slope")
  }
  
  Reduce(c, coper_static)
}

#' Retrieves a predictable covariate cube for a specific version
#' Not directly used in generate_prediction_cubes, but useful for diagnostics
#' Is duplicating code from generate_prediction_chunk
#' @param config version yaml config
#' @param dates Date, vector of desired dates
#' @return predictable tibble
generate_covariate_cube <- function(config, dates) {
  # Base information
  ci_phys <- get_coper_info("chfc", "phys")
  ci_bgc <- get_coper_info("world", "bgc")
  
  # Retrieving copernicus stars data
  coper_data <- retrieve_dynamic_coper_data(config, dates, 
                                            ci_phys, ci_bgc, 
                                            diagnose = TRUE)
  coper_static <- retrieve_static_coper_data(config)
  for(attribute in names(coper_static)) {
    coper_data[[attribute]] <- coper_static[[attribute]]
  }

  # Converting to tibble and adding calculated variables
  coper_data <- as_tibble(coper_data) |>
    mutate(date = as.Date(time), .after = y)
  coper_data <- coper_data |>
    rename(lon = x, lat = y) |>
    mutate(day_of_year = lubridate::yday(date), 
           ind_m2 = -1) |> # Can't be NA bc of apply_quantile_preds
    derive_calculated_variables(config)
  
  coper_data
}

################# MAIN FUNCTIONS

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
#' @param fold_subset int, if not NULL subset workflows to reduce calculation time
#' @param as_float bool, saving prediction cube values as float or dbl? 
#' @param add bool, adding to existing material saved to file? 
#' @param parallel bool, use parallel processing to speed up predictions?
#'  If FALSE, only unused save_folder names are allowed
#' @return either prediction stars object or list of partitions with success booleans
generate_prediction_cubes <- function(v, dates, 
                                      save_folder = NULL, 
                                      verbose = TRUE, 
                                      max_chunk_size = 92, 
                                      desired_quants = c(0, .05, .25, .5, .75, .95, 1),
                                      fold_subset = NULL,
                                      as_float = FALSE,
                                      add = FALSE, 
                                      parallel = FALSE) {
  
  if (class(max_chunk_size) != "numeric") { stop("max_chunk_size must be an integer") }
  
  save_path <- NULL
  # Are we saving to file? 
  if (!is.null(save_folder)) {
    save_path <- v_path(v, "preds", save_folder)
    # Create directory OR check that it's valid to add to existing directory
    if (dir.exists(save_path)) {
      if (!add) {stop("Unless 'add' is TRUE, must specify new save folder.")}
    } else {
      dir.create(save_path, recursive = TRUE)
    }
  # If we're not saving, check restraints on dates length
  } else if (length(unlist(dates)) > max_chunk_size) {
    stop("Must save to file for dates selection larger than max chunk size.")
  }
  
  config <- read_config(v)
  v_wkfs <- get_v_wkfs(v)
  # Subset workflows if desired
  if (!is.null(fold_subset) && fold_subset < length(v_wkfs)) {
    v_wkfs <- v_wkfs[1:fold_subset]
  }
  # Coper information
  ci_phys <- get_coper_info("chfc", "phys")
  ci_bgc <- get_coper_info("world", "bgc")
  coper_static <- retrieve_static_coper_data(config)
  
  #' Helper: Processes a single date vector and saves to file
  #' Divides data into smaller chunks as necessary
  #' @param dates_vec dates to save 
  #' @param partition_name name of partition, NULL if no partition
  #' @return TRUE if successful OR stars object if no save
  generate_partition_cube <- function(dates_vec, partition_name = NULL) {
    if (is.null(partition_name)) {partition_name <- "all"}
    if (verbose) {cat("\r Processing partition:", partition_name, 
                      "( n =", length(dates_vec), ")")}
    
    requires_chunking <- length(dates_vec) > max_chunk_size
    
    dates_to_calculate <- dates_vec
    recovered_chunks <- NULL
    partition_file_path <- NULL
    tmp_chunks_path <- NULL
    # Are we saving to file?
    if (!is.null(save_path)) {
      # Defining file paths
      file_prefix <- file.path(save_path, 
                               c(species, save_folder, partition_name) |> 
                                 paste(collapse = "_"))
      partition_file_path <- paste0(file_prefix, ".nc")
      tmp_chunks_path <- paste0(file_prefix, "_tmp")
      
      # Has the partition already been processed?
      if (add && file.exists(partition_file_path)) {
        cat("\n Partition already exists. Skipping...")
        return(TRUE)
      # Is the partition only partially processed? 
      } else if (add && dir.exists(tmp_chunks_path)) {
        ##### MORE WORK REQUIRED HERE
        recovered_chunks <- list.files(tmp_chunks_path, full.names = TRUE) |>
          map(read_quantile_stars)
        recovered_dates <- recovered_chunks |>
          map(st_get_dimension_values, which = "date") |> unlist() |> as.Date()
        dates_to_calculate <- dates_vec[!(dates_vec %in% recovered_dates)]
        cat("\n Recovering partition...", length(recovered_chunks), "chunks retrieved.")
      # Calculating from scratch. Initialize chunk directory if needed. 
      } else if (requires_chunking) {
        dir.create(tmp_chunks_path)
      }
    }
    
    #' Helper: Processes a date chunk and returns stars object
    #' Saves chunks to temporary directory if desired
    #' @param dates_chunk dates to process
    #' @param tmp_chunks_path str, path to tmp chunks directory or NULL for no save
    #' @return stars object of predictions
    generate_prediction_chunk <- function(dates_chunk, tmp_chunks_path = NULL) {
      if (verbose) {cat("\n Processing chunk: size", length(dates_chunk))}
      
      coper_data <- retrieve_dynamic_coper_data(config, dates_chunk, 
                                                ci_phys, ci_bgc, 
                                                diagnose = verbose)
      for(attribute in names(coper_static)) {
        coper_data[[attribute]] <- coper_static[[attribute]]
      }
      
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
                                          verbose = verbose, 
                                          parallel = parallel,
                                          parallel_seed = config$model$seed)
      rm(coper_data)
      coper_chunk <- coper_chunk |>
        st_as_stars(dims = c("lon", "lat", "date")) |>
        st_set_crs(4326) # Hardcoded, based on copernicus CRS
      
      # Saving to file if tmp_chunk_path specified
      if (!is.null(tmp_chunks_path)) {
        chunkfile <- range(dates_chunk) |>
          date_range_to_string("CHUNK") |>
          paste0(".nc")
        write_quantile_stars(coper_chunk, 
                             save_path = file.path(tmp_chunks_path, chunkfile), 
                             as_float = as_float)
      }
      
      coper_chunk
    }
    
    coper_preds <- NULL
    # Case 1: we can just process everything in one go
    if (!requires_chunking) {
      coper_preds <- generate_prediction_chunk(dates_to_calculate, NULL)
    # Case 2: we have to chunk the data and save backups in case something goes wrong
    } else { 
      dates_chunks <- split(dates_to_calculate, 
                            ceiling(seq_along(dates_to_calculate)/max_chunk_size))
      
      # Process one chunk at a time. If something breaks, allows chunk recovery
      recovery_info <- tryCatch({
        # Map through and retrieve all chunks, saving intermediary chunks to file
        coper_preds <- dates_chunks |>
          map(~generate_prediction_chunk(.x, tmp_chunks_path))
        coper_preds <- c(recovered_chunks, coper_preds) # adding back recovered chunks
        
        coper_preds <- Reduce(function(a, b) c(a, b, along = 3), coper_preds) |>
          st_set_dimensions("date", values = dates_vec)
        
        0
      }, 
      error = function(e) {
        # Provide information on error and assemble chunk meta stats
        chunk_files <- list.files(path = tmp_chunks_path)
        saved_dates <- chunk_files |> map(string_to_date_range) |>
          unlist() |> as.Date() |> unique()
        saved_date_range <- ifelse(is.null(saved_dates), "none", 
                                   paste(range(saved_dates), collapse = " to "))
        
        message("\nSomething went wrong while processing partition. \n
                Error: ", e, "\n", 
                "Partition name: ", ifelse(is.null(partition_name), "all", partition_name), "\n",
                "Saved chunks: ", length(chunk_files), "\n",
                "Saved date range: ", saved_date_range, "\n",
                "Returning recovery information...")
        
        # Returning information necessary for recovery
        list(
          error = e,
          v = v, 
          save_folder = save_folder, 
          partition_name = partition_name,
          saved_dates = saved_dates,
          dates_chunks = dates_chunks,
          partition_dates = dates_vec
        )
      })
      
      if (!(is.numeric(recovery_info) && recovery_info == 0)) {
        cat("\n Partition error. Returning recovery information. \n")
        return(recovery_info)
      }
    }
    
    # Save and return stars object
    success <- write_quantile_stars(coper_preds, save_path = partition_file_path, 
                                    as_float = as_float)
    if (requires_chunking) { unlink(tmp_chunks_path, recursive = TRUE) }
    
    cat("\n Partition Done! \n")
    success
  }

  #' Appropriate pass date vectors to generate_partition_cube and return
  #' Using tryCatch to ensure that parallel plans are always reset to sequential
  tryCatch({
    if (parallel) {
      # Bump up maximum passable dataset size
      options(future.globals.maxSize = 1.0 * 1e9)  
      plan(multisession, workers = 4)
    }
    
    if (class(dates) == "Date") {
      generate_partition_cube(dates, partition_name = NULL)
    } else {
      dates |>
        imap(generate_partition_cube)
    }
  },
  finally = { # Make sure parallel is turned off, regardless of error or not!
    plan(sequential)
    options(future.globals.maxSize = 500 * 1024 ^ 2) ## 500 MiB, default
  })
}

#' Generates data cubes for a version at yearly resolution
#' Wrapper for generate_prediction_cubes
#' 
#' @param v str, version
#' @param date_start Date, start date
#' @param date_end Date, end date
#' @param save_folder str, filename for main folder. Autogenerated if NULL
#' @param verbose bool, print progress?
#' @param fold_subset int, if not NULL subset workflows to reduce calculation time
#' @param date_downsample int, downsample dates vector integer value or NULL
#' @param as_float bool, save values as float or dbl? 
#' @param add bool, adding to existing material saved to file? 
#' @return path to save folder
generate_yearly_cubes <- function(v, 
                                  date_start, 
                                  date_end,
                                  save_folder = NULL,
                                  verbose = TRUE, 
                                  fold_subset = NULL,
                                  date_downsample = NULL,
                                  as_float = FALSE, 
                                  add = FALSE) {
  
  # Create partitioned date vector
  all_dates <- seq(date_start, date_end, by = "days")
  if (!is.null(date_downsample)) {
    all_dates <- all_dates[seq(1,length(all_dates), date_downsample)]
  }
  dates_years <- split(all_dates, lubridate::year(all_dates))
  
  # Create main folder filename if no override provided
  if (is.null(save_folder)) {
    save_folder <- paste0(
      ifelse(is.null(date_downsample), "daily_resolution", 
            paste0(date_downsample, "day_resolution")),
      ifelse(is.null(fold_subset), "", 
             paste0("_", fold_subset, "fold"))
    )
  }
  
  res <- generate_prediction_cubes(v, dates_years, 
                                   save_folder = save_folder, 
                                   verbose = verbose, 
                                   max_chunk_size = 74, 
                                   fold_subset = fold_subset,
                                   as_float = as_float,
                                   add = add, 
                                   parallel = FALSE)
  
  # Are all entries a TRUE?? 
  if (!all(unlist(res) |> vapply(isTRUE, logical(1)))) {
    return(res) # return results if not
  } else {
    if(verbose) {cat("Success!")}
    return(v_path(v, "preds", save_folder))
  }
}
