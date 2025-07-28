source("data_preparation/derive_calculated_variables.R")


test_df <- data.frame(x = c(1, 2, 3, 4, 5, 6), 
                      val = c(NA, 2, NA, NA, 4, 5))

predictable_indices <- complete.cases(test_df)

replacement_df <- data.frame(x = c(2, 5, 6),
                             val = c(1, 1, 1))

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
  
  if(verbose) {cat("\r Predicting... 0 %")}
  
  # Calculates predictions from a single workflow
  get_wkf_column <- function(wkf, idx) {
    res <- predict(wkf, data[predictable_indices,], type = "prob") |>
      select(.pred_1)
    
    if (verbose) {
      cat("\r Predicting...", round((idx)/n_folds, 2) * 100, "%")
    }
    
    res
  }
  
  pred_quantiles <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages()
  
  if(verbose) {cat("\r Calculating quantiles...")}
  pred_quantiles <- apply(pred_quantiles, 1, 
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

dates <- as.Date(c(
  "2014/12/20", "2015/1/20", "2015/2/20", "2015/3/20", "2015/4/20"
  # "2015/5/20", "2015/6/20", "2015/7/20", "2015/8/20",
  # "2015/9/20", "2015/10/20", "2015/11/20", "2015/3/15"
))

# One layer of the biogeochemical coper stars is 47 MB
# One layer of the physical coper stars is 6 MB

#' Generates prediction data cubes and saves to file
#' @param v str, version
#' @param dates list, Date objects to predict
#' @param save_folder str, name of folder to create and save to. No save if NULL.
#' @param verbose bool, print progression?
#' @return either prediction stars or file storage layout
generate_prediction_cube <- function(v, dates, 
                                     save_folder = NULL, 
                                     verbose = TRUE) {
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
  
  # Physical copernicus data - must replace incorrect NAs in mlotst
  coper_phys <- ci_phys |>
    get_coper_stars(dates) |>
    correct_andreas(replacement_values = list("mlotst" = 700))
  # BGC copernicus data - must warp to match physical data
  coper_bgc <- ci_bgc |>
    get_coper_stars(dates) |>
    st_warp(dest = coper_phys, method = "near")
  
  # Combining into single dataset
  coper_data <- c(coper_phys, coper_bgc)
  coper_data$bathy_depth <- coper_bathy
  rm(coper_phys, coper_bgc)
  gc()
  
  # Converting to tibble, and adding calculated variables
  coper_data <- as_tibble(coper_data)
  coper_data <- (if(length(dates) > 1) {
    mutate(coper_data, date = as.Date(time)) |>
      select(-time)
  } else {
    mutate(coper_data, date = dates[[1]], .after = lat)
  })
  coper_data <- coper_data |>
    rename(lon = x, lat = y) |>
    mutate(day_of_year = lubridate::yday(date), 
           ind_m2 = -1) |> # Can't be NA bc of apply_quantile_preds
    derive_calculated_variables(config)
  
  # Retrieving predictions
  coper_preds <- apply_quantile_preds(v_wkfs, 
                                      coper_data, 
                                      desired_quants = c(.05, .5, .95), 
                                      verbose = verbose)
  rm(coper_data)
  
  # Converting to stars object
  coper_preds <- coper_preds |>
    st_as_stars(dims = c("lon", "lat", "date"))
  
  ##### Save time!
  if (is.null(save_folder)) {
    return(coper_preds)
  } else {
    main_folder <- v_path(v, "preds", save_folder)
    if (!dir.exists(main_folder)) {dir.create(main_folder)}
    
    save_layer <- function(attribute_name, index) {
      filename <- paste0(paste(c(species, save_folder, 
                                 gsub("%", "", attribute_name)), collapse = "_"), 
                         ".tif")
      write_stars(coper_preds, file.path(main_folder, filename), layer = index)
    }
    
    names(coper_preds) |>
      iwalk(save_layer)
    
    TRUE
  }
}

