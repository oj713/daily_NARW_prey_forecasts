species <- "jellyfish"
source("setup.R")
source("data_preparation/data_from_config.R")
source("generate_prediction_cubes.R")

v <- "je.0.00"
wkfs <- get_v_wkfs(v)
n_folds <- length(wkfs)

data <- generate_covariate_cube(read_config(v), as.Date(c("2015-07-14", "2015-07-16")))

# Calculates predictions from a single workflow
get_wkf_column <- function(wkf, idx) {
  res <- predict(wkf, data, type = "prob") |>
    select(.pred_1)
  
  if (TRUE) {
    cat("\r Predicting...", idx, "/", n_folds)
  }
  
  res
}
wkf_preds <- wkfs |>
  imap(get_wkf_column) |>
  bind_cols() |>
  suppressMessages()



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
  wkf_preds <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages()
  
  if(verbose) {cat("\r Calculating quantiles...")}
  pred_quantiles <- NULL
  # Different calculation methods for high fold versus fold count ≤ 3
  if (n_folds >= 3) {
    # Calculate quantiles
    if (n_folds == 3) {desired_quants <- c(0, .5, 1)}
    # matrixStats is 10x faster than apply(x, 1, quantile)
    pred_quantiles <- wkf_preds |>
      t() |> 
      matrixStats::colQuantiles(probs = desired_quants)
    
  } else if (n_folds <= 2) {
    # Mean (n_folds = 2) OR identity (n_folds = 1)
    desired_quants <- c(.5)
    if (n_folds == 2) {
      pred_quantiles <- rowMeans(wkf_preds) |>
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