#' Calculates quantile predictions for a set of workflows and a dataset
#' @param wkfs workflow set
#' @param data df, data to predict on
#' @param desired_quants numeric, list of percentiles to return
#' @param verbose bool, print progress? 
#' @return df, quantile results
apply_quantile_preds <- function(wkfs, data, desired_quants, verbose = FALSE) {
  n_folds <- length(wkfs)
  # Calculates predictions from a single workflow
  get_wkf_column <- function(wkf, idx) {
    if (verbose) {
      cat("\r", round((idx - 1)/n_folds, 2) * 100, "%")
    }
    predict(wkf, data, type = "prob") |>
      select(.pred_1)
  }
  if(verbose) {cat("\r", "100 %")}
  
  pred_quantiles <- wkfs |>
    imap(get_wkf_column) |>
    bind_cols() |>
    suppressMessages() |>
    apply(1, function(x) quantile(x, probs = desired_quants))
  
  if (length(desired_quants) > 1) {
    pred_quantiles |>
      t() |>
      as_tibble(.name_repair = "unique")
  } else {
    pred_quantiles |>
      as_tibble_col(paste0(desired_quants * 100, "%"))
  }
}
