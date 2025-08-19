species <- "cfin"
source("setup.R")
source("generate_prediction_cubes.R")

v <- "cf.0.00"
#### Data Preparation
testdates <- seq(as.Date("2015-07-15"), by = "days", length.out = 10)
config <- read_config(v)
v_wkfs <- get_v_wkfs(v)

coper_data <- generate_covariate_cube(config, testdates)

### Predicting
n_folds <- length(v_wkfs)
predictable_indices <- complete.cases(coper_data)

# Calculates predictions from a single workflow
get_wkf_column <- function(wkf, idx) {
  cat("\r Predicting...", idx, "/", n_folds)
  predict(wkf, coper_data[predictable_indices,], type = "prob") |>
    select(.pred_1)
}
pred_quantiles <- v_wkfs |>
  imap(get_wkf_column) |>
  bind_cols() |>
  suppressMessages()

#' Calculates time taken to do a thing, average of three runs
#' basically system.time with printing
timeTaken <- function(functionToRun) {
  oneIter <- function(n) {
    cat("\r",  n, "/ 3 ...")
    starttime <- Sys.time()
    functionToRun()
    as.numeric(Sys.time() - starttime)
  }
  1:3 |> sapply(oneIter) |> mean()
}

#### Quantiles
desired_quants <- c(0, .05, .5, .95, 1)

# 1.585 minutes
quant_pbapply <- function() {
  pbapply::pboptions(type = "timer")
  pbapply(pred_quantiles, 1, quantile, probs = desired_quants)
}

# 1.577 minutes
quant_pbapply_quiet <- function() {
  pbapply::pboptions(type = "none")
  pbapply(pred_quantiles, 1, quantile, probs = desired_quants)
}

# 1.637 minutes 
quant_matrix_apply <- function() {
  pred_quantiles |> t() |>
    apply(2, quantile, probs = desired_quants)
}

# 1.543 minutes
quant_matrix_pbapply <- function() {
  pbapply::pboptions(type = "none")
  pred_quantiles |> t() |>
    pbapply(2, quantile, probs = desired_quants)
}

# 1.539 minutes
quant_matrix_col <- function() {
  pbapply::pboptions(type = "none")
  as.matrix(pred_quantiles) |>
    pbapply(1, quantile, probs = desired_quants)
}

library(matrixStats)

# 11.108 seconds (yayyyyyy)
quant_matrixstats_col <- function() {
  t(pred_quantiles) |>
    colQuantiles(probs = desired_quants)
}

##### Alternatives untried
# 
# library(collapse) multithreaded solution
# 
# quant_collapse <- function() {
#   qapply(as.matrix(pred_quantiles), MARGIN = 1, FUN = quantile, probs = desired_quants)
# }
# 
# library(future.apply) parallel processing
# plan(multisession)  # or plan(multicore) on Unix
# 
# quant_future <- function() {
#   future_apply(as.matrix(pred_quantiles), 1, quantile, probs = desired_quants)
# }

