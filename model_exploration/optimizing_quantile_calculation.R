species <- "jellyfish"
source("setup.R")
source("generate_data_cubes.R")

v <- "je.0.00"
#### Data Preparation
testdates <- seq(as.Date("2015-07-15"), by = "days", length.out = 10)
config <- read_config(v)
v_wkfs <- get_v_wkfs(v)
ci_phys <- get_coper_info("chfc", "phys")
ci_bgc <- get_coper_info("world", "bgc")
coper_bathy <- read_static(name = "deptho", path = ci_phys$coper_path)

coper_data <- retrieve_dynamic_coper_data(config, testdates, 
                                          ci_phys, ci_bgc, 
                                          diagnose = TRUE)
coper_data$bathy_depth <- coper_bathy

# Converting to tibble, and adding calculated variables
coper_data <- as_tibble(coper_data)
coper_data <- mutate(coper_data, date = testdate, .after = y)
coper_data <- coper_data |>
  rename(lon = x, lat = y) |>
  mutate(day_of_year = lubridate::yday(date), 
         ind_m2 = -1) |> # Can't be NA bc of apply_quantile_preds
  derive_calculated_variables(config)

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



